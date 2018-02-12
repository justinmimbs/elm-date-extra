module Date.Extra
    exposing
        ( DateSpec
        , Interval(..)
        , OffsetSpec
        , TimeSpec
        , add
        , calendarDate
        , ceiling
        , clamp
        , compare
        , diff
        , equal
        , equalBy
        , floor
        , fractionalDay
        , fromCalendarDate
        , fromIsoString
        , fromParts
        , fromRataDie
        , fromSpec
        , isBetween
        , local
        , midnight
        , monthNumber
        , monthToNumber
        , numberToMonth
        , numberToWeekday
        , offset
        , offsetFromUtc
        , ordinalDate
        , ordinalDay
        , quarter
        , range
        , time
        , toFormattedString
        , toIsoString
        , toRataDie
        , toUtcFormattedString
        , toUtcIsoString
        , utc
        , weekDate
        , weekNumber
        , weekYear
        , weekdayNumber
        , weekdayToNumber
        )

{-| A `Date` represents a moment in time, encoded by two essential pieces of
information: the number of milliseconds since 1 January 1970 UTC, and the
offset between UTC and the current machine's local time. Extractions of a
`Date` (e.g. `Date.month`, `Date.hour`) return representations of the date in
local time, while `Date.toTime` returns the UTC representation.


# Common Constructors

@docs fromParts, fromCalendarDate, fromIsoString


# Formatted Strings

@docs toFormattedString, toUtcFormattedString, toIsoString, toUtcIsoString


# Operations


## Dates as Atomic Values

@docs equal, compare, isBetween, clamp


## Dates as Composite Values

These functions work on dates within the context of a given interval of time.
@docs Interval, equalBy, add, diff, floor, ceiling, range


# Extractions

@docs monthNumber, quarter, ordinalDay, fractionalDay, weekdayNumber, weekNumber, weekYear, offsetFromUtc


# Detailed Specification

In some cases you may want to specify a date with a time offset or from
week-date or ordinal-date parts. The `fromSpec` function provides a way to
do this.
@docs fromSpec


## DateSpec

@docs DateSpec, calendarDate, weekDate, ordinalDate


## TimeSpec

@docs TimeSpec, midnight, time


## OffsetSpec

@docs OffsetSpec, utc, offset, local


# Rata Die

[Rata Die](https://en.wikipedia.org/wiki/Rata_Die) represents each calendar day
as a number, using 1 for its base date, _1 January 0001_, and increasing by 1
each day. Converting to and from Rata Die uses the local representation of a
`Date`.

    date = Date.fromCalendarDate 2007 Mar 15

    Date.equal
        (date |> Date.toRataDie |> Date.fromRataDie)
        date
    -- True

@docs toRataDie, fromRataDie


# Month and Weekday numbers

@docs monthToNumber, numberToMonth, weekdayToNumber, numberToWeekday

-}

import Date exposing (Date, Day(..), Month(..), day, dayOfWeek, fromTime, hour, millisecond, minute, month, second, toTime, year)
import Date.Facts exposing (daysBeforeMonth, msPerDay, msPerHour, msPerMinute, msPerSecond)
import Date.RataDie as RataDie exposing (RataDie)
import Regex exposing (HowMany(All, AtMost), Regex, regex)


-- Create


unixTimeFromRataDie : RataDie -> Int
unixTimeFromRataDie rd =
    (rd - 719163) * msPerDay


msFromTimeParts : Int -> Int -> Int -> Int -> Int
msFromTimeParts hh mm ss ms =
    msPerHour * hh + msPerMinute * mm + msPerSecond * ss + ms


{-| Represents a day.
-}
type DateSpec
    = DateMS Int


{-| Create a `DateSpec` from calendar-date parts (year, month, day).
-}
calendarDate : Int -> Month -> Int -> DateSpec
calendarDate y m d =
    DateMS <| unixTimeFromRataDie (RataDie.fromCalendarDate y m d)


{-| Create a `DateSpec` from ordinal-date parts (year, ordinalDay).
-}
ordinalDate : Int -> Int -> DateSpec
ordinalDate y od =
    DateMS <| unixTimeFromRataDie (RataDie.fromOrdinalDate y od)


{-| Create a `DateSpec` from week-date parts (weekYear, weekNumber, weekday).
-}
weekDate : Int -> Int -> Day -> DateSpec
weekDate wy wn wd =
    DateMS <| unixTimeFromRataDie (RataDie.fromWeekDate wy wn wd)


{-| Represents a time of day.
-}
type TimeSpec
    = TimeMS Int


{-| Do not specify a time of day (default to 00:00).
-}
midnight : TimeSpec
midnight =
    TimeMS 0


{-| Create a `TimeSpec` from time parts (hour, minute, second, millisecond).
-}
time : Int -> Int -> Int -> Int -> TimeSpec
time hh mm ss ms =
    TimeMS <|
        msFromTimeParts
            (hh |> Basics.clamp 0 23)
            (mm |> Basics.clamp 0 59)
            (ss |> Basics.clamp 0 59)
            (ms |> Basics.clamp 0 999)


{-| Represents a time offset from UTC.
-}
type OffsetSpec
    = Offset Int
    | Local


{-| Use no offset.
-}
utc : OffsetSpec
utc =
    Offset 0


{-| Use a specific offset from UTC, given in minutes.
-}
offset : Int -> OffsetSpec
offset =
    Offset


{-| Use the local offset.
-}
local : OffsetSpec
local =
    Local


{-| Create a `Date` from a specified day, time of day, and time offset.

    Date.fromSpec
        (calendarDate 2000 Jan 1)
        midnight
        local
    -- <1 January 2000, local time>

    Date.fromSpec
        (weekDate 2009 1 Mon)
        midnight
        utc
    -- <29 December 2008, UTC>

    Date.fromSpec
        (ordinalDate 2016 218)
        (time 20 0 0 0)
        (offset -180)
    -- <5 August 2016, 23:00, UTC>

When a `Date` is created with a specified time offset (e.g. `offset -180`),
its extractions still reflect the current machine's local time, and
`Date.toTime` still reflects its UTC time.

-}
fromSpec : DateSpec -> TimeSpec -> OffsetSpec -> Date
fromSpec (DateMS dateMS) (TimeMS timeMS) offsetSpec =
    case offsetSpec of
        Offset offset ->
            fromUnixTime (dateMS + timeMS - offset * msPerMinute)

        Local ->
            -- find the local offset
            let
                unixTime =
                    dateMS + timeMS

                offset0 =
                    offsetFromUtc (fromUnixTime unixTime)

                date1 =
                    fromUnixTime (unixTime - offset0 * msPerMinute)

                offset1 =
                    offsetFromUtc date1
            in
            if offset0 == offset1 then
                date1
            else
                -- local offset has changed within `offset0` time period (e.g. DST switch)
                let
                    date2 =
                        fromUnixTime (unixTime - offset1 * msPerMinute)

                    offset2 =
                        offsetFromUtc date2
                in
                if offset1 == offset2 then
                    date2
                else
                    -- `unixTime` is within the lost hour of a local switch
                    date1


fromUnixTime : Int -> Date
fromUnixTime =
    toFloat >> fromTime


{-| Create a `Date` from the following parts, given in local time:

  - year
  - month
  - day
  - hour
  - minute
  - second
  - millisecond

```
import Date exposing (Month(..))
import Date.Extra as Date

Date.fromParts 1999 Dec 31 23 59 0 0
-- <31 December 1999, 23:59, local time>
```

Out-of-range parts are clamped.

    Date.fromParts 2007 Feb 29 0 0 0 0
    -- <28 February 2007>

-}
fromParts : Int -> Month -> Int -> Int -> Int -> Int -> Int -> Date
fromParts y m d hh mm ss ms =
    fromSpec (calendarDate y m d) (time hh mm ss ms) local


{-| Convenience function for creating a `Date` from only the year, month, and
day parts.

    Date.fromCalendarDate 2000 Jan 1

-}
fromCalendarDate : Int -> Month -> Int -> Date
fromCalendarDate y m d =
    fromSpec (calendarDate y m d) midnight local



--------------------------------------------------------------------------------
-- Parse ISO 8601


isoDateRegex : Regex
isoDateRegex =
    let
        year =
            --yyyy
            --1
            "(\\d{4})"

        cal =
            --      mm            dd
            --2     3             4
            "(\\-)?(\\d{2})(?:\\2(\\d{2}))?"

        week =
            --       ww            d
            --5      6             7
            "(\\-)?W(\\d{2})(?:\\5(\\d))?"

        ord =
            --    ddd
            --    8
            "\\-?(\\d{3})"

        time =
            -- hh               mm             ss          .f              Z      +/-      hh             mm
            -- 9          10    11             12          13              14     15       16             17
            "T(\\d{2})(?:(\\:)?(\\d{2})(?:\\10(\\d{2}))?)?([\\.,]\\d+)?(?:(Z)|(?:([+−\\-])(\\d{2})(?:\\:?(\\d{2}))?))?"
    in
    regex <| "^" ++ year ++ "(?:" ++ cal ++ "|" ++ week ++ "|" ++ ord ++ ")?" ++ "(?:" ++ time ++ ")?$"


matchToInt : Int -> Maybe String -> Int
matchToInt default =
    Maybe.andThen (String.toInt >> Result.toMaybe) >> Maybe.withDefault default


dateFromMatches : String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Result String DateSpec
dateFromMatches yyyy calMM calDD weekWW weekD ordDDD =
    Result.map (DateMS << unixTimeFromRataDie)
        (let
            y =
                yyyy |> String.toInt |> Result.withDefault 1
         in
         case ( calMM, weekWW ) of
            ( Just _, Nothing ) ->
                RataDie.fromCalendarParts y (calMM |> matchToInt 1) (calDD |> matchToInt 1)

            ( Nothing, Just _ ) ->
                RataDie.fromWeekParts y (weekWW |> matchToInt 1) (weekD |> matchToInt 1)

            _ ->
                RataDie.fromOrdinalParts y (ordDDD |> matchToInt 1)
        )


timeFromMatches : Maybe String -> Maybe String -> Maybe String -> Maybe String -> Result String TimeSpec
timeFromMatches timeHH timeMM timeSS timeF =
    let
        fractional =
            timeF |> Maybe.andThen (Regex.replace All (regex ",") (\_ -> ".") >> String.toFloat >> Result.toMaybe) |> Maybe.withDefault 0.0

        ( hh, mm, ss ) =
            case [ timeHH, timeMM, timeSS ] |> List.map (Maybe.andThen (String.toFloat >> Result.toMaybe)) of
                [ Just hh, Just mm, Just ss ] ->
                    ( hh, mm, ss + fractional )

                [ Just hh, Just mm, Nothing ] ->
                    ( hh, mm + fractional, 0.0 )

                [ Just hh, Nothing, Nothing ] ->
                    ( hh + fractional, 0.0, 0.0 )

                _ ->
                    ( 0.0, 0.0, 0.0 )
    in
    if hh >= 24 then
        Err <| "Invalid time (hours = " ++ toString hh ++ ")"
    else if mm >= 60 then
        Err <| "Invalid time (minutes = " ++ toString mm ++ ")"
    else if ss >= 60 then
        Err <| "Invalid time (seconds = " ++ toString ss ++ ")"
    else
        Ok <| TimeMS (hh * toFloat msPerHour + mm * toFloat msPerMinute + ss * toFloat msPerSecond |> round)


offsetFromMatches : Maybe String -> Maybe String -> Maybe String -> Maybe String -> Result String OffsetSpec
offsetFromMatches tzZ tzSign tzHH tzMM =
    case ( tzZ, tzSign ) of
        ( Just "Z", Nothing ) ->
            Ok utc

        ( Nothing, Just sign ) ->
            let
                hh =
                    tzHH |> matchToInt 0

                mm =
                    tzMM |> matchToInt 0
            in
            if hh > 23 then
                Err <| "Invalid offset (hours = " ++ toString hh ++ ")"
            else if mm > 59 then
                Err <| "Invalid offset (minutes = " ++ toString mm ++ ")"
            else if sign == "+" then
                Ok <| offset (hh * 60 + mm)
            else
                Ok <| offset (hh * -60 - mm)

        _ ->
            Ok local


fromMatches : List (Maybe String) -> Result String Date
fromMatches matches =
    case matches of
        [ Just yyyy, _, calMM, calDD, _, weekWW, weekD, ordDDD, timeHH, _, timeMM, timeSS, timeF, tzZ, tzSign, tzHH, tzMM ] ->
            Result.map3
                fromSpec
                (dateFromMatches yyyy calMM calDD weekWW weekD ordDDD)
                (timeFromMatches timeHH timeMM timeSS timeF)
                (offsetFromMatches tzZ tzSign tzHH tzMM)

        _ ->
            Err "Unexpected matches"


{-| Attempt to create a `Date` from a string representing a date in
[ISO 8601](https://en.wikipedia.org/wiki/ISO_8601) format.

    Date.fromIsoString "2000-01-01"
    -- Just <1 January 2000, local time>

    Date.fromIsoString "2009-W01-1T00Z"
    -- Just <29 December 2008, UTC>

    Date.fromIsoString "2016-218T20:00-03:00"
    -- Just <5 August 2016, 23:00, UTC>

    Date.fromIsoString "1/1/2000"
    -- Nothing

When a `Date` is created with a specified time offset (e.g. `"-03:00"`),
its extractions still reflect the current machine's local time, and
`Date.toTime` still reflects its UTC time.

-}
fromIsoString : String -> Result String Date
fromIsoString s =
    Regex.find (AtMost 1) isoDateRegex s
        |> List.head
        |> Result.fromMaybe "Invalid ISO 8601 format"
        |> Result.andThen (.submatches >> fromMatches)
        |> Result.mapError ((++) ("Failed to create a Date from string '" ++ s ++ "': "))



--------------------------------------------------------------------------------
-- Extract


monthToQuarter : Month -> Int
monthToQuarter m =
    (monthToNumber m + 2) // 3


quarterToMonth : Int -> Month
quarterToMonth q =
    q * 3 - 2 |> numberToMonth


{-| Extract the month number of a date. Given the date 23 June 1990 at
11:45 a.m. this returns the integer 6.
-}
monthNumber : Date -> Int
monthNumber =
    month >> monthToNumber


{-| Extract the quarter of a date. Given the date 23 June 1990 at
11:45 a.m. this returns the integer 2.
-}
quarter : Date -> Int
quarter =
    month >> monthToQuarter


{-| Extract the ordinal day of a date. Given the date 23 June 1990 at
11:45 a.m. this returns the integer 174.
-}
ordinalDay : Date -> Int
ordinalDay date =
    daysBeforeMonth (year date) (month date) + day date


{-| Extract the fractional day of a date. Given the date 23 June 1990 at
11:45 a.m. this returns the float 0.4895833333333333.
-}
fractionalDay : Date -> Float
fractionalDay date =
    let
        timeOfDayMS =
            msFromTimeParts (hour date) (minute date) (second date) (millisecond date)
    in
    toFloat timeOfDayMS / toFloat msPerDay


{-| Extract the weekday number (beginning at 1 for Monday) of a date. Given
the date 23 June 1990 at 11:45 a.m. this returns the integer 6.
-}
weekdayNumber : Date -> Int
weekdayNumber =
    dayOfWeek >> weekdayToNumber


{-| Extract the week number of a date. Given the date 23 June 1990 at
11:45 a.m. this returns the integer 25.
-}
weekNumber : Date -> Int
weekNumber =
    toRataDie >> RataDie.weekNumber


{-| Extract the week-numbering year of a date. Given the date 23 June
1990 at 11:45 a.m. this returns the integer 1990.
-}
weekYear : Date -> Int
weekYear =
    toRataDie >> RataDie.weekYear


{-| Extract the local offset from UTC time, in minutes, of a date. Given a date
with a local offset of UTC-05:00 this returns the integer -300.
-}
offsetFromUtc : Date -> Int
offsetFromUtc date =
    let
        localTime =
            unixTimeFromRataDie (RataDie.fromCalendarDate (year date) (month date) (day date))
                + msFromTimeParts (hour date) (minute date) (second date) (millisecond date)
                |> toFloat

        utcTime =
            date |> toTime
    in
    Basics.floor (localTime - utcTime) // msPerMinute



--------------------------------------------------------------------------------
-- Format


monthToName : Month -> String
monthToName m =
    case m of
        Jan ->
            "January"

        Feb ->
            "February"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "June"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"


weekdayToName : Day -> String
weekdayToName d =
    case d of
        Mon ->
            "Monday"

        Tue ->
            "Tuesday"

        Wed ->
            "Wednesday"

        Thu ->
            "Thursday"

        Fri ->
            "Friday"

        Sat ->
            "Saturday"

        Sun ->
            "Sunday"


hour12 : Date -> Int
hour12 date =
    case hour date % 12 of
        0 ->
            12

        h ->
            h


type DayPeriod
    = Midnight
    | AM
    | Noon
    | PM


dayPeriod : Date -> DayPeriod
dayPeriod date =
    let
        hh =
            hour date

        onTheHour =
            minute date == 0 && second date == 0 && millisecond date == 0
    in
    if hh == 0 && onTheHour then
        Midnight
    else if hh < 12 then
        AM
    else if hh == 12 && onTheHour then
        Noon
    else
        PM


formatTimeOffset : String -> Bool -> Int -> String
formatTimeOffset separator minutesIsOptional offset =
    let
        sign =
            if offset >= 0 then
                "+"
            else
                "-"

        hh =
            abs offset // 60 |> toString |> String.padLeft 2 '0'

        mm =
            abs offset % 60 |> toString |> String.padLeft 2 '0'
    in
    if minutesIsOptional && mm == "00" then
        sign ++ hh
    else
        sign ++ hh ++ separator ++ mm


ordinalSuffix : Int -> String
ordinalSuffix n =
    let
        -- use 2-digit number
        nn =
            n % 100
    in
    case
        min
            (if nn < 20 then
                nn
             else
                nn % 10
            )
            4
    of
        1 ->
            "st"

        2 ->
            "nd"

        3 ->
            "rd"

        _ ->
            "th"


withOrdinalSuffix : Int -> String
withOrdinalSuffix n =
    toString n ++ ordinalSuffix n



-- Formatting is based on Date Format Patterns in Unicode Technical Standard #35
{- Matches a series of pattern characters, or a single-quoted string (which
   may contain '' inside, representing an escaped single-quote).
-}


patternMatches : Regex
patternMatches =
    regex "([yYQMwdDEeabhHmsSXx])\\1*|'(?:[^']|'')*?'(?!')"


type FormatStyle
    = Abbreviated
    | Full
    | Narrow
    | Short
    | Invalid


formatStyleFromLength : Int -> FormatStyle
formatStyleFromLength length =
    case length of
        1 ->
            Abbreviated

        2 ->
            Abbreviated

        3 ->
            Abbreviated

        4 ->
            Full

        5 ->
            Narrow

        6 ->
            Short

        _ ->
            Invalid


format : Bool -> Date -> String -> String
format asUtc date match =
    let
        char =
            String.left 1 match

        length =
            String.length match
    in
    case char of
        "y" ->
            case length of
                2 ->
                    date |> year |> toString |> String.padLeft length '0' |> String.right 2

                _ ->
                    date |> year |> toString |> String.padLeft length '0'

        "Y" ->
            case length of
                2 ->
                    date |> weekYear |> toString |> String.padLeft length '0' |> String.right 2

                _ ->
                    date |> weekYear |> toString |> String.padLeft length '0'

        "Q" ->
            case length of
                1 ->
                    date |> quarter |> toString

                2 ->
                    date |> quarter |> toString

                3 ->
                    date |> quarter |> toString |> (++) "Q"

                4 ->
                    date |> quarter |> withOrdinalSuffix

                5 ->
                    date |> quarter |> toString

                _ ->
                    ""

        "M" ->
            case length of
                1 ->
                    date |> monthNumber |> toString

                2 ->
                    date |> monthNumber |> toString |> String.padLeft 2 '0'

                3 ->
                    date |> month |> monthToName |> String.left 3

                4 ->
                    date |> month |> monthToName

                5 ->
                    date |> month |> monthToName |> String.left 1

                _ ->
                    ""

        "w" ->
            case length of
                1 ->
                    date |> weekNumber |> toString

                2 ->
                    date |> weekNumber |> toString |> String.padLeft 2 '0'

                _ ->
                    ""

        "d" ->
            case length of
                1 ->
                    date |> day |> toString

                2 ->
                    date |> day |> toString |> String.padLeft 2 '0'

                3 ->
                    date |> day |> withOrdinalSuffix

                -- non-standard
                _ ->
                    ""

        "D" ->
            case length of
                1 ->
                    date |> ordinalDay |> toString

                2 ->
                    date |> ordinalDay |> toString |> String.padLeft 2 '0'

                3 ->
                    date |> ordinalDay |> toString |> String.padLeft 3 '0'

                _ ->
                    ""

        "E" ->
            case formatStyleFromLength length of
                Abbreviated ->
                    date |> dayOfWeek |> weekdayToName |> String.left 3

                Full ->
                    date |> dayOfWeek |> weekdayToName

                Narrow ->
                    date |> dayOfWeek |> weekdayToName |> String.left 1

                Short ->
                    date |> dayOfWeek |> weekdayToName |> String.left 2

                Invalid ->
                    ""

        "e" ->
            case length of
                1 ->
                    date |> weekdayNumber |> toString

                2 ->
                    date |> weekdayNumber |> toString

                _ ->
                    format asUtc date (String.toUpper match)

        "a" ->
            let
                p =
                    date |> dayPeriod

                m =
                    if p == Midnight || p == AM then
                        "A"
                    else
                        "P"
            in
            case formatStyleFromLength length of
                Abbreviated ->
                    m ++ "M"

                Full ->
                    m ++ ".M."

                Narrow ->
                    m

                _ ->
                    ""

        "b" ->
            case formatStyleFromLength length of
                Abbreviated ->
                    case date |> dayPeriod of
                        Midnight ->
                            "mid."

                        AM ->
                            "am"

                        Noon ->
                            "noon"

                        PM ->
                            "pm"

                Full ->
                    case date |> dayPeriod of
                        Midnight ->
                            "midnight"

                        AM ->
                            "a.m."

                        Noon ->
                            "noon"

                        PM ->
                            "p.m."

                Narrow ->
                    case date |> dayPeriod of
                        Midnight ->
                            "md"

                        AM ->
                            "a"

                        Noon ->
                            "nn"

                        PM ->
                            "p"

                _ ->
                    ""

        "h" ->
            case length of
                1 ->
                    date |> hour12 |> toString

                2 ->
                    date |> hour12 |> toString |> String.padLeft 2 '0'

                _ ->
                    ""

        "H" ->
            case length of
                1 ->
                    date |> hour |> toString

                2 ->
                    date |> hour |> toString |> String.padLeft 2 '0'

                _ ->
                    ""

        "m" ->
            case length of
                1 ->
                    date |> minute |> toString

                2 ->
                    date |> minute |> toString |> String.padLeft 2 '0'

                _ ->
                    ""

        "s" ->
            case length of
                1 ->
                    date |> second |> toString

                2 ->
                    date |> second |> toString |> String.padLeft 2 '0'

                _ ->
                    ""

        "S" ->
            date |> millisecond |> toString |> String.padLeft 3 '0' |> String.left length |> String.padRight length '0'

        "X" ->
            if length < 4 && (asUtc || offsetFromUtc date == 0) then
                "Z"
            else
                format asUtc date (String.toLower match)

        "x" ->
            let
                offset =
                    if asUtc then
                        0
                    else
                        offsetFromUtc date
            in
            case length of
                1 ->
                    formatTimeOffset "" True offset

                2 ->
                    formatTimeOffset "" False offset

                3 ->
                    formatTimeOffset ":" False offset

                _ ->
                    ""

        "'" ->
            if match == "''" then
                "'"
            else
                String.slice 1 -1 match |> Regex.replace All (regex "''") (\_ -> "'")

        _ ->
            ""


toFormattedString_ : Bool -> String -> Date -> String
toFormattedString_ asUtc pattern date =
    let
        date_ =
            if asUtc then
                Date.fromTime <| Date.toTime date - (offsetFromUtc date * msPerMinute |> toFloat)
            else
                date
    in
    Regex.replace All patternMatches (.match >> format asUtc date_) pattern


{-| Convert a date to a string using a pattern as a template.

    Date.toFormattedString
        "EEEE, MMMM d, y 'at' h:mm a"
        (Date.fromParts 2007 Mar 15 13 45 56 67)
    -- "Thursday, March 15, 2007 at 1:45 PM"

Each alphabetic character in the pattern represents date or time information;
the number of times a character is repeated specifies the form of the name to
use (e.g. "Tue", "Tuesday") or the padding of numbers (e.g. "1", "01").
Formatting characters are escaped within single-quotes, and a single-quote is
escaped as a sequence of two single-quotes, whether appearing inside or outside
an escaped sequence.

Patterns are based on Date Format Patterns in [Unicode Technical
Standard #35](http://www.unicode.org/reports/tr35/tr35-43/tr35-dates.html#Date_Format_Patterns).
Only the following subset of formatting characters are available:

    "y" -- year
    "Y" -- week-numbering year
    "Q" -- quarter
    "M" -- month
    "w" -- week number
    "d" -- day
    "D" -- ordinal day
    "E" -- day of week
    "e" -- weekday number / day of week
    "a" -- day period (AM, PM)
    "b" -- day period (am, pm, noon, midnight)
    "h" -- hour (12-hour clock)
    "H" -- hour (24-hour clock)
    "m" -- minute
    "s" -- second
    "S" -- fractional second
    "X" -- time offset, using "Z" when offset is 0
    "x" -- time offset

The non-standard pattern field "ddd" is available to indicate the day of the
month with an ordinal suffix (e.g. "1st", "15th"), as the current standard does
not include such a field.

    Date.toFormattedString
        "MMMM ddd, y"
        (Date.fromParts 2007 Mar 15 13 45 56 67)
    -- "March 15th, 2007"

-}
toFormattedString : String -> Date -> String
toFormattedString =
    toFormattedString_ False


{-| Convert a date to a string just like `toFormattedString`, but using the UTC
representation instead of the local representation of the date.
-}
toUtcFormattedString : String -> Date -> String
toUtcFormattedString =
    toFormattedString_ True


{-| Convenience function for formatting a date to ISO 8601 (extended
date and time format with local time offset).

    Date.toIsoString
        (Date.fromParts 2007 Mar 15 13 45 56 67)
    -- "2007-03-15T13:45:56.067-04:00"
    -- (example has a local offset of UTC-04:00)

-}
toIsoString : Date -> String
toIsoString =
    toFormattedString_ False "yyyy-MM-dd'T'HH:mm:ss.SSSxxx"


{-| Convenience function for formatting a date, in UTC representation, to ISO
8601 (extended date and time format with "Z" for time offset).

    Date.toUtcIsoString
        (Date.fromParts 2007 Mar 15 13 45 56 67)
    -- "2007-03-15T17:45:56.067Z"
    -- (example has a local offset of UTC-04:00)

-}
toUtcIsoString : Date -> String
toUtcIsoString =
    toFormattedString_ True "yyyy-MM-dd'T'HH:mm:ss.SSSXXX"



--------------------------------------------------------------------------------
-- Compare


{-| Test equality of two dates.
-}
equal : Date -> Date -> Bool
equal a b =
    toTime a == toTime b


{-| Compare two dates. This can be used as the compare function for
`List.sortWith`.
-}
compare : Date -> Date -> Order
compare a b =
    Basics.compare (toTime a) (toTime b)


{-| Test if a date is within a given range, inclusive of the range values. The
expression `Date.isBetween a b x` tests if `x` is between `a` and `b`.
-}
isBetween : Date -> Date -> Date -> Bool
isBetween a b x =
    toTime a <= toTime x && toTime x <= toTime b


{-| Clamp a date within a given range. The expression `Date.clamp min max x`
returns one of `min`, `max`, or `x`, ensuring the returned date is not before
`min` and not after `max`.
-}
clamp : Date -> Date -> Date -> Date
clamp minimum maximum date =
    if toTime date < toTime minimum then
        minimum
    else if toTime date > toTime maximum then
        maximum
    else
        date



--------------------------------------------------------------------------------
-- Intervals


{-| Represents an interval of time.
-}
type Interval
    = Millisecond
    | Second
    | Minute
    | Hour
    | Day
    | Month
    | Year
    | Quarter
    | Week
    | Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday


{-| Test if two dates fall within the same interval.

    dec31 = Date.fromCalendarDate 1999 Dec 31
    jan1 = Date.fromCalendarDate 2000 Jan 1

    Date.equalBy Month dec31 jan1 -- False
    Date.equalBy Week  dec31 jan1 -- True

-}
equalBy : Interval -> Date -> Date -> Bool
equalBy interval date1 date2 =
    case interval of
        Millisecond ->
            toTime date1 == toTime date2

        Second ->
            second date1 == second date2 && equalBy Minute date1 date2

        Minute ->
            minute date1 == minute date2 && equalBy Hour date1 date2

        Hour ->
            hour date1 == hour date2 && equalBy Day date1 date2

        Day ->
            day date1 == day date2 && equalBy Month date1 date2

        Month ->
            month date1 == month date2 && equalBy Year date1 date2

        Year ->
            year date1 == year date2

        Quarter ->
            quarter date1 == quarter date2 && equalBy Year date1 date2

        Week ->
            weekNumber date1 == weekNumber date2 && weekYear date1 == weekYear date2

        weekday ->
            equalBy Day (floor weekday date1) (floor weekday date2)



--------------------------------------------------------------------------------
-- Arithmetic


{-| Add a number of whole intervals to a date.

    Date.add Week 2 (Date.fromParts 2007 Mar 15 11 55 0 0)
    -- <29 March 2007, 11:55>

When adding Month, Quarter, or Year intervals, day values are clamped at the
end of the month if necessary.

    Date.add Month 1 (Date.fromParts 2000 Jan 31 0 0 0 0)
    -- <29 February 2000>

-}
add : Interval -> Int -> Date -> Date
add interval n date =
    case interval of
        Millisecond ->
            fromTime <| toTime date + toFloat n

        Second ->
            fromTime <| toTime date + toFloat (n * msPerSecond)

        Minute ->
            fromTime <| toTime date + toFloat (n * msPerMinute)

        Hour ->
            fromTime <| toTime date + toFloat (n * msPerHour)

        Day ->
            let
                ( y, m, d, hh, mm, ss, ms ) =
                    ( year date, month date, day date, hour date, minute date, second date, millisecond date )
            in
            fromSpec (DateMS <| unixTimeFromRataDie (RataDie.fromCalendarDate y m d + n)) (time hh mm ss ms) local

        Month ->
            let
                ( y, mn, d, hh, mm, ss, ms ) =
                    ( year date, monthNumber date, day date, hour date, minute date, second date, millisecond date )

                wholeMonths =
                    12 * (y - 1) + mn - 1 + n
            in
            fromParts (wholeMonths // 12 + 1) (wholeMonths % 12 + 1 |> numberToMonth) d hh mm ss ms

        Year ->
            add Month (n * 12) date

        Quarter ->
            add Month (n * 3) date

        Week ->
            add Day (n * 7) date

        weekday ->
            add Day (n * 7) date


{-| The number of whole months between date and 0001-01-01 plus fraction
representing the current month. Only used for diffing months.
-}
toMonths : Date -> Float
toMonths date =
    let
        ( y, m, d ) =
            ( year date, month date, day date )

        wholeMonths =
            12 * (y - 1) + monthToNumber m - 1
    in
    toFloat wholeMonths + (toFloat d / 100) + (fractionalDay date / 100)


{-| Find the difference, as a number of whole intervals, between two dates.

    Date.diff Month
        (Date.fromParts 2007 Mar 15 11 55 0 0)
        (Date.fromParts 2007 Sep 1 0 0 0 0)
    -- 5

-}
diff : Interval -> Date -> Date -> Int
diff interval date1 date2 =
    case interval of
        Millisecond ->
            toTime date2 - toTime date1 |> Basics.floor

        Second ->
            diff Millisecond date1 date2 // msPerSecond

        Minute ->
            diff Millisecond date1 date2 // msPerMinute

        Hour ->
            diff Millisecond date1 date2 // msPerHour

        Day ->
            let
                rdm1 =
                    (date1 |> toRataDie |> toFloat) + (date1 |> fractionalDay)

                rdm2 =
                    (date2 |> toRataDie |> toFloat) + (date2 |> fractionalDay)
            in
            rdm2 - rdm1 |> truncate

        Month ->
            toMonths date2 - toMonths date1 |> truncate

        Year ->
            diff Month date1 date2 // 12

        Quarter ->
            diff Month date1 date2 // 3

        Week ->
            diff Day date1 date2 // 7

        weekday ->
            diff Day (floor weekday date1) (floor weekday date2) // 7



--------------------------------------------------------------------------------
-- Round


daysSincePreviousWeekday : Day -> Date -> Int
daysSincePreviousWeekday wd date =
    (weekdayNumber date + 7 - weekdayToNumber wd) % 7


{-| Round down a date to the beginning of the closest interval. The resulting
date will be less than or equal to the one provided.

    Date.floor Hour
        (Date.fromParts 1999 Dec 31 23 59 59 999)
    -- <31 December 1999, 23:00>

-}
floor : Interval -> Date -> Date
floor interval date =
    case interval of
        Millisecond ->
            date

        Second ->
            fromParts (year date) (month date) (day date) (hour date) (minute date) (second date) 0

        Minute ->
            fromParts (year date) (month date) (day date) (hour date) (minute date) 0 0

        Hour ->
            fromParts (year date) (month date) (day date) (hour date) 0 0 0

        Day ->
            fromCalendarDate (year date) (month date) (day date)

        Month ->
            fromCalendarDate (year date) (month date) 1

        Year ->
            fromCalendarDate (year date) Jan 1

        Quarter ->
            fromCalendarDate (year date) (date |> quarter |> quarterToMonth) 1

        Week ->
            fromRataDie ((date |> toRataDie) - daysSincePreviousWeekday Mon date)

        Monday ->
            fromRataDie ((date |> toRataDie) - daysSincePreviousWeekday Mon date)

        Tuesday ->
            fromRataDie ((date |> toRataDie) - daysSincePreviousWeekday Tue date)

        Wednesday ->
            fromRataDie ((date |> toRataDie) - daysSincePreviousWeekday Wed date)

        Thursday ->
            fromRataDie ((date |> toRataDie) - daysSincePreviousWeekday Thu date)

        Friday ->
            fromRataDie ((date |> toRataDie) - daysSincePreviousWeekday Fri date)

        Saturday ->
            fromRataDie ((date |> toRataDie) - daysSincePreviousWeekday Sat date)

        Sunday ->
            fromRataDie ((date |> toRataDie) - daysSincePreviousWeekday Sun date)


{-| Round up a date to the beginning of the closest interval. The resulting
date will be greater than or equal to the one provided.

    Date.ceiling Monday
        (Date.fromParts 1999 Dec 31 23 59 59 999)
    -- <3 January 2000>

-}
ceiling : Interval -> Date -> Date
ceiling interval date =
    let
        floored =
            date |> floor interval
    in
    if toTime date == toTime floored then
        date
    else
        floored |> add interval 1



--------------------------------------------------------------------------------
-- Range


{-| Create a list of dates, at rounded intervals, increasing by a step value,
between two dates. The list will start at or after the first date, and end
before the second date.

    Date.range Day 2
        (Date.fromParts 2007 Mar 15 11 55 0 0)
        (Date.fromParts 2007 Mar 22 0 0 0 0)
    -- [ <16 March 2007>
    -- , <18 March 2007>
    -- , <20 March 2007>
    -- ]

-}
range : Interval -> Int -> Date -> Date -> List Date
range interval step start end =
    let
        first =
            start |> ceiling interval
    in
    if toTime first < toTime end then
        rangeHelp interval (max 1 step) end [] first
    else
        []


rangeHelp : Interval -> Int -> Date -> List Date -> Date -> List Date
rangeHelp interval step end revList date =
    if toTime date < toTime end then
        rangeHelp interval step end (date :: revList) (date |> add interval step)
    else
        List.reverse revList



--------------------------------------------------------------------------------
-- Rata Die


{-| -}
toRataDie : Date -> Int
toRataDie date =
    RataDie.fromCalendarDate (year date) (month date) (day date)


{-| -}
fromRataDie : Int -> Date
fromRataDie rd =
    fromSpec (DateMS <| unixTimeFromRataDie rd) midnight local



--------------------------------------------------------------------------------
-- Helpers


{-|

    monthToNumber Jan -- 1

-}
monthToNumber : Month -> Int
monthToNumber =
    Date.Facts.monthToNumber


{-| Numbers outside the range [1, 12] are clamped.

    numberToMonth -2 -- Jan
    numberToMonth 15 -- Dec

-}
numberToMonth : Int -> Month
numberToMonth =
    Date.Facts.numberToMonth


{-|

    weekdayToNumber Mon -- 1

-}
weekdayToNumber : Day -> Int
weekdayToNumber =
    Date.Facts.weekdayToNumber


{-| Numbers outside the range [1, 7] are clamped.

    numberToWeekday -2 -- Mon
    numberToWeekday 10 -- Sun

-}
numberToWeekday : Int -> Day
numberToWeekday =
    Date.Facts.numberToWeekday
