module Date.Extra
    exposing
        ( DateSpec
        , Interval(..)
        , TimeSpec
        , TimeZone
        , add
        , atTime
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
        , monthNumber
        , noTime
        , offset
        , offsetFromUtc
        , ordinalDate
        , ordinalDay
        , quarter
        , range
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
@docs Interval, equalBy, floor, ceiling, add, diff, range


# Extractions

@docs monthNumber, quarter, ordinalDay, fractionalDay, weekdayNumber, weekNumber, weekYear, offsetFromUtc


# Detailed Specification

In some cases you may want to specify a date with a time offset or from
week-date or ordinal-date parts. The `fromSpec` function provides a way to
do this.
@docs fromSpec


## TimeZone

@docs TimeZone, utc, offset, local


## TimeSpec

@docs TimeSpec, noTime, atTime


## DateSpec

@docs DateSpec, calendarDate, weekDate, ordinalDate


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

-}

import Date exposing (Date, Day(..), Month(..), day, dayOfWeek, hour, millisecond, minute, month, second, toTime, year)
import Date.Extra.Facts exposing (daysInMonth, monthFromMonthNumber, msPerDay, msPerHour, msPerMinute, msPerSecond, weekdayNumberFromDayOfWeek)
import Date.Internal.Core exposing (msFromTimeParts, unixTimeFromCalendarDate, unixTimeFromOrdinalDate, unixTimeFromParts, unixTimeFromWeekDate)
import Date.Internal.Extract
import Date.Internal.Format
import Date.Internal.Parse exposing (offsetTimeFromIsoString)
import Date.Internal.RataDie


-- Create


fromTime : Int -> Date
fromTime =
    Date.fromTime << toFloat


fromOffsetTime : ( Maybe Int, Int ) -> Date
fromOffsetTime ( offset, time ) =
    case offset of
        Just minutes ->
            fromTime <| time - msPerMinute * minutes

        Nothing ->
            -- find the local offset
            let
                offset0 =
                    offsetFromUtc <| fromTime time

                date1 =
                    fromTime <| time - msPerMinute * offset0

                offset1 =
                    offsetFromUtc <| date1
            in
            if offset0 == offset1 then
                date1
            else
                -- local offset has changed within `offset0` time period (e.g. DST switch)
                let
                    date2 =
                        fromTime <| time - msPerMinute * offset1

                    offset2 =
                        offsetFromUtc <| date2
                in
                if offset1 == offset2 then
                    date2
                else
                    -- `time` is within the lost hour of a local switch
                    date1


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

The values of the parts are not checked to ensure a valid date representation,
nor are they clamped to a valid range; instead, providing values outside a
valid range results in underflow or overflow.

    Date.fromParts 2007 Feb 29 0 0 0 0
    -- <1 March 2007>

-}
fromParts : Int -> Month -> Int -> Int -> Int -> Int -> Int -> Date
fromParts y m d hh mm ss ms =
    fromOffsetTime ( Nothing, unixTimeFromParts y m d hh mm ss ms )


{-| Convenience function for creating a `Date` from only the year, month, and
day parts.

    Date.fromCalendarDate 2000 Jan 1

-}
fromCalendarDate : Int -> Month -> Int -> Date
fromCalendarDate y m d =
    fromOffsetTime ( Nothing, unixTimeFromCalendarDate y m d )


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
fromIsoString : String -> Maybe Date
fromIsoString =
    offsetTimeFromIsoString >> Maybe.map fromOffsetTime


{-| Represents a time offset from UTC.
-}
type TimeZone
    = Offset (Maybe Int)


{-| Use no offset.
-}
utc : TimeZone
utc =
    Offset (Just 0)


{-| Use a specific offset from UTC, given in minutes.
-}
offset : Int -> TimeZone
offset minutes =
    Offset (Just minutes)


{-| Use the local offset.
-}
local : TimeZone
local =
    Offset Nothing


{-| Represents a time of day.
-}
type TimeSpec
    = TimeMS Int


{-| Do not specify a time of day (default to 00:00).
-}
noTime : TimeSpec
noTime =
    TimeMS 0


{-| Create a `TimeSpec` from time parts (hour, minute, second, millisecond).
-}
atTime : Int -> Int -> Int -> Int -> TimeSpec
atTime hh mm ss ms =
    TimeMS <| msFromTimeParts hh mm ss ms


{-| Represents a day.
-}
type DateSpec
    = DateMS Int


{-| Create a `DateSpec` from calendar-date parts (year, month, day).
-}
calendarDate : Int -> Month -> Int -> DateSpec
calendarDate y m d =
    DateMS <| unixTimeFromCalendarDate y m d


{-| Create a `DateSpec` from ordinal-date parts (year, ordinalDay).
-}
ordinalDate : Int -> Int -> DateSpec
ordinalDate y d =
    DateMS <| unixTimeFromOrdinalDate y d


{-| Create a `DateSpec` from week-date parts (weekYear, weekNumber, weekdayNumber).
-}
weekDate : Int -> Int -> Int -> DateSpec
weekDate y w d =
    DateMS <| unixTimeFromWeekDate y w d


{-| Create a `Date` from a specified time offset, time of day, and day.

    Date.fromSpec
        local
        noTime
        (calendarDate 2000 Jan 1)
    -- <1 January 2000, local time>

    Date.fromSpec
        utc
        noTime
        (weekDate 2009 1 1)
    -- <29 December 2008, UTC>

    Date.fromSpec
        (offset -180)
        (atTime 20 0 0 0)
        (ordinalDate 2016 218)
    -- <5 August 2016, 23:00, UTC>

When a `Date` is created with a specified time offset (e.g. `offset -180`),
its extractions still reflect the current machine's local time, and
`Date.toTime` still reflects its UTC time.

-}
fromSpec : TimeZone -> TimeSpec -> DateSpec -> Date
fromSpec (Offset o) (TimeMS t) (DateMS d) =
    fromOffsetTime ( o, d + t )



-- Format


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
    Date.Internal.Format.toFormattedString False


{-| Convert a date to a string just like `toFormattedString`, but using the UTC
representation instead of the local representation of the date.
-}
toUtcFormattedString : String -> Date -> String
toUtcFormattedString =
    Date.Internal.Format.toFormattedString True


{-| Convenience function for formatting a date to ISO 8601 (extended
date and time format with local time offset).

    Date.toIsoString
        (Date.fromParts 2007 Mar 15 13 45 56 67)
    -- "2007-03-15T13:45:56.067-04:00"
    -- (example has a local offset of UTC-04:00)

-}
toIsoString : Date -> String
toIsoString =
    toFormattedString "yyyy-MM-dd'T'HH:mm:ss.SSSxxx"


{-| Convenience function for formatting a date, in UTC representation, to ISO
8601 (extended date and time format with "Z" for time offset).

    Date.toUtcIsoString
        (Date.fromParts 2007 Mar 15 13 45 56 67)
    -- "2007-03-15T17:45:56.067Z"
    -- (example has a local offset of UTC-04:00)

-}
toUtcIsoString : Date -> String
toUtcIsoString =
    toUtcFormattedString "yyyy-MM-dd'T'HH:mm:ss.SSSXXX"



-- Extract


{-| Extract the month number of a date. Given the date 23 June 1990 at
11:45 a.m. this returns the integer 6.
-}
monthNumber : Date -> Int
monthNumber =
    Date.Internal.Extract.monthNumber


{-| Extract the quarter of a date. Given the date 23 June 1990 at
11:45 a.m. this returns the integer 2.
-}
quarter : Date -> Int
quarter =
    Date.Internal.Extract.quarter


{-| Extract the ordinal day of a date. Given the date 23 June 1990 at
11:45 a.m. this returns the integer 174.
-}
ordinalDay : Date -> Int
ordinalDay =
    Date.Internal.Extract.ordinalDay


{-| Extract the fractional day of a date. Given the date 23 June 1990 at
11:45 a.m. this returns the float 0.4895833333333333.
-}
fractionalDay : Date -> Float
fractionalDay =
    Date.Internal.Extract.fractionalDay


{-| Extract the weekday number (beginning at 1 for Monday) of a date. Given
the date 23 June 1990 at 11:45 a.m. this returns the integer 6.
-}
weekdayNumber : Date -> Int
weekdayNumber =
    Date.Internal.Extract.weekdayNumber


{-| Extract the week number of a date. Given the date 23 June 1990 at
11:45 a.m. this returns the integer 25.
-}
weekNumber : Date -> Int
weekNumber =
    Date.Internal.Extract.weekNumber


{-| Extract the week-numbering year of a date. Given the date 23 June
1990 at 11:45 a.m. this returns the integer 1990.
-}
weekYear : Date -> Int
weekYear =
    Date.Internal.Extract.weekYear


{-| Extract the local offset from UTC time, in minutes, of a date. Given a date
with a local offset of UTC-05:00 this returns the integer -300.
-}
offsetFromUtc : Date -> Int
offsetFromUtc =
    Date.Internal.Extract.offsetFromUtc



-- Math


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


comparableIsBetween : comparable -> comparable -> comparable -> Bool
comparableIsBetween a b x =
    a <= x && x <= b || b <= x && x <= a


{-| Test if a date is within a given range, inclusive of the range values. The
expression `Date.isBetween a b x` tests if `x` is between `a` and `b`.
-}
isBetween : Date -> Date -> Date -> Bool
isBetween date1 date2 date =
    comparableIsBetween (toTime date1) (toTime date2) (toTime date)


{-| Clamp a date within a given range. The expression `Date.clamp min max x`
returns one of `min`, `max`, or `x`, ensuring the returned date is not before
`min` and not after `max`.
-}
clamp : Date -> Date -> Date -> Date
clamp min max date =
    if toTime date < toTime min then
        min
    else if toTime date > toTime max then
        max
    else
        date


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



-- facts


monthFromQuarter : Int -> Month
monthFromQuarter q =
    case q of
        1 ->
            Jan

        2 ->
            Apr

        3 ->
            Jul

        _ ->
            Oct



-- extractions


ordinalMonth : Date -> Int
ordinalMonth date =
    year date * 12 + monthNumber date



-- conversions


toParts : Date -> ( Int, Month, Int, Int, Int, Int, Int )
toParts date =
    ( year date, month date, day date, hour date, minute date, second date, millisecond date )



-- operations


daysToPreviousDayOfWeek : Day -> Date -> Int
daysToPreviousDayOfWeek d date =
    negate <| (weekdayNumber date - weekdayNumberFromDayOfWeek d + 7) % 7


{-| Round down a date to the beginning of the closest interval. The resulting
date will be less than or equal to the one provided.

    Date.floor Hour
        (Date.fromParts 1999 Dec 31 23 59 59 999)
    -- <31 December 1999, 23:00>

-}
floor : Interval -> Date -> Date
floor interval date =
    let
        ( y, m, d, hh, mm, ss, _ ) =
            toParts date
    in
    case interval of
        Millisecond ->
            date

        Second ->
            fromParts y m d hh mm ss 0

        Minute ->
            fromParts y m d hh mm 0 0

        Hour ->
            fromParts y m d hh 0 0 0

        Day ->
            fromCalendarDate y m d

        Month ->
            fromCalendarDate y m 1

        Year ->
            fromCalendarDate y Jan 1

        Quarter ->
            fromCalendarDate y (monthFromQuarter <| quarter date) 1

        Week ->
            fromCalendarDate y m (d + daysToPreviousDayOfWeek Mon date)

        Monday ->
            fromCalendarDate y m (d + daysToPreviousDayOfWeek Mon date)

        Tuesday ->
            fromCalendarDate y m (d + daysToPreviousDayOfWeek Tue date)

        Wednesday ->
            fromCalendarDate y m (d + daysToPreviousDayOfWeek Wed date)

        Thursday ->
            fromCalendarDate y m (d + daysToPreviousDayOfWeek Thu date)

        Friday ->
            fromCalendarDate y m (d + daysToPreviousDayOfWeek Fri date)

        Saturday ->
            fromCalendarDate y m (d + daysToPreviousDayOfWeek Sat date)

        Sunday ->
            fromCalendarDate y m (d + daysToPreviousDayOfWeek Sun date)


addMonths : Int -> Date -> Date
addMonths n date =
    let
        ( y, m, d, hh, mm, ss, ms ) =
            toParts date

        om =
            ordinalMonth date + n + -1

        y_ =
            om // 12

        m_ =
            om % 12 + 1 |> monthFromMonthNumber

        d_ =
            Basics.min d (daysInMonth y_ m_)
    in
    fromParts y_ m_ d_ hh mm ss ms


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
    let
        ( y, m, d, hh, mm, ss, ms ) =
            toParts date
    in
    case interval of
        Millisecond ->
            Date.fromTime <| toTime date + toFloat n

        Second ->
            Date.fromTime <| toTime date + (toFloat <| n * msPerSecond)

        Minute ->
            Date.fromTime <| toTime date + (toFloat <| n * msPerMinute)

        Hour ->
            Date.fromTime <| toTime date + (toFloat <| n * msPerHour)

        Day ->
            fromParts y m (d + n) hh mm ss ms

        Month ->
            addMonths n date

        Year ->
            addMonths (n * 12) date

        Quarter ->
            addMonths (n * 3) date

        Week ->
            fromParts y m (d + n * 7) hh mm ss ms

        weekday ->
            fromParts y m (d + n * 7) hh mm ss ms


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
            floor interval date
    in
    if toTime date == toTime floored then
        date
    else
        add interval 1 floored


diffMonth : Date -> Date -> Int
diffMonth date1 date2 =
    let
        fractionalMonth : Date -> Float
        fractionalMonth date =
            ((day date - 1 |> toFloat) + fractionalDay date) / 31

        ordinalMonthFloat : Date -> Float
        ordinalMonthFloat date =
            (ordinalMonth date |> toFloat) + fractionalMonth date
    in
    ordinalMonthFloat date2 - ordinalMonthFloat date1 |> truncate


{-| Find the difference, as a number of whole intervals, between two dates.

    Date.diff Month
        (Date.fromParts 2007 Mar 15 11 55 0 0)
        (Date.fromParts 2007 Sep 1 0 0 0 0)
    -- 5

-}
diff : Interval -> Date -> Date -> Int
diff interval date1 date2 =
    let
        diffMS =
            toTime date2 - toTime date1 |> Basics.floor
    in
    case interval of
        Millisecond ->
            diffMS

        Second ->
            diffMS // msPerSecond

        Minute ->
            diffMS // msPerMinute

        Hour ->
            diffMS // msPerHour

        Day ->
            diffMS // msPerDay

        Month ->
            diffMonth date1 date2

        Year ->
            diffMonth date1 date2 // 12

        Quarter ->
            diffMonth date1 date2 // 3

        Week ->
            diff Day date1 date2 // 7

        weekday ->
            diff Day (floor weekday date1) (floor weekday date2) // 7


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



-- Rata Die


{-| -}
toRataDie : Date -> Int
toRataDie date =
    Date.Internal.RataDie.fromCalendarDate (year date) (month date) (day date)


{-| -}
fromRataDie : Int -> Date
fromRataDie rd =
    fromCalendarDate 1970 Jan 1 |> add Day (rd - 719163)
