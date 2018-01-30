module Date.Internal.Format exposing (toFormattedString)

import Date exposing (Date, Day(..), Month(..), day, dayOfWeek, hour, millisecond, minute, month, second, year)
import Date.Extra.Facts exposing (msPerMinute)
import Date.Internal.Extract exposing (monthNumber, offsetFromUtc, ordinalDay, quarter, weekNumber, weekYear, weekdayNumber)
import Regex exposing (HowMany(..), Regex, regex, replace)
import String exposing (left, padLeft, padRight, right, slice, toLower, toUpper)


monthName : Month -> String
monthName m =
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


dayOfWeekName : Day -> String
dayOfWeekName d =
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
        0 ->
            "th"

        1 ->
            "st"

        2 ->
            "nd"

        3 ->
            "rd"

        4 ->
            "th"

        _ ->
            ""


withOrdinalSuffix : Int -> String
withOrdinalSuffix n =
    toString n ++ ordinalSuffix n


formatTimeOffset : String -> Bool -> Int -> String
formatTimeOffset separator minutesOptional offset =
    let
        sign =
            if offset >= 0 then
                "+"
            else
                "-"

        hh =
            abs offset // 60 |> toString |> padLeft 2 '0'

        mm =
            abs offset % 60 |> toString |> padLeft 2 '0'
    in
    if minutesOptional && mm == "00" then
        sign ++ hh
    else
        sign ++ hh ++ separator ++ mm



-- Formatting is based on Date Format Patterns in Unicode Technical Standard #35
{- Matches a series of pattern characters, or a single-quoted string (which
   may contain '' inside, representing an escaped single-quote).
-}


patternMatches : Regex
patternMatches =
    regex "([yYQMwdDEeabhHmsSXx])\\1*|'(?:[^']|'')*?'(?!')"


nameForm : Int -> String
nameForm length =
    case length of
        1 ->
            "abbreviated"

        2 ->
            "abbreviated"

        3 ->
            "abbreviated"

        4 ->
            "full"

        5 ->
            "narrow"

        6 ->
            "short"

        _ ->
            "invalid"


format : Bool -> Date -> String -> String
format asUtc date match =
    let
        char =
            left 1 match

        length =
            String.length match
    in
    case char of
        "y" ->
            case length of
                2 ->
                    year date |> toString |> padLeft length '0' |> right 2

                _ ->
                    year date |> toString |> padLeft length '0'

        "Y" ->
            case length of
                2 ->
                    weekYear date |> toString |> padLeft length '0' |> right 2

                _ ->
                    weekYear date |> toString |> padLeft length '0'

        "Q" ->
            case length of
                1 ->
                    quarter date |> toString

                2 ->
                    quarter date |> toString

                3 ->
                    quarter date |> toString |> (++) "Q"

                4 ->
                    quarter date |> withOrdinalSuffix

                5 ->
                    quarter date |> toString

                _ ->
                    ""

        "M" ->
            case length of
                1 ->
                    monthNumber date |> toString

                2 ->
                    monthNumber date |> toString |> padLeft 2 '0'

                3 ->
                    month date |> monthName |> left 3

                4 ->
                    month date |> monthName

                5 ->
                    month date |> monthName |> left 1

                _ ->
                    ""

        "w" ->
            case length of
                1 ->
                    weekNumber date |> toString

                2 ->
                    weekNumber date |> toString |> padLeft 2 '0'

                _ ->
                    ""

        "d" ->
            case length of
                1 ->
                    day date |> toString

                2 ->
                    day date |> toString |> padLeft 2 '0'

                3 ->
                    day date |> withOrdinalSuffix

                -- non-standard
                _ ->
                    ""

        "D" ->
            case length of
                1 ->
                    ordinalDay date |> toString

                2 ->
                    ordinalDay date |> toString |> padLeft 2 '0'

                3 ->
                    ordinalDay date |> toString |> padLeft 3 '0'

                _ ->
                    ""

        "E" ->
            case nameForm length of
                "abbreviated" ->
                    dayOfWeek date |> dayOfWeekName |> left 3

                "full" ->
                    dayOfWeek date |> dayOfWeekName

                "narrow" ->
                    dayOfWeek date |> dayOfWeekName |> left 1

                "short" ->
                    dayOfWeek date |> dayOfWeekName |> left 2

                _ ->
                    ""

        "e" ->
            case length of
                1 ->
                    weekdayNumber date |> toString

                2 ->
                    weekdayNumber date |> toString

                _ ->
                    format asUtc date (toUpper match)

        "a" ->
            let
                p =
                    dayPeriod date

                m =
                    if p == Midnight || p == AM then
                        "A"
                    else
                        "P"
            in
            case nameForm length of
                "abbreviated" ->
                    m ++ "M"

                "full" ->
                    m ++ ".M."

                "narrow" ->
                    m

                _ ->
                    ""

        "b" ->
            case nameForm length of
                "abbreviated" ->
                    case dayPeriod date of
                        Midnight ->
                            "mid."

                        AM ->
                            "am"

                        Noon ->
                            "noon"

                        PM ->
                            "pm"

                "full" ->
                    case dayPeriod date of
                        Midnight ->
                            "midnight"

                        AM ->
                            "a.m."

                        Noon ->
                            "noon"

                        PM ->
                            "p.m."

                "narrow" ->
                    case dayPeriod date of
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
                    hour12 date |> toString

                2 ->
                    hour12 date |> toString |> padLeft 2 '0'

                _ ->
                    ""

        "H" ->
            case length of
                1 ->
                    hour date |> toString

                2 ->
                    hour date |> toString |> padLeft 2 '0'

                _ ->
                    ""

        "m" ->
            case length of
                1 ->
                    minute date |> toString

                2 ->
                    minute date |> toString |> padLeft 2 '0'

                _ ->
                    ""

        "s" ->
            case length of
                1 ->
                    second date |> toString

                2 ->
                    second date |> toString |> padLeft 2 '0'

                _ ->
                    ""

        "S" ->
            millisecond date |> toString |> padLeft 3 '0' |> left length |> padRight length '0'

        "X" ->
            if length < 4 && (asUtc || offsetFromUtc date == 0) then
                "Z"
            else
                format asUtc date (toLower match)

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
                slice 1 -1 match |> replace All (regex "''") (\_ -> "'")

        _ ->
            ""


toUtc : Date -> Date
toUtc date =
    Date.fromTime <| Date.toTime date - (toFloat <| offsetFromUtc date * msPerMinute)


toFormattedString : Bool -> String -> Date -> String
toFormattedString asUtc pattern date =
    let
        date_ =
            if asUtc then
                toUtc date
            else
                date
    in
    replace All patternMatches (.match >> format asUtc date_) pattern
