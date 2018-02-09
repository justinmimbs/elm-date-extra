module Date.Facts
    exposing
        ( daysBeforeMonth
        , daysInMonth
        , isLeapYear
        , monthToNumber
        , msPerDay
        , msPerHour
        , msPerMinute
        , msPerSecond
        , numberToMonth
        , numberToWeekday
        , weekdayToNumber
        )

{-| This module contains reference information that may be useful when working with
dates, but it doesn't contain functions for working with the `Date` type directly.


# Basics

@docs isLeapYear, daysInMonth, daysBeforeMonth


# Conversions

@docs monthToNumber, numberToMonth, weekdayToNumber, numberToWeekday


# Constants

Values for the number of milliseconds per date part. These are equivalent to
the constants available in the `Time` core library, but typed as integers
instead of floats.
@docs msPerSecond, msPerMinute, msPerHour, msPerDay

-}

import Date exposing (Day(..), Month(..))


{-| -}
isLeapYear : Int -> Bool
isLeapYear y =
    y % 4 == 0 && y % 100 /= 0 || y % 400 == 0


{-|

    daysInMonth 2000 Feb -- 29
    daysInMonth 2001 Feb -- 28

-}
daysInMonth : Int -> Month -> Int
daysInMonth y m =
    case m of
        Jan ->
            31

        Feb ->
            if isLeapYear y then
                29
            else
                28

        Mar ->
            31

        Apr ->
            30

        May ->
            31

        Jun ->
            30

        Jul ->
            31

        Aug ->
            31

        Sep ->
            30

        Oct ->
            31

        Nov ->
            30

        Dec ->
            31


{-|

    daysBeforeMonth 2000 Mar -- 60
    daysBeforeMonth 2001 Mar -- 59

-}
daysBeforeMonth : Int -> Month -> Int
daysBeforeMonth y m =
    let
        leapDays =
            if isLeapYear y then
                1
            else
                0
    in
    case m of
        Jan ->
            0

        Feb ->
            31

        Mar ->
            59 + leapDays

        Apr ->
            90 + leapDays

        May ->
            120 + leapDays

        Jun ->
            151 + leapDays

        Jul ->
            181 + leapDays

        Aug ->
            212 + leapDays

        Sep ->
            243 + leapDays

        Oct ->
            273 + leapDays

        Nov ->
            304 + leapDays

        Dec ->
            334 + leapDays


{-|

    monthToNumber Jan -- 1

-}
monthToNumber : Month -> Int
monthToNumber m =
    case m of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


{-|

    numberToMonth 1 -- Jan

-}
numberToMonth : Int -> Month
numberToMonth n =
    case max 1 n of
        1 ->
            Jan

        2 ->
            Feb

        3 ->
            Mar

        4 ->
            Apr

        5 ->
            May

        6 ->
            Jun

        7 ->
            Jul

        8 ->
            Aug

        9 ->
            Sep

        10 ->
            Oct

        11 ->
            Nov

        _ ->
            Dec


{-|

    weekdayToNumber Mon -- 1

-}
weekdayToNumber : Day -> Int
weekdayToNumber d =
    case d of
        Mon ->
            1

        Tue ->
            2

        Wed ->
            3

        Thu ->
            4

        Fri ->
            5

        Sat ->
            6

        Sun ->
            7


{-|

    numberToWeekday 1 -- Mon

-}
numberToWeekday : Int -> Day
numberToWeekday n =
    case max 1 n of
        1 ->
            Mon

        2 ->
            Tue

        3 ->
            Wed

        4 ->
            Thu

        5 ->
            Fri

        6 ->
            Sat

        _ ->
            Sun


{-| -}
msPerSecond : Int
msPerSecond =
    1000


{-| -}
msPerMinute : Int
msPerMinute =
    60 * msPerSecond


{-| -}
msPerHour : Int
msPerHour =
    60 * msPerMinute


{-| -}
msPerDay : Int
msPerDay =
    24 * msPerHour
