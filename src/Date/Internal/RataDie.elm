module Date.Internal.RataDie
    exposing
        ( RataDie
        , fromCalendarDate
        , fromOrdinalDate
        , fromWeekDate
        , weekNumber
        , weekYear
        )

import Date exposing (Month)
import Date.Extra.Facts exposing (daysBeforeStartOfMonth, isLeapYear)


type alias RataDie =
    Int


fromOrdinalDate : Int -> Int -> RataDie
fromOrdinalDate y od =
    daysBeforeYear y + od


fromCalendarDate : Int -> Month -> Int -> RataDie
fromCalendarDate y m d =
    daysBeforeYear y + daysBeforeStartOfMonth y m + d


fromWeekDate : Int -> Int -> Int -> RataDie
fromWeekDate wy wn wdn =
    daysBeforeWeekYear wy + (wn - 1) * 7 + wdn


weekYear : RataDie -> Int
weekYear rd =
    -- `year <thursday of this week>`
    year (rd + (4 - weekdayNumber rd))


weekNumber : RataDie -> Int
weekNumber rd =
    let
        week1Day1 =
            daysBeforeWeekYear (weekYear rd) + 1
    in
    (rd - week1Day1) // 7 + 1



-- calculations


daysBeforeYear : Int -> Int
daysBeforeYear y1 =
    let
        y =
            y1 - 1

        leapYears =
            (y // 4) - (y // 100) + (y // 400)
    in
    365 * y + leapYears


weekdayNumber : RataDie -> Int
weekdayNumber rd =
    case rd % 7 of
        0 ->
            7

        n ->
            n


daysBeforeWeekYear : Int -> Int
daysBeforeWeekYear y =
    let
        jan4 =
            daysBeforeYear y + 4
    in
    jan4 - weekdayNumber jan4


year : RataDie -> Int
year rd =
    let
        ( n400, r400 ) =
            -- 400 * 365 + 97
            divideInt rd 146097

        ( n100, r100 ) =
            -- 100 * 365 + 24
            divideInt r400 36524

        ( n4, r4 ) =
            -- 4 * 365 + 1
            divideInt r100 1461

        ( n1, r1 ) =
            divideInt r4 365

        n =
            if r1 == 0 then
                0
            else
                1
    in
    n400 * 400 + n100 * 100 + n4 * 4 + n1 + n


{-| integer division, returning (Quotient, Remainder)
-}
divideInt : Int -> Int -> ( Int, Int )
divideInt a b =
    ( a // b, rem a b )
