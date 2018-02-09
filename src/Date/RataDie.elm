module Date.RataDie
    exposing
        ( RataDie
        , fromCalendarDate
        , fromCalendarParts
        , fromOrdinalDate
        , fromOrdinalParts
        , fromWeekDate
        , fromWeekParts
        , weekNumber
        , weekYear
        )

import Date exposing (Day, Month)
import Date.Facts exposing (daysBeforeMonth, daysInMonth, isLeapYear, numberToMonth, weekdayToNumber)


type alias RataDie =
    Int



-- calculations


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


weekdayNumber : RataDie -> Int
weekdayNumber rd =
    case rd % 7 of
        0 ->
            7

        n ->
            n


daysBeforeYear : Int -> Int
daysBeforeYear y1 =
    let
        y =
            y1 - 1

        leapYears =
            (y // 4) - (y // 100) + (y // 400)
    in
    365 * y + leapYears


daysBeforeWeekYear : Int -> Int
daysBeforeWeekYear y =
    let
        jan4 =
            daysBeforeYear y + 4
    in
    jan4 - weekdayNumber jan4


is53WeekYear : Int -> Bool
is53WeekYear y =
    let
        wdnJan1 =
            daysBeforeYear y + 1 |> weekdayNumber
    in
    -- any year starting on Thursday and any leap year starting on Wednesday
    wdnJan1 == 4 || (wdnJan1 == 3 && isLeapYear y)


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



-- constructors, strict


fromOrdinalParts : Int -> Int -> Result String RataDie
fromOrdinalParts y od =
    if
        (od |> isBetween 1 365)
            || (od == 366 && isLeapYear y)
    then
        Ok <| daysBeforeYear y + od
    else
        Err <| "Invalid ordinal date (" ++ toString y ++ ", " ++ toString od ++ ")"


fromCalendarParts : Int -> Int -> Int -> Result String RataDie
fromCalendarParts y mn d =
    if
        (mn |> isBetween 1 12)
            && (d |> isBetween 1 (daysInMonth y (mn |> numberToMonth)))
    then
        Ok <| daysBeforeYear y + daysBeforeMonth y (mn |> numberToMonth) + d
    else
        Err <| "Invalid calendar date (" ++ toString y ++ ", " ++ toString mn ++ ", " ++ toString d ++ ")"


fromWeekParts : Int -> Int -> Int -> Result String RataDie
fromWeekParts wy wn wdn =
    if
        (wdn |> isBetween 1 7)
            && ((wn |> isBetween 1 52)
                    || (wn == 53 && is53WeekYear wy)
               )
    then
        Ok <| daysBeforeWeekYear wy + (wn - 1) * 7 + wdn
    else
        Err <| "Invalid week date (" ++ toString wy ++ ", " ++ toString wn ++ ", " ++ toString wdn ++ ")"


isBetween : Int -> Int -> Int -> Bool
isBetween a b x =
    a <= x && x <= b



-- constructors, clamping


fromOrdinalDate : Int -> Int -> RataDie
fromOrdinalDate y od =
    let
        daysInY =
            if isLeapYear y then
                366
            else
                365
    in
    daysBeforeYear y + (od |> clamp 1 daysInY)


fromCalendarDate : Int -> Month -> Int -> RataDie
fromCalendarDate y m d =
    daysBeforeYear y + daysBeforeMonth y m + (d |> clamp 1 (daysInMonth y m))


fromWeekDate : Int -> Int -> Day -> RataDie
fromWeekDate wy wn wd =
    let
        weeksInWY =
            if is53WeekYear wy then
                53
            else
                52
    in
    daysBeforeWeekYear wy + ((wn |> clamp 1 weeksInWY) - 1) * 7 + (wd |> weekdayToNumber)
