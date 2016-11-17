module Date.Internal.RataDie exposing
  ( fromOrdinalDate
  , fromCalendarDate
  , fromWeekDate
  , year
  , month
  , day
  , ordinalDay
  , weekdayNumber
  , weekNumber
  , weekYear
  , toUnixTime
  )

import Date exposing (Month)
import Date.Extra.Facts exposing (daysBeforeStartOfMonth, months, msPerDay)


find : (a -> Bool) -> List a -> Maybe a
find pred list =
  case list of
    [] ->
      Nothing
    x :: xs ->
      if pred x then Just x else find pred xs


-- integer division returning (Quotient, Remainder)

divideInt : Int -> Int -> (Int, Int)
divideInt a b =
  (a // b, rem a b)


-- RataDie

type alias RataDie = Int


leapYearsInCommonEra : Int -> Int
leapYearsInCommonEra y =
  (y // 4) - (y // 100) + (y // 400)


rataDieBeforeStartOfYear : Int -> RataDie
rataDieBeforeStartOfYear y =
  365 * (y - 1) + leapYearsInCommonEra (y - 1)


-- Create

fromOrdinalDate : Int -> Int -> RataDie
fromOrdinalDate y d =
  rataDieBeforeStartOfYear y + d


fromCalendarDate : Int -> Month -> Int -> RataDie
fromCalendarDate y m d =
  let
    yd = rataDieBeforeStartOfYear y
    md = daysBeforeStartOfMonth y m
  in
    yd + md + d


fromWeekDate : Int -> Int -> Int -> RataDie
fromWeekDate y w d =
  let
    week1Day0RD = week1Day1OfWeekYear y - 1
  in
    week1Day0RD + (w - 1) * 7 + d


-- Extract

year : RataDie -> Int
year rd =
  let
    (q400, r400) = divideInt rd   146097 -- 400 * 365 + 97
    (q100, r100) = divideInt r400  36524 -- 100 * 365 + 24
    (q4,   r4)   = divideInt r100   1461 --   4 * 365 +  1
    (q1,   r1)   = divideInt r4      365
    n = if r1 == 0 then 0 else 1
  in
    q400 * 400 + q100 * 100 + q4 * 4 + q1 + n


ordinalDay : RataDie -> Int
ordinalDay rd =
  rd - rataDieBeforeStartOfYear (year rd)


month : RataDie -> Month
month rd =
  let
    y = year rd
    od = ordinalDay rd
  in
    List.reverse months
      |> find (\m -> daysBeforeStartOfMonth y m < od)
      |> Maybe.withDefault Date.Jan


day : RataDie -> Int
day rd =
  let
    y = year rd
    m = month rd
    od = ordinalDay rd
  in
    od - daysBeforeStartOfMonth y m


weekdayNumber : RataDie -> Int
weekdayNumber rd =
  case rd % 7 of
    0 -> 7
    n -> n


weekYear : RataDie -> Int
weekYear rd =
  let
    daysToThursday = 4 - weekdayNumber rd
  in
    year (rd + daysToThursday)


week1Day1OfWeekYear : Int -> RataDie
week1Day1OfWeekYear y =
  let
    jan4RD = fromOrdinalDate y 4
  in
    jan4RD - weekdayNumber jan4RD + 1


weekNumber: RataDie -> Int
weekNumber rd =
  let
    week1Day1RD = week1Day1OfWeekYear (weekYear rd)
  in
    (rd - week1Day1RD) // 7 + 1


-- Convert

toUnixTime : RataDie -> Int
toUnixTime rd =
  (rd - 719163) * msPerDay
