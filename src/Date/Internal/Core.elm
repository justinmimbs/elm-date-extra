module Date.Internal.Core exposing (
  unixTimeFromParts,
  unixTimeFromCalendarDate,
  unixTimeFromWeekDate,
  msFromTimeParts,
  weekYearFromCalendarDate,
  weekNumberFromCalendarDate
  )

import Date exposing (Month)
import Date.Facts exposing (daysBeforeStartOfMonth, months, msPerSecond, msPerMinute, msPerHour, msPerDay)


find : (a -> Bool) -> List a -> Maybe a
find pred list =
  case list of
    [] -> Nothing
    x::xs -> if pred x then Just x else find pred xs


-- integer division returning (Quotient, Remainder)

(///) : Int -> Int -> (Int, Int)
(///) n d =
  (n // d, n % d)

infixl 7 ///


-- RataDie

type alias RataDie = Int


leapYearsInCommonEra : Int -> Int
leapYearsInCommonEra y =
  (y // 4) - (y // 100) + (y // 400)


rataDieBeforeStartOfYear : Int -> RataDie
rataDieBeforeStartOfYear y =
  365 * (y - 1) + leapYearsInCommonEra (y - 1)


yearFromRataDie : RataDie -> Int
yearFromRataDie rd =
  let
    (q400, r400) = rd /// 146097 -- 400 * 365 + 97
    (q100, r100) = r400 /// 36524 -- 100 * 365 + 24
    (q4, r4) = r100 /// 1461 -- 4 * 365 + 1
    (q1, r1) = r4 /// 365
    n = if r1 == 0 then 0 else 1
  in
    q400 * 400 + q100 * 100 + q4 * 4 + q1 + n


rataDieFromOrdinalDate : Int -> Int -> RataDie
rataDieFromOrdinalDate y d =
  rataDieBeforeStartOfYear y + d


ordinalDateFromRataDie : RataDie -> (Int, Int)
ordinalDateFromRataDie rd =
  let
    y = yearFromRataDie rd
    d = rd - rataDieBeforeStartOfYear y
  in
    (y, d)


rataDieFromCalendarDate : Int -> Month -> Int -> RataDie
rataDieFromCalendarDate y m d =
  let
    yd = rataDieBeforeStartOfYear y
    md = daysBeforeStartOfMonth y m
  in
    yd + md + d


calendarDateFromRataDie : RataDie -> (Int, Month, Int)
calendarDateFromRataDie rd =
  let
    (y, ordinalDay) = ordinalDateFromRataDie rd
    m = List.reverse months |> find (\m -> daysBeforeStartOfMonth y m < ordinalDay) |> Maybe.withDefault Date.Jan
    d = ordinalDay - daysBeforeStartOfMonth y m
  in
    (y, m, d)


weekdayNumberFromRataDie : RataDie -> Int
weekdayNumberFromRataDie rd =
  case rd % 7 of
    0 -> 7
    n -> n


weekYearFromRataDie : RataDie -> Int
weekYearFromRataDie rd =
  let
    daysToThursday = 4 - weekdayNumberFromRataDie rd
  in
    yearFromRataDie (rd + daysToThursday)


week1Day1FromWeekYear : Int -> RataDie
week1Day1FromWeekYear y =
  let
    jan4RD = rataDieFromOrdinalDate y 4
  in
    jan4RD - weekdayNumberFromRataDie jan4RD + 1


weekNumberFromRataDie: RataDie -> Int
weekNumberFromRataDie rd =
  let
    week1Day1RD = week1Day1FromWeekYear <| weekYearFromRataDie rd
  in
    (rd - week1Day1RD) // 7 + 1


rataDieFromWeekDate : Int -> Int -> Int -> RataDie
rataDieFromWeekDate y w d =
  let
    week1Day0RD = week1Day1FromWeekYear y - 1
  in
    week1Day0RD + (w - 1) * 7 + d


-- Unix time

unixEpochRD : RataDie
unixEpochRD =
  719163


unixTimeFromRataDie : RataDie -> Int
unixTimeFromRataDie rd =
  msPerDay * (rd - unixEpochRD)


msFromTimeParts : Int -> Int -> Int -> Int -> Int
msFromTimeParts hh mm ss ms =
  ms
  + msPerSecond * ss
  + msPerMinute * mm
  + msPerHour * hh


unixTimeFromParts : Int -> Month -> Int -> Int -> Int -> Int -> Int -> Int
unixTimeFromParts y m d hh mm ss ms =
  unixTimeFromRataDie (rataDieFromCalendarDate y m d)
  + msFromTimeParts hh mm ss ms


unixTimeFromCalendarDate : Int -> Month -> Int -> Int
unixTimeFromCalendarDate y m d =
  unixTimeFromRataDie <| rataDieFromCalendarDate y m d


unixTimeFromWeekDate : Int -> Int -> Int -> Int
unixTimeFromWeekDate y w d =
  unixTimeFromRataDie <| rataDieFromWeekDate y w d


-- WeekDate functions

weekYearFromCalendarDate : Int -> Month -> Int -> Int
weekYearFromCalendarDate y m d =
  weekYearFromRataDie <| rataDieFromCalendarDate y m d


weekNumberFromCalendarDate : Int -> Month -> Int -> Int
weekNumberFromCalendarDate y m d =
  weekNumberFromRataDie <| rataDieFromCalendarDate y m d
