module Date.Internal.Core exposing (
  rataDieFromYMD,
  ymdFromRataDie,
  yearFromRataDie,
  isoWeekdayFromRataDie,
  msFromTimeParts,
  unixTimeFromParts
  )

import Date exposing (Month(..))
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
    p = if r1 == 0 then 0 else 1
  in
    q400 * 400 + q100 * 100 + q4 * 4 + q1 + p


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


-- rataDieFromCalendarDate
rataDieFromYMD : Int -> Month -> Int -> RataDie
rataDieFromYMD y m d =
  let
    yd = rataDieBeforeStartOfYear y
    md = daysBeforeStartOfMonth y m
  in
    yd + md + d


-- calendarDateFromRataDie
ymdFromRataDie : RataDie -> (Int, Month, Int)
ymdFromRataDie rd =
  let
    (y, ordinalDay) = ordinalDateFromRataDie rd
    m = List.reverse months |> find (\m -> daysBeforeStartOfMonth y m < ordinalDay) |> Maybe.withDefault Jan
    d = ordinalDay - daysBeforeStartOfMonth y m
  in
    (y, m, d)


-- weekdayFromRataDie
isoWeekdayFromRataDie : RataDie -> Int
isoWeekdayFromRataDie rd =
  case rd % 7 of
    0 -> 7
    n -> n


rataDieFromWeekDate : Int -> Int -> Int -> RataDie
rataDieFromWeekDate y w d =
  let
    jan4RD = rataDieFromYMD y Jan 4
    week1Day0RD = jan4RD - isoWeekdayFromRataDie jan4RD
  in
    week1Day0RD + (w - 1) * 7 + d


--weekDateFromRataDie : RataDie -> (Int, Int, Int)
--weekDateFromRataDie rd =


-- Unix time

unixEpochRD : RataDie
unixEpochRD =
  719163


unixDaysFromYMD : Int -> Month -> Int -> Int
unixDaysFromYMD y m d =
  rataDieFromYMD y m d - unixEpochRD


msFromTimeParts : Int -> Int -> Int -> Int -> Int
msFromTimeParts hh mm ss ms =
  ms
  + msPerSecond * ss
  + msPerMinute * mm
  + msPerHour * hh


unixTimeFromParts : Int -> Month -> Int -> Int -> Int -> Int -> Int -> Int
unixTimeFromParts y m d hh mm ss ms =
  msPerDay * unixDaysFromYMD y m d
  + msFromTimeParts hh mm ss ms
