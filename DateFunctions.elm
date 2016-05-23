module DateFunctions exposing (..)

import Date exposing (Date, toTime, year, month, day, hour, minute, second, millisecond, Month(..), Day(..), dayOfWeek)

takeWhile : (a -> Bool) -> List a -> List a
takeWhile pred list = 
  case list of
    [] -> []
    x::xs -> if pred x then x :: (takeWhile pred xs) else []

find : (a -> Bool) -> List a -> Maybe a
find pred list =
  case list of
    [] -> Nothing
    x::xs -> if pred x then Just x else find pred xs

last : List a -> Maybe a
last =
  List.head << List.reverse

isLeapYear : Int -> Bool
isLeapYear y =
  y % 4 == 0 && y % 100 /= 0 || y % 400 == 0

daysInMonth : Int -> Month -> Int
daysInMonth y m =
  case m of
    Jan -> 31
    Feb -> if isLeapYear y then 29 else 28
    Mar -> 31
    Apr -> 30
    May -> 31
    Jun -> 30
    Jul -> 31
    Aug -> 31
    Sep -> 30
    Oct -> 31
    Nov -> 30
    Dec -> 31

daysAtStartOfMonth : Int -> Month -> Int
daysAtStartOfMonth y m =
  case m of
    Jan -> 0
    Feb -> 31
    Mar -> if isLeapYear y then 60  else 59
    Apr -> if isLeapYear y then 91  else 90
    May -> if isLeapYear y then 121 else 120
    Jun -> if isLeapYear y then 152 else 151
    Jul -> if isLeapYear y then 182 else 181
    Aug -> if isLeapYear y then 213 else 212
    Sep -> if isLeapYear y then 244 else 243
    Oct -> if isLeapYear y then 274 else 273
    Nov -> if isLeapYear y then 305 else 304
    Dec -> if isLeapYear y then 335 else 334

months : List Month
months = [ Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec ]

leapYearsInCommonEra : Int -> Int
leapYearsInCommonEra y =
  (y // 4) - (y // 100) + (y // 400)

-- RataDie

type alias RataDie = Int

rataDieFromYMD : Int -> Month -> Int -> RataDie
rataDieFromYMD y m d =
  let
    yd = 365 * (y - 1) + leapYearsInCommonEra (y - 1)
    md = daysAtStartOfMonth y m
  in
    yd + md + d

-- integer division returning (Quotient, Remainder)
(///) : Int -> Int -> (Int, Int)
(///) n d =
  (n // d, n % d)

infixr 8 ///

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

ymdFromRataDie : RataDie -> (Int, Month, Int)
ymdFromRataDie rd =
  let
    y = yearFromRataDie rd
    ordinalDay = 1 + rd - (rataDieFromYMD y Jan 1)
    m = find (\m -> daysAtStartOfMonth y m < ordinalDay) (List.reverse months) |> Maybe.withDefault Jan
    d = ordinalDay - daysAtStartOfMonth y m
  in
    (y, m, d)
{-
dayOfWeekFromRataDie : RataDie -> Day
dayOfWeekFromRataDie rd =
  case rd % 7 of
    1 -> Mon
    2 -> Tue
    3 -> Wed
    4 -> Thu
    5 -> Fri
    6 -> Sat
    _ -> Sun
-}
isoWeekdayFromRataDie : RataDie -> Int
isoWeekdayFromRataDie rd =
  case rd % 7 of
    0 -> 7
    n -> n

-- unix time

unixEpochRD : RataDie
unixEpochRD = 719163

unixDaysFromYMD : Int -> Month -> Int -> Int
unixDaysFromYMD y m d =
  rataDieFromYMD y m d - unixEpochRD

unixTimeFromSpec : Int -> Month -> Int -> Int -> Int -> Int -> Int -> Int
unixTimeFromSpec y m d hh mm ss ms =
  unixDaysFromYMD y m d * 86400000
    + hh * 3600000
    + mm * 60000
    + ss * 1000
    + ms

-- extractions

timezoneOffsetMS : Date -> Int
timezoneOffsetMS date =
  let
    t = unixTimeFromSpec (year date) (month date) (day date) (hour date) (minute date) (second date) (millisecond date)
  in
    floor (toTime date) - t

timezoneOffset : Date -> Int
timezoneOffset date =
  (timezoneOffsetMS date) // 1000 // 60

-- create Date
-- TODO default to Local?
type Timezone = UTC | Local

dateFromSpec : Timezone -> Int -> Month -> Int -> Int -> Int -> Int -> Int -> Date
dateFromSpec tz y m d hh mm ss ms =
  let
    date = Date.fromTime <| toFloat <| unixTimeFromSpec y m d hh mm ss ms
  in
    case tz of
      UTC -> date
      Local -> Date.fromTime <| toFloat <| unixTimeFromSpec y m d hh mm ss ms + timezoneOffsetMS date

-- extractions

{-
isoWeekdayFromDayOfWeek : Day -> Int
isoWeekdayFromDayOfWeek d =
-}

isoWeekday : Date -> Int
isoWeekday date =
  case dayOfWeek date of
    Mon -> 1
    Tue -> 2
    Wed -> 3
    Thu -> 4
    Fri -> 5
    Sat -> 6
    Sun -> 7

isoYear : Date -> Int
isoYear date =
  let
    daysToThursday = 4 - isoWeekday date
    thursdayRD = rataDieFromYMD (year date) (month date) (day date) + daysToThursday
  in
    yearFromRataDie thursdayRD

isoWeek : Date -> Int
isoWeek date =
  let
    y = isoYear date
    jan4RD = rataDieFromYMD y Jan 4
    daysToMonday = 1 - (isoWeekdayFromRataDie jan4RD)
    week1Day1RD = jan4RD + daysToMonday
  in
    (+) 1 <| floor <| toFloat (rataDieFromYMD (year date) (month date) (day date) - week1Day1RD) / 7

-- temp

isoWeekDateFromDate : Date -> (Int, Int, Int)
isoWeekDateFromDate date =
  (isoYear date, isoWeek date, isoWeekday date)

dateFromYMD : Int -> Month -> Int -> Date
dateFromYMD y m d =
  dateFromSpec Local y m d 0 0 0 0
