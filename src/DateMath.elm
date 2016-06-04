module DateMath exposing (..)

import Date exposing (Date, toTime, year, month, day, hour, minute, second, millisecond, Month(..), Day(..), dayOfWeek)
import DateFunctions exposing (..) --TODO name imports

unfold : (b -> Maybe (a, b)) -> b -> List a
unfold f seed =
  case f seed of
    Nothing -> []
    Just (x, seed') -> x :: unfold f seed'

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

-- conversions

specFromDate : Date -> (Int, Month, Int, Int, Int, Int, Int)
specFromDate date =
  (year date, month date, day date, hour date, minute date, second date, millisecond date)

monthFromQuarter : Int -> Month
monthFromQuarter q =
  case q of
    1 -> Jan
    2 -> Apr
    3 -> Jul
    _ -> Oct

-- extractions

timeOfDayMS : Date -> Int
timeOfDayMS date =
  toTime date - toTime (dateFromYMD (year date) (month date) (day date)) |> floor

fractionOfDay : Date -> Float
fractionOfDay date =
  (timeOfDayMS date |> toFloat) / 86400000

rataDie : Date -> Float
rataDie date =
  (rataDieFromYMD (year date) (month date) (day date) |> toFloat) + fractionOfDay date

ordinalMonth : Date -> Int
ordinalMonth date =
  (year date) * 12 + (monthNumber date)

-- operations

daysToPreviousDayOfWeek : Day -> Date -> Int
daysToPreviousDayOfWeek d date =
  negate <| (isoWeekday date - isoWeekdayFromDayOfWeek d + 7) % 7

dateFloor : Interval -> Date -> Date
dateFloor interval date =
  let
    (y, m, d, hh, mm, ss, _) = specFromDate date
  in
    case interval of
      Millisecond -> date
      Second      -> dateFromSpec y m d hh mm ss 0
      Minute      -> dateFromSpec y m d hh mm 0 0
      Hour        -> dateFromSpec y m d hh 0 0 0
      Day         -> dateFromYMD y m d
      Month       -> dateFromYMD y m 1
      Year        -> dateFromYMD y Jan 1
      Quarter     -> dateFromYMD y (monthFromQuarter <| quarter date) 1
      Week        -> dateFromYMD y m (d + daysToPreviousDayOfWeek Mon date)
      Monday      -> dateFromYMD y m (d + daysToPreviousDayOfWeek Mon date)
      Tuesday     -> dateFromYMD y m (d + daysToPreviousDayOfWeek Tue date)
      Wednesday   -> dateFromYMD y m (d + daysToPreviousDayOfWeek Wed date)
      Thursday    -> dateFromYMD y m (d + daysToPreviousDayOfWeek Thu date)
      Friday      -> dateFromYMD y m (d + daysToPreviousDayOfWeek Fri date)
      Saturday    -> dateFromYMD y m (d + daysToPreviousDayOfWeek Sat date)
      Sunday      -> dateFromYMD y m (d + daysToPreviousDayOfWeek Sun date)

dateAddMonths : Int -> Date -> Date
dateAddMonths n date =
  let
    (y, m, d, hh, mm, ss, ms) = specFromDate date
    om = ordinalMonth date + n + -1
    y' = om // 12
    m' = om % 12 + 1 |> monthFromMonthNumber
  in
    dateFromSpec y' m' d hh mm ss ms

dateAdd : Interval -> Int -> Date -> Date
dateAdd interval n date =
  let
    (y, m, d, hh, mm, ss, ms) = specFromDate date
  in
    case interval of
      Millisecond -> dateFromSpec y m d hh mm ss (ms + n)
      Second      -> dateFromSpec y m d hh mm (ss + n) ms
      Minute      -> dateFromSpec y m d hh (mm + n) ss ms
      Hour        -> dateFromSpec y m d (hh + n) mm ss ms
      Day         -> dateFromSpec y m (d + n) hh mm ss ms
      Month       -> dateAddMonths n date
      Year        -> dateFromSpec (y + n) m d hh mm ss ms
      Quarter     -> dateAddMonths (n * 3) date
      Week        -> dateFromSpec y m (d + n * 7) hh mm ss ms
      Monday      -> dateFromSpec y m (d + n * 7 + daysToPreviousDayOfWeek Mon date) hh mm ss ms
      Tuesday     -> dateFromSpec y m (d + n * 7 + daysToPreviousDayOfWeek Tue date) hh mm ss ms
      Wednesday   -> dateFromSpec y m (d + n * 7 + daysToPreviousDayOfWeek Wed date) hh mm ss ms
      Thursday    -> dateFromSpec y m (d + n * 7 + daysToPreviousDayOfWeek Thu date) hh mm ss ms
      Friday      -> dateFromSpec y m (d + n * 7 + daysToPreviousDayOfWeek Fri date) hh mm ss ms
      Saturday    -> dateFromSpec y m (d + n * 7 + daysToPreviousDayOfWeek Sat date) hh mm ss ms
      Sunday      -> dateFromSpec y m (d + n * 7 + daysToPreviousDayOfWeek Sun date) hh mm ss ms

dateCeil : Interval -> Date -> Date
dateCeil interval date =
  let
    floored = dateFloor interval date
  in
    if toTime date == toTime floored then date else dateAdd interval 1 floored

dateDiffMonth : Date -> Date -> Int
dateDiffMonth date1 date2 =
  let
    fractionOfMonth : Date -> Float
    fractionOfMonth date =
      ((day date - 1 |> toFloat) + fractionOfDay date) / 31

    ordinalMonth' : Date -> Float
    ordinalMonth' date =
      (ordinalMonth date |> toFloat) + fractionOfMonth date
  in
    ordinalMonth' date2 - ordinalMonth' date1 |> truncate

dateDiff : Interval -> Date -> Date -> Int
dateDiff interval date1 date2 =
  let
    diffMS = toTime date2 - toTime date1 |> truncate
  in
    case interval of
      Millisecond -> diffMS
      Second      -> diffMS // 1000
      Minute      -> diffMS // 60000
      Hour        -> diffMS // 3600000
      Day         -> rataDie date2 - rataDie date1 |> truncate
      Month       -> dateDiffMonth date1 date2
      Year        -> dateDiffMonth date1 date2 // 12
      Quarter     -> dateDiffMonth date1 date2 // 3
      Week        -> dateDiff Day date1 date2 // 7
      weekday     -> dateDiff Day (dateFloor weekday date1) (dateFloor weekday date2) // 7

dateRange : Interval -> Int -> Date -> Date -> List Date
dateRange interval step start end =
  let
    next : Date -> Maybe (Date, Date)
    next date =
      if toTime date >= toTime end then
        Nothing
      else
        Just (date, dateAdd interval (max 1 step) date)
  in
    unfold next <| dateCeil interval start
