module Date.Math exposing (
  Interval(..),
  floor,
  ceiling,
  add,
  diff,
  range
  )

import Date exposing (Date, toTime, year, month, day, hour, minute, second, millisecond, Month(..), Day(..), dayOfWeek)
import Date.Fact exposing (monthFromMonthNumber, isoWeekdayFromDayOfWeek)
import Date.Internal exposing (rataDieFromYMD)
import Date.Extract exposing (monthNumber, quarter, isoWeekday)
import Date.Create exposing (fromSpec, fromYMD)


unfold : (b -> Maybe (a, b)) -> b -> List a
unfold f seed =
  case f seed of
    Nothing -> []
    Just (x, nextSeed) -> x :: unfold f nextSeed


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
  toTime date - toTime (fromYMD (year date) (month date) (day date)) |> Basics.floor


fractionOfDay : Date -> Float
fractionOfDay date =
  (timeOfDayMS date |> toFloat) / 86400000


toRataDieMoment : Date -> Float
toRataDieMoment date =
  (rataDieFromYMD (year date) (month date) (day date) |> toFloat) + fractionOfDay date


ordinalMonth : Date -> Int
ordinalMonth date =
  (year date) * 12 + (monthNumber date)


-- operations

daysToPreviousDayOfWeek : Day -> Date -> Int
daysToPreviousDayOfWeek d date =
  negate <| (isoWeekday date - isoWeekdayFromDayOfWeek d + 7) % 7


floor : Interval -> Date -> Date
floor interval date =
  let
    (y, m, d, hh, mm, ss, _) = specFromDate date
  in
    case interval of
      Millisecond -> date
      Second      -> fromSpec y m d hh mm ss 0
      Minute      -> fromSpec y m d hh mm 0 0
      Hour        -> fromSpec y m d hh 0 0 0
      Day         -> fromYMD y m d
      Month       -> fromYMD y m 1
      Year        -> fromYMD y Jan 1
      Quarter     -> fromYMD y (monthFromQuarter <| quarter date) 1
      Week        -> fromYMD y m (d + daysToPreviousDayOfWeek Mon date)
      Monday      -> fromYMD y m (d + daysToPreviousDayOfWeek Mon date)
      Tuesday     -> fromYMD y m (d + daysToPreviousDayOfWeek Tue date)
      Wednesday   -> fromYMD y m (d + daysToPreviousDayOfWeek Wed date)
      Thursday    -> fromYMD y m (d + daysToPreviousDayOfWeek Thu date)
      Friday      -> fromYMD y m (d + daysToPreviousDayOfWeek Fri date)
      Saturday    -> fromYMD y m (d + daysToPreviousDayOfWeek Sat date)
      Sunday      -> fromYMD y m (d + daysToPreviousDayOfWeek Sun date)


addMonths : Int -> Date -> Date
addMonths n date =
  let
    (y, m, d, hh, mm, ss, ms) = specFromDate date
    om = ordinalMonth date + n + -1
    y' = om // 12
    m' = om % 12 + 1 |> monthFromMonthNumber
  in
    fromSpec y' m' d hh mm ss ms


add : Interval -> Int -> Date -> Date
add interval n date =
  let
    (y, m, d, hh, mm, ss, ms) = specFromDate date
  in
    case interval of
      Millisecond -> fromSpec y m d hh mm ss (ms + n)
      Second      -> fromSpec y m d hh mm (ss + n) ms
      Minute      -> fromSpec y m d hh (mm + n) ss ms
      Hour        -> fromSpec y m d (hh + n) mm ss ms
      Day         -> fromSpec y m (d + n) hh mm ss ms
      Month       -> addMonths n date
      Year        -> fromSpec (y + n) m d hh mm ss ms
      Quarter     -> addMonths (n * 3) date
      Week        -> fromSpec y m (d + n * 7) hh mm ss ms
      Monday      -> fromSpec y m (d + n * 7 + daysToPreviousDayOfWeek Mon date) hh mm ss ms
      Tuesday     -> fromSpec y m (d + n * 7 + daysToPreviousDayOfWeek Tue date) hh mm ss ms
      Wednesday   -> fromSpec y m (d + n * 7 + daysToPreviousDayOfWeek Wed date) hh mm ss ms
      Thursday    -> fromSpec y m (d + n * 7 + daysToPreviousDayOfWeek Thu date) hh mm ss ms
      Friday      -> fromSpec y m (d + n * 7 + daysToPreviousDayOfWeek Fri date) hh mm ss ms
      Saturday    -> fromSpec y m (d + n * 7 + daysToPreviousDayOfWeek Sat date) hh mm ss ms
      Sunday      -> fromSpec y m (d + n * 7 + daysToPreviousDayOfWeek Sun date) hh mm ss ms


ceiling : Interval -> Date -> Date
ceiling interval date =
  let
    floored = floor interval date
  in
    if toTime date == toTime floored then date else add interval 1 floored


diffMonth : Date -> Date -> Int
diffMonth date1 date2 =
  let
    fractionOfMonth : Date -> Float
    fractionOfMonth date =
      ((day date - 1 |> toFloat) + fractionOfDay date) / 31

    ordinalMonth' : Date -> Float
    ordinalMonth' date =
      (ordinalMonth date |> toFloat) + fractionOfMonth date
  in
    ordinalMonth' date2 - ordinalMonth' date1 |> truncate


diff : Interval -> Date -> Date -> Int
diff interval date1 date2 =
  let
    diffMS = toTime date2 - toTime date1 |> truncate
  in
    case interval of
      Millisecond -> diffMS
      Second      -> diffMS // 1000
      Minute      -> diffMS // 60000
      Hour        -> diffMS // 3600000
      Day         -> toRataDieMoment date2 - toRataDieMoment date1 |> truncate
      Month       -> diffMonth date1 date2
      Year        -> diffMonth date1 date2 // 12
      Quarter     -> diffMonth date1 date2 // 3
      Week        -> diff Day date1 date2 // 7
      weekday     -> diff Day (floor weekday date1) (floor weekday date2) // 7


range : Interval -> Int -> Date -> Date -> List Date
range interval step start end =
  let
    next : Date -> Maybe (Date, Date)
    next date =
      if toTime date >= toTime end then
        Nothing
      else
        Just (date, add interval (max 1 step) date)
  in
    unfold next <| ceiling interval start
