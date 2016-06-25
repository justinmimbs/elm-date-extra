module Date.Math exposing (
  Interval(..),
  equal,
  compare,
  isBetween,
  clamp,
  equalBy,
  floor,
  ceiling,
  add,
  diff,
  range
  )

import Date exposing (Date, Month(..), Day(..), toTime, year, month, day, hour, minute, second, millisecond, dayOfWeek)
import Date.Facts exposing (monthFromMonthNumber, weekdayNumberFromDayOfWeek, msPerSecond, msPerMinute, msPerHour, msPerDay)
import Date.Extract exposing (monthNumber, quarter, weekYear, weekNumber, weekdayNumber, fractionalDay)
import Date.Create exposing (fromParts, fromCalendarDate)


-- Operations for dates as singular values

equal : Date -> Date -> Bool
equal a b =
  toTime a == toTime b


compare : Date -> Date -> Order
compare a b =
  Basics.compare (toTime a) (toTime b)


comparableIsBetween : comparable -> comparable -> comparable -> Bool
comparableIsBetween a b x =
  a <= x && x <= b || b <= x && x <= a


isBetween : Date -> Date -> Date -> Bool
isBetween date1 date2 date =
  comparableIsBetween (toTime date1) (toTime date2) (toTime date)


clamp : Date -> Date -> Date -> Date
clamp min max date =
  if toTime date < toTime min then
    min
  else if toTime date > toTime max then
    max
  else
    date


-- Operations for dates as composite values

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


equalBy : Interval -> Date -> Date -> Bool
equalBy interval date1 date2 =
  case interval of
    Millisecond -> toTime date1 == toTime date2
    Second      -> second date1 == second date2 && equalBy Minute date1 date2
    Minute      -> minute date1 == minute date2 && equalBy Hour date1 date2
    Hour        -> hour date1 == hour date2 && equalBy Day date1 date2
    Day         -> day date1 == day date2 && equalBy Month date1 date2
    Month       -> month date1 == month date2 && equalBy Year date1 date2
    Year        -> year date1 == year date2
    Quarter     -> quarter date1 == quarter date2 && equalBy Year date1 date2
    Week        -> weekNumber date1 == weekNumber date2 && weekYear date1 == weekYear date2
    weekday     -> equalBy Day (floor weekday date1) (floor weekday date2)


-- facts

monthFromQuarter : Int -> Month
monthFromQuarter q =
  case q of
    1 -> Jan
    2 -> Apr
    3 -> Jul
    _ -> Oct


-- extractions

ordinalMonth : Date -> Int
ordinalMonth date =
  (year date) * 12 + (monthNumber date)


-- conversions

toParts : Date -> (Int, Month, Int, Int, Int, Int, Int)
toParts date =
  (year date, month date, day date, hour date, minute date, second date, millisecond date)


-- operations

daysToPreviousDayOfWeek : Day -> Date -> Int
daysToPreviousDayOfWeek d date =
  negate <| (weekdayNumber date - weekdayNumberFromDayOfWeek d + 7) % 7


floor : Interval -> Date -> Date
floor interval date =
  let
    (y, m, d, hh, mm, ss, _) = toParts date
  in
    case interval of
      Millisecond -> date
      Second      -> fromParts y m d hh mm ss 0
      Minute      -> fromParts y m d hh mm 0 0
      Hour        -> fromParts y m d hh 0 0 0
      Day         -> fromCalendarDate y m d
      Month       -> fromCalendarDate y m 1
      Year        -> fromCalendarDate y Jan 1
      Quarter     -> fromCalendarDate y (monthFromQuarter <| quarter date) 1
      Week        -> fromCalendarDate y m (d + daysToPreviousDayOfWeek Mon date)
      Monday      -> fromCalendarDate y m (d + daysToPreviousDayOfWeek Mon date)
      Tuesday     -> fromCalendarDate y m (d + daysToPreviousDayOfWeek Tue date)
      Wednesday   -> fromCalendarDate y m (d + daysToPreviousDayOfWeek Wed date)
      Thursday    -> fromCalendarDate y m (d + daysToPreviousDayOfWeek Thu date)
      Friday      -> fromCalendarDate y m (d + daysToPreviousDayOfWeek Fri date)
      Saturday    -> fromCalendarDate y m (d + daysToPreviousDayOfWeek Sat date)
      Sunday      -> fromCalendarDate y m (d + daysToPreviousDayOfWeek Sun date)


addMonths : Int -> Date -> Date
addMonths n date =
  let
    (y, m, d, hh, mm, ss, ms) = toParts date
    om = ordinalMonth date + n + -1
    y' = om // 12
    m' = om % 12 + 1 |> monthFromMonthNumber
  in
    fromParts y' m' d hh mm ss ms


add : Interval -> Int -> Date -> Date
add interval n date =
  let
    (y, m, d, hh, mm, ss, ms) = toParts date
  in
    case interval of
      Millisecond -> fromParts y m d hh mm ss (ms + n)
      Second      -> fromParts y m d hh mm (ss + n) ms
      Minute      -> fromParts y m d hh (mm + n) ss ms
      Hour        -> fromParts y m d (hh + n) mm ss ms
      Day         -> fromParts y m (d + n) hh mm ss ms
      Month       -> addMonths n date
      Year        -> fromParts (y + n) m d hh mm ss ms
      Quarter     -> addMonths (n * 3) date
      Week        -> fromParts y m (d + n * 7) hh mm ss ms
      Monday      -> fromParts y m (d + n * 7 + daysToPreviousDayOfWeek Mon date) hh mm ss ms
      Tuesday     -> fromParts y m (d + n * 7 + daysToPreviousDayOfWeek Tue date) hh mm ss ms
      Wednesday   -> fromParts y m (d + n * 7 + daysToPreviousDayOfWeek Wed date) hh mm ss ms
      Thursday    -> fromParts y m (d + n * 7 + daysToPreviousDayOfWeek Thu date) hh mm ss ms
      Friday      -> fromParts y m (d + n * 7 + daysToPreviousDayOfWeek Fri date) hh mm ss ms
      Saturday    -> fromParts y m (d + n * 7 + daysToPreviousDayOfWeek Sat date) hh mm ss ms
      Sunday      -> fromParts y m (d + n * 7 + daysToPreviousDayOfWeek Sun date) hh mm ss ms


ceiling : Interval -> Date -> Date
ceiling interval date =
  let
    floored = floor interval date
  in
    if toTime date == toTime floored then date else add interval 1 floored


diffMonth : Date -> Date -> Int
diffMonth date1 date2 =
  let
    fractionalMonth : Date -> Float
    fractionalMonth date =
      ((day date - 1 |> toFloat) + fractionalDay date) / 31

    ordinalMonth' : Date -> Float
    ordinalMonth' date =
      (ordinalMonth date |> toFloat) + fractionalMonth date
  in
    ordinalMonth' date2 - ordinalMonth' date1 |> truncate


diff : Interval -> Date -> Date -> Int
diff interval date1 date2 =
  let
    diffMS = toTime date2 - toTime date1 |> truncate
  in
    case interval of
      Millisecond -> diffMS
      Second      -> diffMS // msPerSecond
      Minute      -> diffMS // msPerMinute
      Hour        -> diffMS // msPerHour
      Day         -> diffMS // msPerDay
      Month       -> diffMonth date1 date2
      Year        -> diffMonth date1 date2 // 12
      Quarter     -> diffMonth date1 date2 // 3
      Week        -> diff Day date1 date2 // 7
      weekday     -> diff Day (floor weekday date1) (floor weekday date2) // 7


unfold : (b -> Maybe (a, b)) -> b -> List a
unfold f seed =
  case f seed of
    Nothing -> []
    Just (x, nextSeed) -> x :: unfold f nextSeed


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
