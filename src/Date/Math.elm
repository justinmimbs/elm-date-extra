module Date.Math exposing (
  equal,
  compare,
  isBetween,
  clamp,
  Interval(..),
  equalBy,
  floor,
  ceiling,
  add,
  diff,
  range
  )

{-| Functions for working with dates as numeric quantities.

# Dates as Atomic Values
@docs equal, compare, isBetween, clamp

# Dates as Composite Values
@docs Interval, equalBy, floor, ceiling, add, diff, range
-}


import Date exposing (Date, Month(..), Day(..), toTime, year, month, day, hour, minute, second, millisecond, dayOfWeek)
import Date.Facts exposing (daysInMonth, monthFromMonthNumber, weekdayNumberFromDayOfWeek, msPerSecond, msPerMinute, msPerHour, msPerDay)
import Date.Extract exposing (monthNumber, quarter, weekYear, weekNumber, weekdayNumber, fractionalDay)
import Date.Create exposing (fromParts, fromCalendarDate)


{-| Test equality of two dates.
-}
equal : Date -> Date -> Bool
equal a b =
  toTime a == toTime b


{-| Compare two dates. This can be used as the compare function for
`List.sortWith`.

    sortedDates = List.sortWith Date.compare unsortedDates
-}
compare : Date -> Date -> Order
compare a b =
  Basics.compare (toTime a) (toTime b)


comparableIsBetween : comparable -> comparable -> comparable -> Bool
comparableIsBetween a b x =
  a <= x && x <= b || b <= x && x <= a


{-| Test if a date is within a given range, inclusive of the range values. The
expression `Date.isBetween a b x` tests if `x` is between `a` and `b`.
-}
isBetween : Date -> Date -> Date -> Bool
isBetween date1 date2 date =
  comparableIsBetween (toTime date1) (toTime date2) (toTime date)


{-| Clamp a date within a given range.
-}
clamp : Date -> Date -> Date -> Date
clamp min max date =
  if toTime date < toTime min then
    min
  else if toTime date > toTime max then
    max
  else
    date


{-| Represents an interval of time.
-}
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


{-| Test if two dates fall within the same interval.

    dec31 = Date.fromCalendarDate 1999 Dec 31
    jan1 = Date.fromCalendarDate 2000 Jan 1

    Date.equalBy Month dec31 jan1 -- False
    Date.equalBy Week dec31 jan1 -- True
-}
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


{-| Round down a date to the beginning of the closest interval. The resulting
date will be less than or equal to the one provided.

    Date.floor Hour (Date.fromParts 1999 Dec 31 23 59 59 999)
    -- 31 December 1999, 23:00
-}
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
    d' = Basics.min d (daysInMonth y' m')
  in
    fromParts y' m' d' hh mm ss ms


{-| Add a number of whole intervals to a date.

    Date.add Week 2 (Date.fromParts 2007 Mar 15 11 55 0 0)
    -- 29 March 2007, 11:55

When adding Month, Quarter, or Year intervals, day values are clamped at the
end of the month if necessary.

    Date.add Month 1 (Date.fromParts 2000 Jan 31 0 0 0 0)
    -- 29 February 2000
-}
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
      Year        -> addMonths (n * 12) date
      Quarter     -> addMonths (n * 3) date
      Week        -> fromParts y m (d + n * 7) hh mm ss ms
      weekday     -> fromParts y m (d + n * 7) hh mm ss ms


{-| Round up a date to the beginning of the closest interval. The resulting
date will be greater than or equal to the one provided.

    Date.ceiling Monday (Date.fromParts 1999 Dec 31 23 59 59 999)
    -- 3 January 2000
-}
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


{-| Find the difference, as a number of whole intervals, between two dates.

    Date.diff Month
      (Date.fromParts 2007 Mar 15 11 55 0 0)
      (Date.fromParts 2007 Sep 1 0 0 0 0)
    -- 5
-}
diff : Interval -> Date -> Date -> Int
diff interval date1 date2 =
  let
    diffMS = toTime date2 - toTime date1 |> Basics.floor
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


{-| Create a list of dates, at rounded intervals, increasing by a step value,
between two dates. The list will start at or after the first date, and end
before the second date.

    Date.range Day 2
      (Date.fromParts 2007 Mar 15 11 55 0 0)
      (Date.fromParts 2007 Mar 22 0 0 0 0)
    -- [ 16 March 2007, 18 March 2007, 20 March 2007 ]
-}
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
