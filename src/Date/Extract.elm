module Date.Extract exposing (
  monthNumber,
  quarter,
  ordinalDay,
  fractionalDay,
  weekdayNumber,
  weekNumber,
  weekYear,
  offsetFromUtc
  )

{-| This module provides additional extractions to those available in the
Core library.

@docs monthNumber, quarter, ordinalDay, fractionalDay, weekdayNumber, weekNumber, weekYear, offsetFromUtc
-}


import Date exposing (Date, Month(..), toTime, year, month, day, hour, minute, second, millisecond, dayOfWeek)
import Date.Facts exposing (monthNumberFromMonth, weekdayNumberFromDayOfWeek, daysBeforeStartOfMonth, msPerMinute, msPerDay)
import Date.Internal.Core exposing (unixTimeFromParts, weekYearFromCalendarDate, weekNumberFromCalendarDate, msFromTimeParts)


{-| Extract the month number of a date. Given the date 23 June 1990 at
11:45 a.m. this returns the integer 6.
-}
monthNumber : Date -> Int
monthNumber date =
  monthNumberFromMonth <| month date


{-| Extract the quarter of a date. Given the date 23 June 1990 at
11:45 a.m. this returns the integer 2.
-}
quarter : Date -> Int
quarter date =
  monthNumber date |> toFloat |> (\n -> n / 3) |> ceiling


{-| Extract the ordinal day of a date. Given the date 23 June 1990 at
11:45 a.m. this returns the integer 174.
-}
ordinalDay : Date -> Int
ordinalDay date =
  daysBeforeStartOfMonth (year date) (month date) + day date


{-| Extract the fractional day of a date. Given the date 23 June 1990 at
11:45 a.m. this returns the float 0.4895833333333333.
-}
fractionalDay : Date -> Float
fractionalDay date =
  let
    timeOfDayMS = msFromTimeParts (hour date) (minute date) (second date) (millisecond date)
  in
    toFloat timeOfDayMS / toFloat msPerDay


{-| Extract the weekday number (beginning at 1 for Monday) of a date. Given
the date 23 June 1990 at 11:45 a.m. this returns the integer 6.
-}
weekdayNumber : Date -> Int
weekdayNumber date =
  weekdayNumberFromDayOfWeek <| dayOfWeek date


{-| Extract the week number of a date. Given the date 23 June 1990 at
11:45 a.m. this returns the integer 25.
-}
weekNumber : Date -> Int
weekNumber date =
  weekNumberFromCalendarDate (year date) (month date) (day date)


{-| Extract the week-numbering year of a date. Given the date 23 June
1990 at 11:45 a.m. this returns the integer 1990.
-}
weekYear : Date -> Int
weekYear date =
  weekYearFromCalendarDate (year date) (month date) (day date)


msOffsetFromUtc : Date -> Int
msOffsetFromUtc date =
  let
    localTime = toFloat <| unixTimeFromParts (year date) (month date) (day date) (hour date) (minute date) (second date) (millisecond date)
    utcTime = toTime date
  in
    localTime - utcTime |> floor


{-| Extract the offset from UTC time in minutes of a date. Given a date with a local offset of UTC-05:00 this returns the integer -300.
-}
offsetFromUtc : Date -> Int
offsetFromUtc date =
  msOffsetFromUtc date // msPerMinute
