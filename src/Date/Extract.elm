module Date.Extract exposing (
  monthNumber,
  quarter,
  ordinalDay,
  fractionalDay,
  isoWeekday,
  isoYear,
  isoWeek,
  offsetFromUTC
  )

import Date exposing (Date, Month(..), toTime, year, month, day, hour, minute, second, millisecond, dayOfWeek)
import Date.Facts exposing (monthNumberFromMonth, isoWeekdayFromDayOfWeek, daysBeforeStartOfMonth, msPerMinute, msPerDay)
import Date.Internal.Core exposing (unixTimeFromParts, rataDieFromYMD, yearFromRataDie, isoWeekdayFromRataDie, msFromTimeParts)


monthNumber : Date -> Int
monthNumber date =
  monthNumberFromMonth <| month date


quarter : Date -> Int
quarter date =
  monthNumber date |> toFloat |> (\n -> n / 3) |> ceiling


ordinalDay : Date -> Int
ordinalDay date =
  daysBeforeStartOfMonth (year date) (month date) + day date


fractionalDay : Date -> Float
fractionalDay date =
  let
    timeOfDayMS = msFromTimeParts (hour date) (minute date) (second date) (millisecond date)
  in
    toFloat timeOfDayMS / toFloat msPerDay


isoWeekday : Date -> Int
isoWeekday date =
  isoWeekdayFromDayOfWeek <| dayOfWeek date


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
    jan4RD = rataDieFromYMD (isoYear date) Jan 4
    daysToMonday = 1 - (isoWeekdayFromRataDie jan4RD)
    week1Day1RD = jan4RD + daysToMonday
  in
    (rataDieFromYMD (year date) (month date) (day date) - week1Day1RD) // 7 + 1


msOffsetFromUTC : Date -> Int
msOffsetFromUTC date =
  let
    t = unixTimeFromParts (year date) (month date) (day date) (hour date) (minute date) (second date) (millisecond date)
  in
    t - floor (toTime date)


offsetFromUTC : Date -> Int
offsetFromUTC date =
  msOffsetFromUTC date // msPerMinute
