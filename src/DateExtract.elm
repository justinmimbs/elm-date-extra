module DateExtract exposing (..)

import Date exposing (Date, Month(..), year, month, day, hour, minute, second, millisecond, dayOfWeek)
import DateFact exposing (monthNumberFromMonth, isoWeekdayFromDayOfWeek, msPerMinute)
import DateInternal exposing (unixTimeFromSpec, rataDieFromYMD, yearFromRataDie, isoWeekdayFromRataDie)


monthNumber : Date -> Int
monthNumber date =
  monthNumberFromMonth <| month date


quarter : Date -> Int
quarter date =
  monthNumber date |> toFloat |> (\n -> n / 3) |> ceiling


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
    rataDieFromYMD (year date) (month date) (day date) - week1Day1RD |> toFloat |> (\n -> n / 7) |> floor |> (+) 1


timezoneOffsetMS : Date -> Int
timezoneOffsetMS date =
  let
    t = unixTimeFromSpec (year date) (month date) (day date) (hour date) (minute date) (second date) (millisecond date)
  in
    (Date.toTime date |> floor) - t


timezoneOffset : Date -> Int
timezoneOffset date =
  timezoneOffsetMS date // msPerMinute
