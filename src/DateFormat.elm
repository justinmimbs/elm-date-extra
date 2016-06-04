module DateFormat exposing (..)

import Date exposing (Date, year, month, day, hour, minute, second, millisecond, Month(..), Day(..), dayOfWeek)
import DateFunctions exposing (monthNumber, quarter, isoYear, isoWeek, isoWeekday, timezoneOffset)
import String exposing (slice, padLeft)
import Regex exposing (Regex, regex, replace, HowMany(..))

monthName : Month -> String
monthName m =
  case m of
    Jan -> "January"
    Feb -> "February"
    Mar -> "March"
    Apr -> "April"
    May -> "May"
    Jun -> "June"
    Jul -> "July"
    Aug -> "August"
    Sep -> "September"
    Oct -> "October"
    Nov -> "November"
    Dec -> "December"

dayOfWeekName : Day -> String
dayOfWeekName d =
  case d of
    Mon -> "Monday"
    Tue -> "Tuesday"
    Wed -> "Wednesday"
    Thu -> "Thursday"
    Fri -> "Friday"
    Sat -> "Saturday"
    Sun -> "Sunday"

hour12 : Date -> Int
hour12 date =
  case hour date % 12 of
    0 -> 12
    h -> h

ordinalSuffix : Int -> String
ordinalSuffix n =
    let
      -- use 2-digit number
      nn = n % 100
    in
      case min (if nn < 20 then nn else nn % 10) 4 of
        0 -> "th"
        1 -> "st"
        2 -> "nd"
        3 -> "rd"
        4 -> "th"
        _ -> ""

isoOffsetFromMinutesOffset : String -> Int -> String
isoOffsetFromMinutesOffset sep minutes =
  let
    hh = abs minutes // 60 |> toString |> padLeft 2 '0'
    mm = abs minutes % 60 |> toString |> padLeft 2 '0'
    sign = if minutes <= 0 then "+" else "-"
  in
    sign ++ hh ++ sep ++ mm

tokens : Regex
tokens =
  regex "yy(?:yy)?|m{1,4}|d{1,4}|([wHhMsAa])\\1?|[SqNolOP]|\\[.*?\\]"

f : Date -> String -> String
f date token =
  case token of
    -- date
    "yyyy" -> year date |> toString
    "yy"   -> year date |> toString |> slice 2 4
    "mmmm" -> month date |> monthName
    "mmm"  -> month date |> monthName |> slice 0 3
    "mm"   -> monthNumber date |> toString |> padLeft 2 '0'
    "m"    -> monthNumber date |> toString
    "dddd" -> dayOfWeek date |> dayOfWeekName
    "ddd"  -> dayOfWeek date |> dayOfWeekName |> slice 0 3
    "dd"   -> day date |> toString |> padLeft 2 '0'
    "d"    -> day date |> toString
    "S"    -> day date |> ordinalSuffix
    "q"    -> quarter date |> toString
    "o"    -> isoYear date |> toString
    "ww"   -> isoWeek date |> toString |> padLeft 2 '0'
    "w"    -> isoWeek date |> toString
    "N"    -> isoWeekday date |> toString
    -- time
    "HH"   -> hour date |> toString |> padLeft 2 '0'
    "H"    -> hour date |> toString
    "hh"   -> hour12 date |> toString |> padLeft 2 '0'
    "h"    -> hour12 date |> toString
    "MM"   -> minute date |> toString |> padLeft 2 '0'
    "M"    -> minute date |> toString
    "ss"   -> second date |> toString |> padLeft 2 '0'
    "s"    -> second date |> toString
    "l"    -> millisecond date |> toString |> padLeft 3 '0'
    "AA"   -> if hour date < 12 then "AM" else "PM"
    "A"    -> if hour date < 12 then "A" else "P"
    "aa"   -> if hour date < 12 then "am" else "pm"
    "a"    -> if hour date < 12 then "a" else "p"
    "O"    -> timezoneOffset date |> isoOffsetFromMinutesOffset ""
    "P"    -> timezoneOffset date |> isoOffsetFromMinutesOffset ":"
    -- escaped
    s      -> slice 1 -1 s

format : String -> Date -> String
format s date =
  replace All tokens (\{ match } -> f date match) s
