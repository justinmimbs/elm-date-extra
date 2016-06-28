module Test.Utilities exposing (..)

import Date exposing (Date, Month, year, month, day, hour, minute, second, millisecond)
import Date.Facts exposing (daysInMonth, months)


toParts : Date -> (Int, Month, Int, Int, Int, Int, Int)
toParts date =
  (year date, month date, day date, hour date, minute date, second date, millisecond date)


calendarDatesInMonth : Int -> Month -> List (Int, Month, Int)
calendarDatesInMonth y m =
  List.map (\d ->
    (y, m, d)
  ) [ 1 .. (daysInMonth y m) ]


calendarDatesInYear : Int -> List (Int, Month, Int)
calendarDatesInYear y =
  List.concatMap (calendarDatesInMonth y) months
