module Test.Create exposing (tests)

import ElmTest exposing (Test, suite, equals)

import Test.Utilities exposing (toParts, calendarDatesInYear)
import Date exposing (Date, Month)
import Date.Create as Date


fromCalendarDateTests : Test
fromCalendarDateTests =
  let
    calendarDates : List (Int, Month, Int)
    calendarDates =
      List.concatMap calendarDatesInYear <| List.concat [ [ 1897 .. 1905 ], [ 1967 .. 1975 ], [ 1997 .. 2020 ] ]

    calendarDateTest : (Int, Month, Int) -> Test
    calendarDateTest (y, m, d) =
      equals (toParts (Date.fromCalendarDate y m d)) (y, m, d, 0, 0, 0, 0)
  in
    suite "fromCalendarDate" <|
      List.map calendarDateTest calendarDates


tests : Test
tests =
  suite "Date.Create" [
    fromCalendarDateTests
  ]
