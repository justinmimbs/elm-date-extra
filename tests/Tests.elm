module Tests exposing (..)

import Date exposing (Date, Month(..))
import DateFunctions exposing (..)

import ElmTest exposing (Test, suite, test, defaultTest, equals, assertEqual, assertNotEqual, assertionList, runSuiteHtml)

isoWeekDateTests : Test
isoWeekDateTests =
  let
    isoWeekDateFromDate : Date -> (Int, Int, Int)
    isoWeekDateFromDate date =
      (isoYear date, isoWeek date, isoWeekday date)

    isoWeekDateFromYMD : Int -> Month -> Int -> (Int, Int, Int)
    isoWeekDateFromYMD y m d =
      isoWeekDateFromDate <| dateFromYMD y m d
  in
    suite "isoWeekYear, isoWeek, isoWeekday" [
      equals (isoWeekDateFromYMD 2005 Jan  1) (2004, 53, 6),
      equals (isoWeekDateFromYMD 2005 Jan  2) (2004, 53, 7),
      equals (isoWeekDateFromYMD 2005 Dec 31) (2005, 52, 6),
      equals (isoWeekDateFromYMD 2007 Jan  1) (2007,  1, 1),
      equals (isoWeekDateFromYMD 2007 Dec 30) (2007, 52, 7),
      equals (isoWeekDateFromYMD 2007 Dec 31) (2008,  1, 1),
      equals (isoWeekDateFromYMD 2008 Jan  1) (2008,  1, 2),
      equals (isoWeekDateFromYMD 2008 Dec 28) (2008, 52, 7),
      equals (isoWeekDateFromYMD 2008 Dec 29) (2009,  1, 1),
      equals (isoWeekDateFromYMD 2008 Dec 30) (2009,  1, 2),
      equals (isoWeekDateFromYMD 2008 Dec 31) (2009,  1, 3),
      equals (isoWeekDateFromYMD 2009 Jan  1) (2009,  1, 4),
      equals (isoWeekDateFromYMD 2009 Dec 31) (2009, 53, 4),
      equals (isoWeekDateFromYMD 2010 Jan  1) (2009, 53, 5),
      equals (isoWeekDateFromYMD 2010 Jan  2) (2009, 53, 6),
      equals (isoWeekDateFromYMD 2010 Jan  3) (2009, 53, 7)
    ]

-- RataDie losslessly encodes (Year, Month, Day)

rataDieTests : Test
rataDieTests =
  let
    everyYMDInYear : Int -> List (Int, Month, Int)
    everyYMDInYear y =
      List.concatMap (\m ->
        List.map (\d ->
          (y, m, d)
        ) [ 1 .. (daysInMonth y m) ]
      ) months

    ymdList : List (Int, Month, Int)
    ymdList =
      List.concatMap everyYMDInYear <| List.concat [ [ 1 .. 4 ], [ 97 .. 105 ], [ 397 .. 405], [ 1897 .. 1905 ], [ 1997 .. 2005 ] ]

    ymdFromRataDieFromYMD : (Int, Month, Int) -> (Int, Month, Int)
    ymdFromRataDieFromYMD (y, m, d) =
      ymdFromRataDie <| rataDieFromYMD y m d
  in
    suite "RataDie" <|
      List.map defaultTest <| assertionList ymdList <| List.map ymdFromRataDieFromYMD ymdList

tests : Test
tests =
  suite "DateX" [
    isoWeekDateTests,
    rataDieTests
  ]

main : Program Never
main =
  runSuiteHtml tests
