module Tests exposing (..)

import Date exposing (Date, Month(..))
import DateFunctions exposing (..)

import ElmTest exposing (Test, suite, test, defaultTest, equals, assert, assertEqual, assertNotEqual, assertionList, runSuiteHtml)

everyYMDInMonth : Int -> Month -> List (Int, Month, Int)
everyYMDInMonth y m =
  List.map (\d ->
    (y, m, d)
  ) [ 1 .. (daysInMonth y m) ]

everyYMDInYear : Int -> List (Int, Month, Int)
everyYMDInYear y =
  List.concatMap (everyYMDInMonth y) months

everyDateInMonth : Int -> Month -> List Date
everyDateInMonth y m =
  everyYMDInMonth y m |> List.map (\(y, m, d) -> dateFromYMD y m d)


{-| Converting (year, month, day) : (Int, Month, Int) to RataDie and back is lossless.
-}

rataDieTests : Test
rataDieTests =
  let
    ymdList : List (Int, Month, Int)
    ymdList =
      List.concatMap everyYMDInYear <| List.concat [ [ 1 .. 4 ], [ 97 .. 105 ], [ 397 .. 405], [ 1897 .. 1905 ], [ 1997 .. 2005 ] ]

    ymdFromRataDieFromYMD : (Int, Month, Int) -> (Int, Month, Int)
    ymdFromRataDieFromYMD (y, m, d) =
      ymdFromRataDie <| rataDieFromYMD y m d
  in
    suite "RataDie" <|
      List.map defaultTest <| assertionList ymdList <| List.map ymdFromRataDieFromYMD ymdList


{-| isoYear, isoWeek, isoWeekday return expected results.
-}

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
    suite "isoYear, isoWeek, isoWeekday" [
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

{-| quarter returns expected results.
-}

quarterTests : Test
quarterTests =
  let
    everyDateInMonths : Int -> List Month -> List Date
    everyDateInMonths y ms =
      List.concatMap (everyDateInMonth y) ms
  in
    suite "quarter" [
      test "Q1" <| assert
        <| (everyDateInMonths 2016 [Jan, Feb, Mar] |> List.map quarter |> List.all ((==) 1)),
      test "Q2" <| assert
        <| (everyDateInMonths 2016 [Apr, May, Jun] |> List.map quarter |> List.all ((==) 2)),
      test "Q3" <| assert
        <| (everyDateInMonths 2016 [Jul, Aug, Sep] |> List.map quarter |> List.all ((==) 3)),
      test "Q4" <| assert
        <| (everyDateInMonths 2016 [Oct, Nov, Dec] |> List.map quarter |> List.all ((==) 4))
    ]


tests : Test
tests =
  suite "DateX" [
    rataDieTests,
    isoWeekDateTests,
    quarterTests
  ]

main : Program Never
main =
  runSuiteHtml tests
