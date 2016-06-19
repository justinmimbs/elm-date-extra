module Test.Extract exposing (..)

import Date exposing (Date, Month(..))
import Date.Extract exposing (isoYear, isoWeek, isoWeekday, quarter)
import Date.Create exposing (fromYMD)
import Test.Internal exposing (everyYMDInMonth)

import ElmTest exposing (Test, suite, test, equals, assert)


everyDateInMonth : Int -> Month -> List Date
everyDateInMonth y m =
  everyYMDInMonth y m |> List.map (\(y, m, d) -> fromYMD y m d)


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
      isoWeekDateFromDate <| fromYMD y m d
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
  suite "DateExtract" [
    isoWeekDateTests,
    quarterTests
  ]
