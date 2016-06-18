module Test.DateInternal exposing (..)

import Date exposing (Month)
import DateFact exposing (daysInMonth, months)
import DateInternal exposing (rataDieFromYMD, ymdFromRataDie)

import ElmTest exposing (Test, suite, defaultTest, assertionList)


everyYMDInMonth : Int -> Month -> List (Int, Month, Int)
everyYMDInMonth y m =
  List.map (\d ->
    (y, m, d)
  ) [ 1 .. (daysInMonth y m) ]


everyYMDInYear : Int -> List (Int, Month, Int)
everyYMDInYear y =
  List.concatMap (everyYMDInMonth y) months


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


tests : Test
tests =
  suite "DateInternal" [
    rataDieTests
  ]
