module Test.Math exposing (tests)

import Date exposing (Date, Month(..))
import Date.Extra as Date exposing (Interval(..), fromParts)
import Expect
import Test exposing (Test, describe, test)
import Test.Utilities exposing (equals, DateParts, toParts)


equalTests : Test
equalTests =
  describe "equal"
    [ equals True  <| Date.equal (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan 1 0 0 0 0)
    , equals False <| Date.equal (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan 1 0 0 0 1)
    ]


date1 = (fromParts 2000 Jan 1 0 0 0 0)
date2 = (fromParts 2000 Jan 1 0 0 0 1)
date3 = (fromParts 2001 Jan 1 0 0 0 0)


compareTests : Test
compareTests =
  describe "compare"
    [ equals LT <| Date.compare date1 date2
    , equals EQ <| Date.compare date2 date2
    , equals GT <| Date.compare date3 date2
    , equals
        (List.map toParts [ date1, date2, date3 ])
        (List.map toParts <| List.sortWith Date.compare [ date3, date1, date2 ])
    ]


isBetweenTests : Test
isBetweenTests =
  describe "isBetween"
    [ equals True  <| Date.isBetween date1 date3 date2
    , equals True  <| Date.isBetween date1 date3 date3
    , equals True  <| Date.isBetween date1 date3 date1
    , equals True  <| Date.isBetween date3 date1 date2
    , equals True  <| Date.isBetween date3 date1 date3
    , equals True  <| Date.isBetween date3 date1 date1
    , equals False <| Date.isBetween date1 date2 date3
    , equals False <| Date.isBetween date2 date1 date3
    ]


equalDatesTest : Date -> Date -> Test
equalDatesTest a b =
  equals
    (Date.toTime a)
    (Date.toTime b)


clampTests : Test
clampTests =
  describe "clamp"
    [ equalDatesTest date2 <| Date.clamp date1 date3 date2
    , equalDatesTest date2 <| Date.clamp date1 date2 date3
    , equalDatesTest date2 <| Date.clamp date1 date2 date2
    , equalDatesTest date2 <| Date.clamp date2 date3 date1
    , equalDatesTest date2 <| Date.clamp date2 date3 date2
    ]


equalByTests : Test
equalByTests =
  describe "equalBy"
    [ equals True  <| Date.equalBy Millisecond (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan  1  0  0  0   0)
    , equals True  <| Date.equalBy Second      (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan  1  0  0  0 999)
    , equals True  <| Date.equalBy Minute      (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan  1  0  0 59 999)
    , equals True  <| Date.equalBy Hour        (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan  1  0 59 59 999)
    , equals True  <| Date.equalBy Day         (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan  1 23 59 59 999)
    , equals True  <| Date.equalBy Month       (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan 31 23 59 59 999)
    , equals True  <| Date.equalBy Year        (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Dec 31 23 59 59 999)
    , equals True  <| Date.equalBy Quarter     (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Mar 31 23 59 59 999)
    , equals True  <| Date.equalBy Week        (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan  2 23 59 59 999)
    , equals True  <| Date.equalBy Monday      (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan  2 23 59 59 999)
    , equals True  <| Date.equalBy Tuesday     (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan  3 23 59 59 999)
    , equals True  <| Date.equalBy Wednesday   (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan  4 23 59 59 999)
    , equals True  <| Date.equalBy Thursday    (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan  5 23 59 59 999)
    , equals True  <| Date.equalBy Friday      (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan  6 23 59 59 999)
    , equals True  <| Date.equalBy Saturday    (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan  7 23 59 59 999)
    , equals True  <| Date.equalBy Sunday      (fromParts 2000 Jan 2 0 0 0 0) (fromParts 2000 Jan  8 23 59 59 999)

    , equals False <| Date.equalBy Millisecond (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan  1  0  0  0   1)
    , equals False <| Date.equalBy Second      (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan  1  0  0  1   0)
    , equals False <| Date.equalBy Minute      (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan  1  0  1  0   0)
    , equals False <| Date.equalBy Hour        (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan  1  1  0  0   0)
    , equals False <| Date.equalBy Day         (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan  2  0  0  0   0)
    , equals False <| Date.equalBy Month       (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Feb  1  0  0  0   0)
    , equals False <| Date.equalBy Year        (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2001 Jan  1  0  0  0   0)
    , equals False <| Date.equalBy Quarter     (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Apr  1  0  0  0   0)
    , equals False <| Date.equalBy Week        (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan  3  0  0  0   0)
    , equals False <| Date.equalBy Monday      (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan  3  0  0  0   0)
    , equals False <| Date.equalBy Tuesday     (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan  4  0  0  0   0)
    , equals False <| Date.equalBy Wednesday   (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan  5  0  0  0   0)
    , equals False <| Date.equalBy Thursday    (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan  6  0  0  0   0)
    , equals False <| Date.equalBy Friday      (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan  7  0  0  0   0)
    , equals False <| Date.equalBy Saturday    (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan  8  0  0  0   0)
    , equals False <| Date.equalBy Sunday      (fromParts 2000 Jan 2 0 0 0 0) (fromParts 2000 Jan  9  0  0  0   0)
    ]


testsForIdempotentDateOperation : Date -> (Date -> Date) -> Date -> List Test
testsForIdempotentDateOperation x f expected =
  [ equalDatesTest expected <| f x
  , equalDatesTest expected <| f (f x)
  ]


floorTests : Test
floorTests =
  let
    date = fromParts 1999 Dec 31 23 59 59 999
  in
    describe "floor" <|
      List.concatMap
        (\(f, expected) ->
          testsForIdempotentDateOperation date f expected
        )
        [ (Date.floor Millisecond, fromParts 1999 Dec 31 23 59 59 999)
        , (Date.floor Second,      fromParts 1999 Dec 31 23 59 59   0)
        , (Date.floor Minute,      fromParts 1999 Dec 31 23 59  0   0)
        , (Date.floor Hour,        fromParts 1999 Dec 31 23  0  0   0)
        , (Date.floor Day,         fromParts 1999 Dec 31  0  0  0   0)
        , (Date.floor Month,       fromParts 1999 Dec  1  0  0  0   0)
        , (Date.floor Year,        fromParts 1999 Jan  1  0  0  0   0)
        , (Date.floor Quarter,     fromParts 1999 Oct  1  0  0  0   0)
        , (Date.floor Week,        fromParts 1999 Dec 27  0  0  0   0)
        , (Date.floor Monday,      fromParts 1999 Dec 27  0  0  0   0)
        , (Date.floor Tuesday,     fromParts 1999 Dec 28  0  0  0   0)
        , (Date.floor Wednesday,   fromParts 1999 Dec 29  0  0  0   0)
        , (Date.floor Thursday,    fromParts 1999 Dec 30  0  0  0   0)
        , (Date.floor Friday,      fromParts 1999 Dec 31  0  0  0   0)
        , (Date.floor Saturday,    fromParts 1999 Dec 25  0  0  0   0)
        , (Date.floor Sunday,      fromParts 1999 Dec 26  0  0  0   0)
        ]


ceilingTests : Test
ceilingTests =
  let
    date = fromParts 2000 Jan 1 0 0 0 1
  in
    describe "ceiling" <|
      List.concatMap
        (\(f, expected) ->
          testsForIdempotentDateOperation date f expected
        )
        [ (Date.ceiling Millisecond, fromParts 2000 Jan  1  0  0  0   1)
        , (Date.ceiling Second,      fromParts 2000 Jan  1  0  0  1   0)
        , (Date.ceiling Minute,      fromParts 2000 Jan  1  0  1  0   0)
        , (Date.ceiling Hour,        fromParts 2000 Jan  1  1  0  0   0)
        , (Date.ceiling Day,         fromParts 2000 Jan  2  0  0  0   0)
        , (Date.ceiling Month,       fromParts 2000 Feb  1  0  0  0   0)
        , (Date.ceiling Year,        fromParts 2001 Jan  1  0  0  0   0)
        , (Date.ceiling Quarter,     fromParts 2000 Apr  1  0  0  0   0)
        , (Date.ceiling Week,        fromParts 2000 Jan  3  0  0  0   0)
        , (Date.ceiling Monday,      fromParts 2000 Jan  3  0  0  0   0)
        , (Date.ceiling Tuesday,     fromParts 2000 Jan  4  0  0  0   0)
        , (Date.ceiling Wednesday,   fromParts 2000 Jan  5  0  0  0   0)
        , (Date.ceiling Thursday,    fromParts 2000 Jan  6  0  0  0   0)
        , (Date.ceiling Friday,      fromParts 2000 Jan  7  0  0  0   0)
        , (Date.ceiling Saturday,    fromParts 2000 Jan  8  0  0  0   0)
        , (Date.ceiling Sunday,      fromParts 2000 Jan  2  0  0  0   0)
        ]


intervals : List Interval
intervals =
  [ Millisecond
  , Second
  , Minute
  , Hour
  , Day
  , Month
  , Year
  , Quarter
  , Week
  , Monday
  , Tuesday
  , Wednesday
  , Thursday
  , Friday
  , Saturday
  , Sunday
  ]


addTests : Test
addTests =
  let
    date = fromParts 1999 Dec 31 23 59 59 999
  in
    describe "add"
      [ describe "add 0 x == x" <|
          List.map
            (\interval ->
              equalDatesTest date <| Date.add interval 0 date
            )
            intervals

      -- note: not always true for adding Month, Quarter, or Year intervals, as month and year lengths are not consistent
      , describe "add -n (add n x) == x" <|
          List.map
            (\interval ->
              equalDatesTest date <| Date.add interval -5 <| Date.add interval 5 date
            )
            intervals

      , describe "expected results" <|
          List.map
            (\(f, expected) ->
              equalDatesTest expected <| f date
            )
            [ (Date.add Millisecond  500, fromParts 2000 Jan  1  0  0  0 499)
            , (Date.add Millisecond 1500, fromParts 2000 Jan  1  0  0  1 499)
            , (Date.add Second        30, fromParts 2000 Jan  1  0  0 29 999)
            , (Date.add Second        90, fromParts 2000 Jan  1  0  1 29 999)
            , (Date.add Minute        30, fromParts 2000 Jan  1  0 29 59 999)
            , (Date.add Minute        90, fromParts 2000 Jan  1  1 29 59 999)
            , (Date.add Hour          12, fromParts 2000 Jan  1 11 59 59 999)
            , (Date.add Hour          36, fromParts 2000 Jan  2 11 59 59 999)
            , (Date.add Day           15, fromParts 2000 Jan 15 23 59 59 999)
            , (Date.add Day           60, fromParts 2000 Feb 29 23 59 59 999)
            , (Date.add Month          1, fromParts 2000 Jan 31 23 59 59 999)
            , (Date.add Month          2, fromParts 2000 Feb 29 23 59 59 999)
            , (Date.add Month          4, fromParts 2000 Apr 30 23 59 59 999)
            , (Date.add Month         14, fromParts 2001 Feb 28 23 59 59 999)
            , (Date.add Quarter        1, fromParts 2000 Mar 31 23 59 59 999)
            , (Date.add Quarter        3, fromParts 2000 Sep 30 23 59 59 999)
            , (Date.add Year           5, fromParts 2004 Dec 31 23 59 59 999)
            , (Date.add Week           8, fromParts 2000 Feb 25 23 59 59 999)
            ]
          ++
          List.map
            (\(f, expected) ->
              equalDatesTest expected <| f (fromParts 2000 Feb 29 23 59 59 999)
            )
            [ (Date.add Year           1, fromParts 2001 Feb 28 23 59 59 999)
            , (Date.add Year           4, fromParts 2004 Feb 29 23 59 59 999)
            ]
      ]


diffTests : Test
diffTests =
  let
    date1 = fromParts 1999 Dec 31 23 59 59 999
    date2 = fromParts 2001 Jan  1  0  0  0   0
  in
    describe "diff"
      [ describe "diff x x == 0" <|
          List.map
            (\interval ->
              equals 0 <| Date.diff interval date1 date1
            )
            intervals

      , describe "diff a b == -(diff b a)" <|
          List.map
            (\interval ->
              equals (Date.diff interval date1 date2) (negate <| Date.diff interval date2 date1)
            )
            intervals

      , describe "expected results" <|
          List.map
            (\(f, expected) ->
              equals expected <| f date1
            )
            [ (Date.diff Millisecond <| fromParts 2000 Jan  1  0  0  0 499,  -500)
            , (Date.diff Millisecond <| fromParts 2000 Jan  1  0  0  1 499, -1500)
            , (Date.diff Second      <| fromParts 2000 Jan  1  0  0 29 999,   -30)
            , (Date.diff Second      <| fromParts 2000 Jan  1  0  1 29 999,   -90)
            , (Date.diff Minute      <| fromParts 2000 Jan  1  0 29 59 999,   -30)
            , (Date.diff Minute      <| fromParts 2000 Jan  1  1 29 59 999,   -90)
            , (Date.diff Hour        <| fromParts 2000 Jan  1 11 59 59 999,   -12)
            , (Date.diff Hour        <| fromParts 2000 Jan  2 11 59 59 999,   -36)
            , (Date.diff Day         <| fromParts 2000 Jan 15 23 59 59 999,   -15)
            , (Date.diff Day         <| fromParts 2000 Feb 29 23 59 59 999,   -60)
            , (Date.diff Month       <| fromParts 2000 Jan 31 23 59 59 999,    -1)
            , (Date.diff Month       <| fromParts 2000 Feb 29 23 59 59 999,    -1)
            , (Date.diff Month       <| fromParts 2000 Apr 30 23 59 59 999,    -3)
            , (Date.diff Month       <| fromParts 2001 Feb 28 23 59 59 999,   -13)
            , (Date.diff Quarter     <| fromParts 2000 Mar 31 23 59 59 999,    -1)
            , (Date.diff Quarter     <| fromParts 2000 Sep 30 23 59 59 999,    -2)
            , (Date.diff Year        <| fromParts 2004 Dec 31 23 59 59 999,    -5)
            , (Date.diff Week        <| fromParts 2000 Feb 25 23 59 59 999,    -8)
            , (Date.diff Monday      <| fromParts 2000 Jan 11  0  0  0   0,    -2)
            , (Date.diff Tuesday     <| fromParts 2000 Jan 11  0  0  0   0,    -2)
            , (Date.diff Wednesday   <| fromParts 2000 Jan 11  0  0  0   0,    -1)
            , (Date.diff Thursday    <| fromParts 2000 Jan 11  0  0  0   0,    -1)
            , (Date.diff Friday      <| fromParts 2000 Jan 11  0  0  0   0,    -1)
            , (Date.diff Saturday    <| fromParts 2000 Jan 11  0  0  0   0,    -2)
            , (Date.diff Sunday      <| fromParts 2000 Jan 11  0  0  0   0,    -2)
            ]
      ]


rangeTests : Test
rangeTests =
  let
    date = fromParts 2000 Jan 1 0 0 0 0
  in
    describe "range"
      [ equals
          [ (2000, Jan,  1,  0,  0,  0,   0)
          , (2000, Jan,  1,  0,  0,  0, 200)
          , (2000, Jan,  1,  0,  0,  0, 400)
          ]
          (Date.range Millisecond 200 date (fromParts 2000 Jan 1 0 0 0 600) |> List.map toParts)

      , equals
          [ (2000, Jan,  1,  0,  0,  0,   0)
          , (2000, Jan,  1,  0,  0, 30,   0)
          , (2000, Jan,  1,  0,  1,  0,   0)
          ]
          (Date.range Second 30 date (fromParts 2000 Jan 1 0 1 30 0) |> List.map toParts)

      , equals
          [ (2000, Jan,  1,  0,  0,  0,   0)
          , (2000, Jan,  1,  0, 45,  0,   0)
          , (2000, Jan,  1,  1, 30,  0,   0)
          ]
          (Date.range Minute 45 date (fromParts 2000 Jan 1 2 15 0 0) |> List.map toParts)

      , equals
          [ (2000, Jan,  1,  0,  0,  0,   0)
          , (2000, Jan,  1, 18,  0,  0,   0)
          , (2000, Jan,  2, 12,  0,  0,   0)
          ]
          (Date.range Hour 18 date (fromParts 2000 Jan 3 6 0 0 0) |> List.map toParts)

      , equals
          [ (2000, Jan,  1,  0,  0,  0,   0)
          , (2000, Jan,  3,  0,  0,  0,   0)
          , (2000, Jan,  5,  0,  0,  0,   0)
          ]
          (Date.range Day 2 date (fromParts 2000 Jan 7 0 0 0 0) |> List.map toParts)

      , equals
          [ (2000, Jan,  1,  0,  0,  0,   0)
          , (2000, Mar,  1,  0,  0,  0,   0)
          , (2000, May,  1,  0,  0,  0,   0)
          ]
          (Date.range Month 2 date (fromParts 2000 Jul 1 0 0 0 0) |> List.map toParts)

      , equals
          [ (2000, Jan,  1,  0,  0,  0,   0)
          , (2010, Jan,  1,  0,  0,  0,   0)
          , (2020, Jan,  1,  0,  0,  0,   0)
          ]
          (Date.range Year 10 date (fromParts 2030 Jan 1 0 0 0 0) |> List.map toParts)

      , equals
          [ (2000, Jan,  1,  0,  0,  0,   0)
          , (2000, Apr,  1,  0,  0,  0,   0)
          , (2000, Jul,  1,  0,  0,  0,   0)
          ]
          (Date.range Quarter 1 date (fromParts 2000 Sep 1 0 0 0 0) |> List.map toParts)

      , equals
          [ (2000, Jan,  3,  0,  0,  0,   0)
          , (2000, Jan, 17,  0,  0,  0,   0)
          , (2000, Jan, 31,  0,  0,  0,   0)
          ]
          (Date.range Week 2 date (fromParts 2000 Feb 14 0 0 0 0) |> List.map toParts)

      , equals
          [ (2000, Jan,  3,  0,  0,  0,   0)
          , (2000, Jan, 17,  0,  0,  0,   0)
          , (2000, Jan, 31,  0,  0,  0,   0)
          ]
          (Date.range Monday 2 date (fromParts 2000 Feb 14 0 0 0 0) |> List.map toParts)

      , equals
          [ (2000, Jan,  4,  0,  0,  0,   0)
          , (2000, Jan, 18,  0,  0,  0,   0)
          , (2000, Feb,  1,  0,  0,  0,   0)
          ]
          (Date.range Tuesday 2 date (fromParts 2000 Feb 15 0 0 0 0) |> List.map toParts)

      , equals
          [ (2000, Jan,  5,  0,  0,  0,   0)
          , (2000, Jan, 19,  0,  0,  0,   0)
          , (2000, Feb,  2,  0,  0,  0,   0)
          ]
          (Date.range Wednesday 2 date (fromParts 2000 Feb 16 0 0 0 0) |> List.map toParts)

      , equals
          [ (2000, Jan,  6,  0,  0,  0,   0)
          , (2000, Jan, 20,  0,  0,  0,   0)
          , (2000, Feb,  3,  0,  0,  0,   0)
          ]
          (Date.range Thursday 2 date (fromParts 2000 Feb 17 0 0 0 0) |> List.map toParts)

      , equals
          [ (2000, Jan,  7,  0,  0,  0,   0)
          , (2000, Jan, 21,  0,  0,  0,   0)
          , (2000, Feb,  4,  0,  0,  0,   0)
          ]
          (Date.range Friday 2 date (fromParts 2000 Feb 18 0 0 0 0) |> List.map toParts)

      , equals
          [ (2000, Jan,  1,  0,  0,  0,   0)
          , (2000, Jan, 15,  0,  0,  0,   0)
          , (2000, Jan, 29,  0,  0,  0,   0)
          ]
          (Date.range Saturday 2 date (fromParts 2000 Feb 12 0 0 0 0) |> List.map toParts)

      , equals
          [ (2000, Jan,  2,  0,  0,  0,   0)
          , (2000, Jan, 16,  0,  0,  0,   0)
          , (2000, Jan, 30,  0,  0,  0,   0)
          ]
          (Date.range Sunday 2 date (fromParts 2000 Feb 13 0 0 0 0) |> List.map toParts)

      , equals
          []
          (Date.range Millisecond 1 date date |> List.map toParts)

      , equals
          [ (2000, Jan,  31,  0,  0,  0,   0) ]
          (Date.range Day 1 date (Date.add Month 1 date) |> List.reverse |> List.take 1 |> List.map toParts)

      , equals
          366
          (Date.range Day 1 date (Date.add Year 1 date) |> List.length)

      , test "large range (tail recursion)" <|
          \() ->
              let
                start = fromParts 1970 Jan 1 0 0 0 0
                end = fromParts 2020 Jan 1 0 0 0 0
                result = Date.range Day 1 start end
              in
                Expect.equal (Date.diff Day start end) (List.length result |> Debug.log "length")
      ]


tests : Test
tests =
  describe "Math"
    [ equalTests
    , compareTests
    , isBetweenTests
    , clampTests
    , equalByTests
    , floorTests
    , ceilingTests
    , addTests
    , diffTests
    , rangeTests
    ]
