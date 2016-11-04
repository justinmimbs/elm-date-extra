module Test.Extract exposing (tests)

import Date exposing (Date, Month(..))
import Date.Extra as Date exposing (monthNumber, quarter, ordinalDay, fractionalDay, weekYear, weekNumber, weekdayNumber)
import Date.Extra.Facts exposing (months)
import Test exposing (Test, describe, test)
import Test.Utilities exposing (equals, calendarDatesInMonth)


datesInMonth : Int -> Month -> List Date
datesInMonth y m =
  List.map
    (\(y, m, d) -> Date.fromCalendarDate y m d)
    (calendarDatesInMonth y m)


datesInYear : Int -> List Date
datesInYear y =
  List.concatMap
    (datesInMonth y)
    months


monthNumberTests : Test
monthNumberTests =
  let
    testsForMonth : (Month, Int) -> List Test
    testsForMonth (m, n) =
      List.map
        (equals n << monthNumber)
        (datesInMonth 2016 m)
  in
    describe "monthNumber" <|
      List.concatMap
        testsForMonth
        [ (Jan,  1)
        , (Feb,  2)
        , (Mar,  3)
        , (Apr,  4)
        , (May,  5)
        , (Jun,  6)
        , (Jul,  7)
        , (Aug,  8)
        , (Sep,  9)
        , (Oct, 10)
        , (Nov, 11)
        , (Dec, 12)
        ]


quarterTests : Test
quarterTests =
  let
    testsForQuarter : (List Month, Int) -> List Test
    testsForQuarter (monthsInQuarter, q) =
      List.map
        (equals q << quarter) <|
        List.concatMap
          (datesInMonth 2016)
          monthsInQuarter
  in
    describe "quarter" <|
      List.concatMap
        testsForQuarter
        [ ([ Jan, Feb, Mar ], 1)
        , ([ Apr, May, Jun ], 2)
        , ([ Jul, Aug, Sep ], 3)
        , ([ Oct, Nov, Dec ], 4)
        ]


ordinalDayTests : Test
ordinalDayTests =
  let
    ordinalDayTest : Int -> Date -> Test
    ordinalDayTest i =
      equals (i + 1) << ordinalDay

  in
    describe "ordinalDay"
      [ describe "leap year" <|
          List.indexedMap
            ordinalDayTest
            (datesInYear 2016)
      , describe "non-leap year" <|
          List.indexedMap
            ordinalDayTest
            (datesInYear 2017)
      ]


fractionalDayTests : Test
fractionalDayTests =
  let
    fractionalDayTest : (Float, Date) -> Test
    fractionalDayTest (fractional, date) =
      equals
        fractional
        (fractionalDay date)
  in
    describe "fractionalDay" <|
      List.map
        fractionalDayTest
        [ (0.00,               Date.fromParts 2001 Jan 1  0 0 0 0)
        , (0.25,               Date.fromParts 2001 Jan 1  6 0 0 0)
        , (0.50,               Date.fromParts 2001 Jan 1 12 0 0 0)
        , (0.75,               Date.fromParts 2001 Jan 1 18 0 0 0)
        , (3661001 / 86400000, Date.fromParts 2001 Jan 1  1 1 1 1)
        ]


calendarDateToWeekDatePairs =
  [ ((2005, Jan,  1), (2004, 53, 6))
  , ((2005, Jan,  2), (2004, 53, 7))
  , ((2005, Dec, 31), (2005, 52, 6))
  , ((2007, Jan,  1), (2007,  1, 1))
  , ((2007, Dec, 30), (2007, 52, 7))
  , ((2007, Dec, 31), (2008,  1, 1))
  , ((2008, Jan,  1), (2008,  1, 2))
  , ((2008, Dec, 28), (2008, 52, 7))
  , ((2008, Dec, 29), (2009,  1, 1))
  , ((2008, Dec, 30), (2009,  1, 2))
  , ((2008, Dec, 31), (2009,  1, 3))
  , ((2009, Jan,  1), (2009,  1, 4))
  , ((2009, Dec, 31), (2009, 53, 4))
  , ((2010, Jan,  1), (2009, 53, 5))
  , ((2010, Jan,  2), (2009, 53, 6))
  , ((2010, Jan,  3), (2009, 53, 7))
  ]


weekDateTests : Test
weekDateTests =
  let
    toWeekDate : Date -> (Int, Int, Int)
    toWeekDate date =
      (weekYear date, weekNumber date, weekdayNumber date)

    weekDateTest : (Int, Month, Int) -> (Int, Int, Int) -> Test
    weekDateTest (y, m, d) weekDate =
      equals
        weekDate
        (toWeekDate <| Date.fromCalendarDate y m d)

  in
    describe "weekYear, weekNumber, weekdayNumber" <|
      List.map
        (\(calendarDate, weekDate) ->
          weekDateTest calendarDate weekDate
        )
        calendarDateToWeekDatePairs


tests : Test
tests =
  describe "Extract"
    [ monthNumberTests
    , quarterTests
    , ordinalDayTests
    , fractionalDayTests
    , weekDateTests
    ]
