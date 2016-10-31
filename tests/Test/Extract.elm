module Test.Extract exposing (tests)

import Date exposing (Date, Month(..))
import Date.Extra as Date exposing (monthNumber, quarter, ordinalDay, fractionalDay, weekYear, weekNumber, weekdayNumber)
import Date.Extra.Facts exposing (months)
import Legacy.ElmTest exposing (Test, suite, test, equals, assert)
import Test.Utilities exposing (calendarDatesInMonth)


datesInMonth : Int -> Month -> List Date
datesInMonth y m =
  calendarDatesInMonth y m |> List.map (\(y, m, d) -> Date.fromCalendarDate y m d)


datesInYear : Int -> List Date
datesInYear y =
  List.concatMap (datesInMonth y) months


monthNumberTests : Test
monthNumberTests =
  suite "monthNumber" [
    test  "1" <| assert <| List.all ((==)  1) <| List.map monthNumber <| datesInMonth 2016 Jan,
    test  "2" <| assert <| List.all ((==)  2) <| List.map monthNumber <| datesInMonth 2016 Feb,
    test  "3" <| assert <| List.all ((==)  3) <| List.map monthNumber <| datesInMonth 2016 Mar,
    test  "4" <| assert <| List.all ((==)  4) <| List.map monthNumber <| datesInMonth 2016 Apr,
    test  "5" <| assert <| List.all ((==)  5) <| List.map monthNumber <| datesInMonth 2016 May,
    test  "6" <| assert <| List.all ((==)  6) <| List.map monthNumber <| datesInMonth 2016 Jun,
    test  "7" <| assert <| List.all ((==)  7) <| List.map monthNumber <| datesInMonth 2016 Jul,
    test  "8" <| assert <| List.all ((==)  8) <| List.map monthNumber <| datesInMonth 2016 Aug,
    test  "9" <| assert <| List.all ((==)  9) <| List.map monthNumber <| datesInMonth 2016 Sep,
    test "10" <| assert <| List.all ((==) 10) <| List.map monthNumber <| datesInMonth 2016 Oct,
    test "11" <| assert <| List.all ((==) 11) <| List.map monthNumber <| datesInMonth 2016 Nov,
    test "12" <| assert <| List.all ((==) 12) <| List.map monthNumber <| datesInMonth 2016 Dec
  ]


quarterTests : Test
quarterTests =
  suite "quarter" [
    test "1" <| assert <| List.all ((==) 1) <| List.map quarter <| List.concatMap (datesInMonth 2016) [Jan, Feb, Mar],
    test "2" <| assert <| List.all ((==) 2) <| List.map quarter <| List.concatMap (datesInMonth 2016) [Apr, May, Jun],
    test "3" <| assert <| List.all ((==) 3) <| List.map quarter <| List.concatMap (datesInMonth 2016) [Jul, Aug, Sep],
    test "4" <| assert <| List.all ((==) 4) <| List.map quarter <| List.concatMap (datesInMonth 2016) [Oct, Nov, Dec]
  ]


ordinalDayTests : Test
ordinalDayTests =
  let
    ordinalDayTest : Int -> Date -> Test
    ordinalDayTest i date =
      equals (i + 1) (ordinalDay date)
  
  in
    suite "ordinalDay" [
      suite "leap year"
        <| List.indexedMap ordinalDayTest (datesInYear 2016),
      suite "non-leap year"
        <| List.indexedMap ordinalDayTest (datesInYear 2017)
    ]


fractionalDayTests : Test
fractionalDayTests =
  suite "fractionalDay" [
    equals 0.0                  (fractionalDay <| Date.fromParts 2001 Jan 1  0 0 0 0),
    equals 0.25                 (fractionalDay <| Date.fromParts 2001 Jan 1  6 0 0 0),
    equals 0.5                  (fractionalDay <| Date.fromParts 2001 Jan 1 12 0 0 0),
    equals 0.75                 (fractionalDay <| Date.fromParts 2001 Jan 1 18 0 0 0),
    equals (3661001 / 86400000) (fractionalDay <| Date.fromParts 2001 Jan 1  1 1 1 1)
  ]


calendarDateToWeekDatePairs = [
  ((2005, Jan,  1), (2004, 53, 6)),
  ((2005, Jan,  2), (2004, 53, 7)),
  ((2005, Dec, 31), (2005, 52, 6)),
  ((2007, Jan,  1), (2007,  1, 1)),
  ((2007, Dec, 30), (2007, 52, 7)),
  ((2007, Dec, 31), (2008,  1, 1)),
  ((2008, Jan,  1), (2008,  1, 2)),
  ((2008, Dec, 28), (2008, 52, 7)),
  ((2008, Dec, 29), (2009,  1, 1)),
  ((2008, Dec, 30), (2009,  1, 2)),
  ((2008, Dec, 31), (2009,  1, 3)),
  ((2009, Jan,  1), (2009,  1, 4)),
  ((2009, Dec, 31), (2009, 53, 4)),
  ((2010, Jan,  1), (2009, 53, 5)),
  ((2010, Jan,  2), (2009, 53, 6)),
  ((2010, Jan,  3), (2009, 53, 7))
  ]


weekDateTests : Test
weekDateTests =
  let
    toWeekDate : Date -> (Int, Int, Int)
    toWeekDate date =
      (weekYear date, weekNumber date, weekdayNumber date)

    weekDateTest : (Int, Month, Int) -> (Int, Int, Int) -> Test
    weekDateTest (y, m, d) weekDate =
      equals weekDate (toWeekDate <| Date.fromCalendarDate y m d)

  in
    suite "weekYear, weekNumber, weekdayNumber" <|
      List.map (\(calendarDate, weekDate) ->
        weekDateTest calendarDate weekDate
      ) calendarDateToWeekDatePairs


tests : Test
tests =
  suite "Extract" [
    monthNumberTests,
    quarterTests,
    ordinalDayTests,
    fractionalDayTests,
    weekDateTests
  ]
