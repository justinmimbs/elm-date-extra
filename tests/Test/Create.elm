module Test.Create exposing (tests)

import ElmTest exposing (Test, suite, test, assertEqual, equals)
import String

import Test.Utilities exposing (DateParts, toParts, toUtc, toTimeOffset, calendarDatesInYear)
import Date exposing (Date, Month(..))
import Date.Create exposing (fromParts, fromCalendarDate, fromIsoString, fromSpec, local, offset, utc, noTime, atTime, calendarDate, ordinalDate, weekDate, fromJulianDate)


fromPartsTests : Test
fromPartsTests =
  let
    partsList = [
      (1969, Dec, 31, 23, 59, 59, 999),
      (1970, Jan,  1,  0,  0,  0,   0),
      (1999, Dec, 31, 23, 59, 59, 999),
      (2000, Jan,  1,  0,  0,  0,   0),
      (2008, Dec, 31, 20, 30, 40, 567)
    ]

    fromPartsTest : DateParts -> Test
    fromPartsTest (y, m, d, hh, mm, ss, ms) =
      equals (y, m, d, hh, mm, ss, ms) (toParts (fromParts y m d hh mm ss ms))
  in
    suite "fromParts" <|
      List.map fromPartsTest partsList


fromCalendarDateTests : Test
fromCalendarDateTests =
  let
    calendarDates : List (Int, Month, Int)
    calendarDates =
      List.concatMap calendarDatesInYear <| List.concat [ [ 1897 .. 1905 ], [ 1967 .. 1975 ], [ 1997 .. 2020 ] ]

    calendarDateTest : (Int, Month, Int) -> Test
    calendarDateTest (y, m, d) =
      equals (y, m, d, 0, 0, 0, 0) (toParts (fromCalendarDate y m d))
  in
    suite "fromCalendarDate" <|
      List.map calendarDateTest calendarDates


fromIsoStringTests : Test
fromIsoStringTests =
  let
    replace : String -> String -> String
    replace x string =
      String.split x string |> String.concat

    basicFromExtended : String -> String
    basicFromExtended string =
      case String.split "T" string of
        [d]    -> replace "-" d
        [d, t] -> replace "-" d ++ "T" ++ replace ":" t
        _      -> ""

    fromIsoStringTestList : (Date -> DateParts) -> List (String, Maybe DateParts) -> List Test
    fromIsoStringTestList toDateParts extendedPairs =
      let
        basicPairs = List.map
          (\(extended, expected) -> (basicFromExtended extended, expected))
          extendedPairs
      in
        List.map (\(string, expected) ->
          test string <| assertEqual expected <| Maybe.map toDateParts <| fromIsoString string
        ) (extendedPairs ++ basicPairs)

    suite1 = suite "local" <| fromIsoStringTestList
      toParts
      [
        ("2008-12-31T20:30:40.567", Just (2008, Dec, 31, 20, 30, 40, 567)),
        ("2008-12-31T20:30:40.067", Just (2008, Dec, 31, 20, 30, 40,  67)),
        ("2008-12-31T20:30:40.007", Just (2008, Dec, 31, 20, 30, 40,   7)),
        ("2008-12-31T20:30:40.56",  Just (2008, Dec, 31, 20, 30, 40, 560)),
        ("2008-12-31T20:30:40.5",   Just (2008, Dec, 31, 20, 30, 40, 500)),
        ("2008-12-31T20:30:40",     Just (2008, Dec, 31, 20, 30, 40,   0)),

        ("2008-12-31T20:30.75",     Just (2008, Dec, 31, 20, 30, 45,   0)),
        ("2008-12-31T20:30.75833",  Just (2008, Dec, 31, 20, 30, 45, 500)),
        ("2008-12-31T20:30",        Just (2008, Dec, 31, 20, 30,  0,   0)),

        ("2008-12-31T20.75",        Just (2008, Dec, 31, 20, 45,  0,   0)),
        ("2008-12-31T20.7583333",   Just (2008, Dec, 31, 20, 45, 30,   0)),
        ("2008-12-31T20.75835",     Just (2008, Dec, 31, 20, 45, 30,  60)),
        ("2008-12-31T20",           Just (2008, Dec, 31, 20,  0,  0,   0)),
        ("2008-12-31T00",           Just (2008, Dec, 31,  0,  0,  0,   0)),

        ("2008-12-31",              Just (2008, Dec, 31,  0,  0,  0,   0)),
        ("2008-12",                 Just (2008, Dec,  1,  0,  0,  0,   0)),
        ("2008",                    Just (2008, Jan,  1,  0,  0,  0,   0))
      ]

    suite2 = suite "utc" <| fromIsoStringTestList
      (toParts << toUtc)
      [
        ("2008-12-31T20:30:40.567+00:00", Just (2008, Dec, 31, 20, 30, 40, 567)),
        ("2008-12-31T20:30:40.567+00",    Just (2008, Dec, 31, 20, 30, 40, 567)),
        ("2008-12-31T20:30:40.567Z",      Just (2008, Dec, 31, 20, 30, 40, 567)),
        ("2008-12-31T20:30:40+00:00",     Just (2008, Dec, 31, 20, 30, 40,   0)),
        ("2008-12-31T20:30:40Z",          Just (2008, Dec, 31, 20, 30, 40,   0)),

        ("2008-12-31T20:30.75+00:00",     Just (2008, Dec, 31, 20, 30, 45,   0)),
        ("2008-12-31T20:30.75Z",          Just (2008, Dec, 31, 20, 30, 45,   0)),
        ("2008-12-31T20:30+00:00",        Just (2008, Dec, 31, 20, 30,  0,   0)),
        ("2008-12-31T20:30Z",             Just (2008, Dec, 31, 20, 30,  0,   0)),

        ("2008-12-31T20.75+00:00",        Just (2008, Dec, 31, 20, 45,  0,   0)),
        ("2008-12-31T20.75Z",             Just (2008, Dec, 31, 20, 45,  0,   0)),
        ("2008-12-31T00+00:00",           Just (2008, Dec, 31,  0,  0,  0,   0)),
        ("2008-12-31T00Z",                Just (2008, Dec, 31,  0,  0,  0,   0)),

        ("2008-12-31+00:00",              Nothing),
        ("2008-12-31Z",                   Nothing),
        ("2008-12Z",                      Nothing),
        ("2008Z",                         Nothing)
      ]

    suite3 = suite "offset -07:00" <| fromIsoStringTestList
      (toParts << (toTimeOffset -420))
      [
        ("2008-12-31T20:30:40.567-07:00", Just (2008, Dec, 31, 20, 30, 40, 567)),
        ("2008-12-31T20:30:40.567-07",    Just (2008, Dec, 31, 20, 30, 40, 567)),
        ("2008-12-31T20:30:40-07:00",     Just (2008, Dec, 31, 20, 30, 40,   0)),

        ("2008-12-31T20:30.75-07:00",     Just (2008, Dec, 31, 20, 30, 45,   0)),
        ("2008-12-31T20:30-07:00",        Just (2008, Dec, 31, 20, 30,  0,   0)),

        ("2008-12-31T20.75-07:00",        Just (2008, Dec, 31, 20, 45,  0,   0)),
        ("2008-12-31T00-07:00",           Just (2008, Dec, 31,  0,  0,  0,   0)),
        ("2008-12-31T00-07",              Just (2008, Dec, 31,  0,  0,  0,   0)),

        ("2008-12-31-07:00",              Nothing),
        ("2008-12-07:00",                 Nothing),
        ("2008-07:00",                    Nothing)
      ]

    suite4 = suite "offset +04:30" <| fromIsoStringTestList
      (toParts << (toTimeOffset 270))
      [
        ("2008-12-31T20:30:40.567+04:30", Just (2008, Dec, 31, 20, 30, 40, 567)),
        ("2008-12-31T20:30:40+04:30",     Just (2008, Dec, 31, 20, 30, 40,   0)),

        ("2008-12-31T20:30.75+04:30",     Just (2008, Dec, 31, 20, 30, 45,   0)),
        ("2008-12-31T20:30+04:30",        Just (2008, Dec, 31, 20, 30,  0,   0)),

        ("2008-12-31T20.75+04:30",        Just (2008, Dec, 31, 20, 45,  0,   0)),
        ("2008-12-31T00+04:30",           Just (2008, Dec, 31,  0,  0,  0,   0)),

        ("2008-12-31+04:30",              Nothing),
        ("2008-12+04:30",                 Nothing),
        ("2008+04:30",                    Nothing)
      ]
  in
    suite "fromIsoString" [suite1, suite2, suite3, suite4]


fromSpecTests : Test
fromSpecTests =
  suite "fromSpec" [
    suite "local" [
      equals (2008, Dec, 31,  0,  0,  0,   0) (toParts <| fromSpec local noTime (calendarDate 2008 Dec 31)),
      equals (2008, Dec, 31,  0,  0,  0,   0) (toParts <| fromSpec local noTime (ordinalDate 2008 366)),
      equals (2008, Dec, 31,  0,  0,  0,   0) (toParts <| fromSpec local noTime (weekDate 2009 1 3)),
      equals (2008, Dec, 31, 20, 30, 40, 567) (toParts <| fromSpec local (atTime 20 30 40 567) (calendarDate 2008 Dec 31)),
      equals (2008, Dec, 31, 20, 30, 40, 567) (toParts <| fromSpec local (atTime 20 30 40 567) (ordinalDate 2008 366)),
      equals (2008, Dec, 31, 20, 30, 40, 567) (toParts <| fromSpec local (atTime 20 30 40 567) (weekDate 2009 1 3))
    ],
    suite "utc" [
      equals (2008, Dec, 31,  0,  0,  0,   0) (toParts <| toUtc <| fromSpec utc noTime (calendarDate 2008 Dec 31)),
      equals (2008, Dec, 31,  0,  0,  0,   0) (toParts <| toUtc <| fromSpec utc noTime (ordinalDate 2008 366)),
      equals (2008, Dec, 31,  0,  0,  0,   0) (toParts <| toUtc <| fromSpec utc noTime (weekDate 2009 1 3)),
      equals (2008, Dec, 31, 20, 30, 40, 567) (toParts <| toUtc <| fromSpec utc (atTime 20 30 40 567) (calendarDate 2008 Dec 31)),
      equals (2008, Dec, 31, 20, 30, 40, 567) (toParts <| toUtc <| fromSpec utc (atTime 20 30 40 567) (ordinalDate 2008 366)),
      equals (2008, Dec, 31, 20, 30, 40, 567) (toParts <| toUtc <| fromSpec utc (atTime 20 30 40 567) (weekDate 2009 1 3))
    ],
    suite "offset" [
      equals (2008, Dec, 31,  0,  0,  0,   0) (toParts <| (toTimeOffset 60) <| fromSpec (offset 60) noTime (calendarDate 2008 Dec 31)),
      equals (2008, Dec, 31,  0,  0,  0,   0) (toParts <| (toTimeOffset 60) <| fromSpec (offset 60) noTime (ordinalDate 2008 366)),
      equals (2008, Dec, 31,  0,  0,  0,   0) (toParts <| (toTimeOffset 60) <| fromSpec (offset 60) noTime (weekDate 2009 1 3)),
      equals (2008, Dec, 31, 20, 30, 40, 567) (toParts <| (toTimeOffset 60) <| fromSpec (offset 60) (atTime 20 30 40 567) (calendarDate 2008 Dec 31)),
      equals (2008, Dec, 31, 20, 30, 40, 567) (toParts <| (toTimeOffset 60) <| fromSpec (offset 60) (atTime 20 30 40 567) (ordinalDate 2008 366)),
      equals (2008, Dec, 31, 20, 30, 40, 567) (toParts <| (toTimeOffset 60) <| fromSpec (offset 60) (atTime 20 30 40 567) (weekDate 2009 1 3))
    ]
  ]


fromJulianDateTests : Test
fromJulianDateTests =
  suite "fromJulianDate" [
    equals (2013, Jan, 1, 0, 30, 0, 0) (toParts <| toUtc <| fromJulianDate (2456293 + 12.5 / 24)),
    equals (2000, Jan, 1, 0,  0, 0, 0) (toParts <| toUtc <| fromJulianDate 2451544.5)
  ]


tests : Test
tests =
  suite "Create" [
    fromPartsTests,
    fromCalendarDateTests,
    fromIsoStringTests,
    fromSpecTests,
    fromJulianDateTests
  ]
