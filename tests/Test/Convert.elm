module Test.Convert exposing (tests)

import ElmTest exposing (Test, suite, test, assert, assertEqual, equals)
import Regex exposing (regex, contains)

import Date exposing (Date, Month(..))
import Date.Create as Date exposing (utc, noTime, atTime, calendarDate)
import Date.Convert exposing (toFormattedString, toTimestamp, toUtcFormattedString, toUtcTimestamp, toJulianDate)


toFormattedStringTests : Test
toFormattedStringTests =
  let
    toFormattedStringTest : Date -> (String, String) -> Test
    toFormattedStringTest date (pattern, expected) =
      test pattern <| assertEqual expected (toFormattedString pattern date)

    toFormattedStringSuite : String -> Date -> List (String, String) -> Test
    toFormattedStringSuite name date pairs =
      suite name <| List.map (toFormattedStringTest date) pairs

    suite1 = toFormattedStringSuite
      "symbols"
      (Date.fromParts 2001 Jan 2 3 4 5 67)
      [
        ("y",       "2001"),
        ("yy",      "01"),
        ("yyy",     "2001"),
        ("yyyy",    "2001"),
        ("yyyyy",   "02001"),

        ("Y",       "2001"),
        ("YY",      "01"),
        ("YYY",     "2001"),
        ("YYYY",    "2001"),
        ("YYYYY",   "02001"),

        ("Q",       "1"),
        ("QQ",      "1"),
        ("QQQ",     "Q1"),
        ("QQQQ",    "1st"),
        ("QQQQQ",   "1"),
        ("QQQQQQ",  ""),

        ("M",       "1"),
        ("MM",      "01"),
        ("MMM",     "Jan"),
        ("MMMM",    "January"),
        ("MMMMM",   "J"),
        ("MMMMMM",  ""),

        ("w",       "1"),
        ("ww",      "01"),
        ("www",     ""),

        ("d",       "2"),
        ("dd",      "02"),
        ("ddd",     "2nd"),
        ("dddd",    ""),

        ("D",       "2"),
        ("DD",      "02"),
        ("DDD",     "002"),
        ("DDDD",    ""),

        ("E",       "Tue"),
        ("EE",      "Tue"),
        ("EEE",     "Tue"),
        ("EEEE",    "Tuesday"),
        ("EEEEE",   "T"),
        ("EEEEEE",  "Tu"),
        ("EEEEEEE", ""),

        ("e",       "2"),
        ("ee",      "2"),
        ("eee",     "Tue"),
        ("eeee",    "Tuesday"),
        ("eeeee",   "T"),
        ("eeeeee",  "Tu"),
        ("eeeeeee", ""),

        ("a",       "AM"),
        ("aa",      "AM"),
        ("aaa",     "AM"),
        ("aaaa",    "A.M."),
        ("aaaaa",   "A"),
        ("aaaaaa",  ""),

        ("b",       "am"),
        ("bb",      "am"),
        ("bbb",     "am"),
        ("bbbb",    "a.m."),
        ("bbbbb",   "a"),
        ("bbbbbb",  ""),

        ("h",       "3"),
        ("hh",      "03"),
        ("hhh",     ""),

        ("H",       "3"),
        ("HH",      "03"),
        ("HHH",     ""),

        ("m",       "4"),
        ("mm",      "04"),
        ("mmm",     ""),

        ("s",       "5"),
        ("ss",      "05"),
        ("sss",     ""),

        ("S",       "0"),
        ("SS",      "06"),
        ("SSS",     "067"),
        ("SSSS",    "0670")
      ]
    
    suite2 = toFormattedStringSuite
      "escapes"
      (Date.fromParts 2001 Jan 1 0 0 0 0)
      [
        ("'yYQMwdDEeabhHmsSXx'",  "yYQMwdDEeabhHmsSXx"),
        ("''' '' ''' ''",         "' ' ' '"),
        ("'yyyy:' yyyy",          "yyyy: 2001")
      ]

    suite3 = toFormattedStringSuite
      "symbol 'b' (midnight)"
      (Date.fromParts 2001 Jan 1 0 0 0 0)
      [
        ("b",       "mid."),
        ("bb",      "mid."),
        ("bbb",     "mid."),
        ("bbbb",    "midnight"),
        ("bbbbb",   "md"),
        ("bbbbbb",  "")
      ]

    suite4 = toFormattedStringSuite
      "symbol 'b' (noon)"
      (Date.fromParts 2001 Jan 1 12 0 0 0)
      [
        ("b",       "noon"),
        ("bb",      "noon"),
        ("bbb",     "noon"),
        ("bbbb",    "noon"),
        ("bbbbb",   "nn"),
        ("bbbbbb",  "")
      ]

    suite5 = toFormattedStringSuite
      "common uses"
      (Date.fromParts 2008 Dec 31 20 30 40 567)
      [
        ("yyyy-MM-dd",           "2008-12-31"),
        ("yyyy-DDD",             "2008-366"),
        ("YYYY-'W'ww-e",         "2009-W01-3"),
        ("'T'HH:mm:ss.SSS",      "T20:30:40.567"),
        ("M/d/y",                "12/31/2008"),
        ("''yy",                 "'08"),
        ("h:mm a",               "8:30 PM"),
        ("h 'o''clock' bbbb",    "8 o'clock p.m.")
      ]

  in
    suite "toFormattedString" [suite1, suite2, suite3, suite4, suite5]


toUtcFormattedStringTests : Test
toUtcFormattedStringTests =
  let
    toUtcFormattedStringTest : Date -> (String, String) -> Test
    toUtcFormattedStringTest date (pattern, expected) =
      test pattern <| assertEqual expected (toUtcFormattedString pattern date)

    toUtcFormattedStringSuite : String -> Date -> List (String, String) -> Test
    toUtcFormattedStringSuite name date pairs =
      suite name <| List.map (toUtcFormattedStringTest date) pairs

    suite1 = toUtcFormattedStringSuite
      "symbols"
      (Date.fromSpec utc (atTime 3 4 5 67) (calendarDate 2001 Jan 2))
      [
        ("y",       "2001"),
        ("yy",      "01"),
        ("yyy",     "2001"),
        ("yyyy",    "2001"),
        ("yyyyy",   "02001"),

        ("Y",       "2001"),
        ("YY",      "01"),
        ("YYY",     "2001"),
        ("YYYY",    "2001"),
        ("YYYYY",   "02001"),

        ("Q",       "1"),
        ("QQ",      "1"),
        ("QQQ",     "Q1"),
        ("QQQQ",    "1st"),
        ("QQQQQ",   "1"),
        ("QQQQQQ",  ""),

        ("M",       "1"),
        ("MM",      "01"),
        ("MMM",     "Jan"),
        ("MMMM",    "January"),
        ("MMMMM",   "J"),
        ("MMMMMM",  ""),

        ("w",       "1"),
        ("ww",      "01"),
        ("www",     ""),

        ("d",       "2"),
        ("dd",      "02"),
        ("ddd",     "2nd"),
        ("dddd",    ""),

        ("D",       "2"),
        ("DD",      "02"),
        ("DDD",     "002"),
        ("DDDD",    ""),

        ("E",       "Tue"),
        ("EE",      "Tue"),
        ("EEE",     "Tue"),
        ("EEEE",    "Tuesday"),
        ("EEEEE",   "T"),
        ("EEEEEE",  "Tu"),
        ("EEEEEEE", ""),

        ("e",       "2"),
        ("ee",      "2"),
        ("eee",     "Tue"),
        ("eeee",    "Tuesday"),
        ("eeeee",   "T"),
        ("eeeeee",  "Tu"),
        ("eeeeeee", ""),

        ("a",       "AM"),
        ("aa",      "AM"),
        ("aaa",     "AM"),
        ("aaaa",    "A.M."),
        ("aaaaa",   "A"),
        ("aaaaaa",  ""),

        ("b",       "am"),
        ("bb",      "am"),
        ("bbb",     "am"),
        ("bbbb",    "a.m."),
        ("bbbbb",   "a"),
        ("bbbbbb",  ""),

        ("h",       "3"),
        ("hh",      "03"),
        ("hhh",     ""),

        ("H",       "3"),
        ("HH",      "03"),
        ("HHH",     ""),

        ("m",       "4"),
        ("mm",      "04"),
        ("mmm",     ""),

        ("s",       "5"),
        ("ss",      "05"),
        ("sss",     ""),

        ("S",       "0"),
        ("SS",      "06"),
        ("SSS",     "067"),
        ("SSSS",    "0670"),

        ("X",       "Z"),
        ("XX",      "Z"),
        ("XXX",     "Z"),
        ("XXXX",    ""),

        ("x",       "+00"),
        ("xx",      "+0000"),
        ("xxx",     "+00:00"),
        ("xxxx",    "")
      ]
  in
    suite "toUtcFormattedString" [suite1]


toTimestampTests : Test
toTimestampTests =
  let
    date = Date.fromParts 2001 Jan 2 20 30 40 567
    expected = regex "^2001-01-02T20:30:40.567[+-][0-2]\\d:[0-5]\\d$"
  in
    suite "toTimestamp" [
      test "yyyy-mm-ddThh:mm:ss.fff+hh:mm" <| assert <| contains expected (toTimestamp date)
    ]


toUtcTimestampTests : Test
toUtcTimestampTests =
  suite "toUtcTimestamp" [
    equals
      "2001-01-02T20:30:40.567Z"
      (toUtcTimestamp <| Date.fromSpec utc (atTime 20 30 40 567) (calendarDate 2001 Jan 2))
  ]


toJulianDateTests : Test
toJulianDateTests =
  suite "toJulianDate" [
    equals
      (2456293 + 12.5 / 24)
      (toJulianDate <| Date.fromSpec utc (atTime 0 30 0 0) (calendarDate 2013 Jan 1)),
    equals
      2451544.5
      (toJulianDate <| Date.fromSpec utc noTime (calendarDate 2000 Jan 1))
  ]


tests : Test
tests =
  suite "Convert" [
    toFormattedStringTests,
    toTimestampTests,
    toUtcFormattedStringTests,
    toUtcTimestampTests,
    toJulianDateTests
  ]
