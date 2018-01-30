module Test.Format exposing (tests)

import Date exposing (Date, Month(..))
import Date.Extra as Date exposing (atTime, calendarDate, noTime, toFormattedString, toIsoString, toUtcFormattedString, toUtcIsoString, utc)
import Expect
import Regex exposing (contains, regex)
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "Format"
        [ test_toFormattedString
        , test_toIsoString
        , test_toUtcFormattedString
        , test_toUtcIsoString
        ]


test_toFormattedString : Test
test_toFormattedString =
    let
        toTest : Date -> ( String, String ) -> Test
        toTest date ( pattern, expected ) =
            test pattern <|
                \() ->
                    Expect.equal
                        expected
                        (toFormattedString pattern date)
    in
    describe "toFormattedString"
        [ describe "symbols" <|
            List.map
                (toTest <| Date.fromParts 2001 Jan 2 3 4 5 67)
                [ ( "y", "2001" )
                , ( "yy", "01" )
                , ( "yyy", "2001" )
                , ( "yyyy", "2001" )
                , ( "yyyyy", "02001" )
                , ( "Y", "2001" )
                , ( "YY", "01" )
                , ( "YYY", "2001" )
                , ( "YYYY", "2001" )
                , ( "YYYYY", "02001" )
                , ( "Q", "1" )
                , ( "QQ", "1" )
                , ( "QQQ", "Q1" )
                , ( "QQQQ", "1st" )
                , ( "QQQQQ", "1" )
                , ( "QQQQQQ", "" )
                , ( "M", "1" )
                , ( "MM", "01" )
                , ( "MMM", "Jan" )
                , ( "MMMM", "January" )
                , ( "MMMMM", "J" )
                , ( "MMMMMM", "" )
                , ( "w", "1" )
                , ( "ww", "01" )
                , ( "www", "" )
                , ( "d", "2" )
                , ( "dd", "02" )
                , ( "ddd", "2nd" )
                , ( "dddd", "" )
                , ( "D", "2" )
                , ( "DD", "02" )
                , ( "DDD", "002" )
                , ( "DDDD", "" )
                , ( "E", "Tue" )
                , ( "EE", "Tue" )
                , ( "EEE", "Tue" )
                , ( "EEEE", "Tuesday" )
                , ( "EEEEE", "T" )
                , ( "EEEEEE", "Tu" )
                , ( "EEEEEEE", "" )
                , ( "e", "2" )
                , ( "ee", "2" )
                , ( "eee", "Tue" )
                , ( "eeee", "Tuesday" )
                , ( "eeeee", "T" )
                , ( "eeeeee", "Tu" )
                , ( "eeeeeee", "" )
                , ( "a", "AM" )
                , ( "aa", "AM" )
                , ( "aaa", "AM" )
                , ( "aaaa", "A.M." )
                , ( "aaaaa", "A" )
                , ( "aaaaaa", "" )
                , ( "b", "am" )
                , ( "bb", "am" )
                , ( "bbb", "am" )
                , ( "bbbb", "a.m." )
                , ( "bbbbb", "a" )
                , ( "bbbbbb", "" )
                , ( "h", "3" )
                , ( "hh", "03" )
                , ( "hhh", "" )
                , ( "H", "3" )
                , ( "HH", "03" )
                , ( "HHH", "" )
                , ( "m", "4" )
                , ( "mm", "04" )
                , ( "mmm", "" )
                , ( "s", "5" )
                , ( "ss", "05" )
                , ( "sss", "" )
                , ( "S", "0" )
                , ( "SS", "06" )
                , ( "SSS", "067" )
                , ( "SSSS", "0670" )
                ]
        , describe "escapes" <|
            List.map
                (toTest <| Date.fromParts 2001 Jan 1 0 0 0 0)
                [ ( "'yYQMwdDEeabhHmsSXx'", "yYQMwdDEeabhHmsSXx" )
                , ( "''' '' ''' ''", "' ' ' '" )
                , ( "'yyyy:' yyyy", "yyyy: 2001" )
                ]
        , describe "symbol 'b' (midnight)" <|
            List.map
                (toTest <| Date.fromParts 2001 Jan 1 0 0 0 0)
                [ ( "b", "mid." )
                , ( "bb", "mid." )
                , ( "bbb", "mid." )
                , ( "bbbb", "midnight" )
                , ( "bbbbb", "md" )
                , ( "bbbbbb", "" )
                ]
        , describe "symbol 'b' (noon)" <|
            List.map
                (toTest <| Date.fromParts 2001 Jan 1 12 0 0 0)
                [ ( "b", "noon" )
                , ( "bb", "noon" )
                , ( "bbb", "noon" )
                , ( "bbbb", "noon" )
                , ( "bbbbb", "nn" )
                , ( "bbbbbb", "" )
                ]
        , describe "common uses" <|
            List.map
                (toTest <| Date.fromParts 2008 Dec 31 20 30 40 567)
                [ ( "yyyy-MM-dd", "2008-12-31" )
                , ( "yyyy-DDD", "2008-366" )
                , ( "YYYY-'W'ww-e", "2009-W01-3" )
                , ( "'T'HH:mm:ss.SSS", "T20:30:40.567" )
                , ( "M/d/y", "12/31/2008" )
                , ( "''yy", "'08" )
                , ( "h:mm a", "8:30 PM" )
                , ( "h 'o''clock' bbbb", "8 o'clock p.m." )
                ]
        ]


test_toUtcFormattedString : Test
test_toUtcFormattedString =
    let
        toTest : Date -> ( String, String ) -> Test
        toTest date ( pattern, expected ) =
            test pattern <|
                \() ->
                    Expect.equal
                        expected
                        (toUtcFormattedString pattern date)
    in
    describe "toUtcFormattedString"
        [ describe "symbols" <|
            List.map
                (toTest <| Date.fromSpec utc (atTime 3 4 5 67) (calendarDate 2001 Jan 2))
                [ ( "y", "2001" )
                , ( "yy", "01" )
                , ( "yyy", "2001" )
                , ( "yyyy", "2001" )
                , ( "yyyyy", "02001" )
                , ( "Y", "2001" )
                , ( "YY", "01" )
                , ( "YYY", "2001" )
                , ( "YYYY", "2001" )
                , ( "YYYYY", "02001" )
                , ( "Q", "1" )
                , ( "QQ", "1" )
                , ( "QQQ", "Q1" )
                , ( "QQQQ", "1st" )
                , ( "QQQQQ", "1" )
                , ( "QQQQQQ", "" )
                , ( "M", "1" )
                , ( "MM", "01" )
                , ( "MMM", "Jan" )
                , ( "MMMM", "January" )
                , ( "MMMMM", "J" )
                , ( "MMMMMM", "" )
                , ( "w", "1" )
                , ( "ww", "01" )
                , ( "www", "" )
                , ( "d", "2" )
                , ( "dd", "02" )
                , ( "ddd", "2nd" )
                , ( "dddd", "" )
                , ( "D", "2" )
                , ( "DD", "02" )
                , ( "DDD", "002" )
                , ( "DDDD", "" )
                , ( "E", "Tue" )
                , ( "EE", "Tue" )
                , ( "EEE", "Tue" )
                , ( "EEEE", "Tuesday" )
                , ( "EEEEE", "T" )
                , ( "EEEEEE", "Tu" )
                , ( "EEEEEEE", "" )
                , ( "e", "2" )
                , ( "ee", "2" )
                , ( "eee", "Tue" )
                , ( "eeee", "Tuesday" )
                , ( "eeeee", "T" )
                , ( "eeeeee", "Tu" )
                , ( "eeeeeee", "" )
                , ( "a", "AM" )
                , ( "aa", "AM" )
                , ( "aaa", "AM" )
                , ( "aaaa", "A.M." )
                , ( "aaaaa", "A" )
                , ( "aaaaaa", "" )
                , ( "b", "am" )
                , ( "bb", "am" )
                , ( "bbb", "am" )
                , ( "bbbb", "a.m." )
                , ( "bbbbb", "a" )
                , ( "bbbbbb", "" )
                , ( "h", "3" )
                , ( "hh", "03" )
                , ( "hhh", "" )
                , ( "H", "3" )
                , ( "HH", "03" )
                , ( "HHH", "" )
                , ( "m", "4" )
                , ( "mm", "04" )
                , ( "mmm", "" )
                , ( "s", "5" )
                , ( "ss", "05" )
                , ( "sss", "" )
                , ( "S", "0" )
                , ( "SS", "06" )
                , ( "SSS", "067" )
                , ( "SSSS", "0670" )
                , ( "X", "Z" )
                , ( "XX", "Z" )
                , ( "XXX", "Z" )
                , ( "XXXX", "" )
                , ( "x", "+00" )
                , ( "xx", "+0000" )
                , ( "xxx", "+00:00" )
                , ( "xxxx", "" )
                ]
        ]


test_toIsoString : Test
test_toIsoString =
    let
        date =
            Date.fromParts 2001 Jan 2 20 30 40 567

        expected =
            regex "^2001-01-02T20:30:40.567[+-][0-2]\\d:[0-5]\\d$"
    in
    test "toIsoString" <|
        \() ->
            Expect.true "uses the local offset"
                (date |> toIsoString |> contains expected)


test_toUtcIsoString : Test
test_toUtcIsoString =
    describe "toUtcIsoString"
        [ test "uses the UTC offset" <|
            \() ->
                Expect.equal
                    "2001-01-02T20:30:40.567Z"
                    (toUtcIsoString <| Date.fromSpec utc (atTime 20 30 40 567) (calendarDate 2001 Jan 2))
        ]
