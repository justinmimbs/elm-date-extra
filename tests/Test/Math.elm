module Test.Math exposing (tests)

import Date exposing (Date, Month(..))
import Date.Extra as Date exposing (Interval(..), fromParts)
import Expect exposing (Expectation)
import Test exposing (Test, describe, test)
import Utilities exposing (DateParts, calendarDatesInYear, toParts)


tests : Test
tests =
    describe "Math"
        [ test_equal
        , test_compare
        , test_isBetween
        , test_clamp
        , test_equalBy
        , test_floor
        , test_ceiling
        , test_add
        , test_diff
        , test_range
        ]


test_equal : Test
test_equal =
    describe "equal"
        [ test "==" <| \() -> Date.equal (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan 1 0 0 0 0) |> Expect.equal True
        , test "/=" <| \() -> Date.equal (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan 1 0 0 0 1) |> Expect.equal False
        ]


date1 =
    fromParts 2000 Jan 1 0 0 0 0


date2 =
    fromParts 2000 Jan 1 0 0 0 1


date3 =
    fromParts 2001 Jan 1 0 0 0 0


test_compare : Test
test_compare =
    describe "compare"
        [ test "LT" <| \() -> Date.compare date1 date2 |> Expect.equal LT
        , test "EQ" <| \() -> Date.compare date2 date2 |> Expect.equal EQ
        , test "GT" <| \() -> Date.compare date3 date2 |> Expect.equal GT
        , test "sorting" <|
            \() ->
                Expect.equal
                    (List.map toParts [ date1, date2, date3 ])
                    (List.map toParts <| List.sortWith Date.compare [ date3, date1, date2 ])
        ]


test_isBetween : Test
test_isBetween =
    describe "isBetween"
        [ test "1 3 2" <| \() -> Date.isBetween date1 date3 date2 |> Expect.equal True
        , test "1 3 3" <| \() -> Date.isBetween date1 date3 date3 |> Expect.equal True
        , test "1 3 1" <| \() -> Date.isBetween date1 date3 date1 |> Expect.equal True
        , test "3 1 2" <| \() -> Date.isBetween date3 date1 date2 |> Expect.equal True
        , test "3 1 3" <| \() -> Date.isBetween date3 date1 date3 |> Expect.equal True
        , test "3 1 1" <| \() -> Date.isBetween date3 date1 date1 |> Expect.equal True
        , test "1 2 3" <| \() -> Date.isBetween date1 date2 date3 |> Expect.equal False
        , test "2 1 3" <| \() -> Date.isBetween date2 date1 date3 |> Expect.equal False
        ]


test_clamp : Test
test_clamp =
    describe "clamp"
        [ test "1 3 2" <| \() -> Date.clamp date1 date3 date2 |> expectDateEqual date2
        , test "1 2 3" <| \() -> Date.clamp date1 date2 date3 |> expectDateEqual date2
        , test "1 2 2" <| \() -> Date.clamp date1 date2 date2 |> expectDateEqual date2
        , test "2 3 1" <| \() -> Date.clamp date2 date3 date1 |> expectDateEqual date2
        , test "2 3 2" <| \() -> Date.clamp date2 date3 date2 |> expectDateEqual date2
        ]


test_equalBy : Test
test_equalBy =
    describe "equalBy"
        [ test "Millisecond (true)" <| \() -> Date.equalBy Millisecond (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan 1 0 0 0 0) |> Expect.equal True
        , test "Second (true)" <| \() -> Date.equalBy Second (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan 1 0 0 0 999) |> Expect.equal True
        , test "Minute (true)" <| \() -> Date.equalBy Minute (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan 1 0 0 59 999) |> Expect.equal True
        , test "Hour (true)" <| \() -> Date.equalBy Hour (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan 1 0 59 59 999) |> Expect.equal True
        , test "Day (true)" <| \() -> Date.equalBy Day (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan 1 23 59 59 999) |> Expect.equal True
        , test "Month (true)" <| \() -> Date.equalBy Month (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan 31 23 59 59 999) |> Expect.equal True
        , test "Year (true)" <| \() -> Date.equalBy Year (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Dec 31 23 59 59 999) |> Expect.equal True
        , test "Quarter (true)" <| \() -> Date.equalBy Quarter (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Mar 31 23 59 59 999) |> Expect.equal True
        , test "Week (true)" <| \() -> Date.equalBy Week (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan 2 23 59 59 999) |> Expect.equal True
        , test "Monday (true)" <| \() -> Date.equalBy Monday (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan 2 23 59 59 999) |> Expect.equal True
        , test "Tuesday (true)" <| \() -> Date.equalBy Tuesday (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan 3 23 59 59 999) |> Expect.equal True
        , test "Wednesday (true)" <| \() -> Date.equalBy Wednesday (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan 4 23 59 59 999) |> Expect.equal True
        , test "Thursday (true)" <| \() -> Date.equalBy Thursday (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan 5 23 59 59 999) |> Expect.equal True
        , test "Friday (true)" <| \() -> Date.equalBy Friday (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan 6 23 59 59 999) |> Expect.equal True
        , test "Saturday (true)" <| \() -> Date.equalBy Saturday (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan 7 23 59 59 999) |> Expect.equal True
        , test "Sunday (true)" <| \() -> Date.equalBy Sunday (fromParts 2000 Jan 2 0 0 0 0) (fromParts 2000 Jan 8 23 59 59 999) |> Expect.equal True
        , test "Millisecond (false)" <| \() -> Date.equalBy Millisecond (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan 1 0 0 0 1) |> Expect.equal False
        , test "Second (false)" <| \() -> Date.equalBy Second (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan 1 0 0 1 0) |> Expect.equal False
        , test "Minute (false)" <| \() -> Date.equalBy Minute (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan 1 0 1 0 0) |> Expect.equal False
        , test "Hour (false)" <| \() -> Date.equalBy Hour (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan 1 1 0 0 0) |> Expect.equal False
        , test "Day (false)" <| \() -> Date.equalBy Day (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan 2 0 0 0 0) |> Expect.equal False
        , test "Month (false)" <| \() -> Date.equalBy Month (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Feb 1 0 0 0 0) |> Expect.equal False
        , test "Year (false)" <| \() -> Date.equalBy Year (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2001 Jan 1 0 0 0 0) |> Expect.equal False
        , test "Quarter (false)" <| \() -> Date.equalBy Quarter (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Apr 1 0 0 0 0) |> Expect.equal False
        , test "Week (false)" <| \() -> Date.equalBy Week (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan 3 0 0 0 0) |> Expect.equal False
        , test "Monday (false)" <| \() -> Date.equalBy Monday (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan 3 0 0 0 0) |> Expect.equal False
        , test "Tuesday (false)" <| \() -> Date.equalBy Tuesday (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan 4 0 0 0 0) |> Expect.equal False
        , test "Wednesday (false)" <| \() -> Date.equalBy Wednesday (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan 5 0 0 0 0) |> Expect.equal False
        , test "Thursday (false)" <| \() -> Date.equalBy Thursday (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan 6 0 0 0 0) |> Expect.equal False
        , test "Friday (false)" <| \() -> Date.equalBy Friday (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan 7 0 0 0 0) |> Expect.equal False
        , test "Saturday (false)" <| \() -> Date.equalBy Saturday (fromParts 2000 Jan 1 0 0 0 0) (fromParts 2000 Jan 8 0 0 0 0) |> Expect.equal False
        , test "Sunday (false)" <| \() -> Date.equalBy Sunday (fromParts 2000 Jan 2 0 0 0 0) (fromParts 2000 Jan 9 0 0 0 0) |> Expect.equal False
        ]


expectDateEqual : Date -> Date -> Expectation
expectDateEqual a b =
    Expect.equal (Date.toTime b) (Date.toTime a)


expectDateIdempotence : (Date -> Date) -> Date -> Expectation
expectDateIdempotence f x =
    f (f x) |> expectDateEqual (f x)


test_floor : Test
test_floor =
    let
        date =
            fromParts 1999 Dec 31 23 59 59 999

        toTest : Interval -> Date -> Test
        toTest interval expected =
            describe (toString interval)
                [ test "expected" <| \() -> date |> Date.floor interval |> expectDateEqual expected
                , test "idempotent" <| \() -> date |> expectDateIdempotence (Date.floor interval)
                ]
    in
    describe "floor" <|
        List.map
            (uncurry toTest)
            [ ( Millisecond, fromParts 1999 Dec 31 23 59 59 999 )
            , ( Second, fromParts 1999 Dec 31 23 59 59 0 )
            , ( Minute, fromParts 1999 Dec 31 23 59 0 0 )
            , ( Hour, fromParts 1999 Dec 31 23 0 0 0 )
            , ( Day, fromParts 1999 Dec 31 0 0 0 0 )
            , ( Month, fromParts 1999 Dec 1 0 0 0 0 )
            , ( Year, fromParts 1999 Jan 1 0 0 0 0 )
            , ( Quarter, fromParts 1999 Oct 1 0 0 0 0 )
            , ( Week, fromParts 1999 Dec 27 0 0 0 0 )
            , ( Monday, fromParts 1999 Dec 27 0 0 0 0 )
            , ( Tuesday, fromParts 1999 Dec 28 0 0 0 0 )
            , ( Wednesday, fromParts 1999 Dec 29 0 0 0 0 )
            , ( Thursday, fromParts 1999 Dec 30 0 0 0 0 )
            , ( Friday, fromParts 1999 Dec 31 0 0 0 0 )
            , ( Saturday, fromParts 1999 Dec 25 0 0 0 0 )
            , ( Sunday, fromParts 1999 Dec 26 0 0 0 0 )
            ]


test_ceiling : Test
test_ceiling =
    let
        date =
            fromParts 2000 Jan 1 0 0 0 1

        toTest : Interval -> Date -> Test
        toTest interval expected =
            describe (toString interval)
                [ test "expected" <| \() -> date |> Date.ceiling interval |> expectDateEqual expected
                , test "idempotent" <| \() -> date |> expectDateIdempotence (Date.ceiling interval)
                ]
    in
    describe "ceiling" <|
        List.map
            (uncurry toTest)
            [ ( Millisecond, fromParts 2000 Jan 1 0 0 0 1 )
            , ( Second, fromParts 2000 Jan 1 0 0 1 0 )
            , ( Minute, fromParts 2000 Jan 1 0 1 0 0 )
            , ( Hour, fromParts 2000 Jan 1 1 0 0 0 )
            , ( Day, fromParts 2000 Jan 2 0 0 0 0 )
            , ( Month, fromParts 2000 Feb 1 0 0 0 0 )
            , ( Year, fromParts 2001 Jan 1 0 0 0 0 )
            , ( Quarter, fromParts 2000 Apr 1 0 0 0 0 )
            , ( Week, fromParts 2000 Jan 3 0 0 0 0 )
            , ( Monday, fromParts 2000 Jan 3 0 0 0 0 )
            , ( Tuesday, fromParts 2000 Jan 4 0 0 0 0 )
            , ( Wednesday, fromParts 2000 Jan 5 0 0 0 0 )
            , ( Thursday, fromParts 2000 Jan 6 0 0 0 0 )
            , ( Friday, fromParts 2000 Jan 7 0 0 0 0 )
            , ( Saturday, fromParts 2000 Jan 8 0 0 0 0 )
            , ( Sunday, fromParts 2000 Jan 2 0 0 0 0 )
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


test_add : Test
test_add =
    let
        date =
            fromParts 1999 Dec 31 23 59 59 999
    in
    describe "add"
        [ describe "add 0 x == x" <|
            List.map
                (\interval -> test (toString interval) <| \() -> date |> Date.add interval 0 |> expectDateEqual date)
                intervals
        , describe "add -n (add n x) == x" <|
            List.map
                (\interval -> test (toString interval) <| \() -> date |> Date.add interval -5 |> Date.add interval 5 |> expectDateEqual date)
                -- note: not always true for adding Month, Quarter, or Year intervals, as month and year lengths are not consistent
                (intervals |> List.filter (\i -> not (i == Month || i == Quarter || i == Year)))
        , describe "expected results" <|
            List.map
                (\( interval, n, expected ) ->
                    test (toString ( interval, n )) <|
                        \() -> date |> Date.add interval n |> expectDateEqual expected
                )
                [ ( Millisecond, 500, fromParts 2000 Jan 1 0 0 0 499 )
                , ( Millisecond, 1500, fromParts 2000 Jan 1 0 0 1 499 )
                , ( Second, 30, fromParts 2000 Jan 1 0 0 29 999 )
                , ( Second, 90, fromParts 2000 Jan 1 0 1 29 999 )
                , ( Minute, 30, fromParts 2000 Jan 1 0 29 59 999 )
                , ( Minute, 90, fromParts 2000 Jan 1 1 29 59 999 )
                , ( Hour, 12, fromParts 2000 Jan 1 11 59 59 999 )
                , ( Hour, 36, fromParts 2000 Jan 2 11 59 59 999 )
                , ( Day, 15, fromParts 2000 Jan 15 23 59 59 999 )
                , ( Day, 60, fromParts 2000 Feb 29 23 59 59 999 )
                , ( Month, 1, fromParts 2000 Jan 31 23 59 59 999 )
                , ( Month, 2, fromParts 2000 Feb 29 23 59 59 999 )
                , ( Month, 4, fromParts 2000 Apr 30 23 59 59 999 )
                , ( Month, 14, fromParts 2001 Feb 28 23 59 59 999 )
                , ( Quarter, 1, fromParts 2000 Mar 31 23 59 59 999 )
                , ( Quarter, 3, fromParts 2000 Sep 30 23 59 59 999 )
                , ( Year, 5, fromParts 2004 Dec 31 23 59 59 999 )
                , ( Week, 8, fromParts 2000 Feb 25 23 59 59 999 )
                ]
        , describe "adds Years to a leap day as expected" <|
            List.map
                (\( interval, n, expected ) ->
                    test (toString ( interval, n )) <|
                        \() -> fromParts 2000 Feb 29 23 59 59 999 |> Date.add interval n |> expectDateEqual expected
                )
                [ ( Year, 1, fromParts 2001 Feb 28 23 59 59 999 )
                , ( Year, 4, fromParts 2004 Feb 29 23 59 59 999 )
                ]
        ]


test_diff : Test
test_diff =
    let
        date1 =
            fromParts 1999 Dec 31 23 59 59 999

        date2 =
            fromParts 2001 Jan 1 0 0 0 0
    in
    describe "diff"
        [ describe "diff x x == 0" <|
            List.map
                (\interval -> test (toString interval) <| \() -> Date.diff interval date1 date1 |> Expect.equal 0)
                intervals
        , describe "diff a b == -(diff b a)" <|
            List.map
                (\interval -> test (toString interval) <| \() -> Date.diff interval date1 date2 |> Expect.equal (negate <| Date.diff interval date2 date1))
                intervals
        , describe "expected results" <|
            List.map
                (\( interval, date, expected ) ->
                    test (toString ( interval, Date.toTime date, expected )) <| \() -> Date.diff interval date date1 |> Expect.equal expected
                )
                [ ( Millisecond, fromParts 2000 Jan 1 0 0 0 499, -500 )
                , ( Millisecond, fromParts 2000 Jan 1 0 0 1 499, -1500 )
                , ( Second, fromParts 2000 Jan 1 0 0 29 999, -30 )
                , ( Second, fromParts 2000 Jan 1 0 1 29 999, -90 )
                , ( Minute, fromParts 2000 Jan 1 0 29 59 999, -30 )
                , ( Minute, fromParts 2000 Jan 1 1 29 59 999, -90 )
                , ( Hour, fromParts 2000 Jan 1 11 59 59 999, -12 )
                , ( Hour, fromParts 2000 Jan 2 11 59 59 999, -36 )
                , ( Day, fromParts 2000 Jan 15 23 59 59 999, -15 )
                , ( Day, fromParts 2000 Feb 29 23 59 59 999, -60 )
                , ( Month, fromParts 2000 Jan 31 23 59 59 999, -1 )
                , ( Month, fromParts 2000 Feb 29 23 59 59 999, -1 )
                , ( Month, fromParts 2000 Apr 30 23 59 59 999, -3 )
                , ( Month, fromParts 2001 Feb 28 23 59 59 999, -13 )
                , ( Quarter, fromParts 2000 Mar 31 23 59 59 999, -1 )
                , ( Quarter, fromParts 2000 Sep 30 23 59 59 999, -2 )
                , ( Year, fromParts 2004 Dec 31 23 59 59 999, -5 )
                , ( Week, fromParts 2000 Feb 25 23 59 59 999, -8 )
                , ( Monday, fromParts 2000 Jan 11 0 0 0 0, -2 )
                , ( Tuesday, fromParts 2000 Jan 11 0 0 0 0, -2 )
                , ( Wednesday, fromParts 2000 Jan 11 0 0 0 0, -1 )
                , ( Thursday, fromParts 2000 Jan 11 0 0 0 0, -1 )
                , ( Friday, fromParts 2000 Jan 11 0 0 0 0, -1 )
                , ( Saturday, fromParts 2000 Jan 11 0 0 0 0, -2 )
                , ( Sunday, fromParts 2000 Jan 11 0 0 0 0, -2 )
                ]
        ]


test_range : Test
test_range =
    let
        fromDateParts : DateParts -> Date
        fromDateParts ( y, m, d, hh, mm, ss, ms ) =
            fromParts y m d hh mm ss ms

        toTest : Interval -> Int -> DateParts -> DateParts -> List DateParts -> Test
        toTest interval step start end expected =
            test ([ toString interval, toString step, toString start, toString end ] |> String.join " ") <|
                \() ->
                    (Date.range interval step (fromDateParts start) (fromDateParts end) |> List.map toParts)
                        |> Expect.equal expected
    in
    describe "range"
        [ toTest Millisecond 200 ( 2000, Jan, 1, 0, 0, 0, 0 ) ( 2000, Jan, 1, 0, 0, 0, 600 ) <|
            [ ( 2000, Jan, 1, 0, 0, 0, 0 )
            , ( 2000, Jan, 1, 0, 0, 0, 200 )
            , ( 2000, Jan, 1, 0, 0, 0, 400 )
            ]
        , toTest Second 30 ( 2000, Jan, 1, 0, 0, 0, 0 ) ( 2000, Jan, 1, 0, 1, 30, 0 ) <|
            [ ( 2000, Jan, 1, 0, 0, 0, 0 )
            , ( 2000, Jan, 1, 0, 0, 30, 0 )
            , ( 2000, Jan, 1, 0, 1, 0, 0 )
            ]
        , toTest Minute 45 ( 2000, Jan, 1, 0, 0, 0, 0 ) ( 2000, Jan, 1, 2, 15, 0, 0 ) <|
            [ ( 2000, Jan, 1, 0, 0, 0, 0 )
            , ( 2000, Jan, 1, 0, 45, 0, 0 )
            , ( 2000, Jan, 1, 1, 30, 0, 0 )
            ]
        , toTest Hour 18 ( 2000, Jan, 1, 0, 0, 0, 0 ) ( 2000, Jan, 3, 6, 0, 0, 0 ) <|
            [ ( 2000, Jan, 1, 0, 0, 0, 0 )
            , ( 2000, Jan, 1, 18, 0, 0, 0 )
            , ( 2000, Jan, 2, 12, 0, 0, 0 )
            ]
        , toTest Day 2 ( 2000, Jan, 1, 0, 0, 0, 0 ) ( 2000, Jan, 7, 0, 0, 0, 0 ) <|
            [ ( 2000, Jan, 1, 0, 0, 0, 0 )
            , ( 2000, Jan, 3, 0, 0, 0, 0 )
            , ( 2000, Jan, 5, 0, 0, 0, 0 )
            ]
        , toTest Month 2 ( 2000, Jan, 1, 0, 0, 0, 0 ) ( 2000, Jul, 1, 0, 0, 0, 0 ) <|
            [ ( 2000, Jan, 1, 0, 0, 0, 0 )
            , ( 2000, Mar, 1, 0, 0, 0, 0 )
            , ( 2000, May, 1, 0, 0, 0, 0 )
            ]
        , toTest Year 10 ( 2000, Jan, 1, 0, 0, 0, 0 ) ( 2030, Jan, 1, 0, 0, 0, 0 ) <|
            [ ( 2000, Jan, 1, 0, 0, 0, 0 )
            , ( 2010, Jan, 1, 0, 0, 0, 0 )
            , ( 2020, Jan, 1, 0, 0, 0, 0 )
            ]
        , toTest Quarter 1 ( 2000, Jan, 1, 0, 0, 0, 0 ) ( 2000, Sep, 1, 0, 0, 0, 0 ) <|
            [ ( 2000, Jan, 1, 0, 0, 0, 0 )
            , ( 2000, Apr, 1, 0, 0, 0, 0 )
            , ( 2000, Jul, 1, 0, 0, 0, 0 )
            ]
        , toTest Week 2 ( 2000, Jan, 1, 0, 0, 0, 0 ) ( 2000, Feb, 14, 0, 0, 0, 0 ) <|
            [ ( 2000, Jan, 3, 0, 0, 0, 0 )
            , ( 2000, Jan, 17, 0, 0, 0, 0 )
            , ( 2000, Jan, 31, 0, 0, 0, 0 )
            ]
        , toTest Monday 2 ( 2000, Jan, 1, 0, 0, 0, 0 ) ( 2000, Feb, 14, 0, 0, 0, 0 ) <|
            [ ( 2000, Jan, 3, 0, 0, 0, 0 )
            , ( 2000, Jan, 17, 0, 0, 0, 0 )
            , ( 2000, Jan, 31, 0, 0, 0, 0 )
            ]
        , toTest Tuesday 2 ( 2000, Jan, 1, 0, 0, 0, 0 ) ( 2000, Feb, 15, 0, 0, 0, 0 ) <|
            [ ( 2000, Jan, 4, 0, 0, 0, 0 )
            , ( 2000, Jan, 18, 0, 0, 0, 0 )
            , ( 2000, Feb, 1, 0, 0, 0, 0 )
            ]
        , toTest Wednesday 2 ( 2000, Jan, 1, 0, 0, 0, 0 ) ( 2000, Feb, 16, 0, 0, 0, 0 ) <|
            [ ( 2000, Jan, 5, 0, 0, 0, 0 )
            , ( 2000, Jan, 19, 0, 0, 0, 0 )
            , ( 2000, Feb, 2, 0, 0, 0, 0 )
            ]
        , toTest Thursday 2 ( 2000, Jan, 1, 0, 0, 0, 0 ) ( 2000, Feb, 17, 0, 0, 0, 0 ) <|
            [ ( 2000, Jan, 6, 0, 0, 0, 0 )
            , ( 2000, Jan, 20, 0, 0, 0, 0 )
            , ( 2000, Feb, 3, 0, 0, 0, 0 )
            ]
        , toTest Friday 2 ( 2000, Jan, 1, 0, 0, 0, 0 ) ( 2000, Feb, 18, 0, 0, 0, 0 ) <|
            [ ( 2000, Jan, 7, 0, 0, 0, 0 )
            , ( 2000, Jan, 21, 0, 0, 0, 0 )
            , ( 2000, Feb, 4, 0, 0, 0, 0 )
            ]
        , toTest Saturday 2 ( 2000, Jan, 1, 0, 0, 0, 0 ) ( 2000, Feb, 12, 0, 0, 0, 0 ) <|
            [ ( 2000, Jan, 1, 0, 0, 0, 0 )
            , ( 2000, Jan, 15, 0, 0, 0, 0 )
            , ( 2000, Jan, 29, 0, 0, 0, 0 )
            ]
        , toTest Sunday 2 ( 2000, Jan, 1, 0, 0, 0, 0 ) ( 2000, Feb, 13, 0, 0, 0, 0 ) <|
            [ ( 2000, Jan, 2, 0, 0, 0, 0 )
            , ( 2000, Jan, 16, 0, 0, 0, 0 )
            , ( 2000, Jan, 30, 0, 0, 0, 0 )
            ]
        , describe "example"
            [ toTest Day 2 ( 2007, Mar, 15, 11, 55, 0, 0 ) ( 2007, Mar, 22, 0, 0, 0, 0 ) <|
                [ ( 2007, Mar, 16, 0, 0, 0, 0 )
                , ( 2007, Mar, 18, 0, 0, 0, 0 )
                , ( 2007, Mar, 20, 0, 0, 0, 0 )
                ]
            ]
        , describe "begins with the interval nearest to start date"
            [ toTest Day 10 ( 2000, Jan, 1, 0, 0, 0, 0 ) ( 2000, Jan, 30, 0, 0, 0, 0 ) <|
                [ ( 2000, Jan, 1, 0, 0, 0, 0 )
                , ( 2000, Jan, 11, 0, 0, 0, 0 )
                , ( 2000, Jan, 21, 0, 0, 0, 0 )
                ]
            , toTest Day 10 ( 2000, Jan, 1, 0, 0, 0, 0 ) ( 2000, Jan, 31, 0, 0, 0, 0 ) <|
                [ ( 2000, Jan, 1, 0, 0, 0, 0 )
                , ( 2000, Jan, 11, 0, 0, 0, 0 )
                , ( 2000, Jan, 21, 0, 0, 0, 0 )
                ]
            , toTest Day 10 ( 2000, Jan, 1, 0, 0, 0, 0 ) ( 2000, Feb, 1, 0, 0, 0, 0 ) <|
                [ ( 2000, Jan, 1, 0, 0, 0, 0 )
                , ( 2000, Jan, 11, 0, 0, 0, 0 )
                , ( 2000, Jan, 21, 0, 0, 0, 0 )
                , ( 2000, Jan, 31, 0, 0, 0, 0 )
                ]
            ]
        , test "returns a list of days as expected" <|
            \() ->
                (Date.range Day 1 (fromParts 2020 Jan 1 0 0 0 0) (fromParts 2021 Jan 1 0 0 0 0) |> List.map Date.toTime)
                    |> Expect.equal
                        (calendarDatesInYear 2020 |> List.map (\( y, m, d ) -> fromParts y m d 0 0 0 0 |> Date.toTime))
        , test "can return the empty list" <|
            \() ->
                (Date.range Millisecond 1 (fromParts 2020 Jan 1 0 0 0 0) (fromParts 2020 Jan 1 0 0 0 0) |> List.map toParts)
                    |> Expect.equal
                        []
        , describe "can return a large list (tail recursion)"
            [ let
                start =
                    fromParts 1950 Jan 1 0 0 0 0

                end =
                    fromParts 2050 Jan 1 0 0 0 0

                expectedLength =
                    Date.diff Day start end
              in
              test ("length: " ++ toString expectedLength) <|
                \() -> Date.range Day 1 start end |> List.length |> Expect.equal expectedLength
            ]
        ]
