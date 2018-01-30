module Test.Extract exposing (tests)

import Date exposing (Date, Month(..))
import Date.Extra as Date exposing (fractionalDay, monthNumber, ordinalDay, quarter, weekNumber, weekYear, weekdayNumber)
import Date.Extra.Facts exposing (months)
import Expect
import Test exposing (Test, describe, test)
import Utilities exposing (calendarDatesInMonth)


tests : Test
tests =
    describe "Extract"
        [ test_monthNumber
        , test_quarter
        , test_ordinalDay
        , test_fractionalDay
        , test_weekDate
        ]


datesInMonth : Int -> Month -> List Date
datesInMonth y m =
    List.map
        (\( y, m, d ) -> Date.fromCalendarDate y m d)
        (calendarDatesInMonth y m)


datesInYear : Int -> List Date
datesInYear y =
    List.concatMap
        (datesInMonth y)
        months


dateFunctionTest : (Date -> a) -> a -> Date -> Test
dateFunctionTest f expected date =
    test (date |> Date.toTime |> toString) <|
        \() ->
            f date |> Expect.equal expected


test_monthNumber : Test
test_monthNumber =
    let
        toTest : ( Month, Int ) -> Test
        toTest ( m, mn ) =
            describe (toString mn)
                (datesInMonth 2016 m
                    |> List.map (dateFunctionTest monthNumber mn)
                )
    in
    describe "monthNumber" <|
        List.map
            toTest
            [ ( Jan, 1 )
            , ( Feb, 2 )
            , ( Mar, 3 )
            , ( Apr, 4 )
            , ( May, 5 )
            , ( Jun, 6 )
            , ( Jul, 7 )
            , ( Aug, 8 )
            , ( Sep, 9 )
            , ( Oct, 10 )
            , ( Nov, 11 )
            , ( Dec, 12 )
            ]


test_quarter : Test
test_quarter =
    let
        toTest : ( List Month, Int ) -> Test
        toTest ( monthsInQuarter, q ) =
            describe (toString q)
                (monthsInQuarter
                    |> List.concatMap (datesInMonth 2016)
                    |> List.map (dateFunctionTest quarter q)
                )
    in
    describe "quarter" <|
        List.map
            toTest
            [ ( [ Jan, Feb, Mar ], 1 )
            , ( [ Apr, May, Jun ], 2 )
            , ( [ Jul, Aug, Sep ], 3 )
            , ( [ Oct, Nov, Dec ], 4 )
            ]


test_ordinalDay : Test
test_ordinalDay =
    let
        toTest : Int -> Date -> Test
        toTest i =
            dateFunctionTest ordinalDay (i + 1)
    in
    describe "ordinalDay"
        [ describe "leap year"
            (datesInYear 2016 |> List.indexedMap toTest)
        , describe "non-leap year"
            (datesInYear 2017 |> List.indexedMap toTest)
        ]


test_fractionalDay : Test
test_fractionalDay =
    describe "fractionalDay" <|
        List.map
            (uncurry (dateFunctionTest fractionalDay))
            [ ( 0.0, Date.fromParts 2001 Jan 1 0 0 0 0 )
            , ( 0.25, Date.fromParts 2001 Jan 1 6 0 0 0 )
            , ( 0.5, Date.fromParts 2001 Jan 1 12 0 0 0 )
            , ( 0.75, Date.fromParts 2001 Jan 1 18 0 0 0 )
            , ( 3661001 / 86400000, Date.fromParts 2001 Jan 1 1 1 1 1 )
            ]


test_weekDate : Test
test_weekDate =
    let
        toWeekDate : Date -> ( Int, Int, Int )
        toWeekDate date =
            ( weekYear date, weekNumber date, weekdayNumber date )

        weekDateTest : ( Int, Month, Int ) -> ( Int, Int, Int ) -> Test
        weekDateTest ( y, m, d ) weekDate =
            Date.fromCalendarDate y m d
                |> dateFunctionTest toWeekDate weekDate
    in
    describe "weekYear, weekNumber, weekdayNumber" <|
        List.map
            (\( calendarDate, weekDate ) ->
                weekDateTest calendarDate weekDate
            )
            [ ( ( 2005, Jan, 1 ), ( 2004, 53, 6 ) )
            , ( ( 2005, Jan, 2 ), ( 2004, 53, 7 ) )
            , ( ( 2005, Dec, 31 ), ( 2005, 52, 6 ) )
            , ( ( 2007, Jan, 1 ), ( 2007, 1, 1 ) )
            , ( ( 2007, Dec, 30 ), ( 2007, 52, 7 ) )
            , ( ( 2007, Dec, 31 ), ( 2008, 1, 1 ) )
            , ( ( 2008, Jan, 1 ), ( 2008, 1, 2 ) )
            , ( ( 2008, Dec, 28 ), ( 2008, 52, 7 ) )
            , ( ( 2008, Dec, 29 ), ( 2009, 1, 1 ) )
            , ( ( 2008, Dec, 30 ), ( 2009, 1, 2 ) )
            , ( ( 2008, Dec, 31 ), ( 2009, 1, 3 ) )
            , ( ( 2009, Jan, 1 ), ( 2009, 1, 4 ) )
            , ( ( 2009, Dec, 31 ), ( 2009, 53, 4 ) )
            , ( ( 2010, Jan, 1 ), ( 2009, 53, 5 ) )
            , ( ( 2010, Jan, 2 ), ( 2009, 53, 6 ) )
            , ( ( 2010, Jan, 3 ), ( 2009, 53, 7 ) )
            ]
