module Test.Create exposing (tests)

import Date exposing (Date, Day(..), Month(..))
import Date.Extra exposing (calendarDate, fromCalendarDate, fromIsoString, fromParts, fromSpec, local, midnight, offset, ordinalDate, time, utc, weekDate)
import Expect
import Regex exposing (HowMany(All), Regex, regex, replace)
import String
import Test exposing (Test, describe, test)
import Utilities exposing (DateParts, calendarDatesInYear, toParts, toTimeOffset, toUtc)


tests : Test
tests =
    describe "Create"
        [ test_fromParts
        , test_fromCalendarDate
        , test_fromIsoString
        , test_fromSpec
        ]


test_fromParts : Test
test_fromParts =
    describe "fromParts" <|
        [ describe "assumes provided parts are in local time, i.e. the extractions will match the provided parts" <|
            List.map
                (\(( y, m, d, hh, mm, ss, ms ) as parts) ->
                    test (toString parts) <|
                        \() -> fromParts y m d hh mm ss ms |> toParts |> Expect.equal parts
                )
                [ ( 1969, Dec, 31, 23, 59, 59, 999 )
                , ( 1970, Jan, 1, 0, 0, 0, 0 )
                , ( 1999, Dec, 31, 23, 59, 59, 999 )
                , ( 2000, Jan, 1, 0, 0, 0, 0 )
                , ( 2008, Dec, 31, 20, 30, 40, 567 )
                ]
        , describe "clamps each part to its upper bound"
            [ test "days" <| \() -> fromParts 2001 Feb 31 0 0 0 0 |> toParts |> Expect.equal ( 2001, Feb, 28, 0, 0, 0, 0 )
            , test "days, leap" <| \() -> fromParts 2000 Feb 31 0 0 0 0 |> toParts |> Expect.equal ( 2000, Feb, 29, 0, 0, 0, 0 )
            , test "hours" <| \() -> fromParts 2001 Feb 28 27 0 0 0 |> toParts |> Expect.equal ( 2001, Feb, 28, 23, 0, 0, 0 )
            , test "minutes" <| \() -> fromParts 2001 Feb 28 0 63 0 0 |> toParts |> Expect.equal ( 2001, Feb, 28, 0, 59, 0, 0 )
            , test "seconds" <| \() -> fromParts 2001 Feb 28 0 0 63 0 |> toParts |> Expect.equal ( 2001, Feb, 28, 0, 0, 59, 0 )
            , test "milliseconds" <| \() -> fromParts 2001 Feb 28 0 0 0 1003 |> toParts |> Expect.equal ( 2001, Feb, 28, 0, 0, 0, 999 )
            ]
        , describe "clamps each part to its lower bound"
            [ test "days" <| \() -> fromParts 2001 Feb 0 0 0 0 0 |> toParts |> Expect.equal ( 2001, Feb, 1, 0, 0, 0, 0 )
            , test "hours" <| \() -> fromParts 2001 Feb 1 -1 0 0 0 |> toParts |> Expect.equal ( 2001, Feb, 1, 0, 0, 0, 0 )
            , test "minutes" <| \() -> fromParts 2001 Feb 1 0 -1 0 0 |> toParts |> Expect.equal ( 2001, Feb, 1, 0, 0, 0, 0 )
            , test "seconds" <| \() -> fromParts 2001 Feb 1 0 0 -1 0 |> toParts |> Expect.equal ( 2001, Feb, 1, 0, 0, 0, 0 )
            , test "milliseconds" <| \() -> fromParts 2001 Feb 1 0 0 0 -1 |> toParts |> Expect.equal ( 2001, Feb, 1, 0, 0, 0, 0 )
            ]
        ]


test_fromCalendarDate : Test
test_fromCalendarDate =
    describe "fromCalendarDate" <|
        List.map
            (\( y, m, d ) ->
                test (toString ( y, m, d )) <|
                    \() -> fromCalendarDate y m d |> toParts |> Expect.equal ( y, m, d, 0, 0, 0, 0 )
            )
            ([ List.range 1897 1905
             , List.range 1967 1975
             , List.range 1997 2020
             ]
                |> List.concat
                |> List.concatMap calendarDatesInYear
            )


test_fromIsoString : Test
test_fromIsoString =
    let
        extendedDatePairs =
            [ ( "2008", ( 2008, Jan, 1 ) )
            , ( "2008-12", ( 2008, Dec, 1 ) )
            , ( "2008-12-31", ( 2008, Dec, 31 ) )
            , ( "2009-W01", ( 2008, Dec, 29 ) )
            , ( "2009-W01-4", ( 2009, Jan, 1 ) )
            , ( "2008-061", ( 2008, Mar, 1 ) )
            ]

        extendedTimePairs =
            [ ( "", ( 0, 0, 0, 0 ) )
            , ( "T00", ( 0, 0, 0, 0 ) )
            , ( "T20", ( 20, 0, 0, 0 ) )
            , ( "T20.75", ( 20, 45, 0, 0 ) )
            , ( "T20.7583333", ( 20, 45, 30, 0 ) )
            , ( "T20.75835", ( 20, 45, 30, 60 ) )
            , ( "T20:00", ( 20, 0, 0, 0 ) )
            , ( "T20:30", ( 20, 30, 0, 0 ) )
            , ( "T20:30.75", ( 20, 30, 45, 0 ) )
            , ( "T20:30.75833", ( 20, 30, 45, 500 ) )
            , ( "T20:30:00", ( 20, 30, 0, 0 ) )
            , ( "T20:30:40", ( 20, 30, 40, 0 ) )
            , ( "T20:30:40.007", ( 20, 30, 40, 7 ) )
            , ( "T20:30:40.067", ( 20, 30, 40, 67 ) )
            , ( "T20:30:40.5", ( 20, 30, 40, 500 ) )
            , ( "T20:30:40.56", ( 20, 30, 40, 560 ) )
            , ( "T20:30:40.567", ( 20, 30, 40, 567 ) )
            ]

        basicFromExtended : Regex -> ( String, a ) -> Maybe ( String, a )
        basicFromExtended symbol ( extended, parts ) =
            let
                basic =
                    replace All symbol (\_ -> "") extended
            in
            if basic == extended then
                Nothing
            else
                Just ( basic, parts )

        -- list of "<date>" and "<date>T<time>" formatted strings
        dateAndDateTimePairs : List ( String, Maybe DateParts )
        dateAndDateTimePairs =
            let
                datePairs =
                    extendedDatePairs ++ List.filterMap (basicFromExtended (regex "-")) extendedDatePairs

                timePairs =
                    extendedTimePairs ++ List.filterMap (basicFromExtended (regex ":")) extendedTimePairs
            in
            List.concatMap
                (\( ds, ( y, m, d ) ) ->
                    List.map
                        (\( ts, ( hh, mm, ss, ms ) ) ->
                            ( ds ++ ts, Just ( y, m, d, hh, mm, ss, ms ) )
                        )
                        timePairs
                )
                datePairs

        -- list of "<date>T<time>" formatted strings
        dateTimePairs : List ( String, Maybe DateParts )
        dateTimePairs =
            List.filter (Tuple.first >> String.contains "T") dateAndDateTimePairs

        -- create list of "<date>T<time><offset>" formatted strings
        dateTimePairsWithOffset : String -> List ( String, Maybe DateParts )
        dateTimePairsWithOffset offset =
            List.map (\( s, x ) -> ( s ++ offset, x )) dateTimePairs

        fromIsoStringTest : (Date -> DateParts) -> ( String, Maybe DateParts ) -> Test
        fromIsoStringTest toDateParts ( string, expected ) =
            test string <|
                \() ->
                    fromIsoString string |> Result.toMaybe |> Maybe.map toDateParts |> Expect.equal expected
    in
    describe "fromIsoString"
        [ describe "converts date strings in local time" <|
            List.map
                (fromIsoStringTest toParts)
                dateAndDateTimePairs
        , describe "converts date strings in UTC" <|
            List.map
                (fromIsoStringTest (toParts << toUtc))
                (List.concatMap
                    dateTimePairsWithOffset
                    [ "+00:00"
                    , "+0000"
                    , "+00"
                    , "Z"
                    ]
                )
        , describe "converts date strings in offset -07:00" <|
            List.map
                (fromIsoStringTest (toParts << toTimeOffset -420))
                (List.concatMap
                    dateTimePairsWithOffset
                    [ "-07:00"
                    , "-0700"
                    , "-07"
                    ]
                )
        , describe "converts date strings in offset +04:30" <|
            List.map
                (fromIsoStringTest (toParts << toTimeOffset 270))
                (List.concatMap
                    dateTimePairsWithOffset
                    [ "+04:30"
                    , "+0430"
                    ]
                )
        , describe "fails to convert malformed date strings" <|
            List.map
                (fromIsoStringTest toParts)
                [ ( "2008-1231", Nothing )
                , ( "200812-31", Nothing )
                , ( "2008-W014", Nothing )
                , ( "2008W01-4", Nothing )
                , ( "2008-W01-04", Nothing )
                , ( "2008-12-31T20:3040", Nothing )
                , ( "2008-12-31T2030:40", Nothing )
                , ( "2008-12-31Z", Nothing )
                , ( "2008-12Z", Nothing )
                , ( "2008Z", Nothing )
                , ( "2008-12-31+00:00", Nothing )
                , ( "2008-12-31-07:00", Nothing )
                , ( "2008-12-07:00", Nothing )
                , ( "2008-07:00", Nothing )
                ]
        , describe "fails to convert invalid dates" <|
            List.map
                (fromIsoStringTest toParts)
                [ ( "2008-00", Nothing )
                , ( "2008-13", Nothing )
                , ( "2008-01-00", Nothing )
                , ( "2007-02-29", Nothing )
                , ( "2008-02-30", Nothing )
                , ( "2008-04-31", Nothing )
                , ( "2008-01-32", Nothing )
                , ( "2008-000", Nothing )
                , ( "2007-366", Nothing )
                , ( "2008-367", Nothing )
                , ( "2008-W00", Nothing )
                , ( "2008-W53", Nothing )
                , ( "2009-W54", Nothing )
                , ( "2008-W01-0", Nothing )
                , ( "2008-W01-8", Nothing )
                , ( "2008-01-01T24:00", Nothing )
                , ( "2008-01-01T00:60", Nothing )
                , ( "2008-01-01T00:00:60", Nothing )
                , ( "2008-01-01T00:00:00+24:00", Nothing )
                , ( "2008-01-01T00:00:00+00:60", Nothing )
                ]
        ]


test_fromSpec : Test
test_fromSpec =
    describe "fromSpec"
        [ describe "local"
            [ test "midnight calendarDate" <|
                \() ->
                    (fromSpec (calendarDate 2008 Dec 31) midnight local |> toParts)
                        |> Expect.equal ( 2008, Dec, 31, 0, 0, 0, 0 )
            , test "midnight ordinalDate" <|
                \() ->
                    (fromSpec (ordinalDate 2008 366) midnight local |> toParts)
                        |> Expect.equal ( 2008, Dec, 31, 0, 0, 0, 0 )
            , test "midnight weekDate" <|
                \() ->
                    (fromSpec (weekDate 2009 1 Wed) midnight local |> toParts)
                        |> Expect.equal ( 2008, Dec, 31, 0, 0, 0, 0 )
            , test "time calendarDate" <|
                \() ->
                    (fromSpec (calendarDate 2008 Dec 31) (time 20 30 40 567) local |> toParts)
                        |> Expect.equal ( 2008, Dec, 31, 20, 30, 40, 567 )
            , test "time ordinalDate" <|
                \() ->
                    (fromSpec (ordinalDate 2008 366) (time 20 30 40 567) local |> toParts)
                        |> Expect.equal ( 2008, Dec, 31, 20, 30, 40, 567 )
            , test "time weekDate" <|
                \() ->
                    (fromSpec (weekDate 2009 1 Wed) (time 20 30 40 567) local |> toParts)
                        |> Expect.equal ( 2008, Dec, 31, 20, 30, 40, 567 )
            ]
        , describe "utc"
            [ test "midnight calendarDate" <|
                \() ->
                    (fromSpec (calendarDate 2008 Dec 31) midnight utc |> toUtc |> toParts)
                        |> Expect.equal ( 2008, Dec, 31, 0, 0, 0, 0 )
            , test "midnight ordinalDate" <|
                \() ->
                    (fromSpec (ordinalDate 2008 366) midnight utc |> toUtc |> toParts)
                        |> Expect.equal ( 2008, Dec, 31, 0, 0, 0, 0 )
            , test "midnight weekDate" <|
                \() ->
                    (fromSpec (weekDate 2009 1 Wed) midnight utc |> toUtc |> toParts)
                        |> Expect.equal ( 2008, Dec, 31, 0, 0, 0, 0 )
            , test "time calendarDate" <|
                \() ->
                    (fromSpec (calendarDate 2008 Dec 31) (time 20 30 40 567) utc |> toUtc |> toParts)
                        |> Expect.equal ( 2008, Dec, 31, 20, 30, 40, 567 )
            , test "time ordinalDate" <|
                \() ->
                    (fromSpec (ordinalDate 2008 366) (time 20 30 40 567) utc |> toUtc |> toParts)
                        |> Expect.equal ( 2008, Dec, 31, 20, 30, 40, 567 )
            , test "time weekDate" <|
                \() ->
                    (fromSpec (weekDate 2009 1 Wed) (time 20 30 40 567) utc |> toUtc |> toParts)
                        |> Expect.equal ( 2008, Dec, 31, 20, 30, 40, 567 )
            ]
        , describe "offset"
            [ test "midnight calendarDate" <|
                \() ->
                    (fromSpec (calendarDate 2008 Dec 31) midnight (offset 60) |> toTimeOffset 60 |> toParts)
                        |> Expect.equal ( 2008, Dec, 31, 0, 0, 0, 0 )
            , test "midnight ordinalDate" <|
                \() ->
                    (fromSpec (ordinalDate 2008 366) midnight (offset 60) |> toTimeOffset 60 |> toParts)
                        |> Expect.equal ( 2008, Dec, 31, 0, 0, 0, 0 )
            , test "midnight weekDate" <|
                \() ->
                    (fromSpec (weekDate 2009 1 Wed) midnight (offset 60) |> toTimeOffset 60 |> toParts)
                        |> Expect.equal ( 2008, Dec, 31, 0, 0, 0, 0 )
            , test "time calendarDate" <|
                \() ->
                    (fromSpec (calendarDate 2008 Dec 31) (time 20 30 40 567) (offset 60) |> toTimeOffset 60 |> toParts)
                        |> Expect.equal ( 2008, Dec, 31, 20, 30, 40, 567 )
            , test "time ordinalDate" <|
                \() ->
                    (fromSpec (ordinalDate 2008 366) (time 20 30 40 567) (offset 60) |> toTimeOffset 60 |> toParts)
                        |> Expect.equal ( 2008, Dec, 31, 20, 30, 40, 567 )
            , test "time weekDate" <|
                \() ->
                    (fromSpec (weekDate 2009 1 Wed) (time 20 30 40 567) (offset 60) |> toTimeOffset 60 |> toParts)
                        |> Expect.equal ( 2008, Dec, 31, 20, 30, 40, 567 )
            ]
        ]
