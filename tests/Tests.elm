module Tests exposing (..)

import Date exposing (Date, Month)
import Date.Extra as Date
import Expect
import Test exposing (Test, describe, test)
import Test.Create as Create
import Test.Extract as Extract
import Test.Format as Format
import Test.Math as Math
import Utilities exposing (calendarDatesInYear)


tests : Test
tests =
    Test.concat
        [ Create.tests
        , Extract.tests
        , Format.tests
        , Math.tests
        ]


test_RataDie : Test
test_RataDie =
    describe "RataDie"
        [ describe "toRataDie" <|
            List.indexedMap
                (\i ( y, m, d ) ->
                    test (toString ( y, m, d )) <|
                        \() -> Date.fromCalendarDate y m d |> Date.toRataDie |> Expect.equal (730120 + i)
                )
                ([ 2000, 2001 ] |> List.concatMap calendarDatesInYear)
        , describe "fromRataDie" <|
            List.indexedMap
                (\i calendarDate ->
                    test (toString calendarDate) <|
                        \() -> (730120 + i) |> Date.fromRataDie |> toCalendarDate |> Expect.equal calendarDate
                )
                ([ 2000, 2001 ] |> List.concatMap calendarDatesInYear)
        ]


toCalendarDate : Date -> ( Int, Month, Int )
toCalendarDate date =
    ( Date.year date, Date.month date, Date.day date )
