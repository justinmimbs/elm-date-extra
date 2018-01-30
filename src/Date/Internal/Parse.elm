module Date.Internal.Parse exposing (offsetTimeFromIsoString)

import Date exposing (Date, Month)
import Date.Extra.Facts exposing (monthFromMonthNumber, msPerHour, msPerMinute, msPerSecond)
import Date.Internal.Core exposing (unixTimeFromCalendarDate, unixTimeFromOrdinalDate, unixTimeFromWeekDate)
import Regex exposing (HowMany(AtMost), Regex, regex)


stringToInt : String -> Maybe Int
stringToInt =
    String.toInt >> Result.toMaybe


stringToFloat : String -> Maybe Float
stringToFloat =
    String.toFloat >> Result.toMaybe


isoDateRegex : Regex
isoDateRegex =
    let
        year =
            --yyyy
            --1
            "(\\d{4})"

        cal =
            --      mm            dd
            --2     3             4
            "(\\-)?(\\d{2})(?:\\2(\\d{2}))?"

        week =
            --       ww            d
            --5      6             7
            "(\\-)?W(\\d{2})(?:\\5(\\d))?"

        ord =
            --    ddd
            --    8
            "\\-?(\\d{3})"

        time =
            -- hh               mm             ss          .f           Z      +/-     hh             mm
            -- 9          10    11             12          13           14     15      16             17
            "T(\\d{2})(?:(\\:)?(\\d{2})(?:\\10(\\d{2}))?)?(\\.\\d+)?(?:(Z)|(?:([+\\-])(\\d{2})(?:\\:?(\\d{2}))?))?"
    in
    regex <| "^" ++ year ++ "(?:" ++ cal ++ "|" ++ week ++ "|" ++ ord ++ ")?" ++ "(?:" ++ time ++ ")?$"


offsetTimeFromIsoString : String -> Maybe ( Maybe Int, Int )
offsetTimeFromIsoString s =
    Regex.find (AtMost 1) isoDateRegex s
        |> List.head
        |> Maybe.map .submatches
        |> Maybe.andThen offsetTimeFromMatches


offsetTimeFromMatches : List (Maybe String) -> Maybe ( Maybe Int, Int )
offsetTimeFromMatches matches =
    case matches of
        [ Just yyyy, _, calMM, calDD, _, weekWW, weekD, ordDDD, timeHH, _, timeMM, timeSS, timeF, tzZ, tzSign, tzHH, tzMM ] ->
            let
                dateMS =
                    unixTimeFromMatches yyyy calMM calDD weekWW weekD ordDDD

                timeMS =
                    msFromMatches timeHH timeMM timeSS timeF

                offset =
                    offsetFromMatches tzZ tzSign tzHH tzMM
            in
            Just ( offset, dateMS + timeMS )

        _ ->
            Nothing


unixTimeFromMatches : String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Int
unixTimeFromMatches yyyy calMM calDD weekWW weekD ordDDD =
    let
        y =
            stringToInt yyyy |> Maybe.withDefault 1
    in
    case ( calMM, weekWW ) of
        ( Just _, Nothing ) ->
            unixTimeFromCalendarDate
                y
                (calMM |> Maybe.andThen stringToInt |> Maybe.withDefault 1 |> monthFromMonthNumber)
                (calDD |> Maybe.andThen stringToInt |> Maybe.withDefault 1)

        ( Nothing, Just _ ) ->
            unixTimeFromWeekDate
                y
                (weekWW |> Maybe.andThen stringToInt |> Maybe.withDefault 1)
                (weekD |> Maybe.andThen stringToInt |> Maybe.withDefault 1)

        _ ->
            unixTimeFromOrdinalDate
                y
                ((ordDDD |> Maybe.andThen stringToInt) |> Maybe.withDefault 1)


msFromMatches : Maybe String -> Maybe String -> Maybe String -> Maybe String -> Int
msFromMatches timeHH timeMM timeSS timeF =
    let
        fractional =
            timeF |> Maybe.andThen stringToFloat |> Maybe.withDefault 0.0

        ( hh, mm, ss ) =
            case [ timeHH, timeMM, timeSS ] |> List.map (Maybe.andThen stringToFloat) of
                [ Just hh, Just mm, Just ss ] ->
                    ( hh, mm, ss + fractional )

                [ Just hh, Just mm, Nothing ] ->
                    ( hh, mm + fractional, 0.0 )

                [ Just hh, Nothing, Nothing ] ->
                    ( hh + fractional, 0.0, 0.0 )

                _ ->
                    ( 0.0, 0.0, 0.0 )
    in
    hh
        * toFloat msPerHour
        + mm
        * toFloat msPerMinute
        + ss
        * toFloat msPerSecond
        |> round


offsetFromMatches : Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe Int
offsetFromMatches tzZ tzSign tzHH tzMM =
    case ( tzZ, tzSign ) of
        ( Just "Z", Nothing ) ->
            Just 0

        ( Nothing, Just sign ) ->
            let
                hh =
                    tzHH |> Maybe.andThen stringToInt |> Maybe.withDefault 0

                mm =
                    tzMM |> Maybe.andThen stringToInt |> Maybe.withDefault 0
            in
            Just <|
                (if sign == "+" then
                    1
                 else
                    -1
                )
                    * (hh * 60 + mm)

        _ ->
            Nothing
