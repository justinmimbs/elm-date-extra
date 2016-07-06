module Date.Internal.Parse exposing (
  offsetTimeFromIsoString
  )

import Regex exposing (Regex, HowMany(AtMost), regex)
import String
import Date exposing (Date, Month)
import Date.Facts exposing (monthFromMonthNumber, msPerSecond, msPerMinute, msPerHour)
import Date.Internal.Core exposing (unixTimeFromCalendarDate, unixTimeFromWeekDate, unixTimeFromOrdinalDate)


(>>=) : Maybe a -> (a -> Maybe b) -> Maybe b
(>>=) =
  Maybe.andThen

infixl 1 >>=


(?) : Maybe a -> a -> a
(?) =
  flip Maybe.withDefault

infixl 9 ?


stringToInt : String -> Maybe Int
stringToInt s =
  String.toInt s |> Result.toMaybe


stringToFloat : String -> Maybe Float
stringToFloat s =
  String.toFloat s |> Result.toMaybe


isoDateRegex : Regex
isoDateRegex =
  let
    year =
      "(\\d{4})"
    -- 1
    -- yyyy
    cal =
      "(\\-)?(\\d{2})(?:\\2(\\d{2}))?"
    -- 2     3             4
    --       mm            dd
    week =
      "(\\-)?W(\\d{2})(?:\\5(\\d))?"
    -- 5      6             7
    --        ww            d
    ord =
      "\\-?(\\d{3})"
    --     8
    --     ddd
    time =
      "T(\\d{2})(?:(\\:)?(\\d{2})(?:\\10(\\d{2}))?)?(\\.\\d+)?(?:(Z)|(?:([+\\-])(\\d{2})(?:\\:?(\\d{2}))?))?"
    --  9          10    11             12          13           14     15      16             17
    --  hh               mm             ss          .f           Z      +/-     hh             mm
  in
    regex <| "^" ++ year ++ "(?:" ++ cal ++ "|" ++ week ++ "|" ++ ord  ++ ")?" ++ "(?:" ++ time ++ ")?$"


offsetTimeFromIsoString : String -> Maybe (Maybe Int, Int)
offsetTimeFromIsoString s =
  (Regex.find (AtMost 1) isoDateRegex s |> List.head |> Maybe.map .submatches) >>= offsetTimeFromMatches


offsetTimeFromMatches : List (Maybe String) -> Maybe (Maybe Int, Int)
offsetTimeFromMatches matches =
  case matches of
    [Just yyyy, _, calMM, calDD, _, weekWW, weekD, ordDDD, timeHH, _, timeMM, timeSS, timeF, tzZ, tzSign, tzHH, tzMM] ->
      let
        dateMS = unixTimeFromMatches yyyy calMM calDD weekWW weekD ordDDD
        timeMS = msFromMatches timeHH timeMM timeSS timeF
        offset = offsetFromMatches tzZ tzSign tzHH tzMM
      in
        Just <| (offset, dateMS + timeMS)
    _ ->
      Nothing


unixTimeFromMatches : String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Int
unixTimeFromMatches yyyy calMM calDD weekWW weekD ordDDD =
  let
    y = stringToInt yyyy ? 1
  in
    case (calMM, weekWW) of
      (Just _, Nothing) ->
        unixTimeFromCalendarDate
          y
          ((calMM >>= stringToInt) ? 1 |> monthFromMonthNumber)
          ((calDD >>= stringToInt) ? 1)

      (Nothing, Just _) ->
        unixTimeFromWeekDate
          y
          ((weekWW >>= stringToInt) ? 1)
          ((weekD >>= stringToInt) ? 1)

      _ ->
        unixTimeFromOrdinalDate
          y
          ((ordDDD >>= stringToInt) ? 1)


msFromMatches : Maybe String -> Maybe String -> Maybe String -> Maybe String -> Int
msFromMatches timeHH timeMM timeSS timeF =
  let
    f = (timeF >>= stringToFloat) ? 0.0
    (hh, mm, ss) =
      case List.map (\m -> m >>= stringToFloat) [timeHH, timeMM, timeSS] of
        [Just hh, Just mm, Just ss] -> (hh, mm, ss + f)
        [Just hh, Just mm, Nothing] -> (hh, mm + f, 0.0)
        [Just hh, Nothing, Nothing] -> (hh + f, 0.0, 0.0)
        _ -> (0.0, 0.0, 0.0)
  in
    hh * toFloat msPerHour
    + mm * toFloat msPerMinute
    + ss * toFloat msPerSecond
    |> round


offsetFromMatches : Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe Int
offsetFromMatches tzZ tzSign tzHH tzMM =
  case (tzZ, tzSign) of
    (Just "Z", Nothing) ->
      Just 0

    (Nothing, Just sign) ->
      let
        hh = (tzHH >>= stringToInt) ? 0
        mm = (tzMM >>= stringToInt) ? 0
      in
        Just <| (if sign == "+" then 1 else -1) * (hh * 60 + mm)

    _ ->
      Nothing
