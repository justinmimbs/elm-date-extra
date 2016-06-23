module Date.Internal.Parse exposing (
  offsetTimeFromISOString
  )

import Regex exposing (Regex, HowMany(AtMost), regex)
import String
import Date exposing (Date, Month)
import Date.Facts exposing (monthFromMonthNumber, msPerSecond, msPerMinute, msPerHour)
import Date.Internal.Core exposing (unixTimeFromParts)


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
    date =
      "^(?:(\\d{4})(?:(\\-)?(\\d\\d))?(?:\\2(\\d\\d))?)"
      --   1          2     3               4
      --   yyyy             mm              dd
    time =
      "(?:T(\\d\\d)(?:(\\:)?(\\d\\d)(?:\\6(\\d\\d))?)?(\\.\\d+)?(?:(Z)|(?:([+\\-])(\\d\\d)(?:\\:?(\\d\\d))?))?)?$"
      --   5          6     7             8           9            10     11      12             13
      --   hh               mm            ss          .f           Z      +/-     hh             mm
  in
    regex (date ++ time)


offsetTimeFromISOString : String -> Maybe (Maybe Int, Int)
offsetTimeFromISOString s =
  (Regex.find (AtMost 1) isoDateRegex s |> List.head |> Maybe.map .submatches) >>= offsetTimeFromMatches


offsetTimeFromMatches : List (Maybe String) -> Maybe (Maybe Int, Int)
offsetTimeFromMatches matches =
  case matches of
    [dateY, _, dateM, dateD, timeH, _, timeM, timeS, timeF, tzZ, tzSign, tzH, tzM] ->
      let
        y = (dateY >>= stringToInt) ? 1
        m = (dateM >>= stringToInt) ? 1 |> monthFromMonthNumber
        d = (dateD >>= stringToInt) ? 1
        ms = msFromMatches timeH timeM timeS timeF
        offset = offsetFromMatches tzZ tzSign tzH tzM
      in
        Just <| (offset, unixTimeFromParts y m d 0 0 0 ms)

    _ ->
      Nothing


msFromMatches : Maybe String -> Maybe String -> Maybe String -> Maybe String -> Int
msFromMatches timeH timeM timeS timeF =
  let
    f = (timeF >>= stringToFloat) ? 0.0
    (hh, mm, ss) =
      case List.map (\m -> m >>= stringToFloat) [timeH, timeM, timeS] of
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
offsetFromMatches tzZ tzSign tzH tzM =
  case (tzZ, tzSign) of
    (Just "Z", Nothing) ->
      Just 0

    (Nothing, Just sign) ->
      let
        hh = (tzH >>= stringToInt) ? 0
        mm = (tzM >>= stringToInt) ? 0
      in
        Just <| (if sign == "+" then 1 else -1) * (hh * 60 + mm)

    _ ->
      Nothing
