module Date.Create exposing (
  TimeZone(..),
  fromPartsWithTimeZone,
  fromParts,
  fromYMD
  )

import Date exposing (Date, Month)
import Date.Fact exposing (msPerMinute)
import Date.Internal exposing (unixTimeFromSpec)
import Date.Extract exposing (timeZoneOffset)

type TimeZone
  = UTC
  | Offset Int
  | Local


fromPartsWithTimeZone : TimeZone -> Int -> Month -> Int -> Int -> Int -> Int -> Int -> Date
fromPartsWithTimeZone tz y m d hh mm ss ms =
  let
    unixTime = unixTimeFromSpec y m d hh mm ss ms
  in
    case tz of
      UTC ->
        Date.fromTime <| toFloat unixTime

      Offset minutes ->
        Date.fromTime <| toFloat <| unixTime - minutes * msPerMinute

      Local ->
        let
          date = fromPartsWithTimeZone UTC y m d hh mm ss ms
        in
          Date.fromTime <| toFloat <| unixTime + (timeZoneOffset date * msPerMinute)


fromParts : Int -> Month -> Int -> Int -> Int -> Int -> Int -> Date
fromParts =
  fromPartsWithTimeZone Local


fromYMD : Int -> Month -> Int -> Date
fromYMD y m d =
  fromParts y m d 0 0 0 0
