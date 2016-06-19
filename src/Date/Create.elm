module Date.Create exposing (
  Timezone(..),
  fromTimezoneSpec,
  fromSpec,
  fromYMD
  )

import Date exposing (Date, Month)
import Date.Fact exposing (msPerMinute)
import Date.Internal exposing (unixTimeFromSpec)
import Date.Extract exposing (timezoneOffset)

type Timezone
  = UTC
  | Offset Int
  | Local


fromTimezoneSpec : Timezone -> Int -> Month -> Int -> Int -> Int -> Int -> Int -> Date
fromTimezoneSpec tz y m d hh mm ss ms =
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
          date = fromTimezoneSpec UTC y m d hh mm ss ms
        in
          Date.fromTime <| toFloat <| unixTime + (timezoneOffset date * msPerMinute)


fromSpec : Int -> Month -> Int -> Int -> Int -> Int -> Int -> Date
fromSpec =
  fromTimezoneSpec Local


fromYMD : Int -> Month -> Int -> Date
fromYMD y m d =
  fromSpec y m d 0 0 0 0
