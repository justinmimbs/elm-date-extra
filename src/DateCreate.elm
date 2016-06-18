module DateCreate exposing (..)

import Date exposing (Date, Month)
import DateFact exposing (msPerMinute)
import DateInternal exposing (unixTimeFromSpec)
import DateExtract exposing (timezoneOffset)

type Timezone
  = UTC
  | Offset Int
  | Local


dateFromTimezoneSpec : Timezone -> Int -> Month -> Int -> Int -> Int -> Int -> Int -> Date
dateFromTimezoneSpec tz y m d hh mm ss ms =
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
          date = dateFromTimezoneSpec UTC y m d hh mm ss ms
        in
          Date.fromTime <| toFloat <| unixTime + (timezoneOffset date * msPerMinute)


dateFromSpec : Int -> Month -> Int -> Int -> Int -> Int -> Int -> Date
dateFromSpec =
  dateFromTimezoneSpec Local


dateFromYMD : Int -> Month -> Int -> Date
dateFromYMD y m d =
  dateFromSpec y m d 0 0 0 0
