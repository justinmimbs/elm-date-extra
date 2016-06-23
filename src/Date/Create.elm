module Date.Create exposing (
  TimeZone(..),
  fromPartsWithTimeZone,
  fromParts,
  fromYMD,
  fromISOString
  )

import Date exposing (Date, Month)
import Date.Facts exposing (msPerMinute)
import Date.Extract exposing (offsetFromUTC)
import Date.Internal.Core exposing (unixTimeFromParts)
import Date.Internal.Parse exposing (offsetTimeFromISOString)

type TimeZone
  = UTC
  | Offset Int
  | Local


fromTime : Int -> Date
fromTime =
  Date.fromTime << toFloat


fromOffsetTime : (Maybe Int, Int) -> Date
fromOffsetTime (offset, time) =
  case offset of
    Just minutes ->
      fromTime <| time - msPerMinute * minutes
    Nothing ->
      let
        localOffset = offsetFromUTC <| fromTime time
      in
        fromTime <| time - msPerMinute * localOffset


fromPartsWithTimeZone : TimeZone -> Int -> Month -> Int -> Int -> Int -> Int -> Int -> Date
fromPartsWithTimeZone tz y m d hh mm ss ms =
  let
    time = unixTimeFromParts y m d hh mm ss ms
    offset =
      -- TODO pull this out into offsetFromTimeZone
      case tz of
        UTC            -> Just 0
        Offset minutes -> Just minutes
        Local          -> Nothing
  in
    fromOffsetTime (offset, time)


fromParts : Int -> Month -> Int -> Int -> Int -> Int -> Int -> Date
fromParts =
  fromPartsWithTimeZone Local


fromYMD : Int -> Month -> Int -> Date
fromYMD y m d =
  fromParts y m d 0 0 0 0


fromISOString : String -> Maybe Date
fromISOString s =
  offsetTimeFromISOString s |> Maybe.map fromOffsetTime


--fromRataDie : Int -> Date
--fromRataDie rd =
