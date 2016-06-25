module Date.Convert exposing (
  toFormattedString,
  toTimestamp,
  toJulianDate
  )

import Date exposing (Date, fromTime, toTime, year, month, day, hour, minute, second, millisecond)
import Date.Facts exposing (msPerDay)
import Date.Internal.Core exposing (unixTimeFromParts)
import Date.Internal.Format


toFormattedString : String -> Date -> String
toFormattedString =
  Date.Internal.Format.toFormattedString


toTimestamp: Date -> String
toTimestamp =
  toFormattedString "yyyy-mm-ddTHH:MM:ss.lP"


toJulianDate : Date -> Float
toJulianDate date =
  toTime date / toFloat msPerDay + 2440587.5