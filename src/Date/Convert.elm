module Date.Convert exposing (
  toFormattedString,
  toTimestamp,
  toUtcFormattedString,
  toUtcTimestamp,
  toJulianDate
  )

import Date exposing (Date, fromTime, toTime, year, month, day, hour, minute, second, millisecond)
import Date.Facts exposing (msPerDay)
import Date.Internal.Core exposing (unixTimeFromParts)
import Date.Internal.Format


toFormattedString : String -> Date -> String
toFormattedString =
  Date.Internal.Format.toFormattedString False


toTimestamp: Date -> String
toTimestamp =
  toFormattedString "yyyy-mm-ddTHH:MM:ss.lP"


toUtcFormattedString : String -> Date -> String
toUtcFormattedString =
  Date.Internal.Format.toFormattedString True


toUtcTimestamp: Date -> String
toUtcTimestamp =
  toUtcFormattedString "yyyy-mm-ddTHH:MM:ss.lZ"


toJulianDate : Date -> Float
toJulianDate date =
  toTime date / toFloat msPerDay + 2440587.5