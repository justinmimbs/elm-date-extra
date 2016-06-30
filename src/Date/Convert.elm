module Date.Convert exposing (
  toFormattedString,
  toTimestamp,
  toUtcFormattedString,
  toUtcTimestamp,
  toJulianDate
  )

{-| Convert dates to other types.

# Formatted Strings
@docs toFormattedString, toTimestamp, toUtcFormattedString, toUtcTimestamp

# Julian Dates
@docs toJulianDate
-}


import Date exposing (Date, fromTime, toTime, year, month, day, hour, minute, second, millisecond)
import Date.Facts exposing (msPerDay)
import Date.Internal.Core exposing (unixTimeFromParts)
import Date.Internal.Format


{-|
-}
toFormattedString : String -> Date -> String
toFormattedString =
  Date.Internal.Format.toFormattedString False


{-| Convenience function for formatting a date to ISO 8601 extended date-time
format with local time zone offset.

    -- assuming a local offset of UTC-04:00

    Date.toTimestamp (Date.fromParts 2007 Mar 15 11 55 0 0)
    -- "2007-03-15T11:55:00.000-04:00"
-}
toTimestamp: Date -> String
toTimestamp =
  toFormattedString "yyyy-mm-ddTHH:MM:ss.lP"


{-|
-}
toUtcFormattedString : String -> Date -> String
toUtcFormattedString =
  Date.Internal.Format.toFormattedString True


{-| Convenience function for formatting a date to ISO 8601 extended date-time
format as UTC time.

    -- assuming a local offset of UTC-04:00

    Date.toTimestamp (Date.fromParts 2007 Mar 15 11 55 0 0)
    -- "2007-03-15T15:55:00.000Z"
-}
toUtcTimestamp: Date -> String
toUtcTimestamp =
  toUtcFormattedString "yyyy-mm-ddTHH:MM:ss.lZ"


{-| Convert a `Date` to a [Julian Date](https://en.wikipedia.org/wiki/Julian_day).
Julian Dates always represent UTC time.
-}
toJulianDate : Date -> Float
toJulianDate date =
  toTime date / toFloat msPerDay + 2440587.5