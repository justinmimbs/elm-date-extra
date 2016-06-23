module Date.Convert exposing (
  toFormattedString,
  toRataDie,
  toRataDieMoment,
  toJulianDate,
  toJulianDayNumber
  )

import Date exposing (Date, toTime, year, month, day)
import Date.Facts exposing (msPerDay)
import Date.Extract exposing (fractionalDay)
import Date.Internal.Core exposing (rataDieFromYMD)
import Date.Internal.Format


toFormattedString : String -> Date -> String
toFormattedString =
  Date.Internal.Format.toFormattedString


toRataDie : Date -> Int
toRataDie date =
  rataDieFromYMD (year date) (month date) (day date)


toRataDieMoment : Date -> Float
toRataDieMoment date =
  toFloat (toRataDie date) + fractionalDay date


toJulianDate : Date -> Float
toJulianDate date =
  toTime date / toFloat msPerDay + 2440587.5


toJulianDayNumber : Date -> Int
toJulianDayNumber date =
  toJulianDate date |> floor
