module Date.Convert exposing (..)

import Date exposing (Date, toTime, year, month, day)
import Date.Fact exposing (msPerDay)
import Date.Internal exposing (rataDieFromYMD)
import Date.Extract exposing (fractionalDay)


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
