module Date.Create exposing (
  fromParts,
  fromCalendarDate,
  fromIsoString,
  TimeZone, utc, offset, local,
  TimeSpec, noTime, timeAt,
  DateSpec, calendarDate, ordinalDate, weekDate,
  fromSpec,
  fromJulianDate
  )

import Date exposing (Date, Month)
import Date.Facts exposing (msPerMinute, msPerDay)
import Date.Extract exposing (offsetFromUtc)
import Date.Internal.Core exposing (unixTimeFromParts, unixTimeFromCalendarDate, unixTimeFromWeekDate, msFromTimeParts)
import Date.Internal.Parse exposing (offsetTimeFromIsoString)


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
        localOffset = offsetFromUtc <| fromTime time
      in
        fromTime <| time - msPerMinute * localOffset


fromParts : Int -> Month -> Int -> Int -> Int -> Int -> Int -> Date
fromParts y m d hh mm ss ms =
  fromOffsetTime (Nothing, unixTimeFromParts y m d hh mm ss ms)


fromCalendarDate : Int -> Month -> Int -> Date
fromCalendarDate y m d =
  fromOffsetTime (Nothing, unixTimeFromCalendarDate y m d)


fromIsoString : String -> Maybe Date
fromIsoString s =
  Maybe.map fromOffsetTime <| offsetTimeFromIsoString s


type TimeZone
  = Offset (Maybe Int)


utc : TimeZone
utc =
  Offset (Just 0)


offset : Int -> TimeZone
offset minutes =
  Offset (Just minutes)


local : TimeZone
local =
  Offset Nothing


type TimeSpec
  = TimeMS Int

noTime : TimeSpec
noTime =
  TimeMS 0


timeAt : Int -> Int -> Int -> Int -> TimeSpec
timeAt hh mm ss ms =
  TimeMS <| msFromTimeParts hh mm ss ms


type DateSpec
  = DateMS Int


calendarDate : Int -> Month -> Int -> DateSpec
calendarDate y m d =
  DateMS <| unixTimeFromCalendarDate y m d


ordinalDate : Int -> Int -> DateSpec
ordinalDate y d =
  DateMS <| unixTimeFromCalendarDate y Date.Jan d


weekDate : Int -> Int -> Int -> DateSpec
weekDate y w d =
  DateMS <| unixTimeFromWeekDate y w d


fromSpec : TimeZone -> TimeSpec -> DateSpec -> Date
fromSpec (Offset o) (TimeMS t) (DateMS d) =
  fromOffsetTime (o, d + t)


fromJulianDate : Float -> Date
fromJulianDate j =
  Date.fromTime <| (j - 2440587.5) * toFloat msPerDay
