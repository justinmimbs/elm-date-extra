module Date.Create exposing (
  fromParts,
  fromCalendarDate,
  fromIsoString,
  TimeZone, utc, offset, local,
  TimeSpec, noTime, atTime,
  DateSpec, calendarDate, ordinalDate, weekDate,
  fromSpec,
  fromJulianDate
  )

{-| A `Date` represents a moment in time, encoded by two essential pieces of
information: the number of milliseconds since 1 January 1970 UTC, and the
offset between UTC and the current machine's local time. Extractions of a
`Date` (e.g. `Date.month`, `Date.hour`) return representations of the date in
local time, while `Date.toTime` returns the UTC representation.

# Common Constructors
@docs fromParts, fromCalendarDate, fromIsoString

# Detailed Specification
In some cases you may want to specify a date with a time zone offset or from
week-date or ordinal-date parts. The `fromSpec` function provides a way to
do this.
@docs fromSpec

## TimeZone
@docs TimeZone, utc, offset, local

## TimeSpec
@docs TimeSpec, noTime, atTime

## DateSpec
@docs DateSpec, calendarDate, weekDate, ordinalDate

# Julian Dates
@docs fromJulianDate
-}


import Date exposing (Date, Month)
import Date.Facts exposing (msPerMinute, msPerDay)
import Date.Extract exposing (offsetFromUtc)
import Date.Internal.Core exposing (unixTimeFromParts, unixTimeFromCalendarDate, unixTimeFromWeekDate, unixTimeFromOrdinalDate, msFromTimeParts)
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
      -- find the local offset
      let
        offset0 = offsetFromUtc <| fromTime time
        date1 = fromTime <| time - msPerMinute * offset0
        offset1 = offsetFromUtc <| date1
      in
        if offset0 == offset1 then
          date1
        else
          -- local offset has changed within `offset0` time period (e.g. DST switch)
          let
            date2 = fromTime <| time - msPerMinute * offset1
            offset2 = offsetFromUtc <| date2
          in
            if offset1 == offset2 then
              date2
            else
              -- `time` is within the lost hour of a local switch
              date1


{-| Create a `Date` from the following parts, given in local time:

- year
- month
- day
- hour
- minute
- second
- millisecond


    import Date exposing (Month(..))
    import Date.Create as Date

    Date.fromParts 1999 Dec 31 23 59 0 0
    -- 31 December 1999, 11:59 p.m., local time

The values of the parts are not checked to ensure a valid date representation,
nor are they clamped to valid range; instead, providing values outside a valid
range results in underflow or overflow.

    Date.fromParts 2007 Feb 29 0 0 0 0
    -- 1 March 2007
-}
fromParts : Int -> Month -> Int -> Int -> Int -> Int -> Int -> Date
fromParts y m d hh mm ss ms =
  fromOffsetTime (Nothing, unixTimeFromParts y m d hh mm ss ms)


{-| Convenience function for creating a `Date` from only the year, month, and
day parts.

    Date.fromCalendarDate 2000 Jan 1
-}
fromCalendarDate : Int -> Month -> Int -> Date
fromCalendarDate y m d =
  fromOffsetTime (Nothing, unixTimeFromCalendarDate y m d)


{-| Attempt to create a `Date` from a string representing a date in [ISO 8601](
https://en.wikipedia.org/wiki/ISO_8601) format.

    Date.fromIsoString "2000-01-01"
    -- Just <1 January 2000, local time>

    Date.fromIsoString "2009-W01-1T00Z"
    -- Just <29 December 2008, UTC>

    Date.fromIsoString "2016-218T20:00-03:00"
    -- Just <5 August 2016, 23:00, UTC>

    Date.fromIsoString "1/1/2001"
    -- Nothing

When a `Date` is created with a specified time zone offset (e.g. `"-03:00"`),
its extractions still reflect the current machine's local time, and
`Date.toTime` still reflects its UTC time.
-}
fromIsoString : String -> Maybe Date
fromIsoString s =
  Maybe.map fromOffsetTime <| offsetTimeFromIsoString s


{-| Represents a time zone.
-}
type TimeZone
  = Offset (Maybe Int)


{-| UTC time zone.
-}
utc : TimeZone
utc =
  Offset (Just 0)


{-| Create a `TimeZone` with a specific offset from UTC time, given in minutes.
-}
offset : Int -> TimeZone
offset minutes =
  Offset (Just minutes)


{-| Do not specify a time zone, but use the local offset instead.
-}
local : TimeZone
local =
  Offset Nothing


{-| Represents a time of day.
-}
type TimeSpec
  = TimeMS Int


{-| Do not specify a time of day (i.e. midnight).
-}
noTime : TimeSpec
noTime =
  TimeMS 0


{-| Create a `TimeSpec` from time parts (hour, minute, second, millisecond).
-}
atTime : Int -> Int -> Int -> Int -> TimeSpec
atTime hh mm ss ms =
  TimeMS <| msFromTimeParts hh mm ss ms


{-| Represents a day.
-}
type DateSpec
  = DateMS Int


{-| Create a `DateSpec` from calendar-date parts (year, month, day).
-}
calendarDate : Int -> Month -> Int -> DateSpec
calendarDate y m d =
  DateMS <| unixTimeFromCalendarDate y m d


{-| Create a `DateSpec` from ordinal-date parts (year, ordinalDay).
-}
ordinalDate : Int -> Int -> DateSpec
ordinalDate y d =
  DateMS <| unixTimeFromOrdinalDate y d


{-| Create a `DateSpec` from week-date parts (weekYear, weekNumber, weekdayNumber).
-}
weekDate : Int -> Int -> Int -> DateSpec
weekDate y w d =
  DateMS <| unixTimeFromWeekDate y w d


{-| Create a `Date` from a specified time zone, time of day, and day.

    Date.fromSpec
      local
      noTime
      (calendarDate 2000 Jan 1)
    -- 1 January 2000, local time

    Date.fromSpec
      utc
      noTime
      (weekDate 2009 1 1)
    -- 29 December 2008, UTC

    Date.fromSpec
      (offset -180)
      (atTime 20 0 0 0)
      (ordinalDate 2016 218)
    -- 5 August 2016, 23:00, UTC

When a `Date` is created with a specified time zone offset (e.g. `offset -180`),
its extractions still reflect the current machine's local time, and
`Date.toTime` still reflects its UTC time.
-}
fromSpec : TimeZone -> TimeSpec -> DateSpec -> Date
fromSpec (Offset o) (TimeMS t) (DateMS d) =
  fromOffsetTime (o, d + t)


{-| Create a `Date` from a [Julian Date](https://en.wikipedia.org/wiki/Julian_day).
Julian Dates always represent UTC time.
-}
fromJulianDate : Float -> Date
fromJulianDate j =
  fromTime <| round <| (j - 2440587.5) * toFloat msPerDay
