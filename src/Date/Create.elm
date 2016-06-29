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
information: the number of milliseconds since January 1, 1970 UTC, and the
offset between UTC and your local time. Extractions of a `Date` (e.g.
`Date.month`, `Date.hour`) return representations of the date in local time,
while `Date.toTime` returns the UTC representation.

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

    -- 31 December 1999, 11:59 p.m., local time
    date = Date.fromParts 1999 Dec 31 23 59 0 0

Values of the number parts are not checked to ensure a valid date
representation; providing values outside of a valid range results in underflow
or overflow, rather than clamping.

    -- 1 March 2007
    date = Date.fromParts 2007 Feb 29 0 0 0 0
-}
fromParts : Int -> Month -> Int -> Int -> Int -> Int -> Int -> Date
fromParts y m d hh mm ss ms =
  fromOffsetTime (Nothing, unixTimeFromParts y m d hh mm ss ms)


{-| Convenience function for creating a `Date` from only the year, month, and
day parts.

    date = Date.fromCalendarDate 2000 Jan 1
-}
fromCalendarDate : Int -> Month -> Int -> Date
fromCalendarDate y m d =
  fromOffsetTime (Nothing, unixTimeFromCalendarDate y m d)


{-| Attempt to create a `Date` from a string formatted as a valid [ISO 8601](
https://en.wikipedia.org/wiki/ISO_8601) date or date-time representation.

    -- Just <1 January 2000, local time>
    date = Date.fromIsoString "2000-01-01"

    -- Just <5 August 2016, 23:00, UTC>
    opening = Date.fromIsoString "2016-08-05T20:00-03:00"

    -- Nothing
    notIso = Date.fromIsoString "1/1/2001"

When a `Date` is created with a specified time zone offset (e.g. `"-03:00"`),
its extractions still represent local time, and `Date.toTime` still represents
its UTC time.
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
  DateMS <| unixTimeFromCalendarDate y Date.Jan d


{-| Create a `DateSpec` from week-date parts (weekYear, weekNumber, weekdayNumber).
-}
weekDate : Int -> Int -> Int -> DateSpec
weekDate y w d =
  DateMS <| unixTimeFromWeekDate y w d


{-| Create a `Date` from a specified time zone, time of day, and day.

    -- 1 January 2000, UTC
    date = Date.fromSpec
      utc
      noTime
      (calendarDate 2000 Jan 1)

    -- 5 August 2016, 20:00, UTC-03:00
    opening = Date.fromSpec
      (offset -180)
      (atTime 20 0 0 0)
      (calendarDate 2016 Aug 5)

    -- 29 December 2008, local time
    week1 = Date.fromSpec
      local
      noTime
      (weekDate 2009 1 1)

When a `Date` is created with a specified time zone offset (e.g. `offset -180`),
its extractions still represent local time, and `Date.toTime` still represents
its UTC time.
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
