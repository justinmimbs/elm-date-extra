module Date.Convert exposing (
  toFormattedString,
  toIsoString,
  toUtcFormattedString,
  toUtcIsoString,
  toJulianDate
  )

{-| Convert dates to other types.

# Formatted Strings
@docs toFormattedString, toIsoString, toUtcFormattedString, toUtcIsoString

# Julian Dates
@docs toJulianDate
-}


import Date exposing (Date, toTime, year, month, day, hour, minute, second, millisecond)
import Date.Facts exposing (msPerDay)
import Date.Internal.Core exposing (unixTimeFromParts)
import Date.Internal.Format


{-| Convert a date to a string using a pattern as a template.

    Date.toFormattedString
      "EEEE, MMMM d, y 'at' h:mm a"
      (Date.fromParts 2007 Mar 15 13 45 56 67)
    -- "Thursday, March 15, 2007 at 1:45 PM"

Each alphabetic character in the pattern represents date or time information;
the number of times a character is repeated specifies the form of the name to
use (e.g. "Tue", "Tuesday") or the padding of numbers (e.g. "1", "01").
Formatting characters are escaped within single-quotes, and a single-quote is
escaped as a sequence of two single-quotes, whether appearing inside or outside
an escaped sequence.

Patterns are based on Date Format Patterns in [Unicode Technical
Standard #35](http://www.unicode.org/reports/tr35/tr35-43/tr35-dates.html#Date_Format_Patterns).
Only the following subset of formatting characters are available:

    "y" -- year
    "Y" -- week-numbering year
    "Q" -- quarter
    "M" -- month
    "w" -- week number
    "d" -- day
    "D" -- ordinal day
    "E" -- day of week
    "e" -- weekday number / day of week
    "a" -- day period (AM, PM)
    "b" -- day period (am, pm, noon, midnight)
    "h" -- hour (12-hour clock)
    "H" -- hour (24-hour clock)
    "m" -- minute
    "s" -- second
    "S" -- fractional second
    "X" -- time offset, using "Z" when offset is 0
    "x" -- time offset

The non-standard pattern field "ddd" is available to indicate the day of the
month with an ordinal suffix (e.g. "1st", "15th"), as the current standard does
not include such a field.

    Date.toFormattedString
      "MMMM ddd, y"
      (Date.fromParts 2007 Mar 15 13 45 56 67)
    -- "March 15th, 2007"
-}
toFormattedString : String -> Date -> String
toFormattedString =
  Date.Internal.Format.toFormattedString False


{-| Convenience function for formatting a date to ISO 8601 (extended
date and time format with local time offset).

    Date.toIsoString (Date.fromParts 2007 Mar 15 13 45 56 67)
    -- "2007-03-15T13:45:56.067-04:00"
    -- (example has a local offset of UTC-04:00)
-}
toIsoString: Date -> String
toIsoString =
  toFormattedString "yyyy-MM-dd'T'HH:mm:ss.SSSxxx"


{-| Convert a date to a string just like `toFormattedString`, but using the UTC
representation instead of the local representation of the date.
-}
toUtcFormattedString : String -> Date -> String
toUtcFormattedString =
  Date.Internal.Format.toFormattedString True


{-| Convenience function for formatting a date, in UTC representation, to ISO
8601 (extended date and time format with "Z" for time offset).

    Date.toUtcIsoString (Date.fromParts 2007 Mar 15 13 45 56 67)
    -- "2007-03-15T17:45:56.067Z"
    -- (example has a local offset of UTC-04:00)
-}
toUtcIsoString: Date -> String
toUtcIsoString =
  toUtcFormattedString "yyyy-MM-dd'T'HH:mm:ss.SSSXXX"


{-| Convert a date to a [Julian Date](https://en.wikipedia.org/wiki/Julian_day).
Julian Dates always represent UTC time.

    Date.toJulianDate (Date.fromSpec utc noTime (calendarDate 2000 Jan 1))
    -- 2451544.5
-}
toJulianDate : Date -> Float
toJulianDate date =
  toTime date / toFloat msPerDay + 2440587.5
