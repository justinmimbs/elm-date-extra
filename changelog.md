# Changelog

## 3.0.0 - 2018-02-12

### Changed
- `fromParts` now clamps out-of-range parts instead of underflowing or
  overflowing them
- `fromCalendarDate` now clamps out-of-range days instead of underflowing or
  overflowing them
- `fromIsoString`
    - now returns a `Result String Date` instead of a `Maybe Date`, providing a
      reason when failing to produce a `Date`
    - now fails when given an invalid date/time instead of underflowing or
      overflowing the invalid parts
    - now accepts a comma as a decimal mark and a minus sign in the time offset
- `isBetween` now expects its first two arguments given in ascending order
  (i.e. `isBetween min max x`) instead of accepting either order
- `Date.Extra.Facts` number helpers were moved to `Date.Extra` and renamed
    - `monthNumberFromMonth` --> `monthToNumber`
    - `monthFromMonthNumber` --> `numberToMonth`
    - `weekdayNumberFromDayOfWeek` --> `weekdayToNumber`
    - `dayOfWeekFromWeekdayNumber` --> `numberToWeekday`
- `fromSpec` now takes its three arguments in the reverse order
- `TimeSpec` constructors were renamed
    - `atTime` --> `time`
    - `noTime` --> `midnight`
- `DateSpec` constructor `weekDate` now takes a `Date.Day` instead of an `Int`
  to represent a weekday

### Removed
- `Date.Extra.Facts` functions `isLeapYear`, `daysInMonth`, `daysBeforeStartOfMonth`
- `Date.Extra.Facts` constants `msPerSecond`, `msPerMinute`, `msPerHour`, `msPerDay`, `months`


## 2.1.1 - 2018-01-31

### Fixed
- `range`: in cases where there was more than one way to align a list of dates
  within the start and end dates, `range` would align them closest to the end
  date; `range` now aligns them closest to the start date, which is less
  surprising


## 2.1.0 - 2018-01-30

### Added
- `toRataDie`
- `fromRataDie`
