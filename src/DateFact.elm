module DateFact exposing (..)

import Date exposing(Month(..), Day(..))


isLeapYear : Int -> Bool
isLeapYear y =
  y % 4 == 0 && y % 100 /= 0 || y % 400 == 0


daysInMonth : Int -> Month -> Int
daysInMonth y m =
  case m of
    Jan -> 31
    Feb -> if isLeapYear y then 29 else 28
    Mar -> 31
    Apr -> 30
    May -> 31
    Jun -> 30
    Jul -> 31
    Aug -> 31
    Sep -> 30
    Oct -> 31
    Nov -> 30
    Dec -> 31


daysBeforeStartOfMonth : Int -> Month -> Int
daysBeforeStartOfMonth y m =
  case m of
    Jan -> 0
    Feb -> 31
    Mar -> if isLeapYear y then 60  else 59
    Apr -> if isLeapYear y then 91  else 90
    May -> if isLeapYear y then 121 else 120
    Jun -> if isLeapYear y then 152 else 151
    Jul -> if isLeapYear y then 182 else 181
    Aug -> if isLeapYear y then 213 else 212
    Sep -> if isLeapYear y then 244 else 243
    Oct -> if isLeapYear y then 274 else 273
    Nov -> if isLeapYear y then 305 else 304
    Dec -> if isLeapYear y then 335 else 334


months : List Month
months =
  [ Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec ]


monthNumberFromMonth : Month -> Int
monthNumberFromMonth m =
  case m of
    Jan -> 1
    Feb -> 2
    Mar -> 3
    Apr -> 4
    May -> 5
    Jun -> 6
    Jul -> 7
    Aug -> 8
    Sep -> 9
    Oct -> 10
    Nov -> 11
    Dec -> 12


monthFromMonthNumber : Int -> Month
monthFromMonthNumber n =
  case n of
    1  -> Jan
    2  -> Feb
    3  -> Mar
    4  -> Apr
    5  -> May
    6  -> Jun
    7  -> Jul
    8  -> Aug
    9  -> Sep
    10 -> Oct
    11 -> Nov
    _  -> Dec


isoWeekdayFromDayOfWeek : Day -> Int
isoWeekdayFromDayOfWeek d =
  case d of
    Mon -> 1
    Tue -> 2
    Wed -> 3
    Thu -> 4
    Fri -> 5
    Sat -> 6
    Sun -> 7


dayOfWeekFromIsoWeekday : Int -> Day
dayOfWeekFromIsoWeekday n =
  case n of
    1 -> Mon
    2 -> Tue
    3 -> Wed
    4 -> Thu
    5 -> Fri
    6 -> Sat
    _ -> Sun

msPerSecond : Int
msPerSecond =
  1000


msPerMinute : Int
msPerMinute =
  60 * msPerSecond


msPerHour : Int
msPerHour =
  60 * msPerMinute


msPerDay : Int
msPerDay =
  24 * msPerHour
