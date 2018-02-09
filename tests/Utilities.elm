module Utilities exposing (..)

import Date exposing (Date, Month(..), day, hour, millisecond, minute, month, second, year)
import Date.Extra as Date
import Date.Facts exposing (daysInMonth, msPerMinute)


type alias DateParts =
    ( Int, Month, Int, Int, Int, Int, Int )


toParts : Date -> DateParts
toParts date =
    ( year date, month date, day date, hour date, minute date, second date, millisecond date )


toTimeOffset : Int -> Date -> Date
toTimeOffset offset date =
    Date.fromTime <| Date.toTime date - (toFloat <| (Date.offsetFromUtc date - offset) * msPerMinute)


toUtc : Date -> Date
toUtc date =
    Date.fromTime <| Date.toTime date - (toFloat <| Date.offsetFromUtc date * msPerMinute)


calendarDatesInMonth : Int -> Month -> List ( Int, Month, Int )
calendarDatesInMonth y m =
    List.map
        ((,,) y m)
        (List.range 1 (daysInMonth y m))


calendarDatesInYear : Int -> List ( Int, Month, Int )
calendarDatesInYear y =
    List.concatMap (calendarDatesInMonth y) [ Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec ]
