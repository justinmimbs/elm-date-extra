module Date.Internal.Core
    exposing
        ( msFromTimeParts
        , unixTimeFromCalendarDate
        , unixTimeFromOrdinalDate
        , unixTimeFromParts
        , unixTimeFromWeekDate
        , weekNumberFromCalendarDate
        , weekYearFromCalendarDate
        )

import Date exposing (Month)
import Date.Extra.Facts exposing (daysBeforeStartOfMonth, months, msPerDay, msPerHour, msPerMinute, msPerSecond)
import Date.Internal.RataDie as RataDie exposing (RataDie)


-- week numbering


weekYearFromCalendarDate : Int -> Month -> Int -> Int
weekYearFromCalendarDate y m d =
    RataDie.weekYear <| RataDie.fromCalendarDate y m d


weekNumberFromCalendarDate : Int -> Month -> Int -> Int
weekNumberFromCalendarDate y m d =
    RataDie.weekNumber <| RataDie.fromCalendarDate y m d



-- unix time


unixTimeFromRataDie : RataDie -> Int
unixTimeFromRataDie rd =
    (rd - 719163) * msPerDay


unixTimeFromOrdinalDate : Int -> Int -> Int
unixTimeFromOrdinalDate y d =
    unixTimeFromRataDie <| RataDie.fromOrdinalDate y d


unixTimeFromCalendarDate : Int -> Month -> Int -> Int
unixTimeFromCalendarDate y m d =
    unixTimeFromRataDie <| RataDie.fromCalendarDate y m d


unixTimeFromWeekDate : Int -> Int -> Int -> Int
unixTimeFromWeekDate y w d =
    unixTimeFromRataDie <| RataDie.fromWeekDate y w d


msFromTimeParts : Int -> Int -> Int -> Int -> Int
msFromTimeParts hh mm ss ms =
    msPerHour * hh + msPerMinute * mm + msPerSecond * ss + ms


unixTimeFromParts : Int -> Month -> Int -> Int -> Int -> Int -> Int -> Int
unixTimeFromParts y m d hh mm ss ms =
    unixTimeFromCalendarDate y m d
        + msFromTimeParts hh mm ss ms
