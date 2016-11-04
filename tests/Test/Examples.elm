module Test.Examples exposing (tests)

import Date exposing (Date, Month(..))
import Date.Extra as Date exposing (Interval(..), utc, offset, local, noTime, atTime, calendarDate, weekDate, ordinalDate)
import Test exposing (Test, describe, test)
import Test.Utilities exposing (equals, DateParts, toParts, toUtc, toTimeOffset)


convertTests : Test
convertTests =
  describe "Convert"
    [ equals
        (
          Date.toFormattedString
            "EEEE, MMMM d, y 'at' h:mm a"
            (Date.fromParts 2007 Mar 15 13 45 56 67)
        )
        "Thursday, March 15, 2007 at 1:45 PM"

    , equals
        (
          Date.toFormattedString
            "MMMM ddd, y"
            (Date.fromParts 2007 Mar 15 13 45 56 67)
        )
        "March 15th, 2007"

    , equals
        (
          Date.toUtcIsoString (Date.fromSpec (offset -240) (atTime 13 45 56 67) (calendarDate 2007 Mar 15 ))
        )
        "2007-03-15T17:45:56.067Z"
    ]


toLocalDateString : Date -> String
toLocalDateString =
  Date.toFormattedString "<d MMMM yyyy>"


toLocalDateTimeString : Date -> String
toLocalDateTimeString =
  Date.toFormattedString "<d MMMM yyyy, HH:mm"


toUtcDateString : Date -> String
toUtcDateString =
  Date.toUtcFormattedString "<d MMMM yyyy, 'UTC'>"


toUtcDateTimeString : Date -> String
toUtcDateTimeString =
  Date.toUtcFormattedString "<d MMMM yyyy, HH:mm, 'UTC'>"


createTests : Test
createTests =
  describe "Create"
    [ equals
        (
          Date.fromParts 1999 Dec 31 23 59 0 0
          |> toLocalDateTimeString
        )
        "<31 December 1999, 23:59"

    , equals
        (
          Date.fromParts 2007 Feb 29 0 0 0 0
          |> toLocalDateString
        )
        "<1 March 2007>"

    , equals
        (
          Date.fromIsoString "2000-01-01"
          |> Maybe.map toLocalDateString
        )
        (Just "<1 January 2000>")

    , equals
        (
          Date.fromIsoString "2009-W01-1T00Z"
          |> Maybe.map toUtcDateString
        )
        (Just "<29 December 2008, UTC>")

    , equals
        (
          Date.fromIsoString "2016-218T20:00-03:00"
          |> Maybe.map toUtcDateTimeString
        )
        (Just "<5 August 2016, 23:00, UTC>")

    , equals
        (
          Date.fromIsoString "1/1/2000"
        )
        Nothing

    , equals
        (
          Date.fromSpec
            local
            noTime
            (calendarDate 2000 Jan 1)
          |> toLocalDateString
        )
        "<1 January 2000>"

    , equals
        (
          Date.fromSpec
            utc
            noTime
            (weekDate 2009 1 1)
          |> toUtcDateString
        )
        "<29 December 2008, UTC>"

    , equals
        (
          Date.fromSpec
            (offset -180)
            (atTime 20 0 0 0)
            (ordinalDate 2016 218)
          |> toUtcDateTimeString
        )
        "<5 August 2016, 23:00, UTC>"
    ]


mathTests : Test
mathTests =
  describe "Math"
    [ let
        dec31 = Date.fromCalendarDate 1999 Dec 31
        jan1 = Date.fromCalendarDate 2000 Jan 1
      in
        describe "1"
          [ equals
              (Date.equalBy Month dec31 jan1)
              False

          , equals
              (Date.equalBy Week dec31 jan1)
              True
          ]

    , equals
        (
          Date.floor Hour (Date.fromParts 1999 Dec 31 23 59 59 999)
          |> toLocalDateTimeString
        )
        "<31 December 1999, 23:00"

    , equals
        (
          Date.add Week 2 (Date.fromParts 2007 Mar 15 11 55 0 0)
          |> toLocalDateTimeString
        )
        "<29 March 2007, 11:55"

    , equals
        (
          Date.add Month 1 (Date.fromParts 2000 Jan 31 0 0 0 0)
          |> toLocalDateString
        )
        "<29 February 2000>"

    , equals
        (
          Date.ceiling Monday (Date.fromParts 1999 Dec 31 23 59 59 999)
          |> toLocalDateString
        )
        "<3 January 2000>"

    , equals
        (
          Date.diff Month
            (Date.fromParts 2007 Mar 15 11 55 0 0)
            (Date.fromParts 2007 Sep 1 0 0 0 0)
        )
        5

    , equals
        (
          Date.range Day 2
            (Date.fromParts 2007 Mar 15 11 55 0 0)
            (Date.fromParts 2007 Mar 22 0 0 0 0)
          |> List.map toLocalDateString
        )
        [ "<16 March 2007>", "<18 March 2007>", "<20 March 2007>" ]
    ]


readmeTests : Test
readmeTests =
  let
    date =
      Date.fromParts 1999 Dec 31 23 59 59 999
  in
    describe "README"
      [ equals
          (
            Date.floor Hour date
            |> toLocalDateTimeString
          )
          "<31 December 1999, 23:00"

      , equals
          (
            Date.ceiling Monday date
            |> toLocalDateTimeString
          )
          "<3 January 2000, 00:00"

      , equals
          (
            Date.add Week -2 date
            |> Date.toFormattedString "<d MMMM yyyy, HH:mm:ss.SSS>"
          )
          "<17 December 1999, 23:59:59.999>"

      , equals
          (
            Date.diff Day date (Date.add Week 2 date)
          )
          14

      , equals
          (
            Date.range Monday 1
              (Date.floor Month date)   -- <1 December 1999>
              (Date.ceiling Month date) -- <1 January 2000>
            |> List.map toLocalDateString
          )
          [ "<6 December 1999>", "<13 December 1999>", "<20 December 1999>", "<27 December 1999>" ]
      ]


tests : Test
tests =
  describe "examples"
    [ convertTests
    , createTests
    , mathTests
    , readmeTests
    ]
