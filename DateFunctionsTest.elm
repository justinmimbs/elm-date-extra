module DateFunctionsTest exposing (..)

import Date
import DateFunctions exposing (..)

{-
import DatePrime as Date'
Date'.format
-}

--

isoWeekDate = {
    tests = [
      ((2005, Date.Jan, 1),  (2004, 53, 6)),
      ((2005, Date.Jan, 2),  (2004, 53, 7)),
      ((2005, Date.Dec, 31), (2005, 52, 6)),
      ((2007, Date.Jan, 1),  (2007, 1, 1)),
      ((2007, Date.Dec, 30), (2007, 52, 7)),
      ((2007, Date.Dec, 31), (2008, 1, 1)),
      ((2008, Date.Jan, 1),  (2008, 1, 2)),
      ((2008, Date.Dec, 28), (2008, 52, 7)),
      ((2008, Date.Dec, 29), (2009, 1, 1)),
      ((2008, Date.Dec, 30), (2009, 1, 2)),
      ((2008, Date.Dec, 31), (2009, 1, 3)),
      ((2009, Date.Jan, 1),  (2009, 1, 4)),
      ((2009, Date.Dec, 31), (2009, 53, 4)),
      ((2010, Date.Jan, 1),  (2009, 53, 5)),
      ((2010, Date.Jan, 2),  (2009, 53, 6)),
      ((2010, Date.Jan, 3),  (2009, 53, 7)) 
    ],
    
    test = \((y, m, d), isoWeekDate) ->
      isoWeekDate == (isoWeekDateFromDate <| dateFromYMD y m d)
  }

ymd = {
    tests = List.concatMap (\y ->
        List.concatMap (\m ->  
          List.map (\d ->
            (y, m, d)
          ) [ 1 .. (daysInMonth y m) ]
        ) months
      ) ([ 1 .. 4 ] ++ [ 97 .. 105 ] ++ [ 397 .. 405] ++ [ 1897 .. 1905 ] ++ [ 1997 .. 2005 ]),
    
    test = \(y, m, d) ->
      (y, m, d) == (ymdFromRataDie <| rataDieFromYMD y m d)
  }

results =
  List.map isoWeekDate.test isoWeekDate.tests ++
  List.map ymd.test ymd.tests

passAll = List.all identity results
