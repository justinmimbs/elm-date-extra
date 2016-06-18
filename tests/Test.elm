module Test exposing (..)

import Test.DateInternal as DateInternal
import Test.DateExtract as DateExtract

import ElmTest exposing (Test, suite, runSuiteHtml)


tests : Test
tests =
  suite "Date" [
    DateInternal.tests,
    DateExtract.tests
  ]

main : Program Never
main =
  runSuiteHtml tests
