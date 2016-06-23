module Test exposing (..)

import Test.Core as Core
import Test.Extract as Extract

import ElmTest exposing (Test, suite, runSuiteHtml)


tests : Test
tests =
  suite "Date" [
    Core.tests,
    Extract.tests
  ]

main : Program Never
main =
  runSuiteHtml tests
