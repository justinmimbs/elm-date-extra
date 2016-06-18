module Test exposing (..)

import Test.Internal as Internal
import Test.Extract as Extract

import ElmTest exposing (Test, suite, runSuiteHtml)


tests : Test
tests =
  suite "Date" [
    Internal.tests,
    Extract.tests
  ]

main : Program Never
main =
  runSuiteHtml tests
