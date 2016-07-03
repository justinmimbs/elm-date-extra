module Test exposing (main)

import Test.Create as Create
import Test.Extract as Extract
import Test.Convert as Convert

import ElmTest exposing (Test, suite, runSuiteHtml)


tests : Test
tests =
  suite "Date" [
    Create.tests,
    Extract.tests,
    Convert.tests
  ]

main : Program Never
main =
  runSuiteHtml tests
