module Test exposing (main)

import ElmTest exposing (Test, suite, runSuiteHtml)
import Test.Convert as Convert
import Test.Create as Create
import Test.Examples as Examples
import Test.Extract as Extract
import Test.Math as Math


tests : Test
tests =
  suite "Date" [
    Create.tests,
    Extract.tests,
    Math.tests,
    Convert.tests,
    Examples.tests
  ]


main : Program Never
main =
  runSuiteHtml tests
