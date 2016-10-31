module Tests exposing (main)

import Html exposing (Html)
import Legacy.ElmTest exposing (Test, suite)
import Random.Pcg
import Test.Runner
import TestResult
import Test.Convert as Convert
import Test.Create as Create
import Test.Examples as Examples
import Test.Extract as Extract
import Test.Math as Math


tests : Test
tests =
  suite "Date"
    [ Create.tests
    , Extract.tests
    , Math.tests
    , Convert.tests
    , Examples.tests
    ]


main : Html a
main =
  tests
    |> Test.Runner.fromTest 1 (Random.Pcg.initialSeed 0)
    |> TestResult.fromRunner
    |> TestResult.view
