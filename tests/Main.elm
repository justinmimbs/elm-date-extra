module Main exposing (main)

import Html exposing (Html)
import Random.Pcg
import Test exposing (Test)
import Test.Convert as Convert
import Test.Create as Create
import Test.Examples as Examples
import Test.Extract as Extract
import Test.Math as Math
import Test.Runner
import TestResult


tests : Test
tests =
  Test.concat
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
