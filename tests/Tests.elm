module Tests exposing (..)

import Test exposing (Test)
import Test.Create as Create
import Test.Extract as Extract
import Test.Format as Format
import Test.Math as Math


tests : Test
tests =
    Test.concat
        [ Create.tests
        , Extract.tests
        , Format.tests
        , Math.tests
        ]
