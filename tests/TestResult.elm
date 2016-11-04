module TestResult exposing (TestResult, fromRunner, view)

import Expect exposing (Expectation)
import Html exposing (Html, div, ol, li, pre, text)
import Html.Attributes exposing (style)
import String
import Test.Runner exposing (Runner)


type alias TestResult =
  ( Int
  , List (List String)
  )


empty : TestResult
empty =
  ( 0
  , []
  )


combine : TestResult -> TestResult -> TestResult
combine (b1, b2) (a1, a2) =
  ( a1 + b1
  , a2 ++ b2
  )


fromRunner : Runner -> TestResult
fromRunner runner =
  fromLabelsAndRunner [] runner


fromLabelsAndRunner : List String -> Runner -> TestResult
fromLabelsAndRunner labels runner =
  case runner of
    Test.Runner.Runnable thunk ->
      Test.Runner.run thunk
        |> fromExpectations labels

    Test.Runner.Labeled label subRunner ->
      fromLabelsAndRunner (label :: labels) subRunner

    Test.Runner.Batch runners ->
      List.foldl combine empty (List.map (fromLabelsAndRunner labels) runners)


fromExpectations : List String -> List Expectation -> TestResult
fromExpectations labels expectations =
  ( List.length expectations
  , expectations
      |> List.map Expect.getFailure
      |> List.filterMap identity
      |> List.map (.message >> (flip (::)) labels) -- discard "given" part of failure
  )


-- view

view : TestResult -> Html a
view (testCount, failures) =
  div
    [ style
        [ ("font-family", "monospace" )
        , ("margin", "20px")
        ]
    ]
    [ div [] [ text <| toString testCount ++ " tests run" ]
    , div []
        [ text <|
            case List.length failures of
              0 ->
                "All tests passed"
              n ->
                pluralize "test" n ++ " failed"
        ]
    , ol []
        (failures |> List.map viewFailure)
    ]


pluralize : String -> Int -> String
pluralize noun n =
  toString n ++ " " ++ noun ++ (if n == 1 then "" else "s")


viewFailure : List String -> Html a
viewFailure list =
  case list of
    message :: labels ->
      li []
        [ pre
            [ style [ ("color", "#999999")] ]
            [ labels
                |> List.reverse
                |> String.join "\n"
                |> text
            ]
        , pre []
            [ text message ]
        ]
    _ ->
      text ""
