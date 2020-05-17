module CheckerTest exposing (..)


import LambdaParser exposing (parseDefs)
import LambdaChecker exposing (checkDefs, Problem(..))
import Location exposing (fakeLocated)
import Test exposing (Test, describe)
import Expect


suite : Test
suite =
  describe "Lambda Checker Test"
    [ describe "Duplicated Definition"
      [ test "one duplicate"
          """id = \\x. x
id = \\y. y
"""
        [DuplicatedDefinition { from = (1,1), to = (1,3), value = "id" } { from = (2,1), to = (2,3), value = "id" }]
      , test "two duplicates"
          """id = \\x. x
id = \\y. y
id = \\z. z
"""
        [DuplicatedDefinition { from = (1,1), to = (1,3), value = "id" } { from = (3,1), to = (3,3), value = "id" },DuplicatedDefinition { from = (1,1), to = (1,3), value = "id" } { from = (2,1), to = (2,3), value = "id" }]
      ]
    , describe "Undefined Variable"
      [ test "one undefined variable"
        "id = \\x. y"
        [UndefinedVariable { from = (1,10), to = (1,11), value = "y" }]
      , test "two undefined variables"
        "id = \\x. y z"
        [UndefinedVariable { from = (1,10), to = (1,11), value = "y" },UndefinedVariable { from = (1,12), to = (1,13), value = "z" }]
      ]
    ]


test : String -> String -> List Problem -> Test
test title src problems =
  let
    checkResult =
      case parseDefs src of
        Err parseProblems ->
          [ UndefinedVariable <| fakeLocated <| LambdaParser.showProblems src parseProblems ]

        Ok defs ->
          checkDefs defs
  in
  Test.test title <|
  \_ ->
    Expect.equal problems checkResult
