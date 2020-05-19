module LambdaDebug exposing (main)


import Html
import LambdaParser
import LambdaChecker
import LambdaEvaluator


source =
  -- "id = \\x. x"
  -- "always = \\a. \\b. a"
  -- "apply = \\a. \\b. a b"
  -- "f = \\x. \\y. x y x"
  -- "f = x y z"
--   """{- tru in lambda calculus -}
-- v = tru fls fls
-- tru = \\a. \\b. a -- true

-- fls = \\a. \\b. b -- false
--   """
  -- "v = (\\x.x) ((\\x.x) (\\z. (\\x.x) z))"
  """
pair = \\f. \\s. \\b. b f s
fst = \\p. p tru
snd = \\p. p fls
tru = \\t. \\f. t
fls = \\t. \\f. f
v = pair tru fls
v2 = pair (pair tru fls) (pair fls tru)
c0 = \\s. \\z. z
c1 = \\s. \\z. s z
plus = \\m. \\n. \\s. \\z. m s (n s z)
c0Plus1 = plus c0 c1
c1Plus1 = plus c1 c1
-- divergent
merge = (\\x. x x) (\\x. x x)
  """
  -- "v = (\\f. \\s. \\b. b f s) (\\t. \\f. t) (\\t. \\f. f)"
  -- "v = \\b. b (\\t. \\f. t) (\\t. \\f. f)"


main =
  let
    parseResult =
      LambdaParser.parseDefs source
  in
  Html.div []
    [ Html.h1 [] [ Html.text "Source code:" ]
    , Html.pre [] [ Html.text source ]
    , case parseResult of
      Err deadEnds ->
        Html.pre []
        [ Html.text <| "⚠️ I got stuck while parsing:\n"
        , Html.text <| LambdaParser.showProblems source deadEnds
        ]

      Ok defs ->
        Html.div []
          [ Html.h1 [] [ Html.text "Parse tree:"]
          , Html.pre [] [ Html.text <| LambdaParser.showDefs defs ]
          , case LambdaChecker.checkDefs defs of
            [] ->
              let
                resultDefs =
                  LambdaEvaluator.evalDefs LambdaEvaluator.CallByValue defs
              in
              Html.div []
              [ Html.h1 [] [ Html.text "Evaluation result:" ]
              , Html.pre []
                [ Html.text <| LambdaParser.showDefs resultDefs
                ]
              ]
            
            problems ->
              Html.pre [] [ Html.text <| LambdaChecker.showProblemsWithSingleSource source problems ]
          ]
    ]

