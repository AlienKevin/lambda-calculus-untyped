module Main exposing (main)


import Html
import LambdaParser
import LambdaChecker
import LambdaEvaluator
import Dict


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
  "v = (\\x.x) ((\\x.x) (\\z. (\\x.x) z))"


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
                  LambdaEvaluator.evalDefs defs
              in
              Html.div []
              [ Html.h1 [] [ Html.text "Evaluation result:" ]
              , Html.pre []
                [ Html.text <| LambdaParser.showDefs resultDefs
                ]
              ]
            
            problems ->
              Html.pre [] [ Html.text <| LambdaChecker.showProblems source problems ]
          ]
    ]

