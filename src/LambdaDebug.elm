module LambdaDebug exposing (main)


import Html
import LambdaParser
import LambdaChecker
import LambdaEvaluator


source =
  -- "v = (\\x:Bool->Bool. x) (\\x:Bool. false) true"
  -- "id = if (\\x:Bool->Bool. x) (\\x:Bool. false) true then \\x:(Bool-> Bool) -> Bool. x else \\y:(Bool->Bool)->Bool. y"
  """v = id (id (id true))
id = \\x:Bool. x
  """

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
              -- Html.text "✔ Passsed check!"
              let
                resultDefs =
                  LambdaEvaluator.evalDefs LambdaEvaluator.FullEvaluation defs
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

