module LambdaDebug exposing (main)


import Html
import LambdaParser
import LambdaChecker
import LambdaEvaluator


source =
  -- "v = (\\x:Bool->Bool. x) (\\x:Bool. false) true"
  -- "id = if (\\x:Bool->Bool. x) (\\x:Bool. false) true then \\x:(Bool-> Bool) -> Bool. x else \\y:(Bool->Bool)->Bool. y"
  """v = (id true)
v2 = negate (succ 0)
v3 = negate (pred 0)
v4 = half (double 29302)
-- v5 = \\x:Bool. (\\x:Bool. not (not false)) == (\\y:Bool. id (id (id true)))
v6 = 3 >= 4
v7 = (\\x:(Bool, (Int -> Int) -> Bool). (3, true)) (true, \\x: (Int -> Int). false)
v8 = ((\\x:(Int, Int). x) ((1, 2), v7).1).2
v9 = ((1, 2), (3, 4)).2.2
id = \\x:Bool. x
succ = \\a:Int. a + 1
pred = \\a:Int. a - 1
double = \\a:Int. a * 2
half = \\a:Int. a / 2
negate = \\a:Int. 2 + -2
eq = \\a:Int. \\b:Int. a + 2 != b - 2
not = \\a:Bool. if a then false else true
recordFunc = \\x:Bool. if x then { a = 3, b = true, ewioew = { b = 3, c = x } } else { a = 0, b = false, ewioew = { b = negate 3, c = false } }
record = recordFunc false
recordEquality = { a = { a = 1, b = 2 } , b = 2 } == { b = 2, a = { a = 1, b = 2 }  }
pairEquality = (1, (false, true)) == (1, (false, true))
recordAccess = ({ a = id (id (not true)), b = 4 }, 4).1.a
"""
  -- "negate = \\a:Int. --2"

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

