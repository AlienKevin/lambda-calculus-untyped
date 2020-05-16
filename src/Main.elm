module Main exposing (main)


import Html
import LambdaParser
import LambdaChecker
import Dict


source =
  -- "id = (\\x. \\x. x) x"
  -- "always = \\a. \\b. a"
  -- "apply = \\a. \\b. a b"
  -- "f = \\x. \\y. x y x"
  -- "f = x y z"
  """tru = \\a. \\b. a
fls = \\a. \\b. b
v = tru fls fls
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
        [ Html.text <| "⚠️ I got stuck during parsing:"
        , Html.text <| LambdaParser.showProblems source deadEnds
        ]

      Ok defs ->
        Html.div []
          [ Html.h1 [] [ Html.text "Parse tree:"]
          , Html.pre [] [ Html.text <| LambdaParser.showDefs defs ]
          , case LambdaChecker.checkDefs defs of
            [] ->
              Html.pre [] [ Html.text "✔ Passed check." ]
            
            problems ->
              Html.pre [] [ Html.text <| LambdaChecker.showProblems source problems ]
          ]
    ]

