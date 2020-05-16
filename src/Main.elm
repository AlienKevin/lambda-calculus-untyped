module Main exposing (main)


import Html
import LambdaParser


source =
  -- "id = (\\x. \\x. x) x"
  -- "always = \\a. \\b. a"
  -- "apply = \\a. \\b. a b"
  "f = \\x. \\y. x y x"
  -- "f = x y z"


main =
  let
    parseResult =
      LambdaParser.parseDef source
  in
  Html.div []
    [ Html.h1 [] [ Html.text "Source code:" ]
    , Html.pre [] [ Html.text source ]
    , case parseResult of
      Err deadEnds ->
        Html.pre []
        [ Html.text <| LambdaParser.showDeadEnds source deadEnds ]

      Ok def ->
        Html.div []
          [ Html.h1 [] [ Html.text "Parse tree:"]
          , Html.pre [] [ Html.text <| LambdaParser.showDef def ]
          ]
    ]

