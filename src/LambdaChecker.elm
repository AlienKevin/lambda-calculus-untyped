module LambdaChecker exposing (checkDefs, checkDef, checkExpr, showProblems)


import LambdaParser exposing (Def, Expr(..))
import Dict exposing (Dict)
import Location exposing (Located, showLocation)


type Problem
  = DuplicatedDefinition (Located String) (Located String)
  | UndefinedVariable (Located String)


checkDefs : List Def -> List Problem
checkDefs defs =
  Tuple.first <|
  List.foldl
    (\def (problems, names) ->
      Tuple.mapFirst ((++) <| checkExpr names def.expr.value)
      ( case Dict.get def.name.value names of
        Nothing ->
          ( problems
          , Dict.insert def.name.value def.name names
          )

        Just previousName ->
          ( DuplicatedDefinition previousName def.name :: problems
          , names
          )
      )
    )
    ([], Dict.empty)
    defs


checkDef : Dict String (Located String) -> Def -> List Problem
checkDef names def =
  checkExpr names def.expr.value


checkExpr : Dict String (Located String) -> Expr -> List Problem
checkExpr names expr =
  case expr of
    EVariable name ->
      if Dict.member name.value names then
        []
      else
        [ UndefinedVariable name ]
    
    EAbstraction boundVar innerExpr ->
      checkExpr (Dict.insert boundVar.value boundVar names) innerExpr.value
    
    EApplication func arg ->
      checkExpr names func.value
      ++ checkExpr names arg.value


showProblems : String -> List Problem -> String
showProblems src problems =
  String.join "\n\n" <|
  List.indexedMap
    (\index problem ->
      "#" ++ String.fromInt (index + 1) ++ ": " ++ showProblemHelper src problem
    )
    problems


showProblemHelper : String -> Problem -> String
showProblemHelper src problem =
  String.join "\n" <|
  case problem of
    DuplicatedDefinition d1 d2 ->
      [ "I found that you defined `" ++ d1.value ++ "` twice. It first appeared here:"
      , showLocation src d1
      , "It then appeared a second time here:"
      , showLocation src d2
      , "Hint: Try renaming one of them to avoid duplicated definition."
      ]
    
    UndefinedVariable name ->
      [ "I found an undefined variable `" ++ name.value ++ "` here:"
      , showLocation src name
      , "Hint: Try defining `" ++ name.value ++ "` somewhere."
      ]
  