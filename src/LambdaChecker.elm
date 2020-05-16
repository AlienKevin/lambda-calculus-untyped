module LambdaChecker exposing (checkDefs, checkDef, checkExpr, showProblems, sortDefs)


import LambdaParser exposing (fakeDef, Def, Expr(..))
import Dict exposing (Dict)
import Location exposing (showLocation, Located)
import Set exposing (Set)
import List.Extra


type Problem
  = DuplicatedDefinition (Located String) (Located String)
  | UndefinedVariable (Located String)


checkDefs : List Def -> List Problem
checkDefs defs =
  let
    sortedDefs =
      sortDefs defs
  in
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
    sortedDefs


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


sortDefs : List Def -> List Def
sortDefs defs =
  let
    dependencies =
      List.foldl
      (\def deps ->
        Dict.insert
        def.name.value
        (List.map .value <| getFreeVariables def.expr.value)
        deps
      )
      Dict.empty
      defs
    
    sortedNames =
      sortDependencies dependencies
  in
  List.map
  (\name ->
    Maybe.withDefault fakeDef <| -- impossible
    List.Extra.find
      (\def ->
        def.name.value == name
      )
      defs
  )
  sortedNames


getFreeVariables : Expr -> List (Located String)
getFreeVariables expr =
  case expr of
    EVariable name ->
      [ name ]
    
    EAbstraction boundVar innerExpr ->
      getFreeVariables innerExpr.value
    
    EApplication func arg ->
      getFreeVariables func.value ++ getFreeVariables arg.value


type alias Dependencies =
  Dict String (List String)


sortDependencies : Dependencies -> List String
sortDependencies dep =
  let
    (result, _, _) =
      sortDependenciesHelper ([], Set.empty, dep)
  in
  List.reverse result
  

sortDependenciesHelper : (List String, Set String, Dependencies) -> (List String, Set String, Dependencies)
sortDependenciesHelper (result0, used0, dep0) =
  let
    (result1, used1, dep1) =
      Dict.foldl
      (\k v (result, used, dep) ->
        if List.all (\value -> Set.member value used) v then
          (k :: result, Set.insert k used, Dict.filter (\k1 _ -> k /= k1) dep)
        else
          (result, used, dep)
      )
      (result0, used0, dep0)
      dep0
  in
  if Dict.isEmpty dep1 then
    (result1, used1, dep1)
  else if Dict.size dep0 == Dict.size dep1 then
    ((List.reverse <| Dict.keys dep1) ++ result1, used1, dep1)
  else
    sortDependenciesHelper (result1, used1, dep1)
