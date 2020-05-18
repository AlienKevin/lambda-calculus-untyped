module LambdaChecker exposing (checkDefs, checkDef, checkExpr, showProblems, showProblemsWithSingleSource, sortDefs, Problem(..))


import LambdaParser exposing (fakeDef, Def, Expr(..))
import Dict exposing (Dict)
import Location exposing (showLocation, isFakeLocated, Located)
import Set exposing (Set)
import List.Extra


type Problem
  = DuplicatedDefinition (Int, Located String) (Int, Located String)
  | UndefinedVariable (Located String)


checkDefs : List Def -> List Problem
checkDefs defs =
  let
    allNames =
      List.foldl
        (\def names ->
          Dict.insert def.name.value def.name names
        )
        Dict.empty
        defs
  in
  Tuple.first <|
  List.Extra.indexedFoldl
    (\index def (problems, names) ->
      if isFakeLocated def.name then
        (problems, names)
      else
        Tuple.mapFirst ((++) <| checkExpr (Dict.filter (\_ name -> name /= def.name) allNames) def.expr.value)
        ( case Dict.get def.name.value names of
          Nothing ->
            ( problems
            , Dict.insert def.name.value (index, def.name) names
            )

          Just (previousIndex, previousName) ->
            ( DuplicatedDefinition (previousIndex, previousName) (index, def.name) :: problems
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


showProblems : List String -> Int -> List Problem -> String
showProblems srcs currentIndex problems =
  String.join "\n\n" <|
  List.map
    (\problem ->
      showProblemHelper srcs currentIndex problem
    )
    problems


showProblemHelper : List String -> Int -> Problem -> String
showProblemHelper srcs currentIndex problem =
  String.join "\n" <|
  case problem of
    DuplicatedDefinition (index1, name1) (index2, name2) ->
      let
        indexStr1 =
          "[" ++ String.fromInt (index1 + 1) ++ "] "

        src1 =
          Maybe.withDefault "" <| -- impossible
          List.Extra.getAt index1 srcs
        
        location1 =
          indexStr1
          ++ ( String.replace "\n" ("\n" ++ String.repeat (String.length indexStr1) " ")
            <| showLocation src1 name1
          )

        indexStr2 =
          "[" ++ String.fromInt (index2 + 1) ++ "] "

        src2 =
          Maybe.withDefault "" <| -- impossible
          List.Extra.getAt index2 srcs
        
        location2 =
          indexStr2
          ++ ( String.replace "\n" ("\n" ++ String.repeat (String.length indexStr2) " ")
            <| showLocation src2 name2
          )
      in
      [ "-- DUPLICATED DEFINITION\n"
      , "I found that you defined `" ++ name1.value ++ "` twice. It first appeared here:"
      , location1
      , "It then appeared a second time here:"
      , location2
      , "Hint: Try renaming one of them to avoid duplicated definition."
      ]
    
    UndefinedVariable name ->
      let
        src =
          Maybe.withDefault "" <| -- impossible
          List.Extra.getAt currentIndex srcs
      in
      [ "-- UNDEFINED VARIABLE\n"
      , "I found an undefined variable `" ++ name.value ++ "` here:"
      , showLocation src name
      , "Hint: Try defining `" ++ name.value ++ "` somewhere."
      ]


showProblemsWithSingleSource : String -> List Problem -> String
showProblemsWithSingleSource src problems =
  String.join "\n\n" <|
  List.map
    (\problem ->
      showProblemWithSingleSourceHelper src problem
    )
    problems


showProblemWithSingleSourceHelper : String -> Problem -> String
showProblemWithSingleSourceHelper src problem =
  String.join "\n" <|
  case problem of
    DuplicatedDefinition (_, d1) (_, d2) ->
      [ "-- DUPLICATED DEFINITION\n"
      , "I found that you defined `" ++ d1.value ++ "` twice. It first appeared here:"
      , showLocation src d1
      , "It then appeared a second time here:"
      , showLocation src d2
      , "Hint: Try renaming one of them to avoid duplicated definition."
      ]
    
    UndefinedVariable name ->
      [ "-- UNDEFINED VARIABLE\n"
      , "I found an undefined variable `" ++ name.value ++ "` here:"
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
  getFreeVariablesHelper Set.empty expr


getFreeVariablesHelper : Set String -> Expr -> List (Located String)
getFreeVariablesHelper boundVariables expr =
  case expr of
    EVariable name ->
      if Set.member name.value boundVariables then
        []
      else
        [ name ]
    
    EAbstraction boundVar innerExpr ->
      getFreeVariablesHelper (Set.insert boundVar.value boundVariables) innerExpr.value
    
    EApplication func arg ->
      getFreeVariablesHelper boundVariables func.value ++ getFreeVariablesHelper boundVariables arg.value


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
