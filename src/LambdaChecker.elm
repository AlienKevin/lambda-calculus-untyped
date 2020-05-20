module LambdaChecker exposing (checkDefs, checkDef, checkExpr, showProblems, showProblemsWithSingleSource, sortDefs, Problem(..))


import LambdaParser exposing (showType, fakeDef, Def, Expr(..), Type(..))
import Dict exposing (Dict)
import Location exposing (showLocation, isFakeLocated, withLocation, Located)
import Set exposing (Set)
import List.Extra


type Problem
  = DuplicatedDefinition (Int, Located String) (Int, Located String)
  | UndefinedVariable (Located String)
  | ExpectingTyFunc (Located Type)
  | ExpectingTyBool (Located Type)
  | MismatchedType (Located Type) (Located Type)


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
        Tuple.mapFirst ((++) <| checkExpr (Dict.filter (\_ name -> name /= def.name) allNames) def.expr)
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
  checkExpr names def.expr


checkExpr : Dict String (Located String) -> Located Expr -> List Problem
checkExpr names expr =
  ( case expr.value of
    EVariable name ->
      if Dict.member name.value names then
        []
      else
        [ UndefinedVariable name ]
    
    EAbstraction boundVar _ innerExpr ->
      checkExpr (Dict.insert boundVar.value boundVar names) innerExpr
    
    EApplication func arg ->
      checkExpr names func
      ++ checkExpr names arg

    EBool _ ->
      []
    
    EIf condition thenBranch elseBranch ->
      checkExpr names condition
      ++ checkExpr names thenBranch
      ++ checkExpr names elseBranch
  )
  |> (\problems ->
    case problems of
      [] ->
        case getType [] expr of
          Err typeProblem ->
            [ typeProblem ]
          
          Ok _ ->
            []
      
      _ ->
        problems
  )


type alias Ctx
  = List (String, Type)


getType : Ctx -> Located Expr -> Result Problem (Located Type)
getType ctx expr =
  case expr.value of
    EVariable name ->
      Ok <| withLocation expr <| getTypeFromContext ctx name.value

    EAbstraction boundVar boundType innerExpr ->
      let
        innerCtx =
          addBinding ctx boundVar.value boundType.value
      in
      getType innerCtx innerExpr |>
      Result.map
      (\innerType ->
        withLocation expr <| TyFunc boundType innerType
      )

    EApplication e1 e2 ->
      getType ctx e1 |>
      Result.andThen
      (\ty1 ->
        getType ctx e2 |>
        Result.andThen
        (\ty2 ->
          case ty1.value of
            TyFunc fromType toType ->
              if areEqualTypes fromType.value ty2.value then
                Ok toType
              else
                Err <| MismatchedType fromType ty2

            _ ->
              Err <| ExpectingTyFunc ty1
        )
      )

    EIf condition thenBranch elseBranch ->
      getType ctx condition |>
      Result.andThen
      (\conditionType ->
        case conditionType.value of
          TyBool ->
            getType ctx thenBranch |>
            Result.andThen
            (\thenType ->
              getType ctx elseBranch |>
              Result.andThen
              (\elseType ->
                if areEqualTypes thenType.value elseType.value then
                  Ok elseType
                else
                  Err <| MismatchedType thenType elseType
              )
            )
        
          _ ->
            Err <| ExpectingTyBool conditionType 
      )

    EBool _ ->
      Ok <| withLocation expr TyBool


areEqualTypes : Type -> Type -> Bool
areEqualTypes ty1 ty2 =
  case (ty1, ty2) of
    (TyBool, TyBool) ->
      True
    
    (TyFunc fromType1 toType1, TyFunc fromType2 toType2) ->
      areEqualTypes fromType1.value fromType2.value
      && areEqualTypes toType1.value toType2.value

    _ ->
      False


addBinding : Ctx -> String -> Type -> Ctx
addBinding ctx name ty =
  (name, ty) :: ctx


getTypeFromContext : Ctx -> String -> Type
getTypeFromContext ctx name =
  Maybe.withDefault TyBool <| -- impossible
  Maybe.map Tuple.second <|
  List.Extra.find (\(currentName, _) -> currentName == name) ctx


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
      String.join "\n" <|
      [ "-- DUPLICATED DEFINITION\n"
      , "I found that you defined `" ++ name1.value ++ "` twice. It first appeared here:"
      , location1
      , "It then appeared a second time here:"
      , location2
      , "Hint: Try renaming one of them to avoid duplicated definition."
      ]
    
    _ ->
      let
        src =
          Maybe.withDefault "" <| -- impossible
          List.Extra.getAt currentIndex srcs
      in
      showProblemWithSingleSourceHelper src problem


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

    ExpectingTyFunc ty ->
      [ "-- EXPECTING FUNCTION TYPE\n"
      , "I'm expecting a function type here:"
      , showLocation src ty
      , "but got " ++ showType ty.value ++ "."
      , "Hint: Try changing it to a function type."
      ]

    ExpectingTyBool ty ->
      [ "-- EXPECTING BOOL TYPE\n"
      , "I'm expecting a Bool here:"
      , showLocation src ty
      , "but got " ++ showType ty.value ++ "."
      , "Hint: Try changing it to a Bool."
      ]

    MismatchedType ty1 ty2 ->
      [ "-- EXPECTING FUNCTION TYPE\n"
      , "I'm expecting a " ++ showType ty1.value ++ " type here:"
      , showLocation src ty2
      , "but got " ++ showType ty2.value ++ "."
      , "Hint: Try changing it to a " ++ showType ty1.value ++ "."
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
    
    EAbstraction boundVar _ innerExpr ->
      getFreeVariablesHelper (Set.insert boundVar.value boundVariables) innerExpr.value
    
    EApplication func arg ->
      getFreeVariablesHelper boundVariables func.value ++ getFreeVariablesHelper boundVariables arg.value

    EBool _ ->
      []

    EIf condition thenBranch elseBranch ->
      getFreeVariablesHelper boundVariables condition.value
      ++ getFreeVariablesHelper boundVariables thenBranch.value
      ++ getFreeVariablesHelper boundVariables elseBranch.value


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
