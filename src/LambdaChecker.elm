module LambdaChecker exposing (checkDefs, showProblems, showProblemsWithSingleSource, sortDefs, Problem(..))


import LambdaParser exposing (showType, fakeDef, Def, Expr(..), Type(..))
import Dict exposing (Dict)
import Location exposing (showLocation, isFakeLocated, withLocation, Located)
import Set exposing (Set)
import List.Extra


type Problem
  = DuplicatedDefinition (Int, Located String) (Int, Located String)
  | UndefinedVariable (Located String)
  | TypeError (Located String)
  | ExpectingTyFunc (Located Type)
  | ExpectingTyBool (Located Type)
  | ExpectingTyInt (Located Type)
  | MismatchedType (Located Type) (Located Type)


checkDefs : List Def -> List Problem
checkDefs defs =
  case checkDefsNames defs of
    [] ->
      checkDefsTypes defs
    
    namesProblems ->
      namesProblems


checkDefsNames : List Def -> List Problem
checkDefsNames defs =
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
        Tuple.mapFirst ((++) <| checkDef (Dict.filter (\_ name -> name /= def.name) allNames) def)
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


checkDefsTypes : List Def -> List Problem
checkDefsTypes defs =
  let
    sortedDefs =
      sortDefs defs
  in
  Tuple.first <|
  List.foldl
    (\def (problems, ctx) ->
      case getType ctx def.expr of
        Err typeProblem ->
          ( typeProblem :: problems
          , ctx
          )
        
        Ok ty ->
          ( problems
          , addBinding ctx def.name.value ty.value
          )
    )
    ([], [])
    sortedDefs


checkDef : Dict String (Located String) -> Def -> List Problem
checkDef names def =
  checkExpr names def.expr


checkExpr : Dict String (Located String) -> Located Expr -> List Problem
checkExpr names expr =
  case expr.value of
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

    EInt _ ->
      []

    EAdd left right ->
      checkExpr names left
      ++ checkExpr names right

    ESubtract left right ->
      checkExpr names left
      ++ checkExpr names right
    
    EIf condition thenBranch elseBranch ->
      checkExpr names condition
      ++ checkExpr names thenBranch
      ++ checkExpr names elseBranch


type alias Ctx
  = List (String, Type)


getType : Ctx -> Located Expr -> Result Problem (Located Type)
getType ctx expr =
  case expr.value of
    EVariable name ->
      case getTypeFromContext ctx name of
        Nothing ->
          Err <| TypeError name

        Just ty ->
          Ok <| withLocation expr ty

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
                Ok <| withLocation expr <| toType.value
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

    EAdd left right ->
      getTypeFromBinaryInts ctx expr left right

    ESubtract left right ->
      getTypeFromBinaryInts ctx expr left right

    EBool _ ->
      Ok <| withLocation expr TyBool

    EInt _ ->
      Ok <| withLocation expr TyInt


getTypeFromBinaryInts : Ctx -> Located Expr -> Located Expr -> Located Expr -> Result Problem (Located Type)
getTypeFromBinaryInts ctx expr left right =
  getType ctx left |>
    Result.andThen
    (\leftType ->
      case leftType.value of
        TyInt ->
          getType ctx right |>
          Result.andThen
          (\rightType ->
            case rightType.value of
              TyInt ->
                Ok <| withLocation expr TyInt
              
              _ ->
                Err <| ExpectingTyInt rightType
          )
        
        _ ->
          Err <| ExpectingTyInt leftType
    )


areEqualTypes : Type -> Type -> Bool
areEqualTypes ty1 ty2 =
  case (ty1, ty2) of
    (TyBool, TyBool) ->
      True

    (TyInt, TyInt) ->
      True
    
    (TyFunc fromType1 toType1, TyFunc fromType2 toType2) ->
      areEqualTypes fromType1.value fromType2.value
      && areEqualTypes toType1.value toType2.value

    _ ->
      False


addBinding : Ctx -> String -> Type -> Ctx
addBinding ctx name ty =
  (name, ty) :: ctx


getTypeFromContext : Ctx -> Located String -> Maybe Type
getTypeFromContext ctx name =
  Maybe.map Tuple.second <|
  List.Extra.find (\(currentName, _) -> currentName == name.value) ctx


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

    ExpectingTyInt ty ->
      [ "-- EXPECTING INT TYPE\n"
      , "I'm expecting a Int here:"
      , showLocation src ty
      , "but got " ++ showType ty.value ++ "."
      , "Hint: Try changing it to a Int."
      ]

    MismatchedType ty1 ty2 ->
      [ "-- MISMATCHED TYPES\n"
      , "I'm expecting a " ++ showType ty1.value ++ " type here:"
      , showLocation src ty2
      , "but got " ++ showType ty2.value ++ "."
      , "Hint: Try changing it to a " ++ showType ty1.value ++ "."
      ]

    TypeError variable ->
      [ "-- TYPE ERROR\n"
      , "I found a variable that evaluates to a type error:"
      , showLocation src variable
      , "Hint: Try fixing the type error in the definition of `" ++ variable.value ++ "` first."
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
    
    EInt _ ->
      []

    EAdd left right ->
      getFreeVariablesBinaryHelper boundVariables left right

    ESubtract left right ->
      getFreeVariablesBinaryHelper boundVariables left right

    EIf condition thenBranch elseBranch ->
      getFreeVariablesHelper boundVariables condition.value
      ++ getFreeVariablesHelper boundVariables thenBranch.value
      ++ getFreeVariablesHelper boundVariables elseBranch.value


getFreeVariablesBinaryHelper : Set String -> Located Expr -> Located Expr -> List (Located String)
getFreeVariablesBinaryHelper boundVariables left right =
  getFreeVariablesHelper boundVariables left.value
  ++ getFreeVariablesHelper boundVariables right.value



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
