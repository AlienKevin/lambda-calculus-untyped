module LambdaEvaluator exposing (evalDefs, evalDef, EvalStrategy(..))


import Dict exposing (Dict)
import LambdaParser exposing (Def, Expr(..))
import LambdaChecker exposing (sortDefs)
import Location exposing (withLocation, Located)


type EvalStrategy
  = CallByName
  | CallByValue


evalDefs : EvalStrategy -> List Def -> List Def
evalDefs strategy defs =
  let
    sortedDefs =
      sortDefs defs
  in
  Tuple.first <|
  List.foldl
    (\def (resultDefs, substs) ->
      let
        resultDef =
          internalEvalDef strategy substs def
      in  
      ( resultDef :: resultDefs
      , Dict.insert def.name.value resultDef.expr substs
      )
    )
    ([], Dict.empty)
    sortedDefs


evalDef : EvalStrategy -> List Def -> Def -> Def
evalDef strategy otherDefs def =
  let
    substs =
      List.foldl
        (\otherDef substitutions ->
          Dict.insert otherDef.name.value otherDef.expr substitutions
        )
        Dict.empty
        otherDefs
  in
  internalEvalDef strategy substs def


internalEvalDef : EvalStrategy -> Dict String (Located Expr) -> Def -> Def
internalEvalDef strategy substs def =
  { def
    | expr =
      withLocation def.expr <| evalExpr strategy substs def.expr.value
  }


evalExpr : EvalStrategy -> Dict String (Located Expr) -> Expr -> Expr
evalExpr strategy substs expr =
  case strategy of
    CallByName ->
      evalExprCallByName substs expr

    CallByValue ->
      evalExprCallByValue substs expr


evalExprCallByName : Dict String (Located Expr) -> Expr -> Expr
evalExprCallByName substs expr =
  case expr of
    EApplication func arg ->
      case func.value of
        EAbstraction boundVar innerExpr ->
          let
            substitutedExpr =
              evalExprCallByName (Dict.insert boundVar.value arg substs) innerExpr.value
          in
          if substitutedExpr == innerExpr.value then
            substitutedExpr
          else
            evalExprCallByName substs <| substitutedExpr
        
        _ ->
          let
            substitutedFunc = 
              withLocation func <| evalExprCallByName substs func.value
          in
          if substitutedFunc.value == func.value then
            EApplication substitutedFunc arg
          else
            evalExprCallByName substs <| EApplication substitutedFunc arg

    EVariable name ->
      case Dict.get name.value substs of
        Nothing ->
          expr
        
        Just subst ->
          subst.value

    EAbstraction _ _ ->
      expr


evalExprCallByValue : Dict String (Located Expr) -> Expr -> Expr
evalExprCallByValue substs expr =
  case expr of
    EApplication func arg ->
      case func.value of
        EAbstraction boundVar innerExpr ->
          let
            substitutedExpr =
              evalExprCallByValue (Dict.insert boundVar.value arg substs) innerExpr.value
          in
          if substitutedExpr == innerExpr.value then
            substitutedExpr
          else
            evalExprCallByValue substs <| substitutedExpr
        
        _ ->
          let
            substitutedFunc = 
              withLocation func <| evalExprCallByValue substs func.value
          in
          if substitutedFunc.value == func.value then
            EApplication substitutedFunc arg
          else
            evalExprCallByValue substs <| EApplication substitutedFunc arg

    EVariable name ->
      case Dict.get name.value substs of
        Nothing ->
          expr
        
        Just subst ->
          subst.value

    EAbstraction boundVar innerExpr ->
      EAbstraction
        boundVar
        (withLocation innerExpr <| evalExprCallByValue (Dict.remove boundVar.value substs) innerExpr.value)