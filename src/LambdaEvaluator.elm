module LambdaEvaluator exposing (evalDefs)


import Dict exposing (Dict)
import LambdaParser exposing (Def, Expr(..))
import LambdaChecker exposing (sortDefs)
import Location exposing (withLocation, Located)


evalDefs : List Def -> List Def
evalDefs defs =
  let
    sortedDefs =
      sortDefs defs
  in
  Tuple.first <|
  List.foldl
    (\def (resultDefs, substs) ->
      let
        resultDef =
          evalDef substs def
      in  
      ( resultDef :: resultDefs
      , Dict.insert def.name.value resultDef.expr substs
      )
    )
    ([], Dict.empty)
    sortedDefs


evalDef : Dict String (Located Expr) -> Def -> Def
evalDef substs def =
  { def
    | expr =
      withLocation def.expr <| evalExpr substs def.expr.value
  }


evalExpr : Dict String (Located Expr) -> Expr -> Expr
evalExpr substs expr =
  let
    _ = Debug.log "expr" <| LambdaParser.showExpr expr
    _ = Debug.log "AL -> substs" <| substs
    _ = Debug.log "AL -> Dict.size substs" <| Dict.size substs
  in
  case expr of
    EApplication func arg ->
      case func.value of
        EAbstraction boundVar innerExpr ->
          let
            substitutedExpr =
              evalExpr (Dict.insert boundVar.value arg substs) innerExpr.value
          in
          if substitutedExpr == innerExpr.value then
            substitutedExpr
          else
            evalExpr substs <| substitutedExpr
        
        _ ->
          let
            substitutedFunc = 
              withLocation func <| evalExpr substs func.value
          in
          if substitutedFunc.value == func.value then
            EApplication substitutedFunc arg
          else
            evalExpr substs <| EApplication substitutedFunc arg

    EVariable name ->
      case Dict.get name.value substs of
        Nothing ->
          expr
        
        Just subst ->
          subst.value

    EAbstraction boundVar innerExpr ->
      EAbstraction
        boundVar
        (withLocation innerExpr <| evalExpr (Dict.remove boundVar.value substs) innerExpr.value)
