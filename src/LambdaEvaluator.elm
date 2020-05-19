module LambdaEvaluator exposing (evalDefs, evalDef, EvalStrategy(..))


import Dict exposing (Dict)
import LambdaParser exposing (fakeDef, Def, Expr(..))
import LambdaChecker exposing (sortDefs)
import Location exposing (withLocation, Located)
import List.Extra


type EvalStrategy
  = CallByName
  | CallByValue
  | FullEvaluation


evalDefs : EvalStrategy -> List Def -> List Def
evalDefs strategy defs =
  let
    sortedDefs =
      sortDefs defs
    
    resultSortedDefs =
      Tuple.first <|
      List.foldl
        (\def (resultDefs, ctx) ->
          let
            resultDef =
              internalEvalDef strategy ctx def
          in  
          ( Dict.insert resultDef.name.value resultDef resultDefs
          , Dict.insert def.name.value (exprToTerm ctx resultDef.expr) ctx
          )
        )
        (Dict.empty, Dict.empty)
        sortedDefs
  in
  -- return in the original order, not in the dependency order
  List.map
    (\{ name } ->
      Maybe.withDefault fakeDef <| -- impossible
      Dict.get name.value resultSortedDefs
    )
    defs


evalDef : EvalStrategy -> List Def -> Def -> Def
evalDef strategy otherDefs def =
  let
    resultCtx =
      List.foldl
        (\otherDef ctx ->
          Dict.insert otherDef.name.value (exprToTerm ctx otherDef.expr) ctx
        )
        Dict.empty
        otherDefs
  in
  internalEvalDef strategy resultCtx def


internalEvalDef : EvalStrategy -> Ctx -> Def -> Def
internalEvalDef strategy ctx def =
  { def
    | expr =
      evalExpr strategy ctx def.expr
  }


type Term
  = TmVariable Int
  | TmAbstraction (Located String) (Located Term)
  | TmApplication (Located Term) (Located Term)


type alias Ctx =
  Dict String (Located Term)


evalExpr : EvalStrategy -> Ctx -> Located Expr -> Located Expr
evalExpr strategy ctx expr =
  let
    term =
      exprToTerm ctx expr
  in
  termToExpr [] <|
  case strategy of
    CallByName ->
      evalTermCallByValue ctx term

    CallByValue ->
      evalTermCallByValue ctx term
    
    FullEvaluation ->
      evalTermFull ctx term


termToExpr : List String -> Located Term -> Located Expr
termToExpr names t =
  withLocation t <|
  case t.value of
    TmVariable index ->
      EVariable <| withLocation t <| Maybe.withDefault "IMPOSSIBLE" <| List.Extra.getAt index names
    
    TmAbstraction boundVar t1 ->
      let
        (newNames, newName) =
          pickNewName names boundVar.value
      in
      EAbstraction (withLocation boundVar newName) <| termToExpr newNames t1
  
    TmApplication t1 t2 ->
      EApplication (termToExpr names t1) (termToExpr names t2)


-- show de brujin index instead of transforming to names
termToExprDebug : List String -> Located Term -> Located Expr
termToExprDebug names t =
  withLocation t <|
  case t.value of
    TmVariable index ->
      EVariable <| withLocation t <| String.fromInt index
    
    TmAbstraction boundVar t1 ->
      EAbstraction (withLocation boundVar "") <| termToExpr names t1
  
    TmApplication t1 t2 ->
      EApplication (termToExpr names t1) (termToExpr names t2)


pickNewName : List String -> String -> (List String, String)
pickNewName names boundVar =
  let
    newName =
      if List.member boundVar names then
        boundVar ++ (String.fromInt <| List.length names)
      else
        boundVar  
  in
  ( newName :: names
  , newName
  )


exprToTerm : Ctx -> Located Expr -> Located Term
exprToTerm ctx expr =
  withLocation expr <|
  case expr.value of
    EVariable name ->
      case Dict.get name.value ctx of
        Nothing -> -- impossible
          TmVariable -1
        
        Just s ->
          s.value

    EAbstraction boundVar e1 ->
      let
        newCtx =
          ctx |>
          Dict.map (\_ s -> termShift 1 0 s) |>
          Dict.insert boundVar.value (withLocation boundVar <| TmVariable 0)
      in
      TmAbstraction boundVar <| exprToTerm newCtx e1

    EApplication e1 e2 ->
      TmApplication
      (exprToTerm ctx e1)
      (exprToTerm ctx e2)


evalTermCallByValue : Ctx -> Located Term -> Located Term
evalTermCallByValue ctx0 t0 =
  let
    eval : Int -> Ctx -> Located Term -> Located Term
    eval iterations ctx t =
      case evalTermCallByValueHelper ctx t of
        Err _ ->
          t
        
        Ok t2 ->
          if iterations >= 10000 then
            t2
          else
            eval (iterations + 1) ctx t2
  in
  eval 0 ctx0 t0


evalTermCallByValueHelper : Ctx -> Located Term -> Result () (Located Term)
evalTermCallByValueHelper ctx t =
  case t.value of
    TmApplication t1 t2 ->
      if isValue t2 then
        case t1.value of
          TmAbstraction _ t12 ->
            Ok <| termShift -1 0 (termSubst 0 (termShift 1 0 t2) t12)
          
          _ ->
            evalTermCallByValueHelper ctx t1 |>
            Result.map
            (\newT1 ->
              withLocation t <| TmApplication newT1 t2
            )
      else if isValue t1 then
        evalTermCallByValueHelper ctx t2 |>
        Result.map
        (\newT2 ->
          withLocation t <| TmApplication t1 newT2
        )
      else
        evalTermCallByValueHelper ctx t1 |>
        Result.map
        (\newT1 ->
          withLocation t <| TmApplication newT1 t2
        )
    
    _ ->
      Err ()


evalTermFull : Ctx -> Located Term -> Located Term
evalTermFull ctx0 t0 =
  let
    eval : Int -> Ctx -> Located Term -> Located Term
    eval iterations ctx t =
      case evalTermFullHelper ctx t of
        Err _ ->
          t
        
        Ok t2 ->
          if iterations >= 10000 then
            t2
          else
            eval (iterations + 1) ctx t2
  in
  eval 0 ctx0 t0


evalTermFullHelper : Ctx -> Located Term -> Result (Located Term) (Located Term)
evalTermFullHelper ctx t =
  case t.value of
    TmApplication t1 t2 ->
      case t1.value of
        TmAbstraction _ t12 ->
          Ok <| termShift -1 0 (termSubst 0 (termShift 1 0 t2) t12)
        
        _ ->
          combineResults
          (\a b -> withLocation t <| TmApplication a b)
          (evalTermFullHelper ctx t1)
          (evalTermFullHelper ctx t2)
    
    TmAbstraction boundVar t1 ->
      mapBothResults
      (\newT1 ->
        withLocation t <| TmAbstraction boundVar newT1
      )
      (evalTermFullHelper ctx t1)

    _ ->
      Err t


mapBothResults : (a -> b) -> Result a a -> Result b b
mapBothResults f r =
  case r of
    Ok o ->
      Ok <| f o
    
    Err e ->
      Err <| f e

combineResults : (a -> a -> b) -> Result a a -> Result a a -> Result b b
combineResults f r1 r2 =
  -- let
    -- _ = Debug.log "AL -> r1" <| r1
    -- _ = Debug.log "AL -> r2" <| r2
  -- in
  case (r1, r2) of
    (Ok o1, Ok o2) ->
      Ok <| f o1 o2
    
    (Ok o1, Err e2) ->
      Ok <| f o1 e2
    
    (Err e1, Ok o2) ->
      Ok <| f e1 o2
    
    (Err e1, Err e2) ->
      Err <| f e1 e2


isValue : Located Term -> Bool
isValue t =
  case t.value of
    TmAbstraction _ _ ->
      True
    
    _ ->
      False


termShift : Int -> Int -> Located Term -> Located Term
termShift d c t =
  withLocation t <|
  case t.value of
    TmVariable k ->
      if k < c then
        TmVariable k
      else
        TmVariable <| k + d
    
    TmAbstraction boundVar t1 ->
      TmAbstraction boundVar <| termShift d (c + 1) t1

    TmApplication t1 t2 ->
      TmApplication (termShift d c t1) (termShift d c t2)


termSubst : Int -> Located Term -> Located Term -> Located Term
termSubst j s t =
  withLocation t <|
  case t.value of
    TmVariable k ->
      if j == k then
        s.value
      else
        t.value
    
    TmAbstraction boundVar t1 ->
      TmAbstraction boundVar <| termSubst (j + 1) (termShift 1 0 s) t1

    TmApplication t1 t2 ->
      TmApplication
      (termSubst j s t1)
      (termSubst j s t2)
    