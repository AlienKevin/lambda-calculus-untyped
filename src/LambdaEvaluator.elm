module LambdaEvaluator exposing (evalDefs, evalDef, EvalStrategy(..))


import Dict exposing (Dict)
import LambdaParser exposing (showExpr, fakeDef, Def, Expr(..), Type)
import LambdaChecker exposing (sortDefs)
import Location exposing (withLocation, fakeLocated, Located)
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
  | TmAbstraction (Located String) (Located Type) (Located Term)
  | TmApplication (Located Term) (Located Term)
  | TmBool Bool
  | TmIf (Located Term) (Located Term) (Located Term)


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
      evalTermCallByName ctx term

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
    
    TmAbstraction boundVar boundType t1 ->
      let
        (newNames, newName) =
          pickNewName names boundVar.value
      in
      EAbstraction (withLocation boundVar newName) boundType <| termToExpr newNames t1
  
    TmApplication t1 t2 ->
      EApplication (termToExpr names t1) (termToExpr names t2)

    TmIf condition thenBranch elseBranch ->
      EIf
      (termToExpr names condition)
      (termToExpr names thenBranch)
      (termToExpr names elseBranch)
    
    TmBool bool ->
      EBool bool


-- show de brujin index instead of transforming to names
termToExprDebug : List String -> Located Term -> Located Expr
termToExprDebug names t =
  withLocation t <|
  case t.value of
    TmVariable index ->
      EVariable <| withLocation t <| String.fromInt index
    
    TmAbstraction boundVar boundType t1 ->
      EAbstraction (withLocation boundVar "") boundType <| termToExprDebug names t1
  
    TmApplication t1 t2 ->
      EApplication (termToExprDebug names t1) (termToExprDebug names t2)

    TmIf condition thenBranch elseBranch ->
      EIf
      (termToExprDebug names condition)
      (termToExprDebug names thenBranch)
      (termToExprDebug names elseBranch)
    
    TmBool bool ->
      EBool bool


showTermDebug : Term -> String
showTermDebug tm =
  showExpr <| .value <| termToExprDebug [] <| fakeLocated tm


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

    EAbstraction boundVar boundType e1 ->
      let
        newCtx =
          ctx |>
          Dict.map (\_ s -> termShift 1 0 s) |>
          Dict.insert boundVar.value (withLocation boundVar <| TmVariable 0)
      in
      TmAbstraction boundVar boundType <| exprToTerm newCtx e1

    EApplication e1 e2 ->
      TmApplication
      (exprToTerm ctx e1)
      (exprToTerm ctx e2)

    EIf condition thenBranch elseBranch ->
      TmIf
      (exprToTerm ctx condition)
      (exprToTerm ctx thenBranch)
      (exprToTerm ctx elseBranch)

    EBool bool ->
      TmBool bool


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


evalTermCallByValueHelper : Ctx -> Located Term -> Result (Located Term) (Located Term)
evalTermCallByValueHelper ctx t =
  case t.value of
    TmApplication t1 t2 ->
      if isValue t2 then
        case t1.value of
          TmAbstraction _ _ t12 ->
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

    TmIf condition thenBranch elseBranch ->
      case condition.value of
        TmBool bool ->
          if bool then
            Ok <| unwrapResult <| evalTermCallByValueHelper ctx thenBranch
          else
            Ok <| unwrapResult <| evalTermCallByValueHelper ctx elseBranch

        _ ->
          evalTermCallByValueHelper ctx condition |>
          Result.map
          (\newCondition ->
            withLocation t <| TmIf newCondition thenBranch elseBranch
          )
    
    _ ->
      Err t


evalTermCallByName : Ctx -> Located Term -> Located Term
evalTermCallByName ctx0 t0 =
  let
    eval : Int -> Ctx -> Located Term -> Located Term
    eval iterations ctx t =
      case evalTermCallByNameHelper ctx t of
        Err _ ->
          t
        
        Ok t2 ->
          if iterations >= 10000 then
            t2
          else
            eval (iterations + 1) ctx t2
  in
  eval 0 ctx0 t0


evalTermCallByNameHelper : Ctx -> Located Term -> Result (Located Term) (Located Term)
evalTermCallByNameHelper ctx t =
  case t.value of
    TmApplication t1 t2 ->
      if isValue t2 then
        case t1.value of
          TmAbstraction _ _ t12 ->
            Ok <| termShift -1 0 (termSubst 0 (termShift 1 0 t2) t12)
          
          _ ->
            evalTermCallByNameHelper ctx t1 |>
            Result.map
            (\newT1 ->
              withLocation t <| TmApplication newT1 t2
            )
      else
        evalTermCallByNameHelper ctx t1 |>
        Result.map
        (\newT1 ->
          withLocation t <| TmApplication newT1 t2
        )

    TmIf condition thenBranch elseBranch ->
      case condition.value of
        TmBool bool ->
          if bool then
            Ok <| unwrapResult <| evalTermCallByNameHelper ctx thenBranch
          else
            Ok <| unwrapResult <| evalTermCallByNameHelper ctx elseBranch

        _ ->
          evalTermCallByNameHelper ctx condition |>
          Result.map
          (\newCondition ->
            withLocation t <| TmIf newCondition thenBranch elseBranch
          )
    
    _ ->
      Err t


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
        TmAbstraction _ _ t12 ->
          Ok <| termShift -1 0 (termSubst 0 (termShift 1 0 t2) t12)
        
        _ ->
          combineResults
          (\a b -> withLocation t <| TmApplication a b)
          (evalTermFullHelper ctx t1)
          (evalTermFullHelper ctx t2)
    
    TmAbstraction boundVar boundType t1 ->
      mapBothResults
      (\newT1 ->
        withLocation t <| TmAbstraction boundVar boundType newT1
      )
      (evalTermFullHelper ctx t1)

    TmIf condition thenBranch elseBranch ->
      case condition.value of
        TmBool bool ->
          if bool then
            Ok <| unwrapResult <| evalTermFullHelper ctx thenBranch
          else
            Ok <| unwrapResult <| evalTermFullHelper ctx elseBranch

        _ ->
          evalTermFullHelper ctx condition |>
          Result.map
          (\newCondition ->
            withLocation t <| TmIf newCondition thenBranch elseBranch
          )

    _ ->
      Err t


unwrapResult : Result a a -> a
unwrapResult r =
  case r of
    Ok o ->
      o
    
    Err e ->
      e


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
    TmAbstraction _ _ _ ->
      True

    TmBool _ ->
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
    
    TmAbstraction boundVar boundType t1 ->
      TmAbstraction boundVar boundType <| termShift d (c + 1) t1

    TmApplication t1 t2 ->
      TmApplication (termShift d c t1) (termShift d c t2)

    TmIf condition thenBranch elseBranch ->
      TmIf
      (termShift d c condition)
      (termShift d c thenBranch)
      (termShift d c elseBranch)

    TmBool _ ->
      t.value


termSubst : Int -> Located Term -> Located Term -> Located Term
termSubst j s t =
  withLocation t <|
  case t.value of
    TmVariable k ->
      if j == k then
        s.value
      else
        t.value
    
    TmAbstraction boundVar boundType t1 ->
      TmAbstraction boundVar boundType <| termSubst (j + 1) (termShift 1 0 s) t1

    TmApplication t1 t2 ->
      TmApplication
      (termSubst j s t1)
      (termSubst j s t2)

    TmIf condition thenBranch elseBranch ->
      TmIf
      (termSubst j s condition)
      (termSubst j s thenBranch)
      (termSubst j s elseBranch)

    TmBool _ ->
      t.value
    