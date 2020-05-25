module LambdaEvaluator exposing (evalDefs, evalDef, EvalStrategy(..))


import Dict exposing (Dict)
import LambdaParser exposing (showExpr, fakeDef, Def(..), Expr(..), Type(..), Comparison(..), PairIndex(..))
import LambdaChecker exposing (sortDefs, getDefName)
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
            -- _ = Debug.log "AL -> ctx" <| ctx
          in
          case resultDef of
            DValue { name, expr } ->
              ( Dict.insert name.value resultDef resultDefs
              , Dict.insert name.value (exprToTerm ctx expr) ctx
              )
            
            DType { name, variants } ->
              ( Dict.insert name.value resultDef resultDefs
              , Dict.foldl
                (\_ (label, ty) nextCtx ->
                  let
                    _ = Debug.log "AL -> label" <| label
                  in
                  ( withLocation label <|
                    TmAbstraction
                    (withLocation ty <| "$variant")
                    ty
                    (withLocation label <| TmVariant label (withLocation ty <| TmVariable 0))
                  ) |>
                  (\tm ->
                    Dict.insert label.value tm nextCtx
                  )
                )
                ctx
                variants
              )
            
            DAlias { name, ty } ->
              ( Dict.insert name.value resultDef resultDefs
              , ctx
              )
        )
        (Dict.empty, Dict.empty)
        sortedDefs
  in
  -- return in the original order, not in the dependency order
  List.map
    (\def ->
      let
        name =
          getDefName def
      in
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
          case otherDef of
            DValue { name, expr } ->
              Dict.insert name.value (exprToTerm ctx expr) ctx
            
            DType { name, variants } ->
              Dict.foldl
                (\_ (label, ty) nextCtx ->
                  ( withLocation label <|
                    TmAbstraction
                    (withLocation ty <| "$variant")
                    (withLocation name <| TyName name Nothing)
                    (withLocation label <| TmVariant label (withLocation ty <| TmVariable 0))
                  ) |>
                  (\tm ->
                    Dict.insert name.value tm nextCtx
                  )
                )
                ctx
                variants
            
            DAlias { name, ty } ->
              ctx
        )
        Dict.empty
        otherDefs
  in
  internalEvalDef strategy resultCtx def


internalEvalDef : EvalStrategy -> Ctx -> Def -> Def
internalEvalDef strategy ctx def =
  case def of
    DValue { name, expr } ->
      DValue
        { name =
          name
        , expr =
          evalExpr strategy ctx expr
        }
    
    _ ->
      def


type Term
  = TmVariable Int
  | TmAbstraction (Located String) (Located Type) (Located Term)
  | TmApplication (Located Term) (Located Term)
  | TmVariant (Located String) (Located Term)
  | TmBool Bool
  | TmInt Int
  | TmUnit
  | TmPair (Located Term) (Located Term)
  | TmPairAccess (Located Term) (Located PairIndex)
  | TmRecord (Dict String (Located String, Located Term))
  | TmRecordAccess (Located Term) (Located String)
  | TmIf (Located Term) (Located Term) (Located Term)
  | TmAdd (Located Term) (Located Term)
  | TmSubtract (Located Term) (Located Term)
  | TmMultiplication (Located Term) (Located Term)
  | TmDivision (Located Term) (Located Term)
  | TmComparison Comparison (Located Term) (Located Term)
  | TmLet (Located String, Located Term) (Located Term)
  | TmCase (Located Term) (Dict String (Located String, Located String, Located Term))


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
      if boundVar.value == "$variant" then
        case t1.value of
          TmVariant variantName _ ->
            EVariable variantName
          
          _ ->
            EVariable (fakeLocated "IMPOSSIBLE")
      else
        let
          (newNames, newBoundVar) =
            pickNewName names boundVar
        in
        EAbstraction newBoundVar boundType <| termToExpr newNames t1
  
    TmApplication t1 t2 ->
      EApplication (termToExpr names t1) (termToExpr names t2)

    TmIf condition thenBranch elseBranch ->
      EIf
      (termToExpr names condition)
      (termToExpr names thenBranch)
      (termToExpr names elseBranch)

    TmLet (label, tm1) tm2 ->
      let
        _ = Debug.log "AL -> label" <| label.value
        _ = Debug.log "AL -> newNames" <| newNames
        (newNames, newLabel) =
          pickNewName names label
      in
      ELet
      (newLabel, termToExpr names tm1)
      (termToExpr newNames tm2)

    TmCase e variants ->
      ECase
      (termToExpr names e)
      ( Dict.map
        (\_ (variantName, valueName, innerTerm) ->
          let
            (newNames, newValueName) =
              pickNewName names valueName
          in
          (variantName, newValueName, termToExpr newNames innerTerm)
        )
        variants
      )

    TmVariant variantName value ->
      EApplication
      (withLocation variantName <| EVariable variantName)
      (termToExpr names value)

    TmPair t1 t2 ->
      termToExprBinaryHelper names EPair t1 t2
    
    TmPairAccess pair index ->
      EPairAccess (termToExpr names pair) index

    TmRecord r ->
      ERecord <|
      Dict.map
        (\_ (label, value) ->
          (label, termToExpr names value)
        )
        r

    TmRecordAccess record label ->
      ERecordAccess (termToExpr names record) label

    TmAdd left right ->
      termToExprBinaryHelper names EAdd left right

    TmSubtract left right ->
      termToExprBinaryHelper names ESubtract left right

    TmMultiplication left right ->
      termToExprBinaryHelper names EMultiplication left right

    TmDivision left right ->
      termToExprBinaryHelper names EDivision left right

    TmComparison comp left right ->
      termToExprBinaryHelper names (EComparison comp) left right
    
    TmBool bool ->
      EBool bool

    TmInt int ->
      EInt int
    
    TmUnit ->
      EUnit


termToExprBinaryHelper : List String -> (Located Expr -> Located Expr -> Expr) -> Located Term -> Located Term -> Expr
termToExprBinaryHelper names f left right =
  f
    (termToExpr names left)
    (termToExpr names right)


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

    TmLet (label, tm1) tm2 ->
      ELet
      (label, termToExprDebug names tm1)
      (termToExprDebug names tm2)

    TmCase e variants ->
      ECase
      (termToExprDebug names e)
      ( Dict.map
        (\_ (variantName, valueName, innerTerm) ->
          let
            (newNames, newValueName) =
              pickNewName names valueName
          in
          (variantName, newValueName, termToExprDebug newNames innerTerm)
        )
        variants
      )

    TmVariant variantName value ->
      EApplication
      (withLocation variantName <| EVariable variantName)
      (termToExprDebug names value)

    TmPair t1 t2 ->
      termToExprDebugBinaryHelper names EPair t1 t2
    
    TmPairAccess pair index ->
      EPairAccess (termToExprDebug names pair) index
    
    TmRecord r ->
      ERecord <|
        Dict.map
          (\_ (label, value) ->
            (label, termToExprDebug names value)
          )
          r

    TmRecordAccess record label ->
      ERecordAccess (termToExprDebug names record) label

    TmAdd left right ->
      termToExprDebugBinaryHelper names EAdd left right
      
    TmSubtract left right ->
      termToExprDebugBinaryHelper names ESubtract left right
    
    TmMultiplication left right ->
      termToExprDebugBinaryHelper names EMultiplication left right
    
    TmDivision left right ->
      termToExprDebugBinaryHelper names EDivision left right

    TmComparison comp left right ->
      termToExprDebugBinaryHelper names (EComparison comp) left right
    
    TmBool bool ->
      EBool bool

    TmInt int ->
      EInt int

    TmUnit ->
      EUnit


termToExprDebugBinaryHelper : List String -> (Located Expr -> Located Expr -> Expr) -> Located Term -> Located Term -> Expr
termToExprDebugBinaryHelper names f left right =
  f
    (termToExprDebug names left)
    (termToExprDebug names right)


showTermDebug : Term -> String
showTermDebug tm =
  showExpr <| .value <| termToExprDebug [] <| fakeLocated tm


pickNewName : List String -> Located String -> (List String, Located String)
pickNewName names boundVar =
  let
    newName =
      if List.member boundVar.value names then
        boundVar.value ++ (String.fromInt <| List.length names)
      else
        boundVar.value
  in
  ( newName :: names
  , withLocation boundVar newName
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

    ELet (label, e1) e2 ->
      let
        innerCtx =
          ctx |>
          Dict.map (\_ s -> termShift 1 0 s) |>
          Dict.insert label.value (withLocation label <| TmVariable 0)
        
        -- _ = Debug.log "AL -> ctx" <| ctx

        -- _ = Debug.log "AL -> innerCtx" <| innerCtx
        
        -- _ = Debug.log "AL -> exprToTerm innerCtx e2" <| showTermDebug <| .value <| exprToTerm innerCtx e2
      in
      TmLet
      (label, exprToTerm ctx e1)
      (exprToTerm innerCtx e2)

    ECase e variants ->
      TmCase
      (exprToTerm ctx e)
      ( Dict.map
        (\_ (variantName, valueName, innerExpr) ->
          let
            innerCtx =
              ctx |>
              Dict.map (\_ s -> termShift 1 0 s) |>
              Dict.insert valueName.value (withLocation valueName <| TmVariable 0)
          in
          (variantName, valueName, exprToTerm innerCtx innerExpr)
        )
        variants
      )

    EPair e1 e2 ->
      exprToTermBinaryHelper ctx TmPair e1 e2

    EPairAccess pair index ->
      TmPairAccess
      (exprToTerm ctx pair)
      index
    
    ERecord r ->
      TmRecord <|
        Dict.map
          (\_ (label, value) ->
            (label, exprToTerm ctx value)
          )
          r

    ERecordAccess record label ->
      TmRecordAccess
      (exprToTerm ctx record)
      label

    EAdd left right ->
      exprToTermBinaryHelper ctx TmAdd left right

    ESubtract left right ->
      exprToTermBinaryHelper ctx TmSubtract left right

    EMultiplication left right ->
      exprToTermBinaryHelper ctx TmMultiplication left right

    EDivision left right ->
      exprToTermBinaryHelper ctx TmDivision left right

    EComparison comp left right ->
      exprToTermBinaryHelper ctx (TmComparison comp) left right

    EBool bool ->
      TmBool bool

    EInt int ->
      TmInt int
    
    EUnit ->
      TmUnit


exprToTermBinaryHelper : Ctx -> (Located Term -> Located Term -> Term) -> Located Expr -> Located Expr -> Term
exprToTermBinaryHelper ctx f left right =
  f
    (exprToTerm ctx left)
    (exprToTerm ctx right)


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

    _ ->
      commonEval evalTermCallByValueHelper ctx t


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


evalTermCallByNameHelper : Ctx -> Located Term -> Result () (Located Term)
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

    _ ->
      commonEval evalTermCallByNameHelper ctx t


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


evalTermFullHelper : Ctx -> Located Term -> Result () (Located Term)
evalTermFullHelper ctx t =
  case t.value of
    TmApplication t1 t2 ->
      case t1.value of
        TmAbstraction _ _ t12 ->
          Ok <| termShift -1 0 (termSubst 0 (termShift 1 0 t2) t12)
        
        _ ->
          let
            a =
              Result.withDefault t1 <| evalTermFullHelper ctx t1
            
            b =
              Result.withDefault t2 <| evalTermFullHelper ctx t2
          in
          Ok <| withLocation t <| TmApplication a b
    
    TmAbstraction boundVar boundType t1 ->
      evalTermFullHelper ctx t1 |>
      Result.map
      (\newT1 ->
        withLocation t <| TmAbstraction boundVar boundType newT1
      )
      
    _ ->
      commonEval evalTermFullHelper ctx t


commonEval : (Ctx -> Located Term -> Result () (Located Term)) -> Ctx -> Located Term -> Result () (Located Term)
commonEval f ctx tm =
  case tm.value of
    TmIf condition thenBranch elseBranch ->
      case condition.value of
        TmBool bool ->
          if bool then
            Ok <| Result.withDefault thenBranch <| f ctx thenBranch
          else
            Ok <| Result.withDefault elseBranch <| f ctx elseBranch

        _ ->
          f ctx condition |>
          Result.map
          (\newCondition ->
            withLocation tm <| TmIf newCondition thenBranch elseBranch
          )

    TmLet (label, tm1) tm2 ->
      -- let
        -- _ = Debug.log "AL -> tm1" <| tm1.value
        -- _ = Debug.log "AL -> termShift 1 0 tm1" <| termShift 1 0 tm1
        -- _ = Debug.log "AL -> (termSubst 0 (termShift 1 0 tm1) tm2)" <| showTermDebug <| (termSubst 0 (termShift 1 0 tm1) tm2).value
        -- _ = Debug.log "AL -> termShift -1 0 (termSubst 0 (termShift 1 0 tm1) tm2)" <| showTermDebug <| .value <| termShift -1 0 (termSubst 0 (termShift 1 0 tm1) tm2)
        -- _ = Debug.log "AL -> tm2" <| showTermDebug tm2.value
      -- in
      if isValue tm1 then
        -- Ok <| termSubst 0 tm1 tm2
        Ok <| termShift -1 0 (termSubst 0 (termShift 1 0 tm1) tm2)
      else
        f ctx tm1 |>
        Result.map
        (\newTm1 ->
          withLocation tm <| TmLet (label, newTm1) tm2
        )

    TmCase tm1 variants ->
      case tm1.value of
        TmVariant variantName value ->
          if isValue value then
            case Dict.get variantName.value variants of
              Just (_, _, tm2) ->
                Ok <| termShift -1 0 (termSubst 0 (termShift 1 0 value) tm2)
              
              Nothing ->
                Err ()
          else
            f ctx tm1 |>
            Result.map
            (\newTm1 ->
              withLocation tm <| TmCase newTm1 variants
            )
        
        _ ->
          f ctx tm1 |>
          Result.map
          (\newTm1 ->
            withLocation tm <| TmCase newTm1 variants
          )
    
    TmVariant variantName value ->
      f ctx value |>
      Result.map
      (\newValue ->
        withLocation tm <| TmVariant variantName newValue
      )


    TmAdd left right ->
      commonEvalBinaryIntsHelper f ctx tm TmAdd (+) left right
    
    TmSubtract left right ->
      commonEvalBinaryIntsHelper f ctx tm TmSubtract (-) left right

    TmMultiplication left right ->
      commonEvalBinaryIntsHelper f ctx tm TmMultiplication (*) left right

    TmDivision left right ->
      commonEvalBinaryIntsHelper f ctx tm TmDivision (//) left right
    
    TmComparison comp left right ->
      case comp of
        CompEQ ->
          commonEvalEqualityHelper f ctx tm (TmComparison comp) areEqualTerms left right
        
        CompNE ->
          commonEvalEqualityHelper f ctx tm (TmComparison comp) (\tm1 tm2 -> not <| areEqualTerms tm1 tm2) left right

        _ ->
          let
            compFunc =
              case comp of
                CompLT ->
                  (<)
                
                CompLE ->
                  (<=)
                
                CompGT ->
                  (>)
                
                _ ->
                  (>=)
          in
          commonEvalComparisonIntsHelper f ctx tm (TmComparison comp) compFunc left right

    TmPair tm1 tm2 ->
      if isValue tm then
        Err ()
      else if isValue tm1 then
        f ctx tm2 |>
        Result.map
        (\newTm2 ->
          withLocation tm <| TmPair tm1 newTm2
        )
      else
        f ctx tm1 |>
        Result.map
        (\newTm1 ->
          withLocation tm <| TmPair newTm1 tm2
        )

    TmPairAccess pair index ->
      case pair.value of
        TmPair tm1 tm2 ->
          if isValue tm1 && isValue tm2 then
            case index.value of
              PairIndexOne ->
                Ok tm1
              
              PairIndexTwo ->
                Ok tm2
          else
            f ctx pair |>
            Result.map
            (\newPair ->
              withLocation tm <| TmPairAccess newPair index
            )

        _ ->
          f ctx pair |>
            Result.map
            (\newPair ->
              withLocation tm <| TmPairAccess newPair index
            )

    TmRecord r ->
      if isValue tm then
        Err ()
      else
        Ok <|
        withLocation tm <|
        TmRecord <|
        Dict.map
          (\_ (label, value) ->
            if isValue value then
              (label, value)
            else
              (label
              , case f ctx value of
                Ok nextValue ->
                  nextValue
                Err _ ->
                  value
              )
          )
          r

    TmRecordAccess record label ->
      if isValue record then
        case record.value of
          TmRecord r ->
            case Dict.get label.value r of
              Just (_, value) ->
                Ok value
              
              Nothing ->
                Err ()
          
          _ ->
            Err ()
      else
        f ctx record |>
          Result.map
          (\newRecord ->
            withLocation tm <| TmRecordAccess newRecord label
          )

    _ ->
      Err ()


areEqualTerms : Term -> Term -> Bool
areEqualTerms tm1 tm2 =
  case (tm1, tm2) of
    (TmBool b1, TmBool b2) ->
      b1 == b2
    
    (TmInt i1, TmInt i2) ->
      i1 == i2
    
    (TmUnit, TmUnit) ->
      True
    
    (TmPair p1tm1 p1tm2, TmPair p2tm1 p2tm2) ->
      areEqualTerms p1tm1.value p2tm1.value
      && areEqualTerms p1tm2.value p2tm2.value
    
    (TmRecord r1, TmRecord r2) ->
      if (Dict.isEmpty <| Dict.diff r1 r2)
      && (Dict.isEmpty <| Dict.diff r2 r1)
      then
        List.all
          (\(k, (_, v1)) ->
            case Dict.get k r2 of
              Just (_, v2) ->
                areEqualTerms v1.value v2.value
              
              Nothing ->
                False
          )
          (Dict.toList r1)
      else
        False
    
    (TmVariant n1 v1, TmVariant n2 v2) ->
      (n1.value == n2.value)
      && areEqualTerms v1.value v2.value
    
    _ ->
      False

commonEvalEqualityHelper :
  (Ctx -> Located Term -> Result () (Located Term))
  -> Ctx
  -> Located Term
  -> (Located Term -> Located Term -> Term)
  -> (Term -> Term -> Bool)
  -> Located Term
  -> Located Term
  -> Result () (Located Term)
commonEvalEqualityHelper f ctx tm tmName op left right =
  if isValue left && isValue right then
    Ok <| withLocation tm <| TmBool <| op left.value right.value
  else if isValue left then
    f ctx right |>
    Result.map
    (\newRight ->
      withLocation tm <| tmName left newRight
    )
  else
    f ctx left |>
    Result.map
    (\newLeft ->
      withLocation tm <| tmName newLeft right
    )


commonEvalComparisonIntsHelper :
  (Ctx -> Located Term -> Result () (Located Term))
  -> Ctx
  -> Located Term
  -> (Located Term -> Located Term -> Term)
  -> (Int -> Int -> Bool)
  -> Located Term
  -> Located Term
  -> Result () (Located Term)
commonEvalComparisonIntsHelper f ctx tm tmName op left right =
  case left.value of
    TmInt leftValue ->
      case right.value of
        TmInt rightValue ->
          Ok <| withLocation tm <| TmBool <| op leftValue rightValue
        
        _ ->
          f ctx right |>
          Result.map
          (\newRight ->
            withLocation tm <| tmName left newRight
          )

    _ ->
      f ctx left |>
      Result.map
      (\newLeft ->
        withLocation tm <| tmName newLeft right
      )


commonEvalBinaryIntsHelper :
  (Ctx -> Located Term -> Result () (Located Term))
  -> Ctx
  -> Located Term
  -> (Located Term -> Located Term -> Term)
  -> (Int -> Int -> Int)
  -> Located Term
  -> Located Term
  -> Result () (Located Term)
commonEvalBinaryIntsHelper f ctx tm tmName op left right =
  case left.value of
    TmInt leftValue ->
      case right.value of
        TmInt rightValue ->
          Ok <| withLocation tm <| TmInt <| op leftValue rightValue
        
        _ ->
          f ctx right |>
          Result.map
          (\newRight ->
            withLocation tm <| tmName left newRight
          )

    _ ->
      f ctx left |>
      Result.map
      (\newLeft ->
        withLocation tm <| tmName newLeft right
      )


isValue : Located Term -> Bool
isValue t =
  case t.value of
    TmAbstraction _ _ _ ->
      True

    TmBool _ ->
      True
    
    TmInt _ ->
      True
    
    TmPair tm1 tm2 ->
      isValue tm1 && isValue tm2

    TmRecord r ->
      List.all
      (\(_, (_, value)) ->
        isValue value
      )
      (Dict.toList r)

    TmVariant _ value ->
      isValue value
    
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

    TmLet (label, tm1) tm2 ->
      TmLet
      (label, termShift d c tm1)
      (termShift d (c + 1) tm2)
    
    TmCase tm variants ->
      TmCase
      (termShift d c tm)
      ( Dict.map
        (\_ (variantName, valueName, innerTerm) ->
          (variantName, valueName, termShift d (c + 1) innerTerm)
        )
        variants
      )
    
    TmVariant variantName value ->
      TmVariant
      variantName
      (termShift d c value)

    TmPair t1 t2 ->
      termShiftBinaryHelper d c TmPair t1 t2

    TmPairAccess pair index ->
      TmPairAccess (termShift d c pair) index

    TmRecord r ->
      TmRecord <|
      Dict.map
        (\_ (label, value) ->
          (label, termShift d c value)
        )
        r

    TmRecordAccess record label ->
      TmRecordAccess (termShift d c record) label

    TmAdd left right ->
      termShiftBinaryHelper d c TmAdd left right

    TmSubtract left right ->
      termShiftBinaryHelper d c TmSubtract left right

    TmMultiplication left right ->
      termShiftBinaryHelper d c TmMultiplication left right

    TmDivision left right ->
      termShiftBinaryHelper d c TmDivision left right

    TmComparison comp left right ->
      termShiftBinaryHelper d c (TmComparison comp) left right

    TmBool _ ->
      t.value

    TmInt _ ->
      t.value
    
    TmUnit ->
      t.value


termShiftBinaryHelper : Int -> Int -> (Located Term -> Located Term -> Term) -> Located Term -> Located Term -> Term
termShiftBinaryHelper d c f left right =
  f
  (termShift d c left)
  (termShift d c right)


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
    
    TmLet (label, tm1) tm2 ->
      TmLet
      (label, termSubst j s tm1)
      (termSubst (j + 1) (termShift 1 0 s) tm2)
    
    TmCase tm variants ->
      TmCase
      (termSubst j s tm)
      ( Dict.map
        (\_ (variantName, valueName, innerTerm) ->
          (variantName, valueName, termSubst (j + 1) (termShift 1 0 s) innerTerm)
        )
        variants
      )
    
    TmVariant variantName value ->
      TmVariant
      variantName
      (termSubst j s value)
    
    TmAdd left right ->
      termSubstBinaryHelper j s TmAdd left right

    TmSubtract left right ->
      termSubstBinaryHelper j s TmSubtract left right

    TmMultiplication left right ->
      termSubstBinaryHelper j s TmMultiplication left right
    
    TmDivision left right ->
      termSubstBinaryHelper j s TmDivision left right

    TmComparison comp left right ->
      termSubstBinaryHelper j s (TmComparison comp) left right

    TmPair t1 t2 ->
      termSubstBinaryHelper j s TmPair t1 t2

    TmPairAccess pair index ->
      TmPairAccess (termSubst j s pair) index

    TmRecord r ->
      TmRecord <|
      Dict.map
        (\_ (label, value) ->
          (label, termSubst j s value)
        )
        r

    TmRecordAccess record label ->
      TmRecordAccess (termSubst j s record) label

    TmBool _ ->
      t.value

    TmInt _ ->
      t.value
    
    TmUnit ->
      t.value


termSubstBinaryHelper : Int -> Located Term -> (Located Term -> Located Term -> Term) -> Located Term -> Located Term -> Term
termSubstBinaryHelper j s f left right =
  f
  (termSubst j s left)
  (termSubst j s right)
    