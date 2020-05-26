module LambdaEvaluator exposing (evalDefs, evalDef, EvalStrategy(..))


import LambdaParser exposing (termShift, termSubst, typeSubst, Def(..), Term(..), Type(..), Comparison(..), PairIndex(..))
import Location exposing (withLocation, Located)
import Dict


type EvalStrategy
  = CallByName
  | CallByValue
  | FullEvaluation


evalDefs : EvalStrategy -> List Def -> List Def
evalDefs strategy sortedDefs =
  List.map
    (\def ->
      internalEvalDef strategy def
    )
    sortedDefs


evalDef : EvalStrategy -> List Def -> Def -> Def
evalDef strategy otherDefs def =
  internalEvalDef strategy  def


internalEvalDef : EvalStrategy -> Def -> Def
internalEvalDef strategy def =
  case def of
    DValue { name, term } ->
      DValue
        { name =
          name
        , term =
          evalTerm strategy term
        }
    
    _ ->
      def


evalTerm : EvalStrategy -> Located Term -> Located Term
evalTerm strategy term =
  case strategy of
    CallByName ->
      evalTermCallByName term

    CallByValue ->
      evalTermCallByValue term
    
    FullEvaluation ->
      evalTermFull term


evalTermCallByValue : Located Term -> Located Term
evalTermCallByValue t0 =
  let
    eval : Int -> Located Term -> Located Term
    eval iterations t =
      case evalTermCallByValueHelper t of
        Err _ ->
          t
        
        Ok t2 ->
          if iterations >= 10000 then
            t2
          else
            eval (iterations + 1) t2
  in
  eval 0 t0


evalTermCallByValueHelper : Located Term -> Result () (Located Term)
evalTermCallByValueHelper t =
  case t.value of
    TmApplication t1 t2 ->
      if isValue t2 then
        case t1.value of
          TmAbstraction _ _ t12 ->
            Ok <| termShift -1 0 (termSubst 0 (termShift 1 0 t2) t12)
          
          _ ->
            evalTermCallByValueHelper t1 |>
            Result.map
            (\newT1 ->
              withLocation t <| TmApplication newT1 t2
            )
      else if isValue t1 then
        evalTermCallByValueHelper t2 |>
        Result.map
        (\newT2 ->
          withLocation t <| TmApplication t1 newT2
        )
      else
        evalTermCallByValueHelper t1 |>
        Result.map
        (\newT1 ->
          withLocation t <| TmApplication newT1 t2
        )

    _ ->
      commonEval evalTermCallByValueHelper t


evalTermCallByName : Located Term -> Located Term
evalTermCallByName t0 =
  let
    eval : Int -> Located Term -> Located Term
    eval iterations t =
      case evalTermCallByNameHelper t of
        Err _ ->
          t
        
        Ok t2 ->
          if iterations >= 10000 then
            t2
          else
            eval (iterations + 1) t2
  in
  eval 0 t0


evalTermCallByNameHelper : Located Term -> Result () (Located Term)
evalTermCallByNameHelper t =
  case t.value of
    TmApplication t1 t2 ->
      if isValue t2 then
        case t1.value of
          TmAbstraction _ _ t12 ->
            Ok <| termShift -1 0 (termSubst 0 (termShift 1 0 t2) t12)
          
          _ ->
            evalTermCallByNameHelper t1 |>
            Result.map
            (\newT1 ->
              withLocation t <| TmApplication newT1 t2
            )
      else
        evalTermCallByNameHelper t1 |>
        Result.map
        (\newT1 ->
          withLocation t <| TmApplication newT1 t2
        )

    _ ->
      commonEval evalTermCallByNameHelper t


evalTermFull : Located Term -> Located Term
evalTermFull t0 =
  let
    eval : Int -> Located Term -> Located Term
    eval iterations t =
      case evalTermFullHelper t of
        Err _ ->
          t
        
        Ok t2 ->
          if iterations >= 10000 then
            t2
          else
            eval (iterations + 1) t2
  in
  eval 0 t0


evalTermFullHelper : Located Term -> Result () (Located Term)
evalTermFullHelper t =
  case t.value of
    TmApplication t1 t2 ->
      case t1.value of
        TmAbstraction _ _ t12 ->
          Ok <| termShift -1 0 (termSubst 0 (termShift 1 0 t2) t12)
        
        _ ->
          let
            a =
              Result.withDefault t1 <| evalTermFullHelper t1
            
            b =
              Result.withDefault t2 <| evalTermFullHelper t2
          in
          Ok <| withLocation t <| TmApplication a b
    
    TmAbstraction boundVar boundType t1 ->
      evalTermFullHelper t1 |>
      Result.map
      (\newT1 ->
        withLocation t <| TmAbstraction boundVar boundType newT1
      )
      
    _ ->
      commonEval evalTermFullHelper t


commonEval : (Located Term -> Result () (Located Term)) -> Located Term -> Result () (Located Term)
commonEval f tm =
  case tm.value of
    TmTApplication tm1 ty ->
      case tm1.value of
        TmTAbstraction _ tm12 ->
          Ok tm12
        
        _ ->
          f tm1 |>
          Result.map
          (\newTm1 ->
            withLocation tm <| TmTApplication newTm1 ty
          )


    TmIf condition thenBranch elseBranch ->
      case condition.value of
        TmBool bool ->
          if bool then
            Ok <| Result.withDefault thenBranch <| f thenBranch
          else
            Ok <| Result.withDefault elseBranch <| f elseBranch

        _ ->
          f condition |>
          Result.map
          (\newCondition ->
            withLocation tm <| TmIf newCondition thenBranch elseBranch
          )

    TmLet (label, tm1) tm2 ->
      if isValue tm1 then
        Ok <| termShift -1 0 (termSubst 0 (termShift 1 0 tm1) tm2)
      else
        f tm1 |>
        Result.map
        (\newTm1 ->
          withLocation tm <| TmLet (label, newTm1) tm2
        )

    TmCase tm1 variants ->
      case tm1.value of
        TmVariant variantName value _ ->
          if isValue value then
            case Dict.get variantName.value variants of
              Just (_, _, tm2) ->
                Ok <| termShift -1 0 (termSubst 0 (termShift 1 0 value) tm2)
              
              Nothing ->
                Err ()
          else
            f tm1 |>
            Result.map
            (\newTm1 ->
              withLocation tm <| TmCase newTm1 variants
            )
        
        _ ->
          f tm1 |>
          Result.map
          (\newTm1 ->
            withLocation tm <| TmCase newTm1 variants
          )
    
    TmVariant variantName value customType ->
      f value |>
      Result.map
      (\newValue ->
        withLocation tm <| TmVariant variantName newValue customType
      )


    TmAdd left right ->
      commonEvalBinaryIntsHelper f tm TmAdd (+) left right
    
    TmSubtract left right ->
      commonEvalBinaryIntsHelper f tm TmSubtract (-) left right

    TmMultiplication left right ->
      commonEvalBinaryIntsHelper f tm TmMultiplication (*) left right

    TmDivision left right ->
      commonEvalBinaryIntsHelper f tm TmDivision (//) left right
    
    TmComparison comp left right ->
      case comp of
        CompEQ ->
          commonEvalEqualityHelper f tm (TmComparison comp) areEqualTerms left right
        
        CompNE ->
          commonEvalEqualityHelper f tm (TmComparison comp) (\tm1 tm2 -> not <| areEqualTerms tm1 tm2) left right

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
          commonEvalComparisonIntsHelper f tm (TmComparison comp) compFunc left right

    TmPair tm1 tm2 ->
      if isValue tm then
        Err ()
      else if isValue tm1 then
        f tm2 |>
        Result.map
        (\newTm2 ->
          withLocation tm <| TmPair tm1 newTm2
        )
      else
        f tm1 |>
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
            f pair |>
            Result.map
            (\newPair ->
              withLocation tm <| TmPairAccess newPair index
            )

        _ ->
          f pair |>
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
              , case f value of
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
        f record |>
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
    
    (TmChar c1, TmChar c2) ->
      c1 == c2
    
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
    
    (TmVariant n1 v1 _, TmVariant n2 v2 _) ->
      (n1.value == n2.value)
      && areEqualTerms v1.value v2.value
    
    _ ->
      False

commonEvalEqualityHelper :
  (Located Term -> Result () (Located Term))
  -> Located Term
  -> (Located Term -> Located Term -> Term)
  -> (Term -> Term -> Bool)
  -> Located Term
  -> Located Term
  -> Result () (Located Term)
commonEvalEqualityHelper f tm tmName op left right =
  if isValue left && isValue right then
    Ok <| withLocation tm <| TmBool <| op left.value right.value
  else if isValue left then
    f right |>
    Result.map
    (\newRight ->
      withLocation tm <| tmName left newRight
    )
  else
    f left |>
    Result.map
    (\newLeft ->
      withLocation tm <| tmName newLeft right
    )


commonEvalComparisonIntsHelper :
  (Located Term -> Result () (Located Term))
  -> Located Term
  -> (Located Term -> Located Term -> Term)
  -> (Int -> Int -> Bool)
  -> Located Term
  -> Located Term
  -> Result () (Located Term)
commonEvalComparisonIntsHelper f tm tmName op left right =
  case left.value of
    TmInt leftValue ->
      case right.value of
        TmInt rightValue ->
          Ok <| withLocation tm <| TmBool <| op leftValue rightValue
        
        _ ->
          f right |>
          Result.map
          (\newRight ->
            withLocation tm <| tmName left newRight
          )

    _ ->
      f left |>
      Result.map
      (\newLeft ->
        withLocation tm <| tmName newLeft right
      )


commonEvalBinaryIntsHelper :
  (Located Term -> Result () (Located Term))
  -> Located Term
  -> (Located Term -> Located Term -> Term)
  -> (Int -> Int -> Int)
  -> Located Term
  -> Located Term
  -> Result () (Located Term)
commonEvalBinaryIntsHelper f tm tmName op left right =
  case left.value of
    TmInt leftValue ->
      case right.value of
        TmInt rightValue ->
          Ok <| withLocation tm <| TmInt <| op leftValue rightValue
        
        _ ->
          f right |>
          Result.map
          (\newRight ->
            withLocation tm <| tmName left newRight
          )

    _ ->
      f left |>
      Result.map
      (\newLeft ->
        withLocation tm <| tmName newLeft right
      )


isValue : Located Term -> Bool
isValue t =
  case t.value of
    TmAbstraction _ _ _ ->
      True

    TmTAbstraction _ _ ->
      True

    TmBool _ ->
      True
    
    TmInt _ ->
      True
    
    TmChar _ ->
      True
    
    TmUnit ->
      True
    
    TmPair tm1 tm2 ->
      isValue tm1 && isValue tm2

    TmRecord r ->
      List.all
      (\(_, (_, value)) ->
        isValue value
      )
      (Dict.toList r)

    TmVariant _ value _ ->
      isValue value
    
    _ ->
      False
