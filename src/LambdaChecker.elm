module LambdaChecker exposing (checkDefs, showProblems, showProblemsWithSingleSource, getDefName, Problem(..))


import LambdaParser exposing (showType, showCustomType, getFreeTypes, typeSubst, fakeType, Def(..), Term(..), Type(..), Comparison(..), PairIndex(..))
import Dict exposing (Dict)
import Location exposing (showLocation, isFakeLocated, withLocation, fakeLocated, Located)
import List.Extra


type Problem
  = DuplicatedDefinition (Int, Located String) (Int, Located String)
  | UndefinedVariable (Located String)
  | UndefinedType (Located String)
  | MisusedVariantAsType (Located String) (Located String)
  | MisusedTypeAsVariable (Located String) (Located String)
  | TypeError (Located String)
  | ExpectingVariantsInECase (List (Located String)) (Located Term)
  | ExpectingVariantInTyCustom (Located String) ((Located String), (Dict String (Located String, Located Type)))
  | ExpectingTyAll (Located Type)
  | ExpectingTyCustom (Located Type)
  | ExpectingTyFunc (Located Type)
  | ExpectingTyBool (Located Type)
  | ExpectingTyInt (Located Type)
  | ExpectingTyPair (Located Type)
  | ExpectingTyRecord (Located Type)
  | ExpectingTyRecordWithLabel (Located String) (Located Type)
  | MismatchedType (Located Type) (Located Type)
  | CompareTyFunc (Located Type) (Located Type)


type alias Ctx =
  List (String, Binding)


type Binding
  = VarBind (Located Type)
  | TyVarBind


fakeBinding : Binding
fakeBinding =
  VarBind <| fakeLocated <| TyName <| fakeLocated "IMPOSSIBLE"


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
    allDeclaredNames =
      List.foldl
        (\def names ->
          let
            declaredNames =
              getAllDeclaredNames def
          in
          List.foldl
            (\name nameDict ->
              Dict.insert name.value name nameDict
            )
            names
            declaredNames
        )
        Dict.empty
        defs
  in
  Tuple.first <|
  List.Extra.indexedFoldl
    (\index def (problems, names) ->
      let
        name =
          getDefName def
      in
      if isFakeLocated name then
        (problems, names)
      else
        Tuple.mapFirst ((++) <| checkDef (Dict.filter (\_ n -> n /= name) allDeclaredNames) def)
        ( case Dict.get name.value names of
          Nothing ->
            ( problems
            , Dict.insert name.value (index, name) names
            )

          Just (previousIndex, previousName) ->
            ( DuplicatedDefinition (previousIndex, previousName) (index, name) :: problems
            , names
            )
        )
    )
    ([], Dict.empty)
    defs


getDefName : Def -> Located String
getDefName def =
  case def of
    DValue { name } ->
      name
    
    DType { name } ->
      name
    
    DAlias { name } ->
      name


getAllDeclaredNames : Def -> List (Located String)
getAllDeclaredNames def =
  case def of
    DValue { name } ->
      [ name ]
    
    DType { name, variants } ->
      name ::
      Dict.foldl
        (\_ (label, _) names ->
          label :: names
        )
        []
        variants
    
    DAlias { name, ty } ->
      name ::
      getFreeTypes ty.value


checkDefsTypes : List Def -> List Problem
checkDefsTypes sortedDefs =
  List.foldl
    (\def problems ->
      case def of
        DValue { term } ->
          case getType [] term of
            Err typeProblem ->
              typeProblem :: problems
            
            Ok _ ->
              problems
        
        DType _ ->
          problems
        
        DAlias _ ->
          problems
    )
    []
    sortedDefs


checkDef : Dict String (Located String) -> Def -> List Problem
checkDef names def =
  case def of
    DValue { term } ->
      checkTerm names term
    
    DType { variants } ->
      Dict.foldl
        (\_ (_, ty) problems ->
          checkFreeTypes names ty ++ problems
        )
        []
        variants
    
    DAlias { ty } ->
      checkFreeTypes names ty


checkFreeTypes : Dict String (Located String) -> Located Type -> List Problem
checkFreeTypes names ty =
  let
    typeNames =
      getFreeTypes ty.value
  in
  List.foldl
  (\name ps ->
    if List.member name.value (Dict.keys names) then
      ps
    else
      UndefinedType name :: ps
  )
  []
  typeNames


checkTerm : Dict String (Located String) -> Located Term -> List Problem
checkTerm names tm =
  case tm.value of
    TmVariable _ maybeUndefinedName ->
      case maybeUndefinedName of
        Just name ->
          [ UndefinedVariable name ]
        
        Nothing ->
          []
    
    TmAbstraction boundVar _ innerExpr ->
      checkTerm (Dict.insert boundVar.value boundVar names) innerExpr
    
    TmApplication func arg ->
      checkTerm names func
      ++ checkTerm names arg

    TmTAbstraction boundTy t1 ->
      checkTerm (Dict.insert boundTy.value boundTy names) t1

    TmTApplication t1 ty ->
      checkTerm names t1
      ++ checkFreeTypes names ty

    TmBool _ ->
      []

    TmInt _ ->
      []
    
    TmChar _ ->
      []

    TmPair e1 e2 ->
      checkTermBinaryHelper names e1 e2

    TmPairAccess pair _ ->
      checkTerm names pair

    TmRecord r ->
      Dict.foldl
        (\_ (_, value) problems ->
          checkTerm names value ++ problems
        )
        []
        r

    TmRecordAccess r _ ->
      checkTerm names r

    TmAdd left right ->
      checkTermBinaryHelper names left right

    TmSubtract left right ->
      checkTermBinaryHelper names left right

    TmMultiplication left right ->
      checkTermBinaryHelper names left right

    TmDivision left right ->
      checkTermBinaryHelper names left right
    
    TmComparison _ left right ->
      checkTermBinaryHelper names left right
    
    TmIf condition thenBranch elseBranch ->
      checkTerm names condition
      ++ checkTerm names thenBranch
      ++ checkTerm names elseBranch

    TmLet (label, e1) e2 ->
      checkTerm names e1
      ++ checkTerm (Dict.insert label.value label names) e2

    TmUnit ->
      []
    
    TmCase e variants ->
      checkTerm names e
      ++ Dict.foldl
        (\_ (_, valueName, innerExpr) problems ->
          checkTerm (Dict.insert valueName.value valueName names) innerExpr ++ problems
        )
        []
        variants
    
    TmVariant _ value _ ->
      checkTerm names value


checkTermBinaryHelper : Dict String (Located String) -> Located Term -> Located Term -> List Problem
checkTermBinaryHelper names left right =
  checkTerm names left
  ++ checkTerm names right


getType : Ctx -> Located Term -> Result Problem (Located Type)
getType ctx tm =
  case tm.value of
    TmVariable index _ ->
      case getTypeFromContext ctx index of
        Nothing ->
          Err <| TypeError <| withLocation tm <| indexToName ctx index

        Just ty ->
          Ok <| withLocation tm ty

    TmAbstraction boundVar boundType innerExpr ->
      let
        innerCtx =
          addBinding ctx boundVar.value (VarBind boundType)
      in
      getType innerCtx innerExpr |>
      Result.map
      (\innerType ->
        withLocation tm <| TyFunc boundType innerType
      )

    TmApplication e1 e2 ->
      getType ctx e1 |>
      Result.andThen
      (\ty1 ->
        getType ctx e2 |>
        Result.andThen
        (\ty2 ->
          case ty1.value of
            TyFunc fromType toType ->
              if areEqualTypes fromType.value ty2.value then
                Ok <| withLocation tm <| toType.value
              else
                Err <| MismatchedType fromType ty2

            _ ->
              Err <| ExpectingTyFunc ty1
        )
      )

    TmTAbstraction boundVar innerExpr ->
      let
        innerCtx =
          addBinding ctx boundVar.value TyVarBind
      in
      getType innerCtx innerExpr |>
      Result.map
      (\innerType ->
        withLocation tm <| TyAll boundVar innerType
      )

    TmTApplication tm1 ty2 ->
      getType ctx tm1 |>
      Result.andThen
      (\ty1 ->
        case ty1.value of
          TyAll _ ty12 ->
            Ok <| typeSubst 0 ty2 ty12
          
          _ ->
            Err <| ExpectingTyAll ty1
      )

    TmIf condition thenBranch elseBranch ->
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
    
    TmLet (label, e1) e2 ->
      getType ctx e1 |>
      Result.andThen
      (\bindingType ->
        let
          innerCtx =
            addBinding ctx label.value (VarBind bindingType)
        in
        getType innerCtx e2
      )

    TmPair e1 e2 ->
      getType ctx e1 |>
      Result.andThen
      (\ty1 ->
        getType ctx e2 |>
        Result.andThen
        (\ty2 ->
          Ok <| withLocation tm <| TyPair ty1 ty2
        )
      )

    TmPairAccess pair index ->
      getType ctx pair |>
      Result.andThen
      (\pairType ->
        case pairType.value of
          TyPair ty1 ty2 ->
            case index.value of
              PairIndexOne ->
                Ok ty1
              
              PairIndexTwo ->
                Ok ty2
          _ ->
            Err <| ExpectingTyPair pairType
      )

    TmRecord r ->
      Dict.foldl
        (\_ (label, value) recordType ->
          recordType |>
          Result.andThen
          (\ty ->
            getType ctx value |>
            Result.map
            (\valueType ->
              Dict.insert label.value (label, valueType) ty
            )
          )
        )
        (Ok Dict.empty)
        r |>
      Result.map (withLocation tm << TyRecord)

    TmRecordAccess record label ->
      getType ctx record |>
      Result.andThen
      (\recordType ->
        case recordType.value of
          TyRecord r ->
            case Dict.get label.value r of
              Just (_, ty) ->
                Ok ty
              
              Nothing ->
                Err <| ExpectingTyRecordWithLabel label recordType

          _ ->
            Err <| ExpectingTyRecord recordType
      )

    TmAdd left right ->
      getTypeFromBinaryInts ctx tm left right

    TmSubtract left right ->
      getTypeFromBinaryInts ctx tm left right

    TmMultiplication left right ->
      getTypeFromBinaryInts ctx tm left right

    TmDivision left right ->
      getTypeFromBinaryInts ctx tm left right

    TmComparison comp left right ->
      case comp of
        CompEQ ->
          getTypeFromEquality ctx tm left right
        
        CompNE ->
          getTypeFromEquality ctx tm left right
      
        _ ->
          getTypeFromBinaryInts ctx tm left right |>
          Result.map
          (\intType ->
            withLocation intType <| TyBool
          )

    TmBool _ ->
      Ok <| withLocation tm TyBool

    TmInt _ ->
      Ok <| withLocation tm TyInt

    TmChar _ ->
      Ok <| withLocation tm TyChar
    
    TmUnit ->
      Ok <| withLocation tm TyUnit

    TmCase e variants ->
      getType ctx e |>
      Result.andThen
      (\exprType ->
        case exprType.value of
          TyCustom tyName tyVariants ->
            getTypeFromVariants ctx tm tyName tyVariants variants

          _ ->
            Err <| ExpectingTyCustom exprType
      )
    
    TmVariant _ _ ty -> -- TODO: maybe need to check if value has the right type
      Ok ty


getTypeFromVariants :
  Ctx ->
  Located Term ->
  Located String ->
  Dict String (Located String, Located Type) ->
  (Dict String (Located String, Located String, Located Term)) ->
  Result Problem (Located Type)
getTypeFromVariants ctx expr customTypeName customTypeVariants variants =
  Dict.foldl
    (\_ (variantName, valueName, innerExpr) result ->
      result |>
      Result.andThen
      (\(types, restVariants) ->
        case Dict.get variantName.value restVariants of
          Just (_, valueTy) ->
            getType (addBinding ctx valueName.value (VarBind valueTy)) innerExpr |>
            Result.map
            (\ty ->
              ( ty :: types
              , Dict.remove variantName.value restVariants
              )
            )
              
          Nothing ->
            Err <| ExpectingVariantInTyCustom variantName (customTypeName, customTypeVariants)
      )
    )
    (Ok ([], customTypeVariants))
    variants |>
    Result.andThen
    (\(revVariantTypes, restTyVariants) ->
      let
        variantTypes =
          List.reverse revVariantTypes
      in
      if Dict.isEmpty restTyVariants then
        List.foldl
        (\(prevTy, currTy) result ->
          result |>
          Result.andThen
          (\_ ->
            if areEqualTypes prevTy.value currTy.value then
              Ok currTy
            else
              Err <| MismatchedType prevTy currTy
          )
        )
        (Ok <| Maybe.withDefault (fakeLocated fakeType) <| List.head variantTypes)
        <|
        List.map2
        Tuple.pair
        variantTypes (Maybe.withDefault [] <| List.tail variantTypes)
      else
        Err <| ExpectingVariantsInECase (List.map (\(_, (label, _)) -> label) <| Dict.toList restTyVariants) expr
    )


getTypeFromEquality : Ctx -> Located Term -> Located Term -> Located Term -> Result Problem (Located Type)
getTypeFromEquality ctx expr left right =
  getType ctx left |>
    Result.andThen
    (\leftType ->
      getType ctx right |>
      Result.andThen
      (\rightType ->
        if areEqualTypes leftType.value rightType.value then
          case leftType.value of
            TyFunc _ _ ->
              Err <| CompareTyFunc leftType rightType
            _ ->
              Ok <| withLocation expr TyBool
        else
          Err <| MismatchedType leftType rightType
      )
    )


getTypeFromBinaryInts : Ctx -> Located Term -> Located Term -> Located Term -> Result Problem (Located Type)
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
    
    (TyChar, TyChar) ->
      True
    
    (TyUnit, TyUnit) ->
      True
    
    (TyName n1, TyName n2) ->
      n1.value == n2.value
    
    (TyCustom n1 _, TyCustom n2 _) ->
      n1.value == n2.value
    
    (TyName n1, TyCustom n2 _) ->
      n1.value == n2.value
    
    (TyCustom n1 _, TyName n2) ->
      n1.value == n2.value

    (TyPair p1ty1 p1ty2, TyPair p2ty1 p2ty2) ->
      areEqualTypes p1ty1.value p2ty1.value
      && areEqualTypes p1ty2.value p2ty2.value

    (TyRecord r1, TyRecord r2) ->
      if not (Dict.isEmpty <| Dict.diff r1 r2)
      || not (Dict.isEmpty <| Dict.diff r2 r1)
      then
        False
      else
        dictFoldlWhile
          (\labelStr (_, r1ty) _ ->
            case Dict.get labelStr r2 of
              Nothing ->
                ( False
                , Done
                )
              
              Just (_, r2ty) ->
                if areEqualTypes r1ty.value r2ty.value then
                  ( True
                  , Loop
                  )
                else
                  ( False
                  , Done
                  )
          )
          True
          r1
    
    (TyFunc fromType1 toType1, TyFunc fromType2 toType2) ->
      areEqualTypes fromType1.value fromType2.value
      && areEqualTypes toType1.value toType2.value
    
    (TyVar i1, TyVar i2) ->
      i1 == i2

    _ ->
      False


type Step
  = Loop
  | Done


dictFoldlWhile : (comparable -> v -> b -> (b, Step)) -> b -> Dict comparable v -> b
dictFoldlWhile f initial dict =
  dictFoldlWhileHelper f initial (Dict.keys dict) dict


dictFoldlWhileHelper : (comparable -> v -> b -> (b, Step)) -> b -> List comparable -> Dict comparable v -> b
dictFoldlWhileHelper f prevResult keys dict =
  case keys of
    [] ->
      prevResult
    
    firstKey :: restKeys ->
      case Dict.get firstKey dict of
        Just firstValue ->
          let
            (result, step) =
              f firstKey firstValue prevResult
          in
          case step of
            Loop ->
              dictFoldlWhileHelper f result restKeys dict
            
            Done ->
              result
        
        Nothing ->
          prevResult


addBinding : Ctx -> String -> Binding -> Ctx
addBinding ctx name bind =
  (name, bind) :: ctx


getBinding : Ctx -> Int -> Binding
getBinding ctx index =
  Maybe.withDefault fakeBinding <|
  Maybe.map Tuple.second <|
  List.Extra.getAt index ctx


indexToName : Ctx -> Int -> String
indexToName ctx index =
  Maybe.withDefault "IMPOSSIBLE VAR INDEX" <|
  Maybe.map Tuple.first <|
  List.Extra.getAt index ctx


getTypeFromContext : Ctx -> Int -> Maybe Type
getTypeFromContext ctx index =
  case getBinding ctx index of
    VarBind ty ->
      Just ty.value
    
    TyVarBind ->
      Nothing


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
    
    UndefinedType name ->
      [ "-- UNDEFINED TYPE\n"
      , "I found an undefined type `" ++ name.value ++ "` here:"
      , showLocation src name
      , "Hint: Try defining `" ++ name.value ++ "` somewhere."
      ]

    MisusedVariantAsType variantName customTypeName ->
      [ "-- MISUSED VARIANT AS TYPE\n"
      , "I found a name `" ++ variantName.value ++ "` used as a type here:"
      , showLocation src variantName
      , "But `" ++ variantName.value ++ "` is a variant of a custom type declared here:"
      , showLocation src customTypeName
      , "Hint: Try changing `" ++ variantName.value ++ "` to `" ++ customTypeName.value ++ "`"
      ]
    
    MisusedTypeAsVariable typeName declaredName ->
      [ "-- MISUSED TYPE AS VARIABLE\n"
      , "I found a name `" ++ typeName.value ++ "` used as a variable here:"
      , showLocation src typeName
      , "But `" ++ typeName.value ++ "` is a type declared here:"
      , showLocation src declaredName
      , "Hint: Try changing the variable name to refer to a value instead of a type."
      ]

    ExpectingVariantsInECase variants caseExpr ->
      [ "-- EXPECTING VARIANT IN CASE EXPRESSION\n"
      , "I'm expecting the variants:"
      ++ (String.join "\n" <| List.map .value variants)
      ++ "in the case expression:"
      ++ showLocation src caseExpr
      ++ "Hint: Try adding the variants listed above to the case expression."
      ]

    ExpectingVariantInTyCustom variantName (customTypeName, customTypeVariants) ->
      [ "-- EXPECTING VARIANT IN CUSTOM TYPE\n"
      , "I'm expecting a variant `" ++ variantName.value ++ "` in a branch of the case expression here:"
      , showLocation src variantName
      , "But the case expression operates on a custom type without this variant:\n"
      , showCustomType customTypeName customTypeVariants ++ "\n"
      , "Hint: Try adding the variant to the custom type or removing it in the case expression."
      ]

    ExpectingTyAll ty ->
      [ "-- EXPECTING UNIVERSAL TYPE\n"
      , "I'm expecting an universal type here:"
      , showLocation src ty
      , "but got " ++ showType [] ty.value ++ "."
      , "Hint: Try changing it to an universal type."
      ]

    ExpectingTyCustom ty ->
      [ "-- EXPECTING CUSTOM TYPE\n"
      , "I'm expecting a custom type here:"
      , showLocation src ty
      , "but got " ++ showType [] ty.value ++ "."
      , "Hint: Try changing it to a custom type."
      ]

    ExpectingTyFunc ty ->
      [ "-- EXPECTING FUNCTION\n"
      , "I'm expecting a function here:"
      , showLocation src ty
      , "but got " ++ showType [] ty.value ++ "."
      , "Hint: Try changing it to a function."
      ]

    ExpectingTyBool ty ->
      [ "-- EXPECTING BOOL\n"
      , "I'm expecting a Bool here:"
      , showLocation src ty
      , "but got " ++ showType [] ty.value ++ "."
      , "Hint: Try changing it to a Bool."
      ]

    ExpectingTyInt ty ->
      [ "-- EXPECTING INT\n"
      , "I'm expecting a Int here:"
      , showLocation src ty
      , "but got " ++ showType [] ty.value ++ "."
      , "Hint: Try changing it to a Int."
      ]
    
    ExpectingTyPair ty ->
      [ "-- EXPECTING PAIR\n"
      , "I'm expecting a pair here:"
      , showLocation src ty
      , "but got " ++ showType [] ty.value ++ "."
      , "Hint: Try changing it to a pair."
      ]

    ExpectingTyRecord ty ->
      [ "-- EXPECTING RECORD\n"
      , "I'm expecting a record here:"
      , showLocation src ty
      , "but got " ++ showType [] ty.value ++ "."
      , "Hint: Try changing it to a record."
      ]

    ExpectingTyRecordWithLabel label recordType ->
      [ "-- EXPECTING LABEL\n"
      , "I'm expecting the label `" ++ label.value ++ "` in this record:"
      , showLocation src recordType
      , "but I can't find it."
      , "Hint: Try adding the label `" ++ label.value ++ "` to this record."
      ]

    MismatchedType ty1 ty2 ->
      [ "-- MISMATCHED TYPES\n"
      , "I'm expecting a " ++ showType [] ty1.value ++ " type here:"
      , showLocation src ty2
      , "but got " ++ showType [] ty2.value ++ "."
      , "Hint: Try changing it to a " ++ showType [] ty1.value ++ "."
      ]

    TypeError variable ->
      [ "-- TYPE ERROR\n"
      , "I found a variable that evaluates to a type error:"
      , showLocation src variable
      , "Hint: Try fixing the type error in the definition of `" ++ variable.value ++ "` first."
      ]

    CompareTyFunc f1 f2 ->
      [ "-- CAN'T COMPARE FUNCTIONS\n"
      , "I found that you are trying to compare two functions."
      , "The first function is here:"
      , showLocation src f1
      , "The second function is here:"
      , showLocation src f2
      , "General function equality is undecidable so it's not supported."
      , "Hint: Try comparing anything that is not a function."
      ]
