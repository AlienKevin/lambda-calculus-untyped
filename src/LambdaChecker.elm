module LambdaChecker exposing (checkDefs, showProblems, showProblemsWithSingleSource, sortDefs, getDefName, Problem(..))


import LambdaParser exposing (showType, showCustomType, fakeDef, fakeType, Def(..), Expr(..), Type(..), Comparison(..), PairIndex(..))
import Dict exposing (Dict)
import Location exposing (showLocation, isFakeLocated, withLocation, fakeLocated, Located)
import Set exposing (Set)
import List.Extra


type Problem
  = DuplicatedDefinition (Int, Located String) (Int, Located String)
  | UndefinedVariable (Located String)
  | UndefinedType (Located String)
  | MisusedVariantAsType (Located String) (Located String)
  | MisusedTypeAsVariable (Located String) (Located String)
  | TypeError (Located String)
  | ExpectingVariantsInECase (List (Located String)) (Located Expr)
  | ExpectingVariantInTyCustom (Located String) ((Located String), (Dict String (Located String, Located Type)))
  | ExpectingTyCustom (Located Type)
  | ExpectingTyFunc (Located Type)
  | ExpectingTyBool (Located Type)
  | ExpectingTyInt (Located Type)
  | ExpectingTyPair (Located Type)
  | ExpectingTyRecord (Located Type)
  | ExpectingTyRecordWithLabel (Located String) (Located Type)
  | MismatchedType (Located Type) (Located Type)
  | CompareTyFunc (Located Type) (Located Type)


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


getAllDeclaredNames : Def -> List (Located String)
getAllDeclaredNames def =
  case def of
    DValue { name } ->
      [ name ]
    
    DType { name, variants } ->
      name ::
      ( Dict.foldl
        (\_ (label, _) names ->
          label :: names
        )
        []
        variants
      )


checkDefsTypes : List Def -> List Problem
checkDefsTypes defs =
  let
    sortedDefs =
      sortDefs defs
  in
  Tuple.first <|
  List.foldl
    (\def (problems, ctx) ->
      case def of
        DValue { name, expr } ->
          case getType ctx expr of
            Err typeProblem ->
              ( typeProblem :: problems
              , ctx
              )
            
            Ok ty ->
              ( problems
              , addBinding ctx name.value ty.value
              )
        
        DType { name, variants } ->
          ( []
          , Dict.foldl
            (\_ (label, ty) nextCtx ->
              addBinding nextCtx label.value (TyFunc ty (withLocation name <| TyCustom name variants))
            )
            ctx
            variants |>
            (\nextCtx2 ->
              addBinding nextCtx2 ("$" ++ name.value) (TyCustom name variants)
            )
          )
    )
    ([], [])
    sortedDefs


checkDef : Dict String (Located String) -> Def -> List Problem
checkDef names def =
  case def of
    DValue { expr } ->
      checkExpr names expr
    
    DType { variants } ->
      Dict.foldl
        (\_ (label, ty) problems ->
          let
            -- _ = Debug.log "AL -> Dict.keys names" <| Dict.keys names
            -- _ = Debug.log "AL -> label.value" <| label.value
            -- _ = Debug.log "AL -> typeNames" <| typeNames
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
          problems
          typeNames
        )
        []
        variants


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

    EPair e1 e2 ->
      checkExprBinaryHelper names e1 e2

    EPairAccess pair _ ->
      checkExpr names pair

    ERecord r ->
      Dict.foldl
        (\_ (_, value) problems ->
          checkExpr names value ++ problems
        )
        []
        r

    ERecordAccess r _ ->
      checkExpr names r

    EAdd left right ->
      checkExprBinaryHelper names left right

    ESubtract left right ->
      checkExprBinaryHelper names left right

    EMultiplication left right ->
      checkExprBinaryHelper names left right

    EDivision left right ->
      checkExprBinaryHelper names left right
    
    EComparison _ left right ->
      checkExprBinaryHelper names left right
    
    EIf condition thenBranch elseBranch ->
      checkExpr names condition
      ++ checkExpr names thenBranch
      ++ checkExpr names elseBranch

    ELet (label, e1) e2 ->
      checkExpr names e1
      ++ checkExpr (Dict.insert label.value label names) e2

    EUnit ->
      []
    
    ECase e variants ->
      checkExpr names e
      ++ Dict.foldl
        (\_ (_, valueName, innerExpr) problems ->
          checkExpr (Dict.insert valueName.value valueName names) innerExpr ++ problems
        )
        []
        variants


checkExprBinaryHelper : Dict String (Located String) -> Located Expr -> Located Expr -> List Problem
checkExprBinaryHelper names left right =
  checkExpr names left
  ++ checkExpr names right


type alias Ctx
  = List (String, Type)


getType : Ctx -> Located Expr -> Result Problem (Located Type)
getType ctx expr =
  case expr.value of
    EVariable name ->
      case getTypeFromContext ctx name of
        Nothing ->
          case getTypeFromContext ctx (withLocation name <| "$" ++ name.value) of
            Just (TyCustom tyName _) ->
              Err <| MisusedTypeAsVariable name tyName
            
            Just (TyName tyName _) ->
              Err <| MisusedTypeAsVariable name tyName
            
            _ ->
              Err <| TypeError name

        Just ty ->
          Ok <| withLocation expr ty

    EAbstraction boundVar boundType innerExpr ->
      let
        innerCtx =
          addBinding ctx boundVar.value boundType.value
        -- _ = Debug.log "AL -> boundType" <| boundType.value
        -- _ = Debug.log "AL -> ctx" <| ctx
      in
      ( case boundType.value of
        TyName name _ ->
          case getTypeFromContext ctx name of
            Just (TyFunc _ innerType) ->
              case getTypeFromContext ctx (withLocation name <| "$" ++ name.value) of
                Just _ ->
                  Ok ()
                
                Nothing ->
                  case innerType.value of
                    TyCustom tyName _ ->
                      Err <| MisusedVariantAsType name tyName
                    
                    _ ->
                      Ok ()
            
            Nothing ->
              case getTypeFromContext ctx (withLocation name <| "$" ++ name.value) of
                Just _ ->
                  Ok ()
                
                Nothing ->
                  Err <| UndefinedType name

            _ ->
              Ok ()

        _ ->
          Ok ()
      ) |>
      Result.andThen
      (\_ ->
      getType innerCtx innerExpr |>
      Result.map
      (\innerType ->
        withLocation expr <| TyFunc boundType innerType
      )
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
    
    ELet (label, e1) e2 ->
      getType ctx e1 |>
      Result.andThen
      (\bindingType ->
        let
          innerCtx =
            addBinding ctx label.value bindingType.value
        in
        getType innerCtx e2
      )

    EPair e1 e2 ->
      getType ctx e1 |>
      Result.andThen
      (\ty1 ->
        getType ctx e2 |>
        Result.andThen
        (\ty2 ->
          Ok <| withLocation expr <| TyPair ty1 ty2
        )
      )

    EPairAccess pair index ->
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

    ERecord r ->
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
      Result.map (withLocation expr << TyRecord)

    ERecordAccess record label ->
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

    EAdd left right ->
      getTypeFromBinaryInts ctx expr left right

    ESubtract left right ->
      getTypeFromBinaryInts ctx expr left right

    EMultiplication left right ->
      getTypeFromBinaryInts ctx expr left right

    EDivision left right ->
      getTypeFromBinaryInts ctx expr left right

    EComparison comp left right ->
      case comp of
        CompEQ ->
          getTypeFromEquality ctx expr left right
        
        CompNE ->
          getTypeFromEquality ctx expr left right
      
        _ ->
          getTypeFromBinaryInts ctx expr left right |>
          Result.map
          (\intType ->
            withLocation intType <| TyBool
          )

    EBool _ ->
      Ok <| withLocation expr TyBool

    EInt _ ->
      Ok <| withLocation expr TyInt
    
    EUnit ->
      Ok <| withLocation expr TyUnit

    ECase e variants ->
      getType ctx e |>
      Result.andThen
      (\exprType ->
        case exprType.value of
          TyCustom tyName tyVariants ->
            getTypeFromVariants ctx expr tyName tyVariants variants

          TyName name _ ->
            ( case getTypeFromContext ctx (withLocation name <| "$" ++ name.value) of
              Just ty ->
                Ok <| withLocation e ty
              
              _ ->
                Err <| TypeError name
            ) |>
            Result.andThen
            (\ty ->
              case ty.value of
                TyCustom tyName tyVariants ->
                  getTypeFromVariants ctx expr tyName tyVariants variants

                TyFunc _ innerType ->
                  case innerType.value of
                    TyCustom tyName tyVariants ->
                      getTypeFromVariants ctx expr tyName tyVariants variants
                    
                    _ ->
                      Err <| ExpectingTyCustom exprType

                _ ->
                  Err <| ExpectingTyCustom exprType
            )
          
          _ ->
            Err <| ExpectingTyCustom exprType
      )


getTypeFromVariants :
  Ctx ->
  Located Expr ->
  Located String ->
  Dict String (Located String, Located Type) ->
  (Dict String (Located String, Located String, Located Expr)) ->
  Result Problem (Located Type)
getTypeFromVariants ctx expr customTypeName customTypeVariants variants =
  Dict.foldl
    (\_ (variantName, valueName, innerExpr) result ->
      result |>
      Result.andThen
      (\(types, restVariants) ->
        case Dict.get variantName.value restVariants of
          Just (_, valueTy) ->
            getType (addBinding ctx valueName.value valueTy.value) innerExpr |>
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


getTypeFromEquality : Ctx -> Located Expr -> Located Expr -> Located Expr -> Result Problem (Located Type)
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
              Ok <| withLocation expr TyInt
        else
          Err <| MismatchedType leftType rightType
      )
    )


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
  -- let
    -- _ = Debug.log "AL -> ty1" <| ty1
    -- _ = Debug.log "AL -> ty2" <| ty2
  -- in
  case (ty1, ty2) of
    (TyBool, TyBool) ->
      True

    (TyInt, TyInt) ->
      True
    
    (TyUnit, TyUnit) ->
      True
    
    (TyName n1 n1ty, TyName n2 n2ty) ->
      n1.value == n2.value
      || ( case (n1ty, n2ty) of
        (Just t1, Just t2) ->
          areEqualTypes t1.value t2.value
        
        _ ->
          False
        )
    
    (TyCustom n1 _, TyCustom n2 _) ->
      n1.value == n2.value
    
    (TyName n1 _, TyCustom n2 _) ->
      n1.value == n2.value
    
    (TyCustom n1 _, TyName n2 _) ->
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

    ExpectingTyCustom ty ->
      [ "-- EXPECTING CUSTOM TYPE\n"
      , "I'm expecting a custom type here:"
      , showLocation src ty
      , "but got " ++ showType ty.value ++ "."
      , "Hint: Try changing it to a custom type."
      ]

    ExpectingTyFunc ty ->
      [ "-- EXPECTING FUNCTION\n"
      , "I'm expecting a function here:"
      , showLocation src ty
      , "but got " ++ showType ty.value ++ "."
      , "Hint: Try changing it to a function."
      ]

    ExpectingTyBool ty ->
      [ "-- EXPECTING BOOL\n"
      , "I'm expecting a Bool here:"
      , showLocation src ty
      , "but got " ++ showType ty.value ++ "."
      , "Hint: Try changing it to a Bool."
      ]

    ExpectingTyInt ty ->
      [ "-- EXPECTING INT\n"
      , "I'm expecting a Int here:"
      , showLocation src ty
      , "but got " ++ showType ty.value ++ "."
      , "Hint: Try changing it to a Int."
      ]
    
    ExpectingTyPair ty ->
      [ "-- EXPECTING PAIR\n"
      , "I'm expecting a pair here:"
      , showLocation src ty
      , "but got " ++ showType ty.value ++ "."
      , "Hint: Try changing it to a pair."
      ]

    ExpectingTyRecord ty ->
      [ "-- EXPECTING RECORD\n"
      , "I'm expecting a record here:"
      , showLocation src ty
      , "but got " ++ showType ty.value ++ "."
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


sortDefs : List Def -> List Def
sortDefs defs =
  let
    dependencies =
      List.foldl
      (\def deps ->
        case def of
          DValue { name, expr } ->
            Dict.insert
            name.value
            (Set.toList <| getFreeVariablesAndTypes expr.value)
            deps
          
          DType { name, variants } ->
            (\nextDeps ->
              Dict.foldl
              (\_ (label, _) resultDeps ->
                Dict.insert label.value [] resultDeps
              )
              nextDeps
              variants
            ) <|
            Dict.insert
            name.value
            (List.concat <|
              List.map (\(_, (_, ty)) -> List.map .value <| getFreeTypes ty.value) <|
              Dict.toList variants
            )
            deps
      )
      Dict.empty
      defs
    
    -- _ = Debug.log "AL -> dependencies" <| dependencies
    
    sortedNames =
      sortDependencies dependencies
    -- _ = Debug.log "AL -> sortedNames" <| sortedNames
  in
  List.filterMap
  identity <|
  List.map
  (\name ->
    List.Extra.find
      (\def ->
        ( case def of
          DValue d ->
            d.name
          
          DType d ->
            d.name
        ) |>
        .value |>
        (==) name
      )
      defs
  )
  sortedNames


getFreeTypes : Type -> List (Located String)
getFreeTypes ty =
  case ty of
    TyName label _ ->
      [ label ]
    
    TyCustom name variants ->
      [ name ]
      -- ++ Dict.foldl
      -- (\_ (label, _) labels ->
      --   label :: labels
      -- )
      -- []
      -- variants
    
    TyPair left right ->
      getFreeTypes left.value
      ++ getFreeTypes right.value
    
    TyRecord r ->
      Dict.foldl
        (\_ (_, valueType) types ->
          getFreeTypes valueType.value ++ types
        )
        []
        r

    TyFunc boundType innerType ->
      getFreeTypes boundType.value
      ++ getFreeTypes innerType.value
    
    _ ->
      []


getFreeVariablesAndTypes : Expr -> Set String
getFreeVariablesAndTypes expr =
  getFreeVariablesAndTypesHelper Set.empty expr


getFreeVariablesAndTypesHelper : Set String -> Expr -> Set String
getFreeVariablesAndTypesHelper boundVariables expr =
  case expr of
    EVariable name ->
      if Set.member name.value boundVariables then
        Set.empty
      else
        Set.singleton name.value 
    
    EAbstraction boundVar boundType innerExpr ->
      Set.union
      ( case boundType.value of
        TyName label _ ->
          Set.singleton label.value
        
        TyCustom name _ ->
          Set.singleton name.value
        
        _ ->
          Set.empty
      )
      (getFreeVariablesAndTypesHelper (Set.insert boundVar.value boundVariables) innerExpr.value)
    
    EApplication func arg ->
      Set.union
      (getFreeVariablesAndTypesHelper boundVariables func.value)
      (getFreeVariablesAndTypesHelper boundVariables arg.value)

    EBool _ ->
      Set.empty
    
    EInt _ ->
      Set.empty

    EUnit ->
      Set.empty

    EPair e1 e2 ->
      getFreeVariablesBinaryHelper boundVariables e1 e2

    EPairAccess pair _ ->
      getFreeVariablesAndTypesHelper boundVariables pair.value

    ERecord r ->
      Dict.foldl
        (\_ (_, value) freeVars ->
          Set.union
          (getFreeVariablesAndTypesHelper boundVariables value.value)
          freeVars
        )
        Set.empty
        r

    ERecordAccess record _ ->
      getFreeVariablesAndTypesHelper boundVariables record.value

    EAdd left right ->
      getFreeVariablesBinaryHelper boundVariables left right

    ESubtract left right ->
      getFreeVariablesBinaryHelper boundVariables left right

    EMultiplication left right ->
      getFreeVariablesBinaryHelper boundVariables left right

    EDivision left right ->
      getFreeVariablesBinaryHelper boundVariables left right
    
    EComparison _ left right ->
      getFreeVariablesBinaryHelper boundVariables left right

    EIf condition thenBranch elseBranch ->
      Set.union
      (getFreeVariablesAndTypesHelper boundVariables condition.value)
      ( Set.union
        (getFreeVariablesAndTypesHelper boundVariables thenBranch.value)
        (getFreeVariablesAndTypesHelper boundVariables elseBranch.value)
      )

    ELet (label, e1) e2 ->
      Set.union
      (getFreeVariablesAndTypesHelper boundVariables e1.value)
      (getFreeVariablesAndTypesHelper (Set.insert label.value boundVariables) e2.value)
  
    ECase e variants ->
      Set.union
      (getFreeVariablesAndTypesHelper boundVariables e.value)
      ( Dict.foldl
        (\_ (_, valueName, innerExpr) set ->
          Set.union
          ( getFreeVariablesAndTypesHelper
            (Set.insert valueName.value boundVariables)
            innerExpr.value
          )
          set
        )
        Set.empty
        variants
      )


getFreeVariablesBinaryHelper : Set String -> Located Expr -> Located Expr -> Set String
getFreeVariablesBinaryHelper boundVariables left right =
  Set.union
  (getFreeVariablesAndTypesHelper boundVariables left.value)
  (getFreeVariablesAndTypesHelper boundVariables right.value)


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
