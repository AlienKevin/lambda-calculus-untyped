module LambdaParser exposing
  ( parseDefs, parseTerm
  , showProblems, showDefs, showDef, showTermHelper, showType, showCustomType
  , getFreeTypes, termShift, termSubst
  , fakeDef, fakeType
  , Def(..), Term(..), Type(..), Comparison(..), PairIndex(..)
  )


import Parser.Advanced exposing (..)
import Pratt.Advanced as Pratt
import Set exposing (Set)
import Dict exposing (Dict)
import List.Extra
import Location exposing (..)


type alias LambdaParser a =
  Parser Context Problem a


type Context
  = CVariable
  | CAbstraction
  | CApplication
  | CIf
  | CType


type Problem
  = ExpectingBackslash
  | ExpectingVariable
  | ExpectingDot
  | ExpectingEqual
  | ExpectingLeftParen
  | ExpectingRightParen
  | ExpectingColon
  | ExpectingArrow
  | ExpectingPlus
  | ExpectingMinus
  | ExpectingAsterisk
  | ExpectingSlash
  | ExpectingComma
  | ExpectingLeftBrace
  | ExpectingRightBrace
  | ExpectingVerticalBar
  | ExpectingSingleQuote
  | ExpectingChar
  | ExpectingOne
  | ExpectingTwo
  | ExpectingComparison Comparison
  | ExpectingStartOfLineComment
  | ExpectingStartOfMultiLineComment
  | ExpectingEndOfMultiLineComment
  | ExpectingIndent
  | ExpectingDefinition
  | ExpectingEndOfDefinition
  | ExpectingEndOfExpression
  | ExpectingTyBool
  | ExpectingTyInt
  | ExpectingTyChar
  | ExpectingIf
  | ExpectingThen
  | ExpectingElse
  | ExpectingTrue
  | ExpectingFalse
  | ExpectingInt
  | ExpectingLet
  | ExpectingIn
  | ExpectingType
  | ExpectingTypeName
  | ExpectingAlias
  | ExpectingCase
  | ExpectingOf


type ParserDef
  = ParserDValue
    { name : Located String
    , expr : Located Expr
    }
  | ParserDType
    { name : Located String
    , variants : Dict String (Located String, Located Type)
    }
  | ParserDAlias
    { name : Located String
    , ty : Located Type
    }


type Def
  = DValue
    { name : Located String
    , term : Located Term
    }
  | DType
    { name : Located String
    , variants : Dict String (Located String, Located Type)
    }
  | DAlias
    { name : Located String
    , ty : Located Type
    }


type Expr
  = EVariable (Located String)
  | EAbstraction (Located String) (Located Type) (Located Expr)
  | EApplication (Located Expr) (Located Expr)
  | EBool Bool
  | EInt Int
  | EChar Char
  | EPair (Located Expr) (Located Expr)
  | EPairAccess (Located Expr) (Located PairIndex)
  | ERecord (Dict String (Located String, Located Expr))
  | ERecordAccess (Located Expr) (Located String)
  | EAdd (Located Expr) (Located Expr)
  | ESubtract (Located Expr) (Located Expr)
  | EMultiplication (Located Expr) (Located Expr)
  | EDivision (Located Expr) (Located Expr)
  | EComparison Comparison (Located Expr) (Located Expr)
  | EIf (Located Expr) (Located Expr) (Located Expr)
  | ELet (Located String, Located Expr) (Located Expr)
  | EUnit
  | ECase (Located Expr) (Dict String (Located String, Located String, Located Expr))


type Term
  = TmVariable Int
  | TmAbstraction (Located String) (Located Type) (Located Term)
  | TmApplication (Located Term) (Located Term)
  | TmVariant (Located String) (Located Term) (Located Type)
  | TmBool Bool
  | TmInt Int
  | TmChar Char
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


type PairIndex
  = PairIndexOne
  | PairIndexTwo


type Comparison
  = CompEQ
  | CompNE
  | CompLT
  | CompLE
  | CompGT
  | CompGE


type Type
  = TyBool
  | TyInt
  | TyChar
  | TyUnit
  | TyName (Located String)
  | TyCustom (Located String) (Dict String (Located String, Located Type))
  | TyPair (Located Type) (Located Type)
  | TyRecord (Dict String (Located String, Located Type))
  | TyFunc (Located Type) (Located Type)


reserved : Set String
reserved =
  Set.fromList
    [ "if", "then", "else", "true", "false", "let", "in", "type", "alias", "case", "of" ]


parseDefs : String -> Result (List (DeadEnd Context Problem)) (List Def)
parseDefs src =
  run
    (succeed
      parserDefsToDefs
      |= withIndent 0 internalParseDefs
      |. end ExpectingDefinition
    )
    src


parserDefsToDefs : List ParserDef -> List Def
parserDefsToDefs parserDefs =
  let
    sortedParserDefs =
      sortParserDefs parserDefs
    
    sortedDefs =
      Tuple.first <|
      List.foldl
        (\parserDef (defs, mappings) ->
          case parserDef of
            ParserDValue { name, expr } ->
              let
                term =
                  exprToTerm mappings expr
                
                def =
                  DValue
                    { name =
                      name
                    , term =
                      term
                    }
              in
              ( Dict.insert name.value def defs
              , Dict.insert name.value term mappings
              )
            
            ParserDType { name, variants } ->
              let
                def =
                  DType
                    { name =
                      name
                    , variants =
                      variants
                    }
                  
                customType =
                  withLocation name <| TyCustom name variants
              in
              ( Dict.insert name.value def defs
              , Dict.foldl
                (\_ (label, ty) nextMappings ->
                  ( withLocation label <|
                    TmAbstraction
                    (withLocation ty <| "x")
                    ty
                    (withLocation label <|
                      TmVariant
                      label
                      (withLocation ty <| TmVariable 0)
                      customType
                    )
                  ) |>
                  (\tm ->
                    Dict.insert label.value tm nextMappings
                  )
                )
                mappings
                variants
                |> Dict.insert name.value (withLocation name <| TmVariant name (withLocation name <| TmVariable -1) customType)
              )
            
            ParserDAlias { name, ty } ->
              let
                def =
                  DAlias
                    { name =
                      name
                    , ty =
                      ty
                    }
              in
              ( Dict.insert name.value def defs
              , mappings
                |> Dict.insert name.value (withLocation name <| TmVariant name (withLocation name <| TmVariable -1) ty)
              )
        )
        (Dict.empty, Dict.empty)
        sortedParserDefs
  in
  -- return in sorted order
  List.map
    (\def ->
      let
        name =
          getParserDefName def
      in
      Maybe.withDefault fakeDef <| -- impossible
      Dict.get name.value sortedDefs
    )
    sortedParserDefs


parseTerm : String -> Result (List (DeadEnd Context Problem)) (Located Term)
parseTerm src =
  run
    (succeed
      (exprToTerm Dict.empty)
      |= withIndent 0 internalParseExpr
      |. end ExpectingEndOfExpression
    )
    src


internalParseDefs : LambdaParser (List ParserDef)
internalParseDefs =
  loop [] <|
    (\revDefs ->
      oneOf
      [ succeed (\def -> Loop (def :: revDefs))
        |. sps
        |= internalParseDef
        |. sps
      , succeed ()
        |> map (\_ -> Done (List.reverse revDefs))
      ]
    )


parseDValue : LambdaParser ParserDef
parseDValue =
  succeed identity
    |= parseName
    |. sps
    |. symbol (Token "=" ExpectingEqual)
    |> andThen
    (\name ->
      indent <|
      succeed (\expr -> ParserDValue { name = name, expr = expr })
      |. sps
      |= internalParseExpr
    )


parseDAlias : LambdaParser ParserDef
parseDAlias =
  succeed
    (\name ty ->
      ParserDAlias
        { name =
          name
        , ty =
          ty
        }
    )
    |. keyword (Token "type" ExpectingType)
    |. sps
    |. keyword (Token "alias" ExpectingAlias)
    |. sps
    |= parseTypeName
    |. sps
    |. symbol (Token "=" ExpectingEqual)
    |. sps
    |= parseType


parseDType : LambdaParser ParserDef
parseDType =
  succeed
  (\name (firstVariant, restVariants) ->
    ParserDType
      { name =
        name
      , variants =
        Dict.fromList <|
        List.map
        (\variant ->
          (.value <| Tuple.first variant, variant)
        )
        (firstVariant :: restVariants)
      }
  )
  |. keyword (Token "type" ExpectingType)
  |. sps
  |= parseTypeName
  |. sps
  |. symbol (Token "=" ExpectingEqual)
  |. sps
  |= ( indent <|
    succeed Tuple.pair
    |= parseTypeVariant
    |. sps
    |= ( loop [] <|
      (\revVariants ->
        oneOf
          [ succeed (\variant -> Loop <| variant :: revVariants)
            |. symbol (Token "|" ExpectingVerticalBar)
            |. sps
            |= parseTypeVariant
            |. sps
          , succeed ()
            |> map (\_ -> Done <| List.reverse revVariants)
          ]
      )
    )
  )


parseTypeVariant : LambdaParser ((Located String), (Located Type))
parseTypeVariant =
  succeed Tuple.pair
    |= parseTypeName
    |. sps
    |= parseType


internalParseDef : LambdaParser ParserDef
internalParseDef =
  oneOf
    [ backtrackable parseDAlias
    , parseDType
    , parseDValue
    ]


internalParseExpr : LambdaParser (Located Expr)
internalParseExpr =
  let
    subexpr =
      Pratt.literal
  in
  Pratt.expression
    { oneOf =
      [ subexpr parseLetBinding
      , subexpr parseCase
      , subexpr parseVariant
      , subexpr parseApplication
      ]
    , andThenOneOf =
      [ Pratt.infixLeft 4 (symbol <| Token "==" <| ExpectingComparison CompEQ)
        (\leftExpr rightExpr -> { from = leftExpr.from, to = rightExpr.to, value = EComparison CompEQ leftExpr rightExpr })
      , Pratt.infixLeft 4 (symbol <| Token "!=" <| ExpectingComparison CompNE)
        (\leftExpr rightExpr -> { from = leftExpr.from, to = rightExpr.to, value = EComparison CompNE leftExpr rightExpr })
      , Pratt.infixLeft 4 (symbol <| Token "<=" <| ExpectingComparison CompLE)
        (\leftExpr rightExpr -> { from = leftExpr.from, to = rightExpr.to, value = EComparison CompLE leftExpr rightExpr })
      , Pratt.infixLeft 4 (symbol <| Token ">=" <| ExpectingComparison CompGE)
        (\leftExpr rightExpr -> { from = leftExpr.from, to = rightExpr.to, value = EComparison CompGE leftExpr rightExpr })
      , Pratt.infixLeft 4 (symbol <| Token "<" <| ExpectingComparison CompLT)
        (\leftExpr rightExpr -> { from = leftExpr.from, to = rightExpr.to, value = EComparison CompLT leftExpr rightExpr })
      , Pratt.infixLeft 4 (symbol <| Token ">" <| ExpectingComparison CompGT)
        (\leftExpr rightExpr -> { from = leftExpr.from, to = rightExpr.to, value = EComparison CompGT leftExpr rightExpr })
      , Pratt.infixLeft 6 (symbol <| Token "+" ExpectingPlus)
        (\leftExpr rightExpr -> { from = leftExpr.from, to = rightExpr.to, value = EAdd leftExpr rightExpr })
      , Pratt.infixLeft 6 (symbol <| Token "-" ExpectingMinus)
        (\leftExpr rightExpr -> { from = leftExpr.from, to = rightExpr.to, value = ESubtract leftExpr rightExpr })
      , Pratt.infixLeft 7 (symbol <| Token "*" ExpectingAsterisk)
        (\leftExpr rightExpr -> { from = leftExpr.from, to = rightExpr.to, value = EMultiplication leftExpr rightExpr })
      , Pratt.infixLeft 7 (symbol <| Token "/" ExpectingSlash)
        (\leftExpr rightExpr -> { from = leftExpr.from, to = rightExpr.to, value = EDivision leftExpr rightExpr })
      ]
    , spaces = sps
    }


parseVariant : LambdaParser (Located Expr)
parseVariant =
  located <|
  succeed
  EApplication
    |= map (\name -> withLocation name <| EVariable name) parseTypeName
    |. sps
    |= lazy (\_ -> internalParseExpr)


parseCase : LambdaParser (Located Expr)
parseCase =
  located <|
  succeed
    (\expr variants ->
      ECase expr variants
    )
    |. keyword (Token "case" ExpectingCase)
    |. sps
    |= lazy (\_ -> internalParseExpr)
    |. sps
    |. keyword (Token "of" ExpectingOf)
    |. sps
    |= loop Dict.empty
      (\variants ->
        oneOf
          [ succeed (\variantName valueName expr -> Loop <| Dict.insert variantName.value (variantName, valueName, expr) variants)
            |= parseTypeName
            |. sps
            |= parseName
            |. sps
            |. symbol (Token "->" ExpectingArrow)
            |. sps
            |= lazy (\_ -> internalParseExpr)
            |. sps
          , succeed ()
            |> map (\_ -> Done variants)
          ]
      )


parseApplication : LambdaParser (Located Expr)
parseApplication =
  let
    formApplication : Located Expr -> List (Located Expr) -> (Located Expr)
    formApplication lastE es =
      case es of
        [] ->
          lastE
        
        e1 :: prevEs ->
          let
            func =
              formApplication e1 prevEs
            
            arg =
              lastE
            
            location =
              { from =
                func.from
              , to =
                arg.to
              , value =
                ()
              }
          in
          withLocation location <| EApplication func arg
  in
  checkIndent <|
  map
    (List.reverse >>
      (\es ->
        case es of
          [] ->
            fakeLocatedExpr -- impossible
          
          e1 :: restEs ->
            formApplication e1 restEs
      )
    )
    parseApplicationHelper


parseLetBinding : LambdaParser (Located Expr)
parseLetBinding =
  located <|
  succeed
  (\binding expr ->
    ELet binding expr
  )
    |. keyword (Token "let" ExpectingLet)
    |= ( indent <|
      succeed Tuple.pair
      |. sps
      |= checkIndent parseName
      |. sps
      |. symbol (Token "=" ExpectingEqual)
      |= ( indent <|
        succeed identity
        |. sps
        |= lazy (\_ -> internalParseExpr)
        |. sps
        |. keyword (Token "in" ExpectingIn)
        |. sps
      )
    )
    |= lazy (\_ -> internalParseExpr)


parseApplicationHelper : LambdaParser (List (Located Expr))
parseApplicationHelper =
  checkIndent <|
  ( succeed identity
    |= parseComponent
    |> andThen
    (\e1 ->
      succeed (\es -> e1 :: es)
      |= loop []
        (\revExprs ->
          oneOf
            [ succeed (\expr -> Loop <| expr :: revExprs)
              |. backtrackable sps
              |= checkIndent parseComponent
            , succeed () |>
              map (\_ ->
                Done (List.reverse revExprs)
              )
            ]
        )
    )
  )


parseComponent : LambdaParser (Located Expr)
parseComponent =
  located <|
  succeed
  (\expr pairIndices ->
    case pairIndices of
      [] ->
        expr.value
      
      _ ->
        .value <|
        List.foldl
          (\index e ->
            case index.value of
              "1" ->
                withLocation e <| EPairAccess e (withLocation index PairIndexOne)
              
              "2" ->
                withLocation e <| EPairAccess e (withLocation index PairIndexTwo)

              _ ->
                withLocation e <| ERecordAccess e index

          )
          expr
          pairIndices
  )
  |= oneOf
    [ parseAbstraction
    , backtrackable parseUnit
    , parseGroupOrPair
    , parseRecord
    , parseIf
    , parseBool
    , parseInt
    , parseChar
    , parseVariable
    ]
  |= loop []
    (\revIndices ->
      oneOf
      [ succeed (\index -> Loop <| index :: revIndices)
        |. symbol (Token "." ExpectingDot)
        |= ( located <| oneOf
          [ map (\_ -> "1") <| symbol (Token "1" ExpectingOne)
          , map (\_ -> "2") <| symbol (Token "2" ExpectingTwo)
          , map .value parseName
          ]
        )
      , succeed ()
        |> map (\_ -> Done <| List.reverse revIndices)
      ]
    )


parseUnit : LambdaParser (Located Expr)
parseUnit =
  located <|
  succeed EUnit
    |. symbol (Token "(" ExpectingLeftParen)
    |. symbol (Token ")" ExpectingRightParen)


parseInt : LambdaParser (Located Expr)
parseInt =
  located <|
  map EInt <|
  succeed
  (\sign value ->
    case sign of
      Just _ ->
        negate value
      
      Nothing ->
        value
  )
  |= optional (symbol <| Token "-" ExpectingMinus)
  |= ( backtrackable <|
    int ExpectingInt ExpectingInt
    )


parseChar : LambdaParser (Located Expr)
parseChar =
  located <|
  succeed
    (\str ->
      EChar <|
      case String.uncons str of
        Just (c, _) ->
          c
        
        Nothing ->
          'I' -- impossible
    )
    |. symbol (Token "'" ExpectingSingleQuote)
    |= (getChompedString <| chompIf (\_ -> True) ExpectingChar)
    |. symbol (Token "'" ExpectingSingleQuote)


parseRecord : LambdaParser (Located Expr)
parseRecord =
  located <|
  map (\pairs ->
    ERecord <|
    List.foldl
      (\(label, value) record ->
        Dict.insert label.value (label, value) record
      )
      Dict.empty
      pairs
  ) <|
  sequence
    { start = Token "{" ExpectingLeftBrace
    , separator = Token "," ExpectingComma
    , end = Token "}" ExpectingRightBrace
    , spaces = sps
    , item =
      succeed Tuple.pair
        |= checkIndent parseName
        |. sps
        |. symbol (Token "=" ExpectingEqual)
        |. sps
        |= lazy (\_ -> internalParseExpr)
        |. sps
    , trailing = Forbidden
    }


parseGroupOrPair : LambdaParser (Located Expr)
parseGroupOrPair =
  located <|
  succeed
    (\e1 maybeE2 ->
      case maybeE2 of
        Just e2 ->
          EPair e1 e2
        
        Nothing ->
          e1.value
    )
    |. symbol (Token "(" ExpectingLeftParen)
    |. sps
    |= lazy (\_ -> internalParseExpr)
    |. sps
    |= ( optional <|
      succeed identity
      |. symbol (Token "," ExpectingComma)
      |. sps
      |= lazy (\_ -> internalParseExpr)
      |. sps
    )
    |. symbol (Token ")" ExpectingRightParen)


-- \x. x
parseAbstraction : LambdaParser (Located Expr)
parseAbstraction =
  inContext CAbstraction <|
  located <|
  succeed EAbstraction
    |. symbol (Token "\\" ExpectingBackslash)
    |. sps
    |= parseName
    |. sps
    |. symbol (Token ":" ExpectingColon)
    |. sps
    |= inContext CType parseType
    |. sps
    |. symbol (Token "." ExpectingDot)
    |. sps
    |= lazy (\_ -> internalParseExpr)


parseIf : LambdaParser (Located Expr)
parseIf =
  inContext CIf <|
  located <|
  succeed EIf
    |. keyword (Token "if" ExpectingIf)
    |. sps
    |= lazy (\_ -> internalParseExpr)
    |. sps
    |. keyword (Token "then" ExpectingThen)
    |. sps
    |= lazy (\_ -> internalParseExpr)
    |. sps
    |. keyword (Token "else" ExpectingElse)
    |. sps
    |= lazy (\_ -> internalParseExpr)


parseBool : LambdaParser (Located Expr)
parseBool =
  located <|
  map EBool <|
  oneOf
    [ map (\_ -> True) <| keyword (Token "true" ExpectingTrue)
    , map (\_ -> False) <| keyword (Token "false" ExpectingFalse)
    ]


parseType : LambdaParser (Located Type)
parseType =
  succeed
    (\t1 maybeT2 ->
      case maybeT2 of
        Nothing ->
          t1
        
        Just t2 ->
          withLocation
          { from =
            t1.from
          , to =
            t2.to
          , value =
            ()
          }
          <| TyFunc t1 t2
    )
    |= oneOf
      [ parseBaseType
      , parseVariantType
      , backtrackable parseUnitType
      , parseGroupOrPairType
      , parseRecordType
      ]
    |= (optional <|
      succeed identity
        |. sps
        |. symbol (Token "->" ExpectingArrow)
        |. sps
        |= lazy (\_ -> parseType)
    )


parseBaseType : LambdaParser (Located Type)
parseBaseType =
  located <|
  oneOf
    [ map (\_ -> TyBool) <| keyword (Token "Bool" ExpectingTyBool)
    , map (\_ -> TyInt) <| keyword (Token "Int" ExpectingTyInt)
    , map (\_ -> TyChar) <| keyword (Token "Char" ExpectingTyChar)
    ]


parseVariantType : LambdaParser (Located Type)
parseVariantType =
  map
  (\name ->
    withLocation name <|
    TyName name
  )
  parseTypeName


parseUnitType : LambdaParser (Located Type)
parseUnitType =
  map (\e -> withLocation e TyUnit) parseUnit


parseGroupOrPairType : LambdaParser (Located Type)
parseGroupOrPairType =
  located <|
  succeed
  (\ty1 maybeTy2 ->
    case maybeTy2 of
      Just ty2 ->
        TyPair ty1 ty2
      
      Nothing ->
        ty1.value
  )
    |. symbol (Token "(" ExpectingLeftParen)
    |. sps
    |= lazy (\_ -> parseType)
    |. sps
    |= ( optional <|
      succeed identity
      |. symbol (Token "," ExpectingComma)
      |. sps
      |= lazy (\_ -> parseType)
      |. sps
    )
    |. symbol (Token ")" ExpectingRightParen)


parseRecordType : LambdaParser (Located Type)
parseRecordType =
  located <|
  map (\pairs ->
    TyRecord <|
    List.foldl
      (\(label, ty) record ->
        Dict.insert label.value (label, ty) record
      )
      Dict.empty
      pairs
  ) <|
  sequence
    { start = Token "{" ExpectingLeftBrace
    , separator = Token "," ExpectingComma
    , end = Token "}" ExpectingRightBrace
    , spaces = sps
    , item =
      succeed Tuple.pair
        |= checkIndent parseName
        |. sps
        |. symbol (Token ":" ExpectingColon)
        |. sps
        |= lazy (\_ -> parseType)
        |. sps
    , trailing = Forbidden
    }


parseVariable : LambdaParser (Located Expr)
parseVariable =
  inContext CVariable <|
  map (\var ->
    withLocation var <|
    EVariable var
  ) <|
  located <|
  variable
    { start =
      Char.isLower
    , inner =
      Char.isAlphaNum
    , reserved =
      reserved
    , expecting =
      ExpectingVariable
    }


parseName : LambdaParser (Located String)
parseName =
  located <|
  variable
    { start =
      Char.isLower
    , inner =
      Char.isAlphaNum
    , reserved =
      reserved
    , expecting =
      ExpectingVariable
    }


parseTypeName : LambdaParser (Located String)
parseTypeName =
  located <|
  variable
    { start =
      Char.isUpper
    , inner =
      Char.isAlphaNum
    , reserved =
      reserved
    , expecting =
      ExpectingTypeName
    }


sps : LambdaParser ()
sps =
  loop 0 <| ifProgress <|
    oneOf
      [ succeed () |. symbol (Token "--" ExpectingStartOfLineComment) |. chompWhile (\c -> c /= '\n')
      , multiComment (Token "{-" ExpectingStartOfMultiLineComment) (Token "-}" ExpectingEndOfMultiLineComment) Nestable
      , spaces
      ]


ifProgress : LambdaParser a -> Int -> LambdaParser (Step Int ())
ifProgress parser offset =
  succeed identity
    |. parser
    |= getOffset
    |> map (\newOffset -> if offset == newOffset then Done () else Loop newOffset)


indent : LambdaParser a -> LambdaParser a
indent parser =
  succeed (\indentation ->
    indentation + 1
  )
  |= getIndent
  |> andThen (\newIndentation ->
    withIndent newIndentation parser
  )


checkIndent : LambdaParser a -> LambdaParser a
checkIndent parser =
  succeed (\indentation offset column source ->
    let
      upToColumn =
        String.slice (offset - column + 1) offset source

      isWhiteSpace : String -> Bool
      isWhiteSpace str =
        String.isEmpty <| String.trim str
    in
    ( parser
    , indentation < column
    , if isWhiteSpace upToColumn then
        column - 1
      else
        indentation
    )
    )
    |= getIndent
    |= getOffset
    |= getCol
    |= getSource
    |> andThen checkIndentHelp


checkIndentHelp : (LambdaParser a, Bool, Int) -> LambdaParser a
checkIndentHelp (parser, isIndented, actualIndentation) =
  if isIndented then
    withIndent actualIndentation parser
  else
    problem ExpectingIndent


showProblems : String -> List (DeadEnd Context Problem) -> String
showProblems src deadEnds =
  let
    deadEndGroups =
      List.Extra.groupWhile (\d1 d2 -> d1.row == d2.row && d1.col == d2.col) <| deadEnds
  in
  "-- PARSE PROBLEM\n\n"
  ++ (String.join "\n" <| List.map (showProblemsHelper src) deadEndGroups)


showProblemsHelper : String -> ((DeadEnd Context Problem), List (DeadEnd Context Problem)) -> String
showProblemsHelper src (first, rests) =
  let
    location =
      showProblemLocation first.row first.col src
    
    context =
      showProblemContextStack first.contextStack
  in
  location ++ "\n"
  ++ (let
        problemStrs =
          List.map (.problem >> showProblem) <| List.reverse <| first :: rests
      in
      "I'm expecting " ++ String.join " or " problemStrs
  )
  ++ (if String.isEmpty context then "" else " in the " ++ context)
  ++ "."


showProblem : Problem -> String
showProblem p =
  case p of
    ExpectingBackslash ->
      "a '\\'"
    
    ExpectingVariable ->
      "a variable"
    
    ExpectingDot ->
      "a '.'"

    ExpectingEqual ->
      "a '='"

    ExpectingLeftParen ->
      "a '('"

    ExpectingRightParen ->
      "a ')'"

    ExpectingColon ->
      "a ':'"

    ExpectingArrow ->
      "a '->'"

    ExpectingPlus ->
      "a '+'"

    ExpectingMinus ->
      "a '-'"

    ExpectingAsterisk ->
      "a '*'"

    ExpectingSlash ->
      "a '/'"

    ExpectingComma ->
      "a ','"

    ExpectingLeftBrace ->
      "a '{'"
    
    ExpectingRightBrace ->
      "a '}'"

    ExpectingVerticalBar ->
      "a '|'"

    ExpectingSingleQuote ->
      "a '''"

    ExpectingChar ->
      "a character"

    ExpectingOne ->
      "a '1'"
    
    ExpectingTwo ->
      "a '2'"

    ExpectingComparison comp ->
      "a '" ++ showComparison comp ++ "'"

    ExpectingStartOfLineComment ->
      "the start of a single-line comment '--'"
    
    ExpectingStartOfMultiLineComment ->
      "the start of a multi-line comment '{-'"
    
    ExpectingEndOfMultiLineComment ->
      "the end of a multi-line comment '-}'"

    ExpectingIndent ->
      "an indentation"

    ExpectingDefinition ->
      "a definition"

    ExpectingEndOfDefinition ->
      "the end of the definition"

    ExpectingEndOfExpression ->
      "the end of the expression"

    ExpectingTyBool ->
      "a type 'Bool'"

    ExpectingTyInt ->
      "a type 'Int'"
    
    ExpectingTyChar ->
      "a type 'Char'"

    ExpectingIf ->
      "a 'if'"
  
    ExpectingThen ->
      "a 'then'"
    
    ExpectingElse ->
      "a 'else'"

    ExpectingTrue ->
      "a 'true'"
    
    ExpectingFalse ->
      "a 'false'"

    ExpectingInt ->
      "an integer"

    ExpectingLet ->
      "the keyword 'let'"
    
    ExpectingIn ->
      "the keyword 'in'"

    ExpectingType ->
      "the keyword 'type'"
    
    ExpectingTypeName ->
      "a type name"
    
    ExpectingAlias ->
      "the keyword 'alias'"
    
    ExpectingCase ->
      "the keyword 'case'"
    
    ExpectingOf ->
      "the keyword 'of'"


showProblemContextStack : List { row : Int, col : Int, context : Context } -> String
showProblemContextStack contexts =
  String.join " of the " <| List.map (.context >> showProblemContext) contexts


showProblemContext : Context -> String
showProblemContext context =
  case context of
    CVariable ->
      "variable"

    CApplication ->
      "application"
    
    CAbstraction ->
      "abstraction"

    CIf ->
      "if expression"
    
    CType ->
      "type annotation"


showProblemLocation : Int -> Int -> String -> String
showProblemLocation row col src =
  let
    rawLine =
      getLine row src
    
    lineNumber =
      row
    
    line =
      String.fromInt lineNumber ++ "| " ++ (String.trimLeft <| rawLine)
    
    offset =
      String.length line - String.length rawLine - 1
    
    offsettedCol =
      offset + col
    
    underline =
      makeUnderline line offsettedCol (offsettedCol + 1)
  in
  line ++ "\n" ++ underline


showDefs : List Def -> String
showDefs defs =
  String.join "\n" <|
  List.map
    showDef
    defs


showDef : Def -> String
showDef def =
  case def of
    DValue { name, term } ->
      name.value ++ " = " ++ showTerm term.value
    
    DType { name, variants } ->
      showCustomType name variants
    
    DAlias { name, ty } ->
      "type alias " ++ name.value ++ " ="
      ++ (indentStr <| "\n" ++ showType ty.value)


showTerm : Term -> String
showTerm tm =
  showTermHelper [] tm


showTermHelper : List String -> Term -> String
showTermHelper names tm =
  case tm of
    TmVariable index ->
      Maybe.withDefault "IMPOSSIBLE" <| List.Extra.getAt index names
    
    TmApplication e1 e2 ->
      ( if needsWrapping e1.value
        && ( case e1.value of
          TmApplication _ _ ->
            False
          
          _ ->
            True
        ) then
          "(" ++ showTermHelper names e1.value ++ ")"
        else
          showTermHelper names e1.value
      )
      ++ " "
      ++ ( if needsWrapping e2.value
        then
          "(" ++ showTermHelper names e2.value ++ ")"
        else
          showTermHelper names e2.value
      )
    
    TmAbstraction boundVar boundType t1 ->
      if boundVar.value == "$variant" then
        case t1.value of
          TmVariant variantName _ _ ->
            variantName.value
          
          _ -> -- impossible
            "IMPOSSIBLE"
      else
        let
          (newNames, newBoundVar) =
            pickNewName names boundVar
        in
        "\\" ++ newBoundVar.value ++ ":" ++ showType boundType.value ++ ". " ++ showTermHelper newNames t1.value

    TmBool bool ->
      if bool then
        "true"
      else
        "false"

    TmInt int ->
      String.fromInt int

    TmPair e1 e2 ->
      "(" ++ showTermHelper names e1.value ++ ", " ++ showTermHelper names e2.value ++ ")"

    TmPairAccess pair index ->
      showTermHelper names pair.value ++ "." ++ showPairIndex index.value

    TmRecord r ->
      (\pairs ->
        if String.isEmpty pairs then
          "{}"
        else if not <| String.contains "\n" pairs then
          "{ " ++ pairs ++ " }"
        else
          indentStr <| "\n{ " ++ pairs ++ "\n}"
      ) <|
      String.join "\n, " <|
      Dict.foldr
        (\_ (label, value) list ->
          (label.value ++ " = " ++ showTermHelper names value.value) :: list
        )
        []
        r

    TmRecordAccess r label ->
      "(" ++ showTermHelper names r.value ++ ")." ++ label.value

    TmAdd left right ->
      showTermWrapped names tm left ++ " + " ++ showTermWrapped names tm right

    TmSubtract left right ->
      showTermWrapped names tm left ++ " - " ++ showTermWrapped names tm right

    TmMultiplication left right ->
      showTermWrapped names tm left ++ " * " ++ showTermWrapped names tm right

    TmDivision left right ->
      showTermWrapped names tm left ++ " / " ++ showTermWrapped names tm right

    TmComparison comp left right ->
      showTermWrapped names tm left ++ " " ++ showComparison comp ++ " " ++ showTermWrapped names tm right
    
    TmIf condition thenBranch elseBranch ->
      "if " ++ showTermHelper names condition.value ++ " then"
      ++ indentStr ( "\n" ++ showTermHelper names thenBranch.value )
      ++ "\nelse"
      ++ indentStr ( "\n" ++ showTermHelper names elseBranch.value)

    TmLet (label, e1) e2 ->
      let
        (newNames, newLabel) =
          pickNewName names label
      in
      indentStr <|
      "\nlet"
      ++ indentStr (
        "\n" ++ newLabel.value ++ " = " ++ showTermHelper names e1.value
      )
      ++ "\nin"
      ++ "\n" ++ showTermHelper newNames e2.value

    TmUnit ->
      "()"
    
    TmCase e variants ->
      "case " ++ showTermHelper names e.value ++ " of"
      ++ Dict.foldl
      (\_ (variantName, valueName, innerExpr) str ->
        indentStr (
          let
            (newNames, newValueName) =
              pickNewName names valueName
          in
          "\n" ++ variantName.value ++ " " ++ newValueName.value ++ " ->"
          ++ indentStr ("\n" ++ showTermHelper newNames innerExpr.value)
        ) ++ str
      )
      ""
      variants

    TmVariant variantName value _ ->
      variantName.value ++ " " ++ showTermHelper names value.value

    TmChar c ->
      "'" ++ String.fromChar c ++ "'"


showTermWrapped : List String -> Term -> Located Term -> String
showTermWrapped names tm subTm =
  if needsWrapping subTm.value
  && (
    getOperatorPrecedence tm
    > getOperatorPrecedence subTm.value
  )
  then
    "(" ++ showTermHelper names subTm.value ++ ")"
  else
    showTermHelper names subTm.value


getOperatorPrecedence : Term -> Int
getOperatorPrecedence tm =
  case tm of
    TmMultiplication _ _ ->
      7
    
    TmDivision _ _ ->
      7
    
    TmAdd _ _ ->
      6
    
    TmSubtract _ _ ->
      6
    
    TmComparison _ _ _ ->
      4
    
    _ ->
      0


needsWrapping : Term -> Bool
needsWrapping tm =
  case tm of
    TmVariable _ ->
      False
    
    TmBool _ ->
      False
    
    TmInt _ ->
      False
    
    TmChar _ ->
      False
    
    TmUnit ->
      False
    
    TmPair _ _ ->
      False
    
    TmPairAccess _ _ ->
      False

    TmRecord _ ->
      False
    
    TmRecordAccess _ _ ->
      False
    
    _ ->
      True


showPairIndex : PairIndex -> String
showPairIndex index =
  case index of
    PairIndexOne ->
      "1"
    
    PairIndexTwo ->
      "2"


showComparison : Comparison -> String
showComparison comp =
  case comp of
    CompEQ ->
      "=="
    
    CompNE ->
      "!="
    
    CompLT ->
      "<"
    
    CompLE ->
      "<="
    
    CompGT ->
      ">"
    
    CompGE ->
      ">="


showCustomType : (Located String) -> (Dict String (Located String, Located Type)) -> String
showCustomType name variants =
  "type " ++ name.value
    ++ "\n  = "
    ++ ( String.join "\n  | " <|
      List.map
      (\(label, (_, ty)) ->
        label ++ " " ++ showType ty.value
      )
      (Dict.toList variants)
    )


showType : Type -> String
showType t =
  case t of
    TyBool ->
      "Bool"

    TyInt ->
      "Int"

    TyUnit ->
      "()"
    
    TyChar ->
      "Char"

    TyName name ->
      name.value
    
    TyCustom name _ ->
      name.value

    TyPair t1 t2 ->
      "(" ++ showType t1.value ++ ", " ++ showType t2.value ++ ")"

    TyRecord r ->
      (\pairs ->
        if String.isEmpty pairs then
          "{}"
        else if not <| String.contains "\n" pairs then
          "{ " ++ pairs ++ " }"
        else
          indentStr <| "\n{ " ++ pairs ++ "\n}"
      ) <|
      String.join "\n, " <|
      Dict.foldr
        (\_ (label, ty) list ->
          (label.value ++ " = " ++ showType ty.value) :: list
        )
        []
        r

    TyFunc t1 t2 ->
      ( case t1.value of
        TyFunc _ _ ->
          "(" ++ showType t1.value ++ ")"
        
        _ ->
          showType t1.value
      )
      ++ "->"
      ++ showType t2.value


indentStr : String -> String
indentStr str =
  String.replace "\n" "\n  " str


located : LambdaParser a -> LambdaParser (Located a)
located parser =
  succeed Located
    |= getPosition
    |= parser
    |= getPosition


fakeLocatedExpr : Located Expr
fakeLocatedExpr =
  fakeLocated <| EVariable <| fakeLocated "IMPOSSIBLE"


fakeLocatedTerm : Located Term
fakeLocatedTerm =
  fakeLocated <| TmVariable -1


fakeDef : Def
fakeDef =
  DValue
    { name = fakeLocated "IMPOSSIBLE"
    , term = fakeLocatedTerm
    }


fakeType : Type
fakeType =
  TyName (fakeLocated "IMPOSSIBLE")


optional : LambdaParser a -> LambdaParser (Maybe a)
optional parser =
  oneOf
    [ backtrackable (map Just parser)
    , succeed Nothing
    ]


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


type alias Mappings =
  Dict String (Located Term)


exprToTerm : Mappings -> Located Expr -> Located Term
exprToTerm mappings expr =
  withLocation expr <|
  case expr.value of
    EVariable name ->
      case Dict.get name.value mappings of
        Nothing -> -- impossible
          TmVariable -1
        
        Just s ->
          s.value

    EAbstraction boundVar boundType e1 ->
      let
        innerMappings =
          mappings |>
          Dict.map (\_ s -> termShift 1 0 s) |>
          Dict.insert boundVar.value (withLocation boundVar <| TmVariable 0)
        
        newBoundType =
          case boundType.value of
            TyName name ->
              case Dict.get name.value mappings of
                Just ty ->
                  case ty.value of
                    TmVariant _ _ substTy ->
                      substTy
                    
                    _ ->
                      boundType
                
                Nothing ->
                  boundType
            
            _ ->
              boundType
      in
      TmAbstraction boundVar newBoundType <| exprToTerm innerMappings e1

    EApplication e1 e2 ->
      TmApplication
      (exprToTerm mappings e1)
      (exprToTerm mappings e2)

    EIf condition thenBranch elseBranch ->
      TmIf
      (exprToTerm mappings condition)
      (exprToTerm mappings thenBranch)
      (exprToTerm mappings elseBranch)

    ELet (label, e1) e2 ->
      let
        innerCtx =
          mappings |>
          Dict.map (\_ s -> termShift 1 0 s) |>
          Dict.insert label.value (withLocation label <| TmVariable 0)
      in
      TmLet
      (label, exprToTerm mappings e1)
      (exprToTerm innerCtx e2)

    ECase e variants ->
      TmCase
      (exprToTerm mappings e)
      ( Dict.map
        (\_ (variantName, valueName, innerExpr) ->
          let
            innerCtx =
              mappings |>
              Dict.map (\_ s -> termShift 1 0 s) |>
              Dict.insert valueName.value (withLocation valueName <| TmVariable 0)
          in
          (variantName, valueName, exprToTerm innerCtx innerExpr)
        )
        variants
      )

    EPair e1 e2 ->
      exprToTermBinaryHelper mappings TmPair e1 e2

    EPairAccess pair index ->
      TmPairAccess
      (exprToTerm mappings pair)
      index
    
    ERecord r ->
      TmRecord <|
        Dict.map
          (\_ (label, value) ->
            (label, exprToTerm mappings value)
          )
          r

    ERecordAccess record label ->
      TmRecordAccess
      (exprToTerm mappings record)
      label

    EAdd left right ->
      exprToTermBinaryHelper mappings TmAdd left right

    ESubtract left right ->
      exprToTermBinaryHelper mappings TmSubtract left right

    EMultiplication left right ->
      exprToTermBinaryHelper mappings TmMultiplication left right

    EDivision left right ->
      exprToTermBinaryHelper mappings TmDivision left right

    EComparison comp left right ->
      exprToTermBinaryHelper mappings (TmComparison comp) left right

    EBool bool ->
      TmBool bool

    EInt int ->
      TmInt int
    
    EChar char ->
      TmChar char
    
    EUnit ->
      TmUnit


exprToTermBinaryHelper : Mappings -> (Located Term -> Located Term -> Term) -> Located Expr -> Located Expr -> Term
exprToTermBinaryHelper mappings f left right =
  f
    (exprToTerm mappings left)
    (exprToTerm mappings right)


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
    
    TmVariant variantName value customType ->
      TmVariant
      variantName
      (termShift d c value)
      customType

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
    
    TmChar _ ->
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
    
    TmVariant variantName value customType ->
      TmVariant
      variantName
      (termSubst j s value)
      customType
    
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
    
    TmChar _ ->
      t.value
    
    TmUnit ->
      t.value


termSubstBinaryHelper : Int -> Located Term -> (Located Term -> Located Term -> Term) -> Located Term -> Located Term -> Term
termSubstBinaryHelper j s f left right =
  f
  (termSubst j s left)
  (termSubst j s right)


sortParserDefs : List ParserDef -> List ParserDef
sortParserDefs defs =
  let
    dependencies =
      List.foldl
      (\def deps ->
        case def of
          ParserDValue { name, expr } ->
            Dict.insert
            name.value
            (Set.toList <| getFreeNames expr.value)
            deps
          
          ParserDType { name, variants } ->
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
          
          ParserDAlias { name, ty } ->
            Dict.insert
            name.value
            (List.map .value <| getFreeTypes ty.value)
            deps
      )
      Dict.empty
      defs
    
    sortedNames =
      sortDependencies dependencies
  in
  List.filterMap
  identity <|
  List.map
  (\name ->
    List.Extra.find
      (\def ->
        (getParserDefName def).value == name
      )
      defs
  )
  sortedNames


getParserDefName : ParserDef -> Located String
getParserDefName def =
  case def of
    ParserDValue { name } ->
      name
    
    ParserDType { name } ->
      name
    
    ParserDAlias { name } ->
      name


getFreeTypes : Type -> List (Located String)
getFreeTypes ty =
  case ty of
    TyName label ->
      [ label ]
    
    TyCustom name _ ->
      [ name ]
    
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


getFreeNames : Expr -> Set String
getFreeNames expr =
  getFreeNamesHelper Set.empty expr


getFreeNamesHelper : Set String -> Expr -> Set String
getFreeNamesHelper boundVariables expr =
  case expr of
    EVariable name ->
      if Set.member name.value boundVariables then
        Set.empty
      else
        Set.singleton name.value 
    
    EAbstraction boundVar boundType innerExpr ->
      Set.union
      ( case boundType.value of
        TyName label ->
          Set.singleton label.value
        
        TyCustom name _ ->
          Set.singleton name.value
        
        _ ->
          Set.empty
      )
      (getFreeNamesHelper (Set.insert boundVar.value boundVariables) innerExpr.value)
    
    EApplication func arg ->
      Set.union
      (getFreeNamesHelper boundVariables func.value)
      (getFreeNamesHelper boundVariables arg.value)

    EBool _ ->
      Set.empty
    
    EInt _ ->
      Set.empty

    EChar _ ->
      Set.empty

    EUnit ->
      Set.empty

    EPair e1 e2 ->
      getFreeVariablesBinaryHelper boundVariables e1 e2

    EPairAccess pair _ ->
      getFreeNamesHelper boundVariables pair.value

    ERecord r ->
      Dict.foldl
        (\_ (_, value) freeVars ->
          Set.union
          (getFreeNamesHelper boundVariables value.value)
          freeVars
        )
        Set.empty
        r

    ERecordAccess record _ ->
      getFreeNamesHelper boundVariables record.value

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
      (getFreeNamesHelper boundVariables condition.value)
      ( Set.union
        (getFreeNamesHelper boundVariables thenBranch.value)
        (getFreeNamesHelper boundVariables elseBranch.value)
      )

    ELet (label, e1) e2 ->
      Set.union
      (getFreeNamesHelper boundVariables e1.value)
      (getFreeNamesHelper (Set.insert label.value boundVariables) e2.value)
  
    ECase e variants ->
      Set.union
      (getFreeNamesHelper boundVariables e.value)
      ( Dict.foldl
        (\_ (_, valueName, innerExpr) set ->
          Set.union
          ( getFreeNamesHelper
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
  (getFreeNamesHelper boundVariables left.value)
  (getFreeNamesHelper boundVariables right.value)


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
