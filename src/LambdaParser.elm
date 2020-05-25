module LambdaParser exposing (parseDefs, parseDef, parseDefOrExpr, parseExpr, showProblems, showDefs, showDef, showExpr, showType, showCustomType, fakeDef, fakeType, Def(..), Expr(..), Type(..), Comparison(..), PairIndex(..))


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


type Def
  = DValue
    { name : Located String
    , expr : Located Expr
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
    (succeed identity
      |= withIndent 0 internalParseDefs
      |. end ExpectingDefinition
    )
    src


parseDef : String -> Result (List (DeadEnd Context Problem)) Def
parseDef src =
  run
    (succeed identity
      |= withIndent 0 internalParseDef
      |. end ExpectingEndOfDefinition
    )
    src


parseDefOrExpr : String -> String -> Result (List (DeadEnd Context Problem)) Def
parseDefOrExpr exprName src =
  run
    ( succeed identity
      |. sps
      |= optionalWithDefault (fakeLocated exprName)
        ( succeed identity
          |= parseName
          |. sps
          |. symbol (Token "=" ExpectingEqual)
          |. sps
        )
      |> andThen
        (\name ->
          succeed (\expr -> { name = name, expr = expr })
          |= ( if isFakeLocated name then
              withIndent 0 internalParseExpr
            else
              withIndent 0 <| indent internalParseExpr
          )
          |. sps
        )
      |> andThen
        (\def ->
          succeed
            ( DValue
              { def
                | name =
                  if isFakeLocated def.name then
                    withLocation def.expr def.name.value
                  else
                    def.name
              }
            )
            |. (
              end <|
              if isFakeLocated def.name then
                ExpectingEndOfExpression
              else
                ExpectingEndOfDefinition
            )
        )
    )
    src


parseExpr : String -> Result (List (DeadEnd Context Problem)) (Located Expr)
parseExpr src =
  run
    (succeed identity
      |= withIndent 0 internalParseExpr
      |. end ExpectingEndOfExpression
    )
    src


internalParseDefs : LambdaParser (List Def)
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


parseDValue : LambdaParser Def
parseDValue =
  succeed identity
    |= parseName
    |. sps
    |. symbol (Token "=" ExpectingEqual)
    |> andThen
    (\name ->
      indent <|
      succeed (\expr -> DValue { name = name, expr = expr })
      |. sps
      |= internalParseExpr
    )


parseDAlias : LambdaParser Def
parseDAlias =
  succeed
    (\name ty ->
      DAlias
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


parseDType : LambdaParser Def
parseDType =
  succeed
  (\name (firstVariant, restVariants) ->
    DType
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


internalParseDef : LambdaParser Def
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
    DValue { name, expr } ->
      name.value ++ " = " ++ showExpr expr.value
    
    DType { name, variants } ->
      showCustomType name variants
    
    DAlias { name, ty } ->
      "type alias " ++ name.value ++ " ="
      ++ (indentStr <| "\n" ++ showType ty.value)


showExpr : Expr -> String
showExpr expr =
  case expr of
    EVariable name ->
      name.value
    
    EApplication e1 e2 ->
      ( if needsWrapping e1.value
        && ( case e1.value of
          EApplication _ _ ->
            False
          
          _ ->
            True
        ) then
          "(" ++ showExpr e1.value ++ ")"
        else
          showExpr e1.value
      )
      ++ " "
      ++ ( if needsWrapping e2.value
        then
          "(" ++ showExpr e2.value ++ ")"
        else
          showExpr e2.value
      )
    
    EAbstraction boundVar boundType innerExpr ->
      "\\" ++ boundVar.value ++ ":" ++ showType boundType.value ++ ". " ++ showExpr innerExpr.value

    EBool bool ->
      if bool then
        "true"
      else
        "false"

    EInt int ->
      String.fromInt int

    EPair e1 e2 ->
      "(" ++ showExpr e1.value ++ ", " ++ showExpr e2.value ++ ")"

    EPairAccess pair index ->
      showExpr pair.value ++ "." ++ showPairIndex index.value

    ERecord r ->
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
          (label.value ++ " = " ++ showExpr value.value) :: list
        )
        []
        r

    ERecordAccess r label ->
      "(" ++ showExpr r.value ++ ")." ++ label.value

    EAdd left right ->
      showExprWrapped expr left ++ " + " ++ showExprWrapped expr right

    ESubtract left right ->
      showExprWrapped expr left ++ " - " ++ showExprWrapped expr right

    EMultiplication left right ->
      showExprWrapped expr left ++ " * " ++ showExprWrapped expr right

    EDivision left right ->
      showExprWrapped expr left ++ " / " ++ showExprWrapped expr right

    EComparison comp left right ->
      showExprWrapped expr left ++ " " ++ showComparison comp ++ " " ++ showExprWrapped expr right
    
    EIf condition thenBranch elseBranch ->
      "if " ++ showExpr condition.value ++ " then"
      ++ indentStr ( "\n" ++ showExpr thenBranch.value )
      ++ "\nelse"
      ++ indentStr ( "\n" ++ showExpr elseBranch.value)

    ELet (label, e1) e2 ->
      indentStr <|
      "\nlet"
      ++ indentStr (
        "\n" ++ label.value ++ " = " ++ showExpr e1.value
      )
      ++ "\nin"
      ++ "\n" ++ showExpr e2.value

    EUnit ->
      "()"
    
    ECase e variants ->
      "case " ++ showExpr e.value ++ " of"
      ++ Dict.foldl
      (\_ (variantName, valueName, innerExpr) str ->
        indentStr (
          "\n" ++ variantName.value ++ " " ++ valueName.value ++ " ->"
          ++ indentStr ("\n" ++ showExpr innerExpr.value)
        ) ++ str
      )
      ""
      variants

    EChar c ->
      "'" ++ String.fromChar c ++ "'"


showExprWrapped : Expr -> Located Expr -> String
showExprWrapped expr subExpr =
  if needsWrapping subExpr.value
  && (
    getOperatorPrecedence expr
    > getOperatorPrecedence subExpr.value
  )
  then
    "(" ++ showExpr subExpr.value ++ ")"
  else
    showExpr subExpr.value


getOperatorPrecedence : Expr -> Int
getOperatorPrecedence expr =
  case expr of
    EMultiplication _ _ ->
      7
    
    EDivision _ _ ->
      7
    
    EAdd _ _ ->
      6
    
    ESubtract _ _ ->
      6
    
    EComparison _ _ _ ->
      4
    
    _ ->
      0


needsWrapping : Expr -> Bool
needsWrapping e =
  case e of
    EVariable _ ->
      False
    
    EBool _ ->
      False
    
    EInt _ ->
      False
    
    EChar _ ->
      False
    
    EUnit ->
      False
    
    EPair _ _ ->
      False
    
    EPairAccess _ _ ->
      False

    ERecord _ ->
      False
    
    ERecordAccess _ _ ->
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


fakeDef : Def
fakeDef =
  DValue
    { name = fakeLocated "IMPOSSIBLE"
    , expr = fakeLocatedExpr
    }


fakeType : Type
fakeType =
  TyName (fakeLocated "IMPOSSIBLE")


optionalWithDefault : a -> LambdaParser a -> LambdaParser a
optionalWithDefault default parser =
  oneOf
    [ backtrackable parser
    , succeed default
    ]


optional : LambdaParser a -> LambdaParser (Maybe a)
optional parser =
  oneOf
    [ backtrackable (map Just parser)
    , succeed Nothing
    ]