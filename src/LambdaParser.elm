module LambdaParser exposing (parseDefs, parseDef, parseDefOrExpr, parseExpr, showProblems, showDefs, showDef, showExpr, showType, fakeDef, Def, Expr(..), Type(..))


import Parser.Advanced exposing (..)
import Set exposing (Set)
import List.Extra
import Location exposing (..)


type alias LambdaParser a =
  Parser Context Problem a


type Context
  = CVariable
  | CAbstraction
  | CApplication
  | CIf


type Problem
  = ExpectingBackslash
  | ExpectingVariable
  | ExpectingDot
  | ExpectingEqual
  | ExpectingLeftParen
  | ExpectingRightParen
  | ExpectingColon
  | ExpectingArrow
  | ExpectingStartOfLineComment
  | ExpectingStartOfMultiLineComment
  | ExpectingEndOfMultiLineComment
  | ExpectingIndent
  | ExpectingDefinition
  | ExpectingEndOfDefinition
  | ExpectingEndOfExpression
  | ExpectingTyBool
  | ExpectingIf
  | ExpectingThen
  | ExpectingElse
  | ExpectingTrue
  | ExpectingFalse


type alias Def =
  { name : Located String
  , expr : Located Expr
  }


type Expr
  = EVariable (Located String)
  | EAbstraction (Located String) (Located Type) (Located Expr)
  | EApplication (Located Expr) (Located Expr)
  | EBool Bool
  | EIf (Located Expr) (Located Expr) (Located Expr) 


type Type
  = TyBool
  | TyFunc (Located Type) (Located Type)


reserved : Set String
reserved =
  Set.fromList
    [ "if", "then", "else", "true", "false" ]


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
          succeed (Def name)
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
            { def
              | name =
                if isFakeLocated def.name then
                  withLocation def.expr def.name.value
                else
                  def.name
            }
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


internalParseDef : LambdaParser Def
internalParseDef =
  succeed identity
    |= parseName
    |. sps
    |. symbol (Token "=" ExpectingEqual)
    |> andThen
    (\name ->
      indent <|
      succeed (Def name)
      |. sps
      |= internalParseExpr
    )


internalParseExpr : LambdaParser (Located Expr)
internalParseExpr =
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
    internalParseExprHelper


internalParseExprHelper : LambdaParser (List (Located Expr))
internalParseExprHelper =
  checkIndent <|
  ( succeed identity
    |= oneOf
      [ parseAbstraction
      , parseGroup
      , parseIf
      , parseBool
      , parseVariable
      ]
    |> andThen
    (\e1 ->
      succeed (\es -> e1 :: es)
      |= loop []
        (\revExprs ->
          oneOf
            [ succeed (\expr -> Loop <| expr :: revExprs)
              |. backtrackable sps
              |= ( checkIndent <| oneOf
                [ parseAbstraction
                , parseGroup
                , parseIf
                , parseBool
                , parseVariable
                ]
              )
            , succeed () |>
              map (\_ ->
                Done (List.reverse revExprs)
              )
            ]
        )
    )
  )


parseGroup : LambdaParser (Located Expr)
parseGroup =
  located <|
  succeed identity
    |. symbol (Token "(" ExpectingLeftParen)
    |. sps
    |= lazy (\_ -> map .value internalParseExpr)
    |. sps
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
    |= parseType
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
      , parseGroupType
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
  succeed TyBool
    |. keyword (Token "Bool" ExpectingTyBool)


parseGroupType : LambdaParser (Located Type)
parseGroupType =
  located <|
  succeed identity
    |. symbol (Token "(" ExpectingLeftParen)
    |= lazy (\_ -> map .value parseType)
    |. symbol (Token ")" ExpectingRightParen)


parseVariable : LambdaParser (Located Expr)
parseVariable =
  map (\var ->
    withLocation var <|
    EVariable var
  )
  parseName


parseName : LambdaParser (Located String)
parseName =
  inContext CVariable <|
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
showDef {name, expr} =
  name.value ++ " = " ++ showExpr expr.value


showExpr : Expr -> String
showExpr expr =
  case expr of
    EVariable name ->
      name.value
    
    EApplication e1 e2 ->
      ( case e1.value of
        EAbstraction _ _ _ ->
          "(" ++ showExpr e1.value ++ ")"
        
        _ ->
          showExpr e1.value
      )
      ++ " "
      ++ case e2.value of
        EVariable _ ->
          showExpr e2.value      
        
        _ ->
          "(" ++ showExpr e2.value ++ ")"
    
    EAbstraction boundVar boundType innerExpr ->
      "\\" ++ boundVar.value ++ ":" ++ showType boundType.value ++ ". " ++ showExpr innerExpr.value

    EBool bool ->
      if bool then
        "true"
      else
        "false"
    
    EIf condition thenBranch elseBranch ->
      "if " ++ showExpr condition.value
      ++ " then " ++ showExpr thenBranch.value
      ++ " else " ++ showExpr elseBranch.value


showType : Type -> String
showType t =
  case t of
    TyBool ->
      "Bool"

    TyFunc t1 t2 ->
      ( case t1.value of
        TyFunc _ _ ->
          "(" ++ showType t1.value ++ ")"
        
        _ ->
          showType t1.value
      )
      ++ "->"
      ++ showType t2.value


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
  { name = fakeLocated "IMPOSSIBLE"
  , expr = fakeLocatedExpr
  }

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