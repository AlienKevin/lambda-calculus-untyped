module LambdaParser exposing (parseDefs, parseDef, parseExpr, showProblems, showDefs, showDef, showExpr, fakeDef, Def, Expr(..))


import Parser.Advanced exposing (..)
import Set
import List.Extra
import Location exposing (..)


type alias LambdaParser a =
  Parser Context Problem a


type Context
  = CVariable
  | CAbstraction
  | CApplication


type Problem
  = ExpectingBackslash
  | ExpectingVariable
  | ExpectingDot
  | ExpectingEqual
  | ExpectingLeftParen
  | ExpectingRightParen
  | ExpectingStartOfLineComment
  | ExpectingStartOfMultiLineComment
  | ExpectingEndOfMultiLineComment
  | ExpectingIndent
  | ExpectingDefinition
  | ExpectingEndOfDefinition
  | ExpectingEndOfExpression


type alias Def =
  { name : Located String
  , expr : Located Expr
  }


type Expr
  = EVariable (Located String)
  | EAbstraction (Located String) (Located Expr)
  | EApplication (Located Expr) (Located Expr)


parseDefs : String -> Result (List (DeadEnd Context Problem)) (List Def)
parseDefs src =
  run
    (succeed identity
      |= internalParseDefs
      |. end ExpectingDefinition
    )
    src


parseDef : String -> Result (List (DeadEnd Context Problem)) Def
parseDef src =
  run
    (succeed identity
      |= internalParseDef
      |. end ExpectingEndOfDefinition
    )
    src


parseExpr : String -> Result (List (DeadEnd Context Problem)) (Located Expr)
parseExpr src =
  run
    (succeed identity
      |= internalParseExpr
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
  succeed Def
    |= parseName
    |. sps
    |. symbol (Token "=" ExpectingEqual)
    |. sps
    |= internalParseExpr


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
  succeed (\e1 es ->
    e1 :: es
  )
    |= oneOf
      [ parseAbstraction
      , parseGroup
      , parseVariable
      ]
    |. sps
    |= loop []
      (\revExprs ->
        oneOf
          [ succeed (\exprs -> Loop <| exprs ++ revExprs)
            |= internalParseExprHelper
          , succeed () |>
            map (\_ ->
              Done revExprs
            )
          ]
      )


parseGroup : LambdaParser (Located Expr)
parseGroup =
  succeed identity
    |. symbol (Token "(" ExpectingLeftParen)
    |. sps
    |= lazy (\_ -> internalParseExpr)
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
    |. symbol (Token "." ExpectingDot)
    |. sps
    |= lazy (\_ -> internalParseExpr)


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
      Set.empty
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


checkIndent : LambdaParser a -> LambdaParser a
checkIndent parser =
  succeed (\indentation column ->
    (parser, indentation < column)
    )
    |= getIndent
    |= getCol
    |> andThen checkIndentHelp


checkIndentHelp : (LambdaParser a, Bool) -> LambdaParser a
checkIndentHelp (parser, isIndented) =
  if isIndented then
    parser
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
        EVariable _ ->
          showExpr e1.value
        
        _ ->
          "(" ++ showExpr e1.value ++ ")"
      )
      ++ " "
      ++ case e2.value of
        EVariable _ ->
          showExpr e2.value      
        
        _ ->
          "(" ++ showExpr e2.value ++ ")"
    
    EAbstraction boundVar innerExpr ->
      "\\" ++ boundVar.value ++ ". " ++ showExpr innerExpr.value


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