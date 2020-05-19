module LambdaParser exposing (parseDefs, parseDef, parseDefOrExpr, parseExpr, showProblems, showDefs, showDef, showExpr, fakeDef, Def, Expr(..))


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
      |= optional (fakeLocated exprName)
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
      _ = Debug.log "AL -> indentation" <| indentation
      _ = Debug.log "AL -> column" <| column
      _ = Debug.log "AL -> String.slice (offset - 2) (offset - 1) source" <| String.slice (offset - 2) (offset - 1) source
      _ = Debug.log "AL -> String.slice (offset - 1) (offset) source" <| String.slice (offset - 1) offset source
      upToColumn =
        String.slice (offset - column + 1) offset source

      isWhiteSpace : String -> Bool
      isWhiteSpace str =
        let
          _ = Debug.log "AL -> str" <| str
        in
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
  let
    _ = Debug.log "AL -> isIndented" <| isIndented
  in
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
        EAbstraction _ _ ->
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

optional : a -> LambdaParser a -> LambdaParser a
optional default parser =
  oneOf
    [ backtrackable parser
    , succeed default
    ]