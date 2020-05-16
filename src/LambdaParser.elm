module LambdaParser exposing (parseDefs, parseDef, parseExpr, showDeadEnds, showDefs, showDef, showExpr)


import Parser.Advanced exposing (..)
import Set
import List.Extra


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


type alias Def =
  { name : String
  , value : Expr
  }


type Expr
  = EVariable String
  | EAbstraction String Expr
  | EApplication Expr Expr


parseDefs : String -> Result (List (DeadEnd Context Problem)) (List Def)
parseDefs src =
  run internalParseDefs src


parseDef : String -> Result (List (DeadEnd Context Problem)) Def
parseDef src =
  run internalParseDef src


parseExpr : String -> Result (List (DeadEnd Context Problem)) Expr
parseExpr src =
  run internalParseExpr src


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


internalParseExpr : LambdaParser Expr
internalParseExpr =
  let
    formApplication : Expr -> List Expr -> Expr
    formApplication lastE es =
      case es of
        [] ->
          lastE
        
        e1 :: prevEs ->
          EApplication (formApplication e1 prevEs) lastE
  in
  map
    (List.reverse >>
      (\es ->
        case es of
          [] ->
            EVariable "IMPOSSIBLE" -- impossible
          
          e1 :: restEs ->
            formApplication e1 restEs
      )
    )
    internalParseExprHelper


internalParseExprHelper : LambdaParser (List Expr)
internalParseExprHelper =
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


parseGroup : LambdaParser Expr
parseGroup =
  succeed identity
    |. symbol (Token "(" ExpectingLeftParen)
    |. sps
    |= lazy (\_ -> internalParseExpr)
    |. sps
    |. symbol (Token ")" ExpectingRightParen)


-- \x. x
parseAbstraction : LambdaParser Expr
parseAbstraction =
  inContext CAbstraction <|
  succeed EAbstraction
    |. symbol (Token "\\" ExpectingBackslash)
    |. sps
    |= parseName
    |. sps
    |. symbol (Token "." ExpectingDot)
    |. sps
    |= lazy (\_ -> internalParseExpr)


parseVariable : LambdaParser Expr
parseVariable =
  map EVariable parseName


parseName : LambdaParser String
parseName =
  inContext CVariable <|
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


optional : LambdaParser a -> LambdaParser (Maybe a)
optional parser =
  oneOf
    [ backtrackable (map Just parser)
    , succeed Nothing
    ]


showDeadEnds : String -> List (DeadEnd Context Problem) -> String
showDeadEnds src deadEnds =
  let
    deadEndGroups =
      List.Extra.groupWhile (\d1 d2 -> d1.row == d2.row && d1.col == d2.col) <| deadEnds
  in
  String.join "\n" <| List.map (showDeadEndsHelper src) deadEndGroups


showDeadEndsHelper : String -> ((DeadEnd Context Problem), List (DeadEnd Context Problem)) -> String
showDeadEndsHelper src (first, rests) =
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


makeUnderline : String -> Int -> Int -> String
makeUnderline row minCol maxCol =
  String.toList row
    |> List.indexedMap (\i _ -> toUnderlineChar minCol maxCol i)
    |> String.fromList


toUnderlineChar : Int -> Int -> Int -> Char
toUnderlineChar minCol maxCol col =
  if minCol <= col && col < maxCol then
    '^'
  else
    ' '


getLine : Int -> String -> String
getLine row src =
  Maybe.withDefault ("CAN'T GET LINE AT ROW " ++ String.fromInt row) -- impossible
    <| List.Extra.getAt (row - 1) <| String.split "\n" src


showDefs : List Def -> String
showDefs defs =
  String.join "\n" <|
  List.map
    showDef
    defs


showDef : Def -> String
showDef {name, value} =
  name ++ " = " ++ showExpr value


showExpr : Expr -> String
showExpr expr =
  case expr of
    EVariable name ->
      name
    
    EApplication e1 e2 ->
      ( case e1 of
        EVariable _ ->
          showExpr e1
        
        _ ->
          "(" ++ showExpr e1 ++ ")"
      ) ++ " " ++ showExpr e2
    
    EAbstraction boundVar innerExpr ->
      "\\" ++ boundVar ++ ". " ++ showExpr innerExpr

