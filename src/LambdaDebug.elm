module LambdaDebug exposing (main)


import Html
import LambdaParser
import LambdaChecker
import LambdaEvaluator


source =
  -- "v = (\\x:Bool->Bool. x) (\\x:Bool. false) true"
  -- "id = if (\\x:Bool->Bool. x) (\\x:Bool. false) true then \\x:(Bool-> Bool) -> Bool. x else \\y:(Bool->Bool)->Bool. y"
--   """v = (id true)
-- v2 = negate (succ 0)
-- v3 = negate (pred 0)
-- v4 = half (double 29302)
-- -- v5 = \\x:Bool. (\\x:Bool. not (not false)) == (\\y:Bool. id (id (id true)))
-- v6 = 3 >= 4
-- v7 = (\\x:(Bool, (Int -> Int) -> Bool). (3, true)) (true, \\x: (Int -> Int). false)
-- v8 = ((\\x:(Int, Int). x) ((1, 2), v7).1).2
-- v9 = ((1, 2), (3, 4)).2.2
-- id = \\x:Bool. x
-- succ = \\a:Int. a + 1
-- pred = \\a:Int. a - 1
-- double = \\a:Int. a * 2
-- half = \\a:Int. a / 2
-- negate = \\a:Int. 2 + -2
-- eq = \\a:Int. \\b:Int. a + 2 != b - 2
-- not = \\a:Bool. if a then false else true
-- recordFunc = \\x:Bool. if x then { a = 3, b = true, ewioew = { b = 3, c = x } } else { a = 0, b = false, ewioew = { b = negate 3, c = false } }
-- record = recordFunc false
-- recordEquality = { a = { a = 1, b = 2 } , b = 2 } == { b = 2, a = { a = 1, b = 2 }  }
-- pairEquality = (1, (false, true)) == (1, (false, true))
-- recordAccess = ({ a = id (id (not true)), b = 4 }, 4).1.a
-- letBinding =
--   let
--     a = double (double 3)
--   in
--   double a
-- letBinding2 = let a = 3 in (let b = \\x:Int. x in b) (let c = a * 2 in c)
-- letBinding3 = let a = 3 in (let b = \\x:Int. x in b) (let c = 3 in c)
-- letBinding4 = let a = 1 in let b = a + 1 in let c = b + 1 in a + b + c
-- unit = (\\x:(). ()) ()
-- """
  -- """letBinding2 =
  -- let a = 3 in (let b = \\x:Int. x in b) (let c = a * 2 in c)
  -- """
--   """double =
--   \\x:Int. x * 2

-- letBinding =
--   let
--     a = 3
--   in
--   let
--     b = a
--   in
--   double (double b)
--   """
  -- "negate = \\a:Int. --2"
--   """p1 = Adult (Age 30)
-- p2 = Teen (Age 15)
-- p3 = quadruplePersonAge p2
-- -- p2 = (\\p: Person. p) (Person (Age 15))
-- -- p3 = (\\p: Adult. p)
-- -- p4 = (\\p: Eiowe. p)

-- a1 = Age 1
-- a2 = doubleAge a1

-- doubleAge = \\a: Age.
--   case a of
--     Age a ->
--       Age (a * 2)


-- isAdultAge = \\a: Age.
--   case a of
--     Age a ->
--       a >= 18


-- quadruplePersonAge = \\p:Person.
--   case p of
--     Adult age ->
--       Adult (doubleAge (doubleAge age))
    
--     Teen age ->
--       let
--         newAge =
--           doubleAge (doubleAge age)
--       in
--       if isAdultAge newAge then
--         Adult newAge
--       else
--         Teen newAge


-- type Person
--   = Adult Age
--   | Teen Age

-- type Age =
--   Age Int
--   """

  """type alias Position =
  (Int, Int)
p1 = (\\p:Position. p) (0, 1)
p2 = (2, 5)
squaredDistance = \\p1:Position. \\p2:Position.
  let
    dx =
      p2.1 - p1.1
  in
  let
    dy =
      p2.2 - p1.2
  in
  dx * dx + dy * dy

d1 = squaredDistance p1 p2
  """

main =
  let
    parseResult =
      LambdaParser.parseDefs source
  in
  Html.div []
    [ Html.h1 [] [ Html.text "Source code:" ]
    , Html.pre [] [ Html.text source ]
    , case parseResult of
      Err deadEnds ->
        Html.pre []
        [ Html.text <| "⚠️ I got stuck while parsing:\n"
        , Html.text <| LambdaParser.showProblems source deadEnds
        ]

      Ok defs ->
        Html.div []
          [ Html.h1 [] [ Html.text "Parse tree:"]
          , Html.pre [] [ Html.text <| LambdaParser.showDefs defs ]
          , case LambdaChecker.checkDefs defs of
            [] ->
              -- Html.text "✔ Passsed check!"
              let
                resultDefs =
                  LambdaEvaluator.evalDefs LambdaEvaluator.CallByValue defs
              in
              Html.div []
              [ Html.h1 [] [ Html.text "Evaluation result:" ]
              , Html.pre []
                [ Html.text <| LambdaParser.showDefs resultDefs
                ]
              ]
            
            problems ->
              Html.pre [] [ Html.text <| LambdaChecker.showProblemsWithSingleSource source problems ]
          ]
    ]

