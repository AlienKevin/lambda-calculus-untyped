module LambdaRepl exposing (main)


import Browser
import Browser.Dom
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Keyboard.Event exposing (KeyboardEvent)
import Keyboard.Key
import Task
import Json.Decode
import Dict exposing (Dict)
import LambdaParser exposing (Def, Expr)
import LambdaChecker
import LambdaEvaluator exposing (EvalStrategy(..))
import Location exposing (Located)
import Element as E
import Element.Input as Input
import Element.Font as Font


type alias Model =
  { cells : Dict Int Cell
  , editingCellIndex : Int
  , evalStrategy : EvalStrategy
  }


type Msg
  = EditCell String
  | HandleKeyDown KeyboardEvent
  | NoOp


type alias Cell =
  (String, Result String Def)


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


init : () -> (Model, Cmd Msg)
init _ =
  ( { cells =
      Dict.singleton 0 emptyCell
    , editingCellIndex =
      0
    , evalStrategy =
      CallByValue
    }
  , Cmd.none
  )


emptyCell : Cell
emptyCell =
  ("", Err "")


view : Model -> Html Msg
view model =
  E.layout
  [ Font.family
    [ Font.monospace
    ]
  , E.padding 30
  , E.width ( E.fill |> E.maximum 700 )
  , E.htmlAttribute <| Html.Attributes.style "margin" "auto"
  ] <|
  E.column
  [ E.spacing 15
  , E.width E.fill
  ] <|
  List.indexedMap
    (\index result ->
      viewCell model.editingCellIndex index result
    )
    (Dict.values model.cells)


viewCell : Int -> Int -> (String, Result String Def) -> E.Element Msg
viewCell editingCellIndex currentCellIndex (src, result) =
  let
    resultDisplay =
      case result of
        Ok def ->
          E.text <| "  " ++ LambdaParser.showDef def
        
        Err msg ->
          E.text <| msg
  in
  E.column
    [ E.spacing 15
    , E.width E.fill
    ]
    [ E.row
      [ E.width E.fill]
      [ E.text <| "> "
      , if editingCellIndex == currentCellIndex then
          Input.text
            [ E.width E.fill
            , Input.focusedOnLoad
            , E.htmlAttribute <|
              Html.Events.on "keydown" <|
              Json.Decode.map HandleKeyDown Keyboard.Event.decodeKeyboardEvent
            , E.htmlAttribute <| Html.Attributes.id <| "cell" ++ String.fromInt currentCellIndex
            ]
            { onChange =
              EditCell
            , text =
              src
            , placeholder =
              Nothing
            , label =
              Input.labelHidden "edit definition"
            }
        else
          E.el
          [ E.htmlAttribute <| Html.Attributes.id <| "cell" ++ String.fromInt currentCellIndex
          ] <|
          E.text src
      ]
    , resultDisplay
    ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    EditCell newSrc ->
      editCell newSrc model

    HandleKeyDown event ->
      handleKeyDown event model
      
    NoOp ->
      (model, Cmd.none)


handleKeyDown : KeyboardEvent -> Model -> (Model, Cmd Msg)
handleKeyDown { keyCode } model =
  case keyCode of
    Keyboard.Key.Enter ->
      addCell model
    
    _ ->
      (model, Cmd.none)


addCell : Model -> (Model, Cmd Msg)
addCell model =
  let
    newEditingCellIndex =
      model.editingCellIndex + 1
  in
  ( { model
      | cells =
        Dict.foldr
          (\index def ->
            if index > model.editingCellIndex then
              Dict.insert (index + 1) def
            else
              Dict.insert index def
          )
          (Dict.singleton newEditingCellIndex emptyCell)
          model.cells
      , editingCellIndex =
        newEditingCellIndex
    }
  , Task.attempt (\_ -> NoOp) <| Browser.Dom.focus <| "cell" ++ String.fromInt newEditingCellIndex
  )


editCell : String -> Model -> (Model, Cmd Msg)
editCell newSrc model =
  ( { model
      | cells =
        Dict.update
          model.editingCellIndex
          (\_ ->
            let
              otherCells =
                Dict.foldl
                  (\index (_, result) others ->
                    if index /= model.editingCellIndex then
                      case result of
                        Ok def ->
                          def :: others
                        
                        Err _ ->
                          others
                    else
                      others
                  )
                  []
                  model.cells
            in
            Just (newSrc, evalDef model.evalStrategy otherCells newSrc)
          )
          model.cells
    }
  , Cmd.none
  )


evalDef : LambdaEvaluator.EvalStrategy -> List Def -> String -> Result String Def
evalDef strategy otherDefs src =
  case LambdaParser.parseDef src of
    Err problems ->
      Err <| LambdaParser.showProblems src problems
    
    Ok def ->
      case LambdaChecker.checkDefs <| def :: otherDefs of
        [] ->
          Ok <| LambdaEvaluator.evalDef strategy otherDefs def

        problems ->
          Err <| LambdaChecker.showProblems src problems
  

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none