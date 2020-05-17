module LambdaRepl exposing (main)


import Browser
import Browser.Dom
import Browser.Events
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
import Element.Border as Border
import Element.Events
import Element.Background as Background
import FeatherIcons


type alias Model =
  { cells : Dict Int Cell
  , activeCellIndex : Int
  , evalStrategy : EvalStrategy
  , orientation : Orientation
  , showHelpWindow : Bool
  }


type Msg
  = EditCell String
  | ActivateCell Int
  | AddCell
  | HandleKeyDown KeyboardEvent
  | HandleOrientation Int
  | OpenHelpWindow
  | CloseHelpWindow
  | NoOp


type alias Cell =
  (String, Result String Def)


type Orientation
  = Portrait
  | Landscape


colors =
  { lightGrey =
    E.rgb255 220 220 220
  }


styles =
  { title =
    [ Font.bold
    ]
  , subtitle =
    [ Font.italic
    ]
  }


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
    , activeCellIndex =
      0
    , evalStrategy =
      CallByValue
    , orientation =
      Landscape
    , showHelpWindow =
      False
    }
  , Task.perform (HandleOrientation << round << .width << .viewport) Browser.Dom.getViewport
  )


emptyCell : Cell
emptyCell =
  ("", Err "")


view : Model -> Html Msg
view model =
  E.layout
  ( [ Font.family
      [ Font.monospace
      ]
    , E.width ( E.fill |> E.maximum 700 )
    , E.htmlAttribute <| Html.Attributes.style "margin" "auto"
    , E.inFront <| viewHelpButton model
    , E.inFront <| viewHelpWindow model
    ] ++ case model.orientation of
      Landscape ->
        [ E.padding 30
        , Font.size 16
        ]
      
      Portrait ->
        [ E.padding 10
        , Font.size 20
        ]
  ) <|
  E.column
  [ E.spacing 15
  , E.width E.fill
  ] <|
  List.indexedMap
    (\index result ->
      viewCell model.activeCellIndex index result
    )
    (Dict.values model.cells)
  ++ [ viewAddCellButton ]


viewHelpWindow : Model -> E.Element Msg
viewHelpWindow model =
  if model.showHelpWindow then
    E.column
    [ E.centerX
    , E.centerY
    , E.padding 30
    , E.spacing 10
    , Background.color colors.lightGrey
    , E.inFront viewCloseHelpButton
    , Border.rounded 10
    ]
    [ E.el styles.title <| E.text "Help"
    , E.el styles.subtitle <| E.text "Keyboard Shortcuts"
    , E.row
      []
      [ E.html
        ( FeatherIcons.arrowUp
        |> FeatherIcons.toHtml
          [ Html.Attributes.style "margin-right" "20px" ]
        )
      , E.text "Go to previous cell"
      ]
    , E.row
      []
      [ E.html
        ( FeatherIcons.arrowDown
        |> FeatherIcons.toHtml
          [ Html.Attributes.style "margin-right" "20px" ]
        )
      , E.text "Go to next cell"
      ]
    , E.row
      []
      [ E.html
        ( FeatherIcons.cornerDownLeft
        |> FeatherIcons.toHtml
          [ Html.Attributes.style "margin-right" "20px" ]
        )
      , E.text "Add newline"
      ]
    , E.row
      [ E.spacing 5 ]
      [ E.text "Ctrl"
      , E.text "+"
      , E.html
        ( FeatherIcons.cornerDownLeft
        |> FeatherIcons.toHtml
          [ Html.Attributes.style "margin-right" "20px"
          , Html.Attributes.style "margin-left" "5px"
          ]
        )
      , E.text "Add cell"
      ]
    ]
  else
    E.none


viewCloseHelpButton : E.Element Msg
viewCloseHelpButton =
  Input.button
    [ E.alignRight ]
    { onPress =
      Just CloseHelpWindow
    , label =
      E.html
        ( FeatherIcons.xCircle
        |> FeatherIcons.toHtml []
        )
    }


viewHelpButton : Model -> E.Element Msg
viewHelpButton model =
  Input.button
    [ E.alignRight
    , E.padding 10
    ]
    { onPress =
      Just OpenHelpWindow
    , label =
      E.html
        ( FeatherIcons.helpCircle
        |> FeatherIcons.toHtml []
        )
    }


viewAddCellButton : E.Element Msg
viewAddCellButton =
  Input.button
    [ E.centerX ]
    { onPress =
      Just AddCell
    , label =
      E.html
        ( FeatherIcons.plusCircle
        |> FeatherIcons.toHtml []
        )
    }


viewCell : Int -> Int -> (String, Result String Def) -> E.Element Msg
viewCell activeCellIndex currentCellIndex (src, result) =
  let
    resultDisplay =
      E.el
      [ E.paddingEach
        { left =
          40
        , right =
          0
        , top =
          0
        , bottom =
          10
        }
      ] <|
      case result of
        Ok def ->
          E.text <| LambdaParser.showDef def
        
        Err msg ->
          E.html <|
          Html.pre
            [ Html.Attributes.style "white-space" "pre-wrap"
            , Html.Attributes.style "margin" "0"
            ]
            [ Html.text msg
            ]
  in
  E.column
    [ E.spacing 15
    , E.width E.fill
    ]
    [ E.row
      [ E.width E.fill]
      [ E.html
        ( FeatherIcons.chevronRight
        |> FeatherIcons.toHtml
          [ Html.Attributes.style "margin-right" "5px"
          , if activeCellIndex == currentCellIndex then
            Html.Attributes.style "color" "black"
          else
            Html.Attributes.style "color" "grey"
          ]
        )
      , if activeCellIndex == currentCellIndex then
          Input.multiline
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
            , spellcheck =
              False
            }
        else
          E.el
          [ E.htmlAttribute <| Html.Attributes.id <| "cell" ++ String.fromInt currentCellIndex
          , E.padding 10
          , E.htmlAttribute <| Html.Attributes.style "min-height" "calc(1em + 24px)"
          , Border.width 1
          , Border.rounded 5
          , Border.color colors.lightGrey
          , E.width E.fill
          , Element.Events.onClick <| ActivateCell currentCellIndex
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

    ActivateCell index ->
      activateCell index model

    AddCell ->
      addCell model

    HandleKeyDown event ->
      handleKeyDown event model

    HandleOrientation width ->
      handleOrientation width model

    OpenHelpWindow ->
      openHelpWindow model

    CloseHelpWindow ->
      closeHelpWindow model
      
    NoOp ->
      (model, Cmd.none)


closeHelpWindow : Model -> (Model, Cmd Msg)
closeHelpWindow model =
  ( { model
      | showHelpWindow =
        False
    }
  , Cmd.none
  )



openHelpWindow : Model -> (Model, Cmd Msg)
openHelpWindow model =
  ( { model
      | showHelpWindow =
        True
    }
  , Cmd.none
  )


handleOrientation : Int -> Model -> (Model, Cmd Msg)
handleOrientation width model =
  let
    orientation =
      if width <= 450 then
        Portrait
      else
        Landscape
  in
  ( { model
      | orientation =
        orientation
    }
  , Cmd.none
  )


activateCell : Int -> Model -> (Model, Cmd Msg)
activateCell index model =
  ( { model
      | activeCellIndex =
        index
    }
  , focusCell index
  )


handleKeyDown : KeyboardEvent -> Model -> (Model, Cmd Msg)
handleKeyDown { keyCode, ctrlKey } model =
  case keyCode of
    Keyboard.Key.Enter ->
      if ctrlKey then
        addCell model
      else
        (model, Cmd.none)
    
    Keyboard.Key.Backspace ->
      removeCell model

    Keyboard.Key.Up ->
      activatePreviousCell model

    Keyboard.Key.Down ->
      activateNextCell model

    _ ->
      (model, Cmd.none)


activateNextCell: Model -> (Model, Cmd Msg)
activateNextCell model =
  let
    oldActiveCellIndex =
      model.activeCellIndex

    newActiveCellIndex =
      if oldActiveCellIndex == Dict.size model.cells - 1 then
        oldActiveCellIndex
      else
        oldActiveCellIndex + 1
  in
  ( { model
      | activeCellIndex =
        newActiveCellIndex
    }
  , focusCell newActiveCellIndex
  )


activatePreviousCell: Model -> (Model, Cmd Msg)
activatePreviousCell model =
  let
    oldActiveCellIndex =
      model.activeCellIndex

    newActiveCellIndex =
      if oldActiveCellIndex == 0 then
        oldActiveCellIndex
      else
        oldActiveCellIndex - 1
  in
  ( { model
      | activeCellIndex =
        newActiveCellIndex
    }
  , focusCell newActiveCellIndex
  )


removeCell : Model -> (Model, Cmd Msg)
removeCell model =
  let
    (activeSrc, _) =
      getActiveCell model
  in
  if activeSrc == "" && Dict.size model.cells > 1 then
    let
      oldActiveCellIndex =
        model.activeCellIndex

      newActiveCellIndex =
        -- first cell
        if oldActiveCellIndex == 0 then
          oldActiveCellIndex
        else -- any cell after the first one
          oldActiveCellIndex - 1

      filterCell : Int -> Cell -> Dict Int Cell -> Dict Int Cell
      filterCell index cell cells =
        if index > oldActiveCellIndex then
          Dict.insert (index - 1) cell cells
        else if index == oldActiveCellIndex then
          cells
        else
          Dict.insert index cell cells
    in
    ( { model
        | cells =
          Dict.foldr
            filterCell
            Dict.empty
            model.cells
        , activeCellIndex =
          newActiveCellIndex
      }
    , focusCell newActiveCellIndex
    )
  else
    ( model
    , Cmd.none
    )


getActiveCell : Model -> Cell
getActiveCell model =
  Maybe.withDefault emptyCell <| -- impossible
  Dict.get model.activeCellIndex model.cells


addCell : Model -> (Model, Cmd Msg)
addCell model =
  let
    newActiveCellIndex =
      model.activeCellIndex + 1
  in
  ( { model
      | cells =
        Dict.foldl
          (\index cell cells ->
            if index > model.activeCellIndex then
              Dict.insert (index + 1) cell cells
            else
              Dict.insert index cell cells
          )
          (Dict.singleton newActiveCellIndex emptyCell)
          model.cells
      , activeCellIndex =
        newActiveCellIndex
    }
  , focusCell newActiveCellIndex
  )


focusCell : Int -> Cmd Msg
focusCell index =
  Task.attempt (\_ -> NoOp) <| Browser.Dom.focus <| "cell" ++ String.fromInt index


editCell : String -> Model -> (Model, Cmd Msg)
editCell newSrc model =
  ( { model
      | cells =
        Dict.update
          model.activeCellIndex
          (\_ ->
            let
              otherDefs =
                Dict.foldl
                  (\index (_, result) others ->
                    if index /= model.activeCellIndex then
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
            Just (newSrc, evalDef model.evalStrategy otherDefs newSrc)
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
  Browser.Events.onResize (\width _ -> HandleOrientation width)