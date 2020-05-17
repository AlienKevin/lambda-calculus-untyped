port module LambdaRepl exposing (main)


import Browser
import Browser.Dom
import Browser.Events
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Keyboard.Event exposing (KeyboardEvent)
import Keyboard.Key
import Task
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Field as Field
import Json.Encode as Encode
import Dict exposing (Dict)
import LambdaParser exposing (fakeDef, Def, Expr)
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
import List.Extra
import Array


port saveModelPort : Encode.Value -> Cmd msg
port pageWillClosePort : (() -> msg) -> Sub msg


type alias Model =
  { cells : Dict Int Cell
  , activeCellIndex : Int
  , evalStrategy : EvalStrategy
  , orientation : Orientation
  , popUp : PopUp
  }


type Msg
  = EditCell String
  | ActivateCell Int
  | AddCell
  | HandleKeyDown KeyboardEvent
  | HandleOrientation Int
  | SetPopUp PopUp
  | SetEvalStrategy EvalStrategy
  | SaveModel ()
  | NoOp


type alias Cell =
  (String, Result String Def)


type Orientation
  = Portrait
  | Landscape


type PopUp
  = HelpPopUp
  | SettingsPopUp
  | NoPopUp


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
  , popUp =
    [ E.centerX
    , E.centerY
    , E.padding 30
    , E.spacing 10
    , Background.color colors.lightGrey
    , Border.rounded 10
    , E.inFront viewClosePopUpButton
    ]
  }


main : Program (Maybe String) Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


init : (Maybe String) -> (Model, Cmd Msg)
init savedModelStr =
  let
    defaultModel =
      { cells =
        Dict.singleton 0 emptyCell
      , activeCellIndex =
        0
      , evalStrategy =
        CallByValue
      , orientation =
        Landscape
      , popUp =
        NoPopUp
      }
    
    initialModel =
      case savedModelStr of
        Just str ->
          Result.withDefault defaultModel <| Decode.decodeString decodeModel str
        
        Nothing ->
          defaultModel
  in
  ( initialModel
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
    , E.inFront <| viewOpenPopUpButtons
    , E.inFront <| viewPopUp model
    ] ++ case model.orientation of
      Landscape ->
        [ E.padding 30
        , Font.size 16
        ]
      
      Portrait ->
        [ E.paddingEach
          { left =
            10
          , right =
            10
          , top = 
            45
          , bottom =
            10
          }
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


viewPopUp : Model -> E.Element Msg
viewPopUp model =
  case model.popUp of
    HelpPopUp ->
      viewHelpPopUp
    
    SettingsPopUp ->
      viewSettingsPopUp model
    
    NoPopUp ->
      E.none


viewSettingsPopUp : Model -> E.Element Msg
viewSettingsPopUp model =
  E.column
  styles.popUp
  [ E.el styles.title <| E.text "Settings"
  , E.el styles.subtitle <| E.text "Evaluation Strategy"
  , Input.radio
    [ E.padding 10
    , E.spacing 20
    ]
    { onChange = SetEvalStrategy
    , selected = Just model.evalStrategy
    , label = Input.labelHidden "Evaluation Strategy"
    , options =
        [ Input.option CallByValue (E.text "call by value")
        , Input.option CallByName (E.text "call by name")
        ]
    }
  ]


viewHelpPopUp : E.Element Msg
viewHelpPopUp =
  E.column
  styles.popUp
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


viewOpenPopUpButtons : E.Element Msg
viewOpenPopUpButtons =
  E.row
  [ E.alignRight
  , E.spacing 10
  ]
  [ viewHelpButton
  , viewSettingsButton
  ]


viewClosePopUpButton : E.Element Msg
viewClosePopUpButton =
  Input.button
    [ E.alignRight
    , E.padding 10
    ]
    { onPress =
      Just <| SetPopUp NoPopUp
    , label =
      E.html
        ( FeatherIcons.xCircle
        |> FeatherIcons.toHtml []
        )
    }


viewSettingsButton : E.Element Msg
viewSettingsButton =
  Input.button
    [ E.alignRight
    , E.padding 10
    ]
    { onPress =
      Just <| SetPopUp SettingsPopUp
    , label =
      E.html
        ( FeatherIcons.settings
        |> FeatherIcons.toHtml []
        )
    }


viewHelpButton : E.Element Msg
viewHelpButton =
  Input.button
    [ E.alignRight
    , E.padding 10
    ]
    { onPress =
      Just <| SetPopUp HelpPopUp
    , label =
      E.html
        ( FeatherIcons.helpCircle
        |> FeatherIcons.toHtml []
        )
    }


viewAddCellButton : E.Element Msg
viewAddCellButton =
  Input.button
    [ E.centerX
    , E.alignBottom
    , E.paddingXY 0 20
    ]
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
          72
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
      [ E.width E.fill ]
      [ E.el
        [ E.paddingEach
          { left =
            0
          , right =
            10
          , top =
            0
          , bottom =
            0
          }
        , E.width <| E.px 60
        ] <|
        E.el
        [ E.centerX
        , E.htmlAttribute <|
          if activeCellIndex == currentCellIndex then
            Html.Attributes.style "color" "black"
          else
            Html.Attributes.style "color" "grey"
        , E.below <|
          if activeCellIndex == currentCellIndex then
            viewAddCellButton
          else
            E.none
        ] <|
        E.text <| "[" ++ String.fromInt (currentCellIndex + 1) ++ "]"
      , if activeCellIndex == currentCellIndex then
          Input.multiline
            [ E.width E.fill
            , Input.focusedOnLoad
            , E.htmlAttribute <|
              Html.Events.on "keydown" <|
              Decode.map HandleKeyDown Keyboard.Event.decodeKeyboardEvent
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

    SetPopUp popUp ->
      setPopUp popUp model

    SetEvalStrategy strategy ->
      setEvalStrategy strategy model

    SaveModel _ ->
      saveModel model

    NoOp ->
      (model, Cmd.none)


saveModel : Model -> (Model, Cmd Msg)
saveModel model =
  ( model
  , saveModelPort <| encodeModel model
  )


setEvalStrategy : EvalStrategy -> Model -> (Model, Cmd Msg)
setEvalStrategy strategy model =
  ( evalAllCells
    { model
      | evalStrategy =
        strategy
    }
  , Cmd.none
  )


setPopUp : PopUp -> Model -> (Model, Cmd Msg)
setPopUp popUp model =
  ( { model
      | popUp =
        popUp
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
  ( evalAllCells
    { model
      | cells =
        Dict.update
          model.activeCellIndex
          (\_ ->
            Just (newSrc, Ok fakeDef) -- only need to update src
          )
          model.cells
    }
  , Cmd.none
  )


evalAllCells : Model -> Model
evalAllCells model =
  let
    unevaluatedCells =
      Dict.map
        (\_ (src, _) ->
          case LambdaParser.parseDef src of
            Err _ ->
              (src, Err "")
            
            Ok def ->
              (src, Ok def)
        )
        model.cells

    defs =
      List.foldl
        (\(_, result) definitions ->
          case result of
            Err _ ->
              definitions
            
            Ok def ->
              def :: definitions
        )
        []
        (Dict.values unevaluatedCells)
    
    sortedDefs =
      LambdaChecker.sortDefs defs

    indexedSrcs =
      Array.toList <|
      Dict.foldr
        (\index (source, result) sources ->
          case result of
            Err _ ->
              Array.push (index, source) sources

            Ok def ->
              case List.Extra.elemIndex def sortedDefs of
                Just sortedIndex ->
                  Array.set sortedIndex (index, source) sources
                
                Nothing -> -- impossible
                  Array.push (index, source) sources
        )
        (Array.repeat (List.length sortedDefs) (0, ""))
        unevaluatedCells
    
    srcs =
      List.map Tuple.second <| List.sortBy Tuple.first <| indexedSrcs
  in
  { model
    | cells =
      Tuple.first <|
      List.foldl
      (\(index, src) (cells, prevDefs) ->
        let
          result =
            evalDef model.evalStrategy prevDefs index srcs
        in
        ( Dict.insert index (src, result) cells
        , case result of
          Err _ ->
            fakeDef :: prevDefs
          
          Ok def ->
            def :: prevDefs
        )
      )
      (Dict.empty, [])
      indexedSrcs
  }


evalDef : LambdaEvaluator.EvalStrategy -> List Def -> Int -> List String -> Result String Def
evalDef strategy otherDefs currentIndex srcs =
  let
    src =
      Maybe.withDefault "" <| -- impossible
      List.Extra.getAt currentIndex srcs
  in
  case LambdaParser.parseDef src of
    Err problems ->
      Err <| LambdaParser.showProblems src problems
    
    Ok def ->
      case LambdaChecker.checkDefs <| insertToList currentIndex def otherDefs of
        [] ->
          Ok <| LambdaEvaluator.evalDef strategy otherDefs def

        problems ->
          Err <| LambdaChecker.showProblems srcs currentIndex problems
  

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Browser.Events.onResize (\width _ -> HandleOrientation width)
    , pageWillClosePort SaveModel
    ]


insertToList : Int -> a -> List a -> List a
insertToList index element list =
  let
    (before, after) =
      List.Extra.splitAt index list
  in
  before ++ (element :: after)


decodeModel : Decoder Model
decodeModel =
  Field.require "cells" (Decode.list Decode.string) <| \srcs ->
  Field.require "activeCellIndex" Decode.int <| \activeCellIndex ->
  Field.require "evalStrategy" decodeEvalStrategy <| \evalStrategy ->

  Decode.succeed <|
    evalAllCells
    { cells =
      Dict.fromList <|
      List.indexedMap
        (\index src ->
          (index, (src, Err ""))
        )
        srcs
    , activeCellIndex =
      activeCellIndex
    , evalStrategy =
      evalStrategy
    , orientation =
      Landscape
    , popUp =
      NoPopUp
    }


decodeEvalStrategy : Decoder EvalStrategy
decodeEvalStrategy =
  Decode.string |>
  Decode.andThen
    (\str ->
      case str of
        "CallByValue" ->
          Decode.succeed CallByValue
        
        "CallByName" ->
          Decode.succeed CallByName
        
        _ ->
          Decode.fail "Invalid evaluation strategy" 
    )


encodeModel : Model -> Encode.Value
encodeModel model =
  --  { cells : Dict Int Cell
  -- , activeCellIndex : Int
  -- , evalStrategy : EvalStrategy
  -- , orientation : Orientation
  -- , popUp : PopUp
  -- }
  Encode.object
    [ ( "cells", Encode.list Encode.string <| List.map Tuple.first <| Dict.values model.cells )
    , ( "activeCellIndex", Encode.int model.activeCellIndex )
    , ( "evalStrategy", encodeEvalStrategy model.evalStrategy )
    ]


encodeEvalStrategy : EvalStrategy -> Encode.Value
encodeEvalStrategy strategy =
  Encode.string <|
  case strategy of
    CallByValue ->
      "CallByValue"
    
    CallByName ->
      "CallByName"
