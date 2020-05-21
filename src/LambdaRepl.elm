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
import Time


port saveModelPort : Encode.Value -> Cmd msg
port pageWillClosePort : (() -> msg) -> Sub msg
port askCellCursorRowPort : String -> Cmd msg
port gotCellCursorRowPort : (Int -> msg) -> Sub msg


type alias Model =
  { cells : Dict Int Cell
  , activeCellIndex : Int
  , pendingKeyAction : Maybe Keyboard.Key.Key
  , evalStrategy : EvalStrategy
  , orientation : Orientation
  , popUp : PopUp
  , toolTip : ToolTip
  , colors : Colors
  , theme : Theme
  , useLigatures: Bool
  }


type Msg
  = EditCell String
  | ActivateCell Int
  | AddCell
  | HandleKeyDown KeyboardEvent
  | GotCellCursorRow Int
  | HandleOrientation Int
  | SetPopUp PopUp
  | SetEvalStrategy EvalStrategy
  | SaveModel
  | SetToolTip ToolTip
  | CloseActiveCell
  | SetTheme Theme
  | SetUseLigatures Bool
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


type ToolTip
  = SavedToolTip
  | SettingsToolTip
  | HelpToolTip
  | NoToolTip


type Theme
  = Light
  | SolarizedLight
  | Dark


type alias Colors =
  { lightFg :
    E.Color
  , darkFg :
    E.Color
  , lightBg :
    E.Color
  , darkBg :
    E.Color
  }


styles =
  { title =
    [ Font.bold
    , E.htmlAttribute <| Html.Attributes.style "font-size" "125%"
    ]
  , subtitle =
    [ E.paddingXY 0 10
    , Font.bold
    , E.htmlAttribute <| Html.Attributes.style "font-size" "110%"
    ]
  , popUp =
    [ E.centerX
    , E.centerY
    , E.padding 30
    , E.spacing 10
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
      , pendingKeyAction =
        Nothing
      , evalStrategy =
        CallByValue
      , orientation =
        Landscape
      , popUp =
        NoPopUp
      , toolTip =
        NoToolTip
      , colors =
        getThemeColors Light
      , theme =
        Light
      , useLigatures =
        True
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
      [ Font.typeface "Fira Code"
      , Font.monospace
      ]
    , E.htmlAttribute <| Html.Attributes.style "font-variant-ligatures" <|
      if model.useLigatures then "normal" else "none"
    , E.inFront <| viewToolButtons model
    , E.inFront <| viewPopUp model
    , Font.size <| scale model.orientation 16
    , Font.color model.colors.darkFg
    , Background.color model.colors.lightBg
    ] ++ case model.orientation of
      Landscape ->
        [ E.padding 30
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
        ]
  ) <|
  E.column
  [ E.spacing 15
  , E.width ( E.fill |> E.maximum 700 )
  , E.htmlAttribute <| Html.Attributes.style "margin" "auto"
  , E.htmlAttribute <| Html.Attributes.style "z-index" "0"
  , E.alignTop
  ] <|
  List.indexedMap
    (\index result ->
      viewCell model.activeCellIndex index result model
    )
    (Dict.values model.cells)


viewPopUp : Model -> E.Element Msg
viewPopUp model =
  case model.popUp of
    HelpPopUp ->
      viewHelpPopUp model
    
    SettingsPopUp ->
      viewSettingsPopUp model
    
    NoPopUp ->
      E.none


viewSettingsPopUp : Model -> E.Element Msg
viewSettingsPopUp model =
  E.column
  ( styles.popUp ++ [ Background.color model.colors.darkBg ])
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
        , Input.option FullEvaluation (E.text "full evaluation")
        ]
    }
  , E.el styles.subtitle <| E.text "Color Theme"
  , Input.radio
    [ E.padding 10
    , E.spacing 20
    ]
    { onChange = SetTheme
    , selected = Just model.theme
    , label = Input.labelHidden "Color Theme"
    , options =
        [ Input.option Light (E.text "Light")
        , Input.option SolarizedLight (E.text "Solarized Light")
        , Input.option Dark (E.text "Dark")
        ]
    }
  , E.el styles.subtitle <| E.text "Ligatures"
  , Input.radioRow
    [ E.padding 10
    , E.spacing 20
    ]
    { onChange = SetUseLigatures
    , selected = Just model.useLigatures
    , label = Input.labelHidden "Ligatures"
    , options =
        [ Input.option True (E.text "On")
        , Input.option False (E.text "Off")
        ]
    }
  ]


viewHelpPopUp : Model -> E.Element Msg
viewHelpPopUp model =
  E.column
  (styles.popUp ++ [ Background.color model.colors.darkBg ])
  [ E.el styles.title <| E.text "Help"
  , E.el styles.subtitle <| E.text "Syntax"
  , E.paragraph
    [ E.htmlAttribute <| Html.Attributes.style "max-width" "60vw"
    ]
    [ E.text "Untyped lambda calculus has 3 components:"
    ]
  , E.text "Variable:    x"
  , E.text "Abstraction: \\x. t"
  , E.text "Application: t1 t2"
  , E.el styles.subtitle <| E.text "Keyboard Shortcuts"
  , E.row
    []
    [ E.html
      ( FeatherIcons.arrowUp
      |> FeatherIcons.toHtml
        [ Html.Attributes.style "margin-right" "20px" ]
      )
    , E.text "Go to prev cell"
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
  , E.el styles.subtitle <| E.text "Source Code"
  , E.newTabLink []
    { url =
      "https://github.com/AlienKevin/lambda-calculus-untyped"
    , label =
      E.el
      [ Font.underline ] <|
      E.text "Open source on GitHub"
    }
  ]


viewToolButtons : Model -> E.Element Msg
viewToolButtons model =
  E.row
  [ E.alignRight
  , E.spacing 20
  , E.paddingXY 10 10
  ]
  [ viewSavedButton model
  , viewHelpButton model
  , viewSettingsButton model
  ]


viewSavedButton : Model -> E.Element Msg
viewSavedButton model =
  E.el
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
  , Element.Events.onMouseEnter <| SetToolTip SavedToolTip
  , Element.Events.onMouseLeave <| SetToolTip NoToolTip
  , E.below <|
    case model.toolTip of
      SavedToolTip ->
        E.el
        [ E.htmlAttribute <| Html.Attributes.style "position" "relative"
        , E.htmlAttribute <| Html.Attributes.style "right" <| String.fromInt <| scale model.orientation 30
        ] <|
        E.text "Repl Saved"
      
      _ ->
        E.none
  ] <|
  E.el
  [ E.onRight <|
    E.html
    ( FeatherIcons.check
    |> FeatherIcons.withSize 12
    |> FeatherIcons.withStrokeWidth 4
    |> FeatherIcons.toHtml []
    )
  ] <|
  E.html
    ( FeatherIcons.save
    |> FeatherIcons.toHtml [ Html.Attributes.style "margin-right" "-3px" ]
    )


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


viewSettingsButton : Model -> E.Element Msg
viewSettingsButton model =
  Input.button
    [ E.alignRight
    , Element.Events.onMouseEnter <| SetToolTip SettingsToolTip
    , Element.Events.onMouseLeave <| SetToolTip NoToolTip
    , E.below <|
      case model.toolTip of
        SettingsToolTip ->
          E.el
          [ E.htmlAttribute <| Html.Attributes.style "position" "relative"
          , E.htmlAttribute <| Html.Attributes.style "right" <| String.fromInt <| scale model.orientation 40
          ] <|
          E.text "Settings"
        
        _ ->
          E.none
    ]
    { onPress =
      Just <| SetPopUp SettingsPopUp
    , label =
      E.html
        ( FeatherIcons.settings
        |> FeatherIcons.toHtml []
        )
    }


viewHelpButton : Model -> E.Element Msg
viewHelpButton model =
  Input.button
    [ E.alignRight
    , Element.Events.onMouseEnter <| SetToolTip HelpToolTip
    , Element.Events.onMouseLeave <| SetToolTip NoToolTip
    , E.below <|
      case model.toolTip of
        HelpToolTip ->
          E.el
          [ E.htmlAttribute <| Html.Attributes.style "position" "relative"
          , E.htmlAttribute <| Html.Attributes.style "right" <| String.fromInt <| scale model.orientation 5
          ] <|
          E.text "Help"
        
        _ ->
          E.none
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


viewCell : Int -> Int -> (String, Result String Def) -> Model -> E.Element Msg
viewCell activeCellIndex currentCellIndex (src, result) model =
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
          E.paragraph []
          [ E.text <|
            if String.startsWith "$" def.name.value then
              LambdaParser.showExpr def.expr.value
            else
              LambdaParser.showDef def
          ]
        
        Err msg ->
          E.html <|
          Html.pre
            [ Html.Attributes.style "white-space" "pre-wrap"
            , Html.Attributes.style "margin" "0"
            , Html.Attributes.style "color" "tomato"
            , Html.Attributes.style "font-family" "inherit"
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
        , if activeCellIndex == currentCellIndex then
            Font.color model.colors.darkFg
          else
            Font.color model.colors.lightFg
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
            , E.htmlAttribute <| Html.Attributes.id <| getCellId currentCellIndex
            , E.onRight <| viewRemoveActiveCellButton
            , Background.color model.colors.lightBg
            , E.htmlAttribute <| Html.Attributes.style "font-variant-ligatures" "inherit"
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
          [ E.htmlAttribute <| Html.Attributes.id <| getCellId currentCellIndex
          , E.padding 10
          , E.htmlAttribute <| Html.Attributes.style "min-height" "calc(1em + 24px)"
          , Border.width 1
          , Border.rounded 5
          , Border.color model.colors.lightFg
          , E.width E.fill
          , Element.Events.onClick <| ActivateCell currentCellIndex
          ] <|
          E.text src
      ]
    , resultDisplay
    ]


viewRemoveActiveCellButton : E.Element Msg
viewRemoveActiveCellButton =
  Input.button
    [ E.centerY
    , E.htmlAttribute <| Html.Attributes.style "margin-left" "-30px"
    , E.htmlAttribute <| onClickNoProp CloseActiveCell
    ]
    { onPress =
      Nothing
    , label =
      E.html
        ( FeatherIcons.xCircle
        |> FeatherIcons.toHtml []
        )
    }


onClickNoProp : msg -> Html.Attribute msg
onClickNoProp msg =
  Html.Events.stopPropagationOn "click" (Decode.map (\msg1 -> ( msg1, True )) (Decode.succeed msg))


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

    SaveModel ->
      saveModel model

    SetToolTip toolTip ->
      setToolTip toolTip model

    CloseActiveCell ->
      closeActiveCell model

    GotCellCursorRow row ->
      gotCellCursorRow row model

    SetTheme theme ->
      setTheme theme model

    SetUseLigatures useLigatures ->
      setUseLigatures useLigatures model

    NoOp ->
      (model, Cmd.none)


setUseLigatures : Bool -> Model -> (Model, Cmd Msg)
setUseLigatures useLigatures model =
  ( { model
      | useLigatures =
        useLigatures
    }
  , Cmd.none
  )


setTheme : Theme -> Model -> (Model, Cmd Msg)
setTheme theme model =
  ( { model
      | theme =
        theme
      , colors =
        getThemeColors theme
    }
  , Cmd.none
  )


getThemeColors : Theme -> Colors
getThemeColors theme =
  case theme of
    Light ->
      { lightFg =
        E.rgb255 220 220 220
      , darkFg =
        E.rgb255 0 0 0
      , lightBg =
        E.rgb255 255 255 255
      , darkBg =
        E.rgb255 220 220 220
      }
    
    SolarizedLight ->
      { lightFg =
        E.rgb255 220 220 220
      , darkFg =
        E.rgb255 0 0 0
      , lightBg =
        E.rgb255 253 246 227
      , darkBg =
        E.rgb255 220 220 220
      }
    
    Dark ->
      { lightBg =
        E.rgb255 0 0 0
      , darkBg =
        E.rgb255 150 150 150
      , lightFg =
        E.rgb255 180 180 180
      , darkFg =
        E.rgb255 255 255 255
      }


{- row
    = -1 -- cursor at middle of the cell
    = 0  -- cursor at both the top and the bottom of the cell (cell has a single row)
    = 1  -- cursor at the top of the cell
    = 2  -- cursor at the bottom of the cell
-}
gotCellCursorRow : Int -> Model -> (Model, Cmd Msg)
gotCellCursorRow row model =
  case model.pendingKeyAction of
    Just Keyboard.Key.Up ->
      if row == 0 || row == 1 then
        activatePreviousCell model
      else
        (model, Cmd.none)
    
    Just Keyboard.Key.Down ->
      if row == 0 || row == 2 then
        activateNextCell model
      else
        (model, Cmd.none)

    _ ->
      (model, Cmd.none)


closeActiveCell : Model -> (Model, Cmd Msg)
closeActiveCell model =
  let
    getNextCellIndex currentCell =
      currentCell
  in
  removeCell getNextCellIndex model


setToolTip : ToolTip -> Model -> (Model, Cmd Msg)
setToolTip toolTip model =
  ( { model
      | toolTip =
        toolTip
    }
  , Cmd.none
  )


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
      let
        (activeSrc, _) =
          getActiveCell model
        
        getNextCellIndex currentCell =
          currentCell - 1
      in
      if activeSrc == "" then
        removeCell getNextCellIndex model
      else
        (model, Cmd.none)

    Keyboard.Key.Up ->
      ( { model
          | pendingKeyAction =
            Just Keyboard.Key.Up
        }
      , askCellCursorRowPort <| getCellId model.activeCellIndex
      )

    Keyboard.Key.Down ->
      ( { model
          | pendingKeyAction =
            Just Keyboard.Key.Down
        }
      , askCellCursorRowPort <| getCellId model.activeCellIndex
      )

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


removeCell : (Int -> Int) -> Model -> (Model, Cmd Msg)
removeCell getNextCellIndex model =
  if Dict.size model.cells > 1 then
    let
      oldActiveCellIndex =
        model.activeCellIndex

      newActiveCellIndex =
        -- first cell
        if oldActiveCellIndex == 0 then
          oldActiveCellIndex
        else -- any cell after the first one
          getNextCellIndex oldActiveCellIndex

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
  Task.attempt (\_ -> NoOp) <| Browser.Dom.focus <| getCellId index


getCellId : Int -> String
getCellId index =
  "cell" ++ String.fromInt index


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
        (\index (src, _) ->
          let
            exprName =
              "$" ++ String.fromInt (index + 1)
          in
          case LambdaParser.parseDefOrExpr exprName src of
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
                  case Array.get sortedIndex sources of
                    Just s ->
                      if s == (-1, "") then
                        Array.set sortedIndex (index, source) sources
                      else
                        Array.push (index, source) sources
                    
                    Nothing -> -- impossible
                      Array.push (index, source) sources
                
                Nothing -> -- impossible
                  Array.push (index, source) sources
        )
        (Array.repeat (List.length sortedDefs) (-1, ""))
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
    
    exprName =
      "$" ++ String.fromInt (currentIndex + 1)
  in
  case LambdaParser.parseDefOrExpr exprName src of
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
    , pageWillClosePort (\_ -> SaveModel)
    , Time.every 1000 (\_ -> SaveModel)
    , gotCellCursorRowPort GotCellCursorRow
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
  Field.require "theme" decodeTheme <| \theme ->
  Field.require "useLigatures" Decode.bool <| \useLigatures ->

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
    , pendingKeyAction =
        Nothing
    , evalStrategy =
      evalStrategy
    , orientation =
      Landscape
    , popUp =
      NoPopUp
    , toolTip =
      NoToolTip
    , colors =
      getThemeColors theme
    , theme =
      theme
    , useLigatures =
      useLigatures
    }


decodeTheme : Decoder Theme
decodeTheme =
  Decode.string |>
  Decode.andThen
    (\str ->
      case str of
        "Light" ->
          Decode.succeed Light
        
        "SolarizedLight" ->
          Decode.succeed SolarizedLight

        "Dark" ->
          Decode.succeed Dark

        _ ->
          Decode.fail "Invalid theme" 
    )


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

        "FullEvaluation" ->
          Decode.succeed FullEvaluation

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
    , ( "theme", encodeTheme model.theme )
    , ( "useLigatures", Encode.bool model.useLigatures )
    ]


encodeTheme : Theme -> Encode.Value
encodeTheme theme =
  Encode.string <|
    case theme of
      Light ->
        "Light"
      
      SolarizedLight ->
        "SolarizedLight"

      Dark ->
        "Dark"


encodeEvalStrategy : EvalStrategy -> Encode.Value
encodeEvalStrategy strategy =
  Encode.string <|
  case strategy of
    CallByValue ->
      "CallByValue"
    
    CallByName ->
      "CallByName"

    FullEvaluation ->
      "FullEvaluation"


scale : Orientation -> Int -> Int
scale  orientation value =
  case orientation of
    Portrait ->
      round <| toFloat value
    
    Landscape ->
      value


elmUiColorToCssColor : E.Color -> String
elmUiColorToCssColor color =
  let
    { red, green, blue } =
      E.toRgb color
  in
  "rgb("
  ++ ( String.join "," <|
    List.map
      (\c ->
        String.fromFloat <| c * 255
      )
      [ red, green, blue ]
  )
  ++ ")"