module Main exposing (main)

import Browser
import Element as Ui
import Element.Background as UiBackground
import Element.Border as UiBorder
import Element.Font as UiFont
import Element.Input as UiInput
import Email exposing (Email)
import Emptiable exposing (Emptiable)
import Forest.Navigate
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as Html
import List.Morph
import Morph exposing (Morph)
import Stack exposing (Stacked)
import Stack.Morph
import Tree exposing (Tree)
import TreeUi


type alias InitialJsInfo =
    ()


type alias State =
    { textInput : String
    , descriptionTreeViewModel : TreeUi.State Morph.DescriptionOrErrorKind
    }


type Event
    = TextInputChangedTo String
    | DescriptionTreeViewEvent TreeUi.Event


main : Program InitialJsInfo State Event
main =
    Browser.document
        { init = init
        , update = reactTo
        , subscriptions = \_ -> Sub.none
        , view = ui
        }


init : InitialJsInfo -> ( State, Cmd Event )
init initialJsData =
    ( initialStateForInput "jOn.dOe@example.com"
    , Cmd.none
    )


initialStateForInput : String -> State
initialStateForInput textInput =
    { textInput = textInput
    , descriptionTreeViewModel =
        (case textInput |> Morph.toNarrow emailString of
            Err error ->
                Morph.descriptionAndErrorToTree
                    (emailString |> Morph.description)
                    error

            Ok _ ->
                Morph.descriptionToTree
                    (emailString |> Morph.description)
                    |> Tree.map
                        (\labelString ->
                            { kind = labelString.kind |> Morph.DescriptionKind, text = labelString.text }
                        )
        )
            |> treeToTreeViewModel
    }


emailString : Morph Email String
emailString =
    Email.chars |> Morph.rowFinish |> Morph.over List.Morph.string


treeToTreeViewModel :
    Tree { text : String, kind : Morph.DescriptionOrErrorKind }
    -> TreeUi.State Morph.DescriptionOrErrorKind
treeToTreeViewModel =
    \tree ->
        tree |> treeToTreeViewNode |> List.singleton


treeToTreeViewNode :
    Tree { text : String, kind : Morph.DescriptionOrErrorKind }
    -> Tree (TreeUi.Label Morph.DescriptionOrErrorKind)
treeToTreeViewNode tree =
    let
        label =
            tree |> Tree.label

        childrenViewNodes =
            tree |> Tree.children |> List.map treeToTreeViewNode
    in
    Tree.tree
        { text = label.text
        , style = label.kind
        , open =
            if
                (childrenViewNodes |> List.any (\child -> (child |> Tree.label |> .open) == TreeUi.Open))
                    || (label.kind == Morph.ErrorKind)
            then
                TreeUi.Open

            else
                TreeUi.Closed
        }
        childrenViewNodes


reactTo : Event -> (State -> ( State, Cmd Event ))
reactTo event =
    case event of
        TextInputChangedTo newTextInput ->
            \state -> ( initialStateForInput newTextInput, Cmd.none )

        DescriptionTreeViewEvent (TreeUi.Toggled path) ->
            \state ->
                ( { state
                    | descriptionTreeViewModel =
                        Forest.Navigate.alter path
                            (Tree.mapLabel TreeUi.labelOpenClosedToggle)
                            state.descriptionTreeViewModel
                  }
                , Cmd.none
                )


ui : State -> Browser.Document Event
ui =
    \state ->
        { title = "morph debug"
        , body =
            Ui.column
                [ Ui.paddingXY 80 50
                , Ui.spacing 14
                ]
                [ Ui.el
                    [ UiFont.size 30
                    , Ui.paddingXY 0 20
                    ]
                    (Ui.text "morph an email")
                , Ui.row []
                    [ Ui.el
                        [ Ui.paddingEach { eachSide0 | right = 5 }
                        ]
                        (Html.i
                            [ HtmlA.class "fa fa-pencil"
                            , HtmlA.attribute "aria-hidden" "true"
                            , HtmlA.attribute "color" "rgb(255, 255, 0)"
                            ]
                            []
                            |> Ui.html
                        )
                    , UiInput.text
                        [ UiBackground.color (Ui.rgba 0 0 0 0)
                        , UiBorder.width 0
                        , HtmlA.style "color" "inherit" |> Ui.htmlAttribute
                        , UiFont.size 17
                        ]
                        { onChange = TextInputChangedTo
                        , text = state.textInput
                        , label = UiInput.labelHidden "sample email string"
                        , placeholder = Nothing
                        }
                    ]
                , [ case state.textInput |> Morph.toNarrow emailString of
                        Err error ->
                            Ui.text "failed:"

                        Ok email ->
                            Ui.text ("succeeded as " ++ (email |> Morph.toBroad emailString))
                  ]
                    |> Ui.paragraph [ UiFont.size 20 ]
                , Ui.map DescriptionTreeViewEvent
                    (TreeUi.ui toStyle state.descriptionTreeViewModel)
                ]
                |> Ui.layout
                    [ UiBackground.color (Ui.rgb 0 0 0)
                    , UiFont.color (Ui.rgb 1 1 1)
                    , UiFont.family [ UiFont.monospace ]
                    ]
                |> List.singleton
        }


eachSide0 : { left : Int, right : Int, top : Int, bottom : Int }
eachSide0 =
    { left = 0, right = 0, top = 0, bottom = 0 }


toStyle : Morph.DescriptionOrErrorKind -> TreeUi.Style
toStyle =
    \styleKind ->
        case styleKind of
            Morph.DescriptionKind Morph.DescriptionNameKind ->
                { attributes = [], icon = Just (TreeUi.iconAlways "fa fa-info") }

            Morph.DescriptionKind Morph.DescriptionStructureKind ->
                { attributes = [], icon = Just (TreeUi.iconAlways "fa fa-cog") }

            Morph.ErrorKind ->
                { attributes = [], icon = Just (TreeUi.iconAlways "fa fa-exclamation") }


treeLinesUi : Tree String -> Ui.Element event
treeLinesUi =
    \tree ->
        Ui.column []
            (tree
                |> Morph.treeToLines
                |> List.map
                    (\line ->
                        Ui.el [ HtmlA.style "white-space" "pre" |> Ui.htmlAttribute ]
                            (Ui.text line)
                    )
            )
