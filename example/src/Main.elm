module Main exposing (main)

import Browser
import Element as Ui
import Element.Background as UiBackground
import Element.Border as UiBorder
import Element.Font as UiFont
import Element.Input as UiInput
import Email
import Emptiable exposing (Emptiable)
import Forest
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as Html
import Morph
import Stack exposing (Stacked)
import Stack.Morph
import Tree exposing (Tree)
import TreeUi


type alias InitialJsInfo =
    ()


type alias State =
    { textInput : String
    , descriptionTreeViewModel : TreeUi.State Morph.LabelKind
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
        Morph.descriptionAndErrorToTree
            (Email.chars |> Morph.description)
            (case textInput |> Morph.toNarrow (Email.chars |> Morph.rowFinish |> Morph.over Stack.Morph.string) of
                Err error ->
                    Just error

                -- Just (Morph.DeadEnd "email is error")
                Ok _ ->
                    Just (Morph.DeadEnd "email is ok")
            )
            |> treeToTreeViewModel
    }


treeToTreeViewModel : Tree Morph.Label -> TreeUi.State Morph.LabelKind
treeToTreeViewModel =
    \tree ->
        tree |> treeToTreeViewNode |> List.singleton


treeToTreeViewNode : Tree Morph.Label -> Tree (TreeUi.Label Morph.LabelKind)
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
                    || (label.kind == Morph.LabelError)
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
                        Forest.alterAt path
                            TreeUi.labelOpenClosedToggle
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
                , Ui.map DescriptionTreeViewEvent
                    (TreeUi.ui toStyle state.descriptionTreeViewModel)

                -- , Ui.column []
                --     (List.map treeLinesUi (state.descriptionTreeViewModel |> Forest.map .text))
                , case state.textInput |> Morph.toNarrow (Email.chars |> Morph.rowFinish |> Morph.over Stack.Morph.string) of
                    Err error ->
                        [ Ui.text
                            (Debug.toString error
                                |> String.replace "InStructureError" "in"
                                |> String.replace "DeadEnd " "!"
                                |> String.replace ", index =" " at"
                                |> String.replace " error =" ""
                            )
                        ]
                            |> Ui.paragraph
                                [ HtmlA.style "overflow-wrap" "break-word" |> Ui.htmlAttribute
                                , UiFont.size 14
                                ]

                    Ok _ ->
                        Ui.text "email is ok"
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


toStyle : Morph.LabelKind -> TreeUi.Style
toStyle =
    \styleKind ->
        case styleKind of
            Morph.LabelDescriptionCustom ->
                { attributes = [], icon = Just (TreeUi.iconAlways "fa fa-info") }

            Morph.LabelDescriptionStructure ->
                { attributes = [], icon = Just (TreeUi.iconAlways "fa fa-cog") }

            Morph.LabelError ->
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
