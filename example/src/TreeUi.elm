module TreeUi exposing
    ( State, Event(..), ui
    , Label, Open(..), Style, Icon, iconAlways
    , openClosedToggle
    , labelOpenClosedToggle
    )

{-| customizable tree-view component

Usage example:

    import Browser
    import Forest
    import Html
    import Tree
    import TreeUi

    state : TreeUi.State StyleKind
    state =
        -- see State documentation
        [ Tree.tree { text = "Project A", style = Folder }
            [ Tree.tree { text = "Report 1" Folder }
                [ Tree.singleton { text = "report_1_revA.pdf", style = Pdf }
                , Tree.singleton { text = "report_1_revB.pdf", style = Pdf }
                , Tree.singleton { text = "report_1_revC.pdf", style = Pdf }
                ]
            , Tree.tree { text = "Report 2", style = Folder }
                [ Tree.singleton { text = "report_2_revA.pdf", style = Pdf }
                , Tree.singleton { text = "report_2_revB.pdf", style = Pdf }
                ]
            , Tree.singleton "lorem.doc" "word" True Nothing
            , Tree.singleton "ipsum.xls" "excel" True Nothing
            ]
        , Tree.tree { text = "Keynote", style = Folder }
            [ Tree.singleton { text = "workshop_part1.ppt", style = Powerpoint }
            , Tree.singleton { text = "workshop_part2.ppt", style = Powerpoint }
            , Tree.singleton { text = "image1.png", style = Image }
            , Tree.singleton { text = "image2.ppt", style = Image }
            , Tree.singleton { text = "image3.ppt", style = Image }
            , Tree.singleton { text = "image4.ppt", style = Image }
            ]
        ]
            |> Forest.map (\label -> { text = label.text, style = label.style, open = TreeUi.Closed })

    toStyle : StyleKind -> TreeUi.Style
    toStyle styleKind =
        -- see Style documentation
        case styleKind of
            Folder ->
                { icon = Just { closed = "folder yellow", open = "folder-open yellow" }, class = "" }

            Archive ->
                { icon = Just (TreeUi.iconAlways "file-archive-o"), class = "" }

            Word ->
                { icon = Just (iconAlways "file-word-o"), class = "blue" }

            Image ->
                { icon = Just (TreeUi.iconAlways "file-image-o"), class = "" }

            Pdf ->
                { icon = Just (TreeUi.iconAlways "file-pdf-o"), class = "red" }

            Powerpoint ->
                { icon = Just (TreeUi.iconAlways "file-powerpoint-o"), class = "orange" }

            Excel ->
                { icon = Just (TreeUi.iconAlways "file-excel-o"), class = "green" }

    config : TreeUi.Config
    config =
        { toStyle = toStyle }

    main : Program () TreeUi.State TreeUi.Event
    main =
        Browser.sandbox
            { init = state
            , view = TreeUi.ui config
            , update =
                \(TreeUi.Toggled path) ->
                    Forest.alterAt path TreeUi.labelOpenClosedToggle
            }

@docs State, Event, ui
@docs Label, Open, Style, Icon, iconAlways

@docs openClosedToggle

---

inspired by [`gribouille/elm-treeview`](http://package.elm-lang.org/packages/gribouille/elm-treeview/latest)

-}

import Element as Ui
import Element.Background as UiBackground
import Element.Border as UiBorder
import Element.Font as UiFont
import Element.Input as UiInput
import Forest exposing (Forest, ForestPath)
import Html exposing (Html)
import Html.Attributes as HtmlA
import Json.Decode as Decode
import Tree exposing (Tree)


{-| State of a tree-view.
-}
type alias State styleKind =
    Forest (Label styleKind)


{-| The element at each node of a tree-view.

Each node has:

  - title text
  - a style
  - information about whether it's [`open or Closed`](#open)

-}
type alias Label styleKind =
    { text : String
    , style : styleKind
    , open : Open
    }


{-| Define the style of a node.

Options:

  - `icon`: icon when the node is closed and when the node is open
  - `attributes`: elm-ui attributes of a node.

-}
type alias Style =
    { icon : Maybe Icon
    , attributes : List (Ui.Attribute Never)
    }


type Open
    = Open
    | Closed


type alias Icon =
    { open : String, closed : String }


iconAlways : String -> Icon
iconAlways iconName =
    { open = iconName, closed = iconName }


{-| Interaction events of tree-view:

  - `Toggled ForestPath`: a node has been open / closed

-}
type Event
    = Toggled ForestPath


{-| [`openClosedToggle`](#openClosedToggle) the [`Label`](#Label)

    -- toggle all items
    Forest.map TreeUi.labelOpenClosedToggle

    -- toggle at specific path
    Forest.alterAt forestPath TreeUi.labelOpenClosedToggle

-}
labelOpenClosedToggle : Label styleKind -> Label styleKind
labelOpenClosedToggle =
    \label ->
        { label | open = label.open |> openClosedToggle }


{-| `Open` ↔ `Closed`
-}
openClosedToggle : Open -> Open
openClosedToggle =
    \open ->
        case open of
            Open ->
                Closed

            Closed ->
                Open



--  ui


{-| Tree tree-view view function.
-}
ui : (styleKind -> Style) -> State styleKind -> Ui.Element Event
ui toStyle model =
    Ui.column
        [ Ui.spacing 10
        , UiFont.size 14
        ]
        (model
            |> List.indexedMap
                (\index ->
                    treeUi
                        { toStyle = toStyle
                        , path = { index = index, indexPath = [] }
                        , baseBefore = []
                        }
                )
        )


treeUi :
    { toStyle : styleKind -> Style
    , path : ForestPath
    , baseBefore : List (Ui.Element Event)
    }
    -> Tree (Label styleKind)
    -> Ui.Element Event
treeUi context tree =
    let
        baseUi : Ui.Element Event
        baseUi =
            Ui.row
                (style.attributes |> List.map (Ui.mapAttribute never))
                [ uiIf ((tree |> Tree.children) /= [])
                    (UiInput.button
                        [ UiBackground.color (Ui.rgba 0 0 0 0)
                        , HtmlA.style "color" "inherit" |> Ui.htmlAttribute
                        , UiBorder.width 0
                        , Ui.width (Ui.px 19)
                        , Ui.focused []
                        ]
                        { label =
                            icon (openClass (tree |> Tree.label |> .open))
                                |> Ui.el [ Ui.alignLeft ]
                        , onPress = Toggled context.path |> Just
                        }
                    )
                , uiOnJust style.icon
                    (\icon_ ->
                        Ui.el [ Ui.paddingEach { eachSide0 | right = 5 } ]
                            (icon
                                (case tree |> Tree.label |> .open of
                                    Open ->
                                        icon_.open

                                    Closed ->
                                        icon_.closed
                                )
                            )
                    )
                , Ui.text (tree |> Tree.label |> .text)
                ]

        style : Style
        style =
            tree |> Tree.label |> .style |> context.toStyle

        path : ForestPath
        path =
            context.path

        baseRow =
            Ui.row [ Ui.spacing 10 ]
                (context.baseBefore ++ [ baseUi ])
    in
    case tree |> Tree.label |> .open of
        Closed ->
            baseRow

        Open ->
            case tree |> Tree.children of
                [] ->
                    baseRow

                [ onlyChild ] ->
                    treeUi
                        { toStyle = context.toStyle
                        , path = { path | indexPath = path.indexPath ++ [ 0 ] }
                        , baseBefore = context.baseBefore ++ [ baseUi ]
                        }
                        onlyChild

                child0 :: child1 :: children1Up ->
                    Ui.column [ Ui.spacing 5 ]
                        [ baseRow
                        , Ui.column
                            [ UiBorder.widthEach { eachSide0 | left = 3 }
                            , UiBorder.color (colorForDepth (path.indexPath |> List.length))
                            , Ui.paddingEach { eachSide0 | left = 35 }
                            , Ui.spacing 4
                            ]
                            (List.indexedMap
                                (\index ->
                                    treeUi
                                        { toStyle = context.toStyle
                                        , path = { path | indexPath = path.indexPath ++ [ index ] }
                                        , baseBefore = []
                                        }
                                )
                                (child0 :: child1 :: children1Up)
                            )
                        ]


colorLayers : List Ui.Color
colorLayers =
    [ Ui.rgba 0.75 0.75 0 colorLineOpacity
    , Ui.rgba 0 0.75 0.75 colorLineOpacity
    , Ui.rgba 0.75 0 0.75 colorLineOpacity
    , Ui.rgba 1 0.2 0.2 colorLineOpacity
    , Ui.rgba 0 1 0 colorLineOpacity
    , Ui.rgba 0.2 0.2 0.8 colorLineOpacity
    ]


colorLineOpacity : Float
colorLineOpacity =
    0.24


colorForDepth : Int -> Ui.Color
colorForDepth depth =
    colorLayers
        |> listAt (depth |> Basics.remainderBy (colorLayers |> List.length))
        |> -- won't happen
           Maybe.withDefault (Ui.rgba 1 1 1 colorLineOpacity)


listAt : Int -> List a -> Maybe a
listAt index =
    \list ->
        list |> List.drop index |> List.head


eachSide0 : { left : Int, right : Int, top : Int, bottom : Int }
eachSide0 =
    { left = 0, right = 0, top = 0, bottom = 0 }


openClass : Open -> String
openClass =
    \open ->
        case open of
            Open ->
                -- "minus-square-o"
                "fa fa-angle-down"

            Closed ->
                -- "plus-square-o"
                "fa fa-angle-right"


icon : String -> Ui.Element msg
icon name =
    Html.i
        [ HtmlA.attribute "aria-hidden" "true"
        , HtmlA.class name
        ]
        []
        |> Ui.html



-- Optional ui component rendering.


uiOnJust : Maybe a -> (a -> Ui.Element msg) -> Ui.Element msg
uiOnJust maybeData dataToHtml =
    case maybeData of
        Nothing ->
            Ui.none

        Just data ->
            data |> dataToHtml


uiIf : Bool -> Ui.Element msg -> Ui.Element msg
uiIf condition htmlIfConditionTrue =
    if condition then
        htmlIfConditionTrue

    else
        Ui.none
