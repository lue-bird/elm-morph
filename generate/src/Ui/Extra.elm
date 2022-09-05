module Ui.Extra exposing (edges, module_, slider)

import Element as Ui
import Element.Background as UiBg
import Element.Border as UiBorder
import Element.Font as UiFont
import Element.Input as UiInput
import Elm.CodeGen.Extra exposing (Module, stringFromModuleFile)
import Html
import Html.Attributes
import SyntaxHighlight


slider : { min : Int, max : Int, value : Int } -> (Int -> msg) -> Ui.Element msg
slider { min, max, value } changeValue =
    UiInput.slider
        [ Ui.behindContent
            (Ui.row [ Ui.width Ui.fill, Ui.centerY ]
                [ Ui.el
                    [ Ui.width (Ui.fillPortion (value - min))
                    , Ui.height (Ui.px 6)
                    , UiBg.color (Ui.rgb 0 0.45 0)
                    , Ui.centerY
                    , UiBorder.rounded 3
                    ]
                    Ui.none
                , Ui.el
                    [ Ui.width (Ui.fillPortion (max - value))
                    , Ui.height (Ui.px 4)
                    , UiBg.color (Ui.rgb 0 0.25 0.25)
                    , Ui.centerY
                    , UiBorder.rounded 2
                    ]
                    Ui.none
                ]
            )
        ]
        { onChange = round >> changeValue
        , label =
            UiInput.labelHidden (String.fromInt max)
        , min = toFloat min
        , max = toFloat max
        , value = toFloat value
        , thumb =
            UiInput.thumb
                [ Ui.behindContent
                    (Ui.el
                        [ Ui.centerY
                        , Ui.moveUp 20
                        , Ui.centerX
                        , UiFont.variant UiFont.ligatures
                        , UiFont.family [ UiFont.typeface "Fira Code" ]
                        , UiBg.color (Ui.rgba 0 0.25 0.25 0.4)
                        , UiBorder.rounded 16
                        , Ui.padding 9
                        , Ui.htmlAttribute
                            (Html.Attributes.style "-webkit-user-select" "none")
                        ]
                        (Ui.text (String.fromInt value))
                    )
                ]
        , step = Just 1
        }


module_ : Module tag_ -> Ui.Element msg_
module_ moduleFile =
    let
        string =
            moduleFile |> stringFromModuleFile
    in
    case string |> SyntaxHighlight.elm of
        Ok highlighted ->
            Ui.html
                (Html.div []
                    [ SyntaxHighlight.useTheme
                        SyntaxHighlight.monokai
                    , highlighted |> SyntaxHighlight.toInlineHtml
                    ]
                )

        Err err ->
            Ui.text
                ("syntax highlighting failed because "
                    ++ Debug.toString err
                    ++ "\n"
                    ++ string
                )


edges : { bottom : number, left : number, top : number, right : number }
edges =
    { bottom = 0, left = 0, top = 0, right = 0 }
