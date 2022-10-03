module Char.Morph exposing (only, string, value)

import Morph exposing (Morph)
import Value exposing (MorphValue)
import Value.Unexposed


{-| `Char` [`Morph`](#Morph)
-}
value : MorphValue Char
value =
    string
        |> Morph.over Value.Unexposed.string
        |> Morph.over Value.literal


string : Morph Char String
string =
    Morph.value "Char"
        { broaden = String.fromChar
        , narrow =
            \stringBroad ->
                case stringBroad |> String.uncons of
                    Just ( charValue, "" ) ->
                        charValue |> Ok

                    Just ( _, stringFrom1 ) ->
                        [ stringFrom1 |> String.length |> String.fromInt
                        , " too many characters"
                        ]
                            |> String.concat
                            |> Err

                    Nothing ->
                        "empty" |> Err
        }


{-| Match only the specific given broad input. See [`Morph.only`](Morph#only)
-}
only : Char -> Morph () Char
only broadConstant =
    Morph.only
        (\char ->
            [ "'", char |> String.fromChar, "'" ] |> String.concat
        )
        broadConstant
