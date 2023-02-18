module Char.Morph exposing (only, string, value)

{-|

@docs only, string, value

-}

import Morph exposing (Morph)
import String.Morph.Internal
import Value


{-| `Char` [`Value.Morph`](Value#Morph)

Be aware, that [special-cased characters as the result of `Char.toUpper`](https://github.com/elm/core/issues/1001)
are [encoded](Morph#broadenFrom) as 2 `Char`s in a `String`
and therefore can't be [decoded](Morph#narrowTo) again

-}
value : Value.Morph Char
value =
    string |> Morph.over String.Morph.Internal.value


{-| [`Morph`](Morph#Morph) a `String` of length 1 to a `Char`

Be aware, that [special-cased characters as the result of `Char.toUpper`](https://github.com/elm/core/issues/1001)
are [encoded](Morph#broadenFrom) as 2 `Char`s in a `String`
and therefore can't be [decoded](Morph#narrowTo) again

-}
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
    Morph.only String.fromChar broadConstant
