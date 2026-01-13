module Bit.Morph exposing (char, only)

{-| [`Morph`](Morph#Morph) for a [`Bit`](https://dark.elm.dmy.fr/packages/lue-bird/elm-bits/latest/Bit)

@docs char, only

-}

import Bit exposing (Bit)
import Char.Morph.Internal
import Morph exposing (Morph, MorphIndependently)
import Util exposing (resultFromMaybeLazy)


{-| `'0'` or `'1'`
-}
char : Morph Bit Char
char =
    Morph.custom "0|1"
        { toBroad = toChar
        , toNarrow =
            \c ->
                c
                    |> fromChar
                    |> resultFromMaybeLazy (\() -> String.fromChar c)
        }


toChar : Bit -> Char
toChar bit =
    case bit of
        Bit.O ->
            '0'

        Bit.I ->
            '1'


fromChar : Char -> Maybe Bit
fromChar bit =
    case bit of
        '0' ->
            Just Bit.O

        '1' ->
            Just Bit.I

        _ ->
            Nothing


{-| Match a specific given `Bit` and not the other one.

    import Morph
    import Bit

    Bit.O |> Morph.toNarrow (Bit.Morph.only Bit.I)
    --> Err (Morph.DeadEnd "0")

-}
only : Bit -> Morph () Bit
only broadConstant =
    Morph.only
        (\bit -> bit |> toChar |> String.fromChar)
        broadConstant
