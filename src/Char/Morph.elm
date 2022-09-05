module Char.Morph exposing (only)

import Morph exposing (Morph)


{-| Match only the specific given broad input. See [`Morph.only`](Morph#only)
-}
only : Char -> Morph () Char
only broadConstant =
    Morph.only
        (\char ->
            [ "'", char |> String.fromChar, "'" ] |> String.concat
        )
        broadConstant
