module Char.Morph.Internal exposing (only)

import Morph exposing (Morph)


only : Char -> Morph () Char
only broadConstant =
    Morph.only
        (\char -> [ "'", char |> String.fromChar, "'" ] |> String.concat)
        broadConstant
