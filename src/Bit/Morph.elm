module Bit.Morph exposing (char, only)

{-| [`Morph`](Morph#Morph) for a [`Bit`](https://dark.elm.dmy.fr/packages/lue-bird/elm-bits/latest/Bit)

@docs char, only

-}

import Bit exposing (Bit)
import Bit.Morph.Internal
import Morph exposing (Morph, MorphIndependently)


{-| `'0'` or `'1'`
-}
char : Morph Bit Char
char =
    Bit.Morph.Internal.char


{-| Match a specific given `Bit` and not the other one.

    import Morph
    import Bit

    Bit.O |> Morph.toNarrow (Bit.Morph.only Bit.I)
    --> Err (Morph.DeadEnd "0")

-}
only : Bit -> Morph () Bit
only broadConstant =
    Bit.Morph.Internal.only broadConstant
