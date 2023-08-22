module Bit.Morph exposing (char, n, only, value)

{-| [`Morph`](Morph#Morph) for a [`Bit`](https://dark.elm.dmy.fr/packages/lue-bird/elm-bits/latest/Bit)

@docs char, n, only, value

-}

import Bit exposing (Bit)
import Bit.Morph.Internal
import Morph exposing (Morph, MorphIndependently)
import N exposing (In, N, N1, To, Up, Up0, Up1)
import Value.Morph.Internal exposing (MorphValue)


{-| `n0` ↔ `O`, `n1` ↔ `I`
-}
n :
    MorphIndependently
        (N (In min_ (Up maxTo1_ To N1)) -> Result error_ Bit)
        (Bit -> N (In (Up0 minX_) (Up1 maxX_)))
n =
    Morph.oneToOne Bit.fromN Bit.toN


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


{-| [`MorphValue`](Value-Morph#MorphValue) from a [`Bit`](https://dark.elm.dmy.fr/packages/lue-bird/elm-bits/latest/Bit)
-}
value : MorphValue Bit
value =
    Morph.choice
        (\o i bit ->
            case bit of
                Bit.O ->
                    o ()

                Bit.I ->
                    i ()
        )
        |> Value.Morph.Internal.variant ( \() -> Bit.O, "zero" ) Value.Morph.Internal.unit
        |> Value.Morph.Internal.variant ( \() -> Bit.I, "one" ) Value.Morph.Internal.unit
        |> Value.Morph.Internal.choiceFinish
