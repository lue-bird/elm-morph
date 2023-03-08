module Bit.Morph exposing (char, n, toN)

{-| [`Bit`](https://dark.elm.dmy.fr/packages/lue-bird/elm-bits/latest/) [`Morph`](Morph#Morph)s

@docs char, n, toN

-}

import Bit exposing (Bit)
import Char.Morph
import Morph exposing (Morph, MorphIndependently)
import N exposing (In, N, N1, To, Up, Up0, Up1)


{-| `n0` ↔ `O`, `n1` ↔ `I`
-}
n :
    MorphIndependently
        (N (In min_ (Up maxTo1_ To N1)) -> Result error_ Bit)
        (Bit -> N (In (Up0 minX_) (Up1 maxX_)))
n =
    Morph.translate Bit.fromN Bit.toN


{-| `O` ↔ `n0`, `I` ↔ `n1`
-}
toN :
    MorphIndependently
        (Bit -> Result error_ (N (In (Up0 minX_) (Up1 maxX_))))
        (N (In min_ (Up maxTo1_ To N1)) -> Bit)
toN =
    Morph.invert n


{-| `'0'` or `'1'`
-}
char : Morph Bit Char
char =
    Morph.choice
        (\o i bit ->
            case bit of
                Bit.O ->
                    o ()

                Bit.I ->
                    i ()
        )
        |> Morph.try (\() -> Bit.O) (Char.Morph.only '0')
        |> Morph.try (\() -> Bit.I) (Char.Morph.only '1')
        |> Morph.choiceFinish