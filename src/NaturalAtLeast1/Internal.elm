module NaturalAtLeast1.Internal exposing (chars, n1, toN)

import ArraySized
import Bit
import Bits
import Linear exposing (Direction(..))
import Morph exposing (MorphRow)
import N exposing (Min, N, N0, On, n0)
import N.Local exposing (n32)
import NaturalAtLeast1Base10
import Number exposing (NaturalAtLeast1)


n1 : NaturalAtLeast1
n1 =
    { bitsAfterI =
        ArraySized.empty |> ArraySized.maxToInfinity |> ArraySized.minToNumber
    }


toN : NaturalAtLeast1 -> N (Min (On N0))
toN =
    \naturalAtLeast1 ->
        naturalAtLeast1.bitsAfterI
            |> ArraySized.minToOn
            |> ArraySized.insertMin ( Up, n0 ) Bit.I
            |> Bits.takeAtMost n32
            |> Bits.toN


chars : MorphRow NaturalAtLeast1 Char
chars =
    Morph.translate NaturalAtLeast1Base10.toBase2 NaturalAtLeast1Base10.fromBase2
        |> Morph.overRow NaturalAtLeast1Base10.chars
