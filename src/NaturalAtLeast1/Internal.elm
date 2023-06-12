module NaturalAtLeast1.Internal exposing (chars, n1, toBitArrayOfSize, toN)

import ArraySized exposing (ArraySized)
import Bit exposing (Bit)
import BitArray
import Linear exposing (Direction(..))
import Morph exposing (MorphRow)
import N exposing (In, Min, N, To, Up, Up0)
import Natural
import NaturalAtLeast1Base10


n1 : Natural.AtLeast1
n1 =
    { bitsAfterI =
        ArraySized.empty |> ArraySized.maxToInfinity |> ArraySized.minToNumber
    }


toN : Natural.AtLeast1 -> N (Min (Up0 minX_))
toN =
    \naturalAtLeast1 ->
        naturalAtLeast1.bitsAfterI
            |> ArraySized.minToOn
            |> ArraySized.insertMin ( Up, N.n1 ) Bit.I
            |> BitArray.toN


chars : MorphRow Natural.AtLeast1 Char
chars =
    Morph.translate NaturalAtLeast1Base10.toBase2 NaturalAtLeast1Base10.fromBase2
        |> Morph.overRow NaturalAtLeast1Base10.chars


toBitArrayOfSize :
    N (In (Up newMinX To newMinPlusX) newMax)
    ->
        (Natural.AtLeast1
         -> ArraySized Bit (In (Up newMinX To newMinPlusX) newMax)
        )
toBitArrayOfSize bitCount =
    \atLeast1 ->
        let
            withI =
                atLeast1.bitsAfterI
                    |> ArraySized.inToOn
                    |> ArraySized.insertMin ( Linear.Up, N.n1 ) Bit.I
        in
        if (withI |> ArraySized.length |> N.toInt) <= (bitCount |> N.toInt) then
            withI |> ArraySized.toSize Down bitCount (\_ -> Bit.O)

        else
            ArraySized.repeat Bit.I bitCount
