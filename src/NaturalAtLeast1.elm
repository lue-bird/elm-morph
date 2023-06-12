module NaturalAtLeast1 exposing
    ( n1
    , add
    , chars
    )

{-| Helpers for [`Natural.AtLeast1`](Natural#AtLeast1)

@docs n1


## alter

@docs add


## morph

@docs chars

-}

import ArraySized exposing (ArraySized)
import Bit exposing (Bit)
import BitArray.Extra
import Linear exposing (Direction(..))
import Morph exposing (MorphRow)
import N exposing (Min, N0, N1, On, n0)
import Natural
import NaturalAtLeast1.Internal


{-| The [positive natural number](Natural#AtLeast1) 1
-}
n1 : Natural.AtLeast1
n1 =
    NaturalAtLeast1.Internal.n1


add :
    Natural.AtLeast1
    -> (Natural.AtLeast1 -> Natural.AtLeast1)
add toAdd =
    \naturalPositive ->
        let
            bitsSum : { inRange : ArraySized Bit (Min (On N1)), overflow : Bit }
            bitsSum =
                naturalPositive |> addBits toAdd

            sumBitsAfterI : ArraySized Bit (Min (On N0))
            sumBitsAfterI =
                case bitsSum.overflow of
                    Bit.I ->
                        bitsSum.inRange
                            |> ArraySized.minTo n0

                    Bit.O ->
                        bitsSum.inRange
                            |> ArraySized.removeMin ( Up, N.n1 )
        in
        { bitsAfterI =
            sumBitsAfterI |> ArraySized.minToNumber
        }


addBits :
    Natural.AtLeast1
    ->
        (Natural.AtLeast1
         ->
            { inRange : ArraySized Bit (Min (On N1))
            , overflow : Bit
            }
        )
addBits toAdd =
    \naturalAtLeast1 ->
        naturalAtLeast1
            |> toBitArray
            |> BitArray.Extra.add
                (toAdd |> toBitArray)


toBitArray :
    Natural.AtLeast1
    -> ArraySized Bit (Min (On N1))
toBitArray =
    \naturalAtLeast1 ->
        naturalAtLeast1.bitsAfterI
            |> ArraySized.minToOn
            |> ArraySized.insertMin ( Up, N.n1 ) Bit.I


chars : MorphRow Natural.AtLeast1 Char
chars =
    NaturalAtLeast1.Internal.chars
