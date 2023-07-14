module NaturalAtLeast1 exposing
    ( n1
    , add
    , chars, bits
    , toBitArrayOfSize
    )

{-| Package-internal helpers for [`Natural.AtLeast1`](Natural#AtLeast1).
**Should not be exposed**

@docs n1


## alter

@docs add


## morph

@docs chars, bits


## transform

@docs toBitArrayOfSize

-}

import ArraySized exposing (ArraySized)
import Bit exposing (Bit)
import Bit.Morph
import BitArray.Extra
import Linear exposing (Direction(..))
import Morph exposing (Morph, MorphRow)
import N exposing (In, Min, N, N1, On, To, Up)
import Natural
import NaturalAtLeast1Base10 exposing (NaturalAtLeast1Base10)


{-| The [positive natural number](Natural#AtLeast1) 1
-}
n1 : Natural.AtLeast1
n1 =
    { bitsAfterI = [] }


add :
    Natural.AtLeast1
    -> (Natural.AtLeast1 -> Natural.AtLeast1)
add toAdd =
    \naturalPositive ->
        let
            bitsSum : { inRange : ArraySized Bit (Min (On N1)), overflow : Bit }
            bitsSum =
                naturalPositive |> addBits toAdd

            sumBitsAfterI : List Bit
            sumBitsAfterI =
                case bitsSum.overflow of
                    Bit.I ->
                        bitsSum.inRange
                            |> ArraySized.toList

                    Bit.O ->
                        bitsSum.inRange
                            |> ArraySized.removeMin ( Up, N.n1 )
                            |> ArraySized.toList
        in
        { bitsAfterI = sumBitsAfterI }


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
            |> ArraySized.fromList
            |> ArraySized.insertMin ( Up, N.n1 ) Bit.I


chars : MorphRow Natural.AtLeast1 Char
chars =
    base10 |> Morph.overRow NaturalAtLeast1Base10.chars


base10 : Morph Natural.AtLeast1 NaturalAtLeast1Base10
base10 =
    Morph.oneToOne NaturalAtLeast1Base10.toBase2 NaturalAtLeast1Base10.fromBase2


bits : MorphRow Natural.AtLeast1 Bit
bits =
    Morph.named "≥ 1"
        (Morph.succeed (\bitsAfterI -> { bitsAfterI = bitsAfterI })
            |> Morph.grab .bitsAfterI bitsVariableCount
        )


bitsVariableCount : MorphRow (List Bit) Bit
bitsVariableCount =
    Morph.before
        { end = Bit.Morph.only Bit.O |> Morph.one
        , element =
            Morph.succeed (\bit -> bit)
                |> Morph.match (Bit.Morph.only Bit.I |> Morph.one)
                |> Morph.grab (\bit -> bit)
                    (Morph.keep |> Morph.one)
        }


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
                Bit.I
                    :: atLeast1.bitsAfterI
                    |> ArraySized.fromList
        in
        if (withI |> ArraySized.length |> N.toInt) <= (bitCount |> N.toInt) then
            withI |> ArraySized.toSize Down bitCount (\_ -> Bit.O)

        else
            ArraySized.repeat Bit.I bitCount
