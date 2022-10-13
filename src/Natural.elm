module Natural exposing (Natural(..))

{-| TODO incorporate into `Integer`

Natural number as bits

@docs Natural


## [`Morph`](Morph#Morph)

TODO bits, toBits

TODO rowChar

Feeling motivated? implement & PR

  - int2 (bin), int8 (oct), int6 (hex)

-}

import ArraySized exposing (ArraySized)
import Bit exposing (Bit)
import Bits
import Bitwise
import Linear exposing (Direction(..))
import Morph exposing (MorphIndependently)
import N exposing (Add1, Down, Fixed, In, InfinityValue, Min, MinValue, N, N0, N1, To, Up, Up0, Up1, Value, n0, n1)
import N.Local exposing (Add31, N31, N32, n31)


{-| Whole number (integer) >= 0 of arbitrary precision.
Either the bit `O` directly or `I` followed by at most a given count of [`Bit`](Bit#Bit)s

If you need a natural number representation with a specific number of bits, go

    ArraySized (Exactly bitLength) Bit

For larger numbers, where you want to allow numbers of arbitrary precision,
only `O | I ...` can enforce that `==` always gives the correct answer,
since the `ArraySized` could be constructed with leading `O`s!

Feel free to incorporate this into a new `type`
with variants `NaN`, `Infinity`, ... based on your specific use-case

The type is pretty rarely useful in its current state,
as the only thing you can do is convert from and to other types.

This is enough for my use-cases
but feel free to PR or open an issue if you'd like to see support
for arbitrary-precision arithmetic like addition, multiplication, ...

-}
type Natural
    = N0
    | Positive { afterI : ArraySized (In (Value N0) InfinityValue) Bit }



-- to ArraySized


{-| Remove `O` padding at the front of the `ArraySized`
to get a [`Natural`](#Natural)
-}
bits :
    MorphIndependently
        (ArraySized (In (Fixed narrowMin_) (Fixed narrowMax_)) Bit
         -> Result error_ Natural
        )
        (Natural
         -> ArraySized (Min (Up0 x_)) Bit
        )
bits =
    Morph.translate fromBitsImplementation toBitsImplementation


{-| Its bits as an `ArraySized (Min (Up0 x_))`.
Proceed from here over [`Bits`](https://dark.elm.dmy.fr/packages/lue-bird/elm-bits/latest/Bits)
-}
toBits :
    MorphIndependently
        (Natural
         -> Result error_ (ArraySized (Min (Up0 narrowX_)) Bit)
        )
        (ArraySized (In (Fixed broadMin_) (Fixed broadMax_)) Bit
         -> Natural
        )
toBits =
    Morph.translate toBitsImplementation fromBitsImplementation


toBitsImplementation :
    Natural
    -> ArraySized (Min (Up0 x_)) Bit
toBitsImplementation =
    \natural ->
        case natural of
            N0 ->
                ArraySized.empty |> ArraySized.maxToInfinity

            Positive { afterI } ->
                afterI
                    |> ArraySized.minFromValue
                    |> ArraySized.maxFromValue
                    |> ArraySized.minTo n0
                    |> ArraySized.insertMin ( Up, n0 ) Bit.I
                    |> ArraySized.minTo n0


fromBitsImplementation :
    ArraySized (In (Fixed min_) (Fixed max_)) Bit
    -> Natural
fromBitsImplementation =
    \arraySized ->
        case arraySized |> Bits.unpad |> ArraySized.hasAtLeast n1 of
            Err _ ->
                N0

            Ok unpaddedAtLeast1 ->
                Positive
                    { afterI =
                        unpaddedAtLeast1
                            |> ArraySized.elementRemoveMin ( Up, n0 )
                            |> ArraySized.minTo n0
                            |> ArraySized.maxToInfinity
                            |> ArraySized.minToValue
                            |> ArraySized.maxToValue
                    }
