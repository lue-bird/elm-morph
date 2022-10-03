module Natural exposing
    ( fromArraySized
    , afterIMap
    , Natural(..), toArraySized
    )

{-| TODO incorporate into `Integer`

Natural number as bits

@docs NaturalBit


## create

@docs fromArraySized

Feeling motivated? implement & PR

  - int2 (bin), int8 (oct), int6 (hex)


## alter

@docs afterIMap


## transform

@docs toArraySizedMin

-}

import ArraySized exposing (ArraySized)
import Bit exposing (Bit)
import Bits
import Bitwise
import Linear exposing (Direction(..))
import N exposing (Add1, Down, Fixed, In, Min, MinValue, N, N0, N1, To, Up, Up0, Value, n0, n1)
import N.Local exposing (Add31, N31, N32, n31)


{-| Whole number (integer) >= 0.
Either the bit `O` directly or `I` followed by at most a given count of [`Bit`](Bit#Bit)s

    type alias NaturalArbitraryPrecision =
        NaturalBit N.Infinity

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
for arbitrary-precision arithmetic like addition, multiplication, ...!

-}
type Natural afterIBitSize
    = N0
    | Positive { afterI : ArraySized (In (Value N0) afterIBitSize) Bit }



-- alter


{-| Change the bits that follow after [`I` or `NegativeI`](#IntNegativeBit).
Commonly, you might use this with for example

    IntBit.afterIMap (ArraySized.max n32)

    IntBit.afterIMap (ArraySized.maxUp n24)

    IntBit.afterIMap ArraySized.maxToValue

-}
afterIMap :
    (ArraySized (In (Up minX To minX) afterIBitSize) Bit
     -> ArraySized (In (Fixed afterIMappedMin_) afterIMappedBitSize) Bit
    )
    ->
        (Natural afterIBitSize
         -> Natural afterIMappedBitSize
        )
afterIMap afterIChange =
    \naturalBit ->
        case naturalBit of
            N0 ->
                N0

            Positive { afterI } ->
                Positive
                    { afterI =
                        afterI
                            |> ArraySized.minFromValue
                            |> ArraySized.minTo n0
                            |> afterIChange
                            |> ArraySized.minTo n0
                            |> ArraySized.minToValue
                    }



-- to ArraySized


{-| Transform into an arbitrary-precision `ArraySized (Min (Up0 x_))`.
-}
toArraySized :
    Natural (Up maxX_ To maxPlusX_)
    -> ArraySized (Min (Up0 x_)) Bit
toArraySized =
    \naturalBit ->
        case naturalBit of
            N0 ->
                ArraySized.empty |> ArraySized.maxToInfinity

            Positive { afterI } ->
                afterI
                    |> ArraySized.minFromValue
                    |> ArraySized.minTo n0
                    |> ArraySized.insertMin ( Up, n0 ) Bit.I
                    |> ArraySized.minTo n0



-- from ArraySized


{-| Remove [`O`](Bit#Bit) padding at the front of the `ArraySized`
to get a [`NaturalBit`](#NaturalBit)
-}
fromArraySized :
    ArraySized (In (Fixed min_) (Fixed (Add1 maxPlus1))) Bit
    -> Natural (Fixed maxPlus1)
fromArraySized =
    \arraySized ->
        case arraySized |> Bits.unpad |> ArraySized.hasAtLeast n1 of
            Err _ ->
                N0

            Ok unpaddedAtLeast1 ->
                Positive
                    { afterI =
                        unpaddedAtLeast1
                            |> ArraySized.elementRemove ( Up, n0 )
                            |> ArraySized.minTo n0
                            |> ArraySized.minToValue
                    }
