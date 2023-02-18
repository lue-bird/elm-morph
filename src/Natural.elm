module Natural exposing (Natural(..))

{-| TODO incorporate into `Integer`

Natural number as bits

@docs Natural


## [`Morph`](Morph#Morph)

TODO bits, toBits

TODO rowChar

TODO integer

Feeling motivated? implement & PR

  - int2 (bin), int8 (oct), int6 (hex)

-}

import ArraySized exposing (ArraySized)
import Bit exposing (Bit)
import Bits
import Bitwise
import Integer exposing (Integer)
import Linear exposing (Direction(..))
import Morph exposing (Morph, MorphIndependently)
import N exposing (Add1, Down, In, Min, N, N0, N1, On, To, Up, Up0, Up1, n0, n1)
import N.Local exposing (Add31, N31, N32, n31)
import Sign


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
    | AtLeast1 { afterI : ArraySized Bit (Min N0) }



-- Morph


{-| [`Morph`](Morph#Morph) to a [`Natural`](#Natural)
from an unsigned [`Integer`](Integer#Integer)
-}
integer : Morph Natural Integer
integer =
    Morph.to "natural"
        (Morph.choiceToFrom
            ( \variantN0 variantSigned integerChoice ->
                case integerChoice of
                    Integer.N0 ->
                        variantN0 ()

                    Integer.Signed signedValue ->
                        variantSigned signedValue
            , \variantN0 variantAtLeast1 natural ->
                case natural of
                    N0 ->
                        variantN0 ()

                    AtLeast1 atLeast1Value ->
                        variantAtLeast1 atLeast1Value
            )
            |> Morph.variant ( \() -> N0, \() -> Integer.N0 )
                (Morph.broad ())
            |> Morph.variant ( AtLeast1, Integer.Signed )
                (Morph.value "positive"
                    { narrow =
                        \{ sign, absoluteAfterI } ->
                            case sign of
                                Sign.Negative ->
                                    "negative" |> Err

                                Sign.Positive ->
                                    { afterI = absoluteAfterI } |> Ok
                    , broaden =
                        \{ afterI } ->
                            { sign = Sign.Positive
                            , absoluteAfterI = afterI
                            }
                    }
                )
            |> Morph.choiceToFromFinish
        )



-- to ArraySized


{-| Remove `O` padding at the front of the `ArraySized`
to get a [`Natural`](#Natural)
-}
bits :
    MorphIndependently
        (ArraySized Bit (In (On narrowMin_) narrowMax_)
         -> Result error_ Natural
        )
        (Natural
         -> ArraySized Bit (Min (Up0 x_))
        )
bits =
    Morph.translate fromBitsImplementation toBitsImplementation


{-| Its bits as an `ArraySized (Min (Up0 x_))`.
Proceed from here over [`Bits`](https://dark.elm.dmy.fr/packages/lue-bird/elm-bits/latest/Bits)
-}
toBits :
    MorphIndependently
        (Natural
         -> Result error_ (ArraySized Bit (Min (Up0 narrowX_)))
        )
        (ArraySized Bit (In (On broadMin_) broadMax_)
         -> Natural
        )
toBits =
    Morph.translate toBitsImplementation fromBitsImplementation


toBitsImplementation : Natural -> ArraySized Bit (Min (Up0 x_))
toBitsImplementation =
    \natural ->
        case natural of
            N0 ->
                ArraySized.empty |> ArraySized.maxToInfinity

            AtLeast1 { afterI } ->
                afterI
                    |> ArraySized.inToOn
                    |> ArraySized.minTo n0
                    |> ArraySized.insertMin ( Up, n0 ) Bit.I
                    |> ArraySized.minTo n0


fromBitsImplementation : ArraySized Bit (In (On min_) max_) -> Natural
fromBitsImplementation =
    \arraySized ->
        case arraySized |> Bits.unpad |> ArraySized.hasAtLeast n1 of
            Err _ ->
                N0

            Ok unpaddedAtLeast1 ->
                AtLeast1
                    { afterI =
                        unpaddedAtLeast1
                            |> ArraySized.removeMin ( Up, n0 )
                            |> ArraySized.minTo n0
                            |> ArraySized.maxToInfinity
                            |> ArraySized.minToNumber
                    }
