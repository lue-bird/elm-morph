module Natural exposing
    ( Natural
    , bits, toBits
    , rowChar
    , integer
    )

{-| Natural number as bits

@docs Natural


## [`Morph`](Morph#Morph)

@docs bits, toBits

@docs rowChar

@docs integer

Feeling motivated? implement & PR

  - int2 (bin), int8 (oct), int6 (hex)

-}

import ArraySized exposing (ArraySized)
import Bit exposing (Bit)
import Bits
import Linear exposing (Direction(..))
import Morph exposing (Morph, MorphIndependently, MorphRow)
import N exposing (In, Min, On, Up0, n0, n1)
import NaturalAtLeast1
import Number exposing (Integer(..), Natural(..), Sign(..))
import String.Morph


{-| Whole number (integer) >= 0 of arbitrary precision.
Either the bit `O` directly or `I` followed by at most a given count of
[`Bit`](https://dark.elm.dmy.fr/packages/lue-bird/elm-bits/latest/Bit)s

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
type alias Natural =
    Number.Natural



-- Morph


{-| [`Morph`](Morph#Morph) to a [`Natural`](#Natural)
from an unsigned [`Integer`](Integer#Integer)
-}
integer : Morph Natural Integer
integer =
    Morph.to "natural"
        (Morph.variants
            ( \variantN0 variantSigned integerChoice ->
                case integerChoice of
                    IntegerN0 ->
                        variantN0 ()

                    IntegerSigned signedValue ->
                        variantSigned signedValue
            , \variantN0 variantAtLeast1 natural ->
                case natural of
                    NaturalN0 ->
                        variantN0 ()

                    NaturalAtLeast1 atLeast1Value ->
                        variantAtLeast1 atLeast1Value
            )
            |> Morph.variant ( \() -> NaturalN0, \() -> IntegerN0 )
                (Morph.broad ())
            |> Morph.variant ( NaturalAtLeast1, IntegerSigned )
                (Morph.value "positive"
                    { narrow =
                        \{ sign, absolute } ->
                            case sign of
                                Negative ->
                                    "negative" |> Err

                                Positive ->
                                    absolute |> Ok
                    , broaden =
                        \atLeast1 ->
                            { sign = Positive
                            , absolute = atLeast1
                            }
                    }
                )
            |> Morph.variantsFinish
        )



-- MorphRow


{-| [`Natural`](#Natural) [`MorphRow`](Morph#MorphRow)

    import Morph.Error

    "123" |> Text.narrowTo integer --> Ok 123

    -- It doesn't work with negative numbers.
    "-123" |> Text.narrowTo integer --> Ok -123

    -- a decimal number is _not_ a natural
    "3.14"
        |> Text.narrowTo integer
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:2: I was expecting an integer value. I got stuck when I got the character '.'."

    -- but not with invalid numbers
    "abc"
        |> Text.narrowTo integer
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:1: I was expecting an integer value. I got stuck when I got the character 'a'."

-}
rowChar : MorphRow Natural Char
rowChar =
    Morph.to "integer"
        (Morph.choice
            (\n0Variant atLeast1Variant integerNarrow ->
                case integerNarrow of
                    NaturalN0 ->
                        n0Variant ()

                    NaturalAtLeast1 atLeast1Value ->
                        atLeast1Variant atLeast1Value
            )
            |> Morph.tryRow (\() -> NaturalN0) (String.Morph.only "0")
            |> Morph.tryRow NaturalAtLeast1 NaturalAtLeast1.rowChar
            |> Morph.choiceRowFinish
        )



-- bits


toBitsImplementation : Natural -> ArraySized Bit (Min (Up0 x_))
toBitsImplementation =
    \natural ->
        case natural of
            NaturalN0 ->
                ArraySized.empty |> ArraySized.maxToInfinity

            NaturalAtLeast1 atLeast1 ->
                atLeast1.bitsAfterI
                    |> ArraySized.inToOn
                    |> ArraySized.minTo n0
                    |> ArraySized.insertMin ( Up, n0 ) Bit.I
                    |> ArraySized.minTo n0


fromBitsImplementation : ArraySized Bit (In (On min_) max_) -> Natural
fromBitsImplementation =
    \arraySized ->
        case arraySized |> Bits.unpad |> ArraySized.hasAtLeast n1 of
            Err _ ->
                NaturalN0

            Ok unpaddedAtLeast1 ->
                NaturalAtLeast1
                    { bitsAfterI =
                        unpaddedAtLeast1
                            |> ArraySized.removeMin ( Up, n0 )
                            |> ArraySized.minTo n0
                            |> ArraySized.maxToInfinity
                            |> ArraySized.minToNumber
                    }


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
