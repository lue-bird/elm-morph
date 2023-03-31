module Natural.Morph exposing
    ( bits, toBits
    , chars
    , integer
    )

{-| [`Natural`](#Natural) [`Morph`](Morph#Morph)

@docs bits, toBits

@docs chars

@docs integer

Feeling motivated? implement & PR

  - base8 (oct), base16 (hex)

-}

import ArraySized exposing (ArraySized)
import Bit exposing (Bit)
import Bits
import Integer exposing (Integer)
import Linear exposing (Direction(..))
import Morph exposing (Morph, MorphIndependently, MorphRow)
import N exposing (In, Min, On, Up0, n0, n1)
import Natural exposing (Natural(..))
import NaturalAtLeast1
import Sign exposing (Sign(..))
import String.Morph



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
            |> Morph.variant "0"
                ( \() -> N0, \() -> Integer.N0 )
                (Morph.broad ())
            |> Morph.variant "signed"
                ( AtLeast1, Integer.Signed )
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

    "123" |> Text.toNarrow integer --> Ok 123

    -- It doesn't work with negative numbers.
    "-123" |> Text.toNarrow integer --> Ok -123

    -- a decimal number is _not_ a natural
    "3.14"
        |> Text.toNarrow integer
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:2: I was expecting an integer value. I got stuck when I got the character '.'."

    -- but not with invalid numbers
    "abc"
        |> Text.toNarrow integer
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:1: I was expecting an integer value. I got stuck when I got the character 'a'."

-}
chars : MorphRow Natural Char
chars =
    Morph.to "integer"
        (Morph.choice
            (\n0Variant atLeast1Variant integerNarrow ->
                case integerNarrow of
                    N0 ->
                        n0Variant ()

                    AtLeast1 atLeast1Value ->
                        atLeast1Variant atLeast1Value
            )
            |> Morph.tryRow (\() -> N0) (String.Morph.only "0")
            |> Morph.tryRow AtLeast1 NaturalAtLeast1.chars
            |> Morph.choiceRowFinish
        )



-- bits


toBitsImplementation : Natural -> ArraySized Bit (Min (Up0 x_))
toBitsImplementation =
    \natural ->
        case natural of
            N0 ->
                ArraySized.empty |> ArraySized.maxToInfinity

            AtLeast1 atLeast1 ->
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
                N0

            Ok unpaddedAtLeast1 ->
                AtLeast1
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
