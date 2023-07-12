module Natural.Morph exposing
    ( integer
    , chars, bits, bitsVariableCount
    )

{-| [`Natural`](Natural#Natural) [`Morph`](Morph#Morph)

@docs integer


## [row](Morph#MorphRow)

@docs chars, bits, bitsVariableCount

-}

import Bit exposing (Bit)
import Bit.Morph
import Bytes
import Integer exposing (Integer)
import Morph exposing (Morph, MorphRow)
import N exposing (In, N, To, Up)
import Natural exposing (Natural(..))
import Natural.Internal
import NaturalAtLeast1
import String.Morph



-- Morph


{-| [`Morph`](Morph#Morph) to a [`Natural`](Natural#Natural)
from an unsigned [`Integer`](Integer#Integer)
-}
integer : Morph Natural Integer
integer =
    Natural.Internal.integer



-- MorphRow


{-| [`Natural`](Natural#Natural) [`MorphRow`](Morph#MorphRow)

    import Morph
    import List.Morph
    import N
    import Natural

    "123"
        |> Morph.toNarrow
            (Natural.Morph.chars |> Morph.rowFinish |> Morph.over List.Morph.string)
        |> Result.map (Natural.toN >> N.toInt)
    --> Ok 123

    -- a negative integer is not a natural
    "-123"
        |> Morph.toNarrow
            (Natural.Morph.chars |> Morph.rowFinish |> Morph.over List.Morph.string)
        |> Result.toMaybe
    --> Nothing

    -- a decimal number is not a natural
    "3.14"
        |> Morph.toNarrow (Natural.Morph.chars |> Morph.rowFinish |> Morph.over List.Morph.string)
        |> Result.toMaybe
    --> Nothing

    -- letters etc are not accepted
    "3e10"
        |> Morph.toNarrow (Natural.Morph.chars |> Morph.rowFinish |> Morph.over List.Morph.string)
        |> Result.toMaybe
    --> Nothing

-}
chars : MorphRow Natural Char
chars =
    Morph.named "integer"
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
            |> Morph.choiceFinish
        )



-- bits


{-| [`MorphRow`](Morph#MorphRow) for a [`Natural`](Natural#Natural) from a given amount of bits.

Note that the bit count can be any number like `n7` or `n128`. Numbers greater than `n16`
can quickly be generated locally, see [this section in the `N` module documentation](https://dark.elm.dmy.fr/packages/lue-bird/elm-bounded-nat/latest/N#specific-numbers)

For [`toBroad`](Morph#toBroad): If the number is greater than the capacity possible with the given bit count,
the greatest possible value will be returned instead.

To keep the whole range → [`bitsVariableCount`](#bitsVariableCount)

-}
bits :
    Bytes.Endianness
    -> N (In (Up bitCountMinX_ To bitCountMinPlusX_) (Up bitCountMaxX_ To bitCountMaxPlusX_))
    -> MorphRow Natural Bit
bits endianness bitCount =
    Natural.Internal.bits endianness bitCount


{-| Unlike [`bits`](#bits) which takes a concrete bit count,
[`bitsVariableCount`](#bitsVariableCount) can preserve any length
-}
bitsVariableCount : MorphRow Natural Bit
bitsVariableCount =
    Morph.choice
        (\n0 atLeast1 natural ->
            case natural of
                N0 ->
                    n0 ()

                AtLeast1 atLeast1Value ->
                    atLeast1 atLeast1Value
        )
        |> Morph.tryRow (\() -> N0) (Bit.Morph.only Bit.O |> Morph.one)
        |> Morph.tryRow AtLeast1
            (Morph.succeed (\atLeast1 -> atLeast1)
                |> Morph.match (Bit.Morph.only Bit.I |> Morph.one)
                |> Morph.grab (\atLeast1 -> atLeast1) NaturalAtLeast1.bits
            )
        |> Morph.choiceFinish
