module Natural.Morph exposing
    ( integer, n
    , chars, bits, bitsVariableCount
    )

{-| [`Morph`](Morph#Morph) for an [arbitrary-sized `Natural`](Natural#Natural)

@docs integer, n


## [row](Morph#MorphRow)

@docs chars, bits, bitsVariableCount

-}

import Bit exposing (Bit)
import Bit.Morph
import Bytes
import Integer exposing (Integer)
import Morph exposing (Morph, MorphIndependently, MorphRow)
import N exposing (In, Min, N, To, Up, Up0)
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


{-| [`Morph`](Morph#Morph) from a [fixed-size `N`](https://dark.elm.dmy.fr/packages/lue-bird/elm-bounded-nat/latest/).

[Inverse](Morph#invert) of [`N.Morph.natural`](N-Morph#natural)

-}
n : MorphIndependently (N range_ -> Result error_ Natural) (Natural -> N (Min (Up0 minX_)))
n =
    Morph.oneToOne Natural.fromN Natural.toN



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
            |> Morph.rowTry (\() -> N0) (String.Morph.only "0")
            |> Morph.rowTry AtLeast1 NaturalAtLeast1.chars
            |> Morph.choiceFinish
        )



-- bits


{-| [`MorphRow`](Morph#MorphRow) for a [`Natural`](Natural#Natural)
from a given count of [`Bit`](https://dark.elm.dmy.fr/packages/lue-bird/elm-bits/latest/Bit)s
and with a given [endianness](https://dark.elm.dmy.fr/packages/elm/bytes/latest/Bytes#Endianness).

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
        |> Morph.rowTry (\() -> N0) (Bit.Morph.only Bit.O |> Morph.one)
        |> Morph.rowTry AtLeast1
            (Morph.narrow (\atLeast1 -> atLeast1)
                |> Morph.match (Bit.Morph.only Bit.I |> Morph.one)
                |> Morph.grab (\atLeast1 -> atLeast1) NaturalAtLeast1.bits
            )
        |> Morph.choiceFinish
