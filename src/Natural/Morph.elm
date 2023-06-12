module Natural.Morph exposing
    ( integer
    , bits, chars
    )

{-| [`Natural`](Natural#Natural) [`Morph`](Morph#Morph)

@docs integer


## row

@docs bits, chars

-}

import Bit exposing (Bit)
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

For [`toBroad`](Morph#toBroad): If the number is greater than the capacity possible with the given bit count,
the greatest possible value will be returned instead.

-}
bits :
    Bytes.Endianness
    -> N (In (Up bitCountMinX_ To bitCountMinPlusX_) (Up bitCountMaxX_ To bitCountMaxPlusX_))
    -> MorphRow Natural Bit
bits endianness bitCount =
    Natural.Internal.bits endianness bitCount
