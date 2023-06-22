module Natural.Internal exposing (bits, fromBitArray, integer)

import ArraySized exposing (ArraySized)
import ArraySized.Morph
import Bit exposing (Bit)
import BitArray.Extra
import Bytes
import Integer exposing (Integer)
import Linear exposing (Direction(..))
import Morph exposing (Morph, MorphIndependently, MorphRow)
import N exposing (In, N, To, Up, n1)
import Natural exposing (Natural)
import NaturalAtLeast1.Internal
import Sign exposing (Sign(..))


integer : Morph Natural Integer
integer =
    Morph.named "natural"
        (Morph.variants
            ( \variantN0 variantSigned integerChoice ->
                case integerChoice of
                    Integer.N0 ->
                        variantN0 ()

                    Integer.Signed signedValue ->
                        variantSigned signedValue
            , \variantN0 variantAtLeast1 natural ->
                case natural of
                    Natural.N0 ->
                        variantN0 ()

                    Natural.AtLeast1 atLeast1Value ->
                        variantAtLeast1 atLeast1Value
            )
            |> Morph.variant "0"
                ( \() -> Natural.N0, \() -> Integer.N0 )
                (Morph.broad ())
            |> Morph.variant "signed"
                ( Natural.AtLeast1, Integer.Signed )
                (Morph.custom "positive"
                    { toNarrow =
                        \{ sign, absolute } ->
                            case sign of
                                Negative ->
                                    "negative" |> Err

                                Positive ->
                                    absolute |> Ok
                    , toBroad =
                        \atLeast1 ->
                            { sign = Positive
                            , absolute = atLeast1
                            }
                    }
                )
            |> Morph.variantsFinish
        )


bits :
    Bytes.Endianness
    -> N (In (Up bitCountMinX_ To bitCountMinPlusX_) (Up bitCountMaxX_ To bitCountMaxPlusX_))
    -> MorphRow Natural Bit
bits endianness bitCount =
    bitArrayOfSize bitCount
        |> (case endianness of
                Bytes.BE ->
                    identity

                Bytes.LE ->
                    Morph.over
                        (Morph.oneToOne BitArray.Extra.reverseEndian BitArray.Extra.reverseEndian)
           )
        |> Morph.overRow
            (ArraySized.Morph.exactly bitCount (Morph.keep |> Morph.one))


{-| Convert from an `ArraySized` of bits.

When converting back:
If the number is greater than the capacity possible with the given bit count,
the greatest possible value will be returned instead.

-}
bitArrayOfSize :
    N (In (Up minX To minPlusX) max)
    ->
        MorphIndependently
            (ArraySized Bit (In beforeToNarrowMin_ beforeToNarrowMax_)
             -> Result error_ Natural
            )
            (Natural -> ArraySized Bit (In (Up minX To minPlusX) max))
bitArrayOfSize bitCount =
    Morph.oneToOne
        fromBitArray
        (toBitArrayOfSize bitCount)


toBitArrayOfSize :
    N (In (Up minX To minPlusX) max)
    ->
        (Natural
         -> ArraySized Bit (In (Up minX To minPlusX) max)
        )
toBitArrayOfSize bitCount =
    \natural ->
        case natural of
            Natural.N0 ->
                ArraySized.repeat Bit.O bitCount

            Natural.AtLeast1 atLeast1 ->
                atLeast1 |> NaturalAtLeast1.Internal.toBitArrayOfSize bitCount


fromBitArray : ArraySized Bit (In min_ max_) -> Natural
fromBitArray =
    \arraySized ->
        case arraySized |> BitArray.Extra.unpad |> ArraySized.hasAtLeast n1 of
            Err _ ->
                Natural.N0

            Ok unpaddedAtLeast1 ->
                Natural.AtLeast1
                    { bitsAfterI =
                        unpaddedAtLeast1
                            |> ArraySized.removeMin ( Up, n1 )
                            |> ArraySized.minToNumber
                            |> ArraySized.maxToInfinity
                    }
