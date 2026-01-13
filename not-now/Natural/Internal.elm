module Natural.Internal exposing (bits, integer)

import Bit exposing (Bit)
import BitArray.Extra
import Bytes
import Integer exposing (Integer)
import Morph exposing (Morph, MorphIndependently, MorphRow)
import Natural exposing (Natural)
import NaturalAtLeast1
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
    -> Int
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
            (List.Morph.exactly bitCount (Morph.keep |> Morph.one))
