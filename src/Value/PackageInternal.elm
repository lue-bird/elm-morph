module Value.PackageInternal exposing (literalKindToString, string, structureKindToString)

import Morph exposing (MorphIndependently)
import Value exposing (MorphValue)


literalKindToString : Value.Literal -> String
literalKindToString =
    \literal_ ->
        case literal_ of
            Value.Unit _ ->
                "Unit"

            Value.Number _ ->
                "Decimal"

            Value.String _ ->
                "String"


structureKindToString : Value.Structure tag_ -> String
structureKindToString =
    \structure_ ->
        case structure_ of
            Value.List _ ->
                "List"

            Value.Array _ ->
                "Array"

            Value.Record _ ->
                "Record"

            Value.Variant _ ->
                "Variant"


{-| `String` [`MorphValue`](Value#MorphValue)
-}
string : MorphValue String
string =
    Morph.value "String"
        { broaden = Value.String
        , narrow =
            \value ->
                case value of
                    Value.String stringNarrow ->
                        stringNarrow |> Ok

                    literalExceptString ->
                        literalExceptString |> literalKindToString |> Err
        }
        |> Morph.over Value.literal
