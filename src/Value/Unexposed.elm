module Value.Unexposed exposing (float, string, structureKindToString)

import Morph exposing (MorphIndependently)
import Value exposing (MorphValue)


float : MorphValue Float
float =
    Morph.value "Float"
        { broaden = Value.Float
        , narrow =
            \value ->
                case value of
                    Value.Float floatValue ->
                        floatValue |> Ok

                    literalExceptFloat ->
                        literalExceptFloat |> literalKindToString |> Err
        }
        |> Morph.over Value.literal


literalKindToString : Value.Literal -> String
literalKindToString =
    \literal_ ->
        case literal_ of
            Value.Unit _ ->
                "Unit"

            Value.Float _ ->
                "Number"

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
