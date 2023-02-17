module Value.PackageInternal exposing (atomKindToString, composedKindToString, string)

import Morph exposing (MorphIndependently)
import Value exposing (MorphValue)


atomKindToString : Value.Atom -> String
atomKindToString =
    \atom_ ->
        case atom_ of
            Value.Unit _ ->
                "Unit"

            Value.Number _ ->
                "Decimal"

            Value.String _ ->
                "String"


composedKindToString : Value.Composed tag_ -> String
composedKindToString =
    \composed_ ->
        case composed_ of
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

                    atomExceptString ->
                        atomExceptString |> atomKindToString |> Err
        }
        |> Morph.over Value.atom
