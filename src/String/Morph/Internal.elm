module String.Morph.Internal exposing (value)

import Morph
import Value


value : Value.Morph String
value =
    Morph.value "String"
        { broaden = Value.String
        , narrow =
            \valueBroad ->
                case valueBroad of
                    Value.String stringNarrow ->
                        stringNarrow |> Ok

                    atomExceptString ->
                        atomExceptString |> Value.atomKindToString |> Err
        }
        |> Morph.over Value.atom
