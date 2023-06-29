module String.Morph.Internal exposing (value)

import Morph
import Value
import Value.Morph exposing (MorphValue)


value : MorphValue String
value =
    Morph.custom "String"
        { toBroad = Value.String
        , toNarrow =
            \valueBroad ->
                case valueBroad of
                    Value.String stringNarrow ->
                        stringNarrow |> Ok

                    atomExceptString ->
                        atomExceptString |> Value.atomKindToString |> Err
        }
        |> Morph.over Value.Morph.atom
