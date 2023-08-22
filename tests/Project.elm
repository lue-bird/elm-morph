module Project exposing (Project, morphValue)

import Morph
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import String.Morph
import Value.Morph exposing (MorphValue)


type alias Project =
    RecordWithoutConstructorFunction
        { name : String, description : String }


morphValue : MorphValue Project
morphValue =
    Value.Morph.group (\name description -> { name = name, description = description })
        |> Value.Morph.part ( .name, "name" ) String.Morph.value
        |> Value.Morph.part ( .description, "description" ) String.Morph.value
        |> Value.Morph.groupFinish
