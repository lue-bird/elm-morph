module Unit exposing (value)

{-| [`Morph`](Morph#Morph) a `()`


## transform

@docs value

-}

import Morph
import Value exposing (MorphValue)
import Value.PackageInternal


{-| `()` [`Morph`](#Morph)

Often used in when [morphing](Value#MorphValue) a [variants](Choice#variantValue)
with 0 attached values

-}
value : MorphValue ()
value =
    Morph.value "Unit"
        { broaden = Value.Unit
        , narrow =
            \value_ ->
                case value_ of
                    Value.Unit unitValue ->
                        unitValue |> Ok

                    literalExceptUnit ->
                        literalExceptUnit
                            |> Value.PackageInternal.literalKindToString
                            |> Err
        }
        |> Morph.over Value.literal
