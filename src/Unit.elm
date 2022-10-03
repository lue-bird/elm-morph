module Unit exposing (value)

{-| [`Morph`](Morph#Morph) a `()`


## transform

@docs value

-}

import Value exposing (MorphValue)


{-| `()` [`Morph`](#Morph)

Often used in when [morphing](Value#MorphValue) [`variant`](Value#variant)s with 0 attached values

-}
value : MorphValue ()
value =
    Morph.value "Unit"
        { broaden = Value.Unit
        , narrow =
            \value ->
                case value of
                    Value.Unit unitValue ->
                        unitValue |> Ok

                    literalExceptUnit ->
                        literalExceptUnit
                            |> Value.Unexposed.literalKindToString
                            |> Err
        }
        |> Morph.over Value.literal
