module Int.Morph exposing (float, value)

{-| `Int` [`Morph`](Morph#Morph)

@docs float, value

-}

import Float.Morph
import Morph exposing (Morph)
import Value


{-| Only [`Morph`](Morph#Morph) a `Float` without a fraction
-}
float : Morph Int Float
float =
    Morph.value "Int"
        { narrow =
            \floatBroad ->
                let
                    rounded =
                        floatBroad |> round
                in
                if (rounded |> toFloat) == floatBroad then
                    rounded |> Ok

                else
                    "has fraction" |> Err
        , broaden = toFloat
        }


{-| `Int` [`MorphValue`](Value#MorphValue)
-}
value : Value.MorphValue Int
value =
    float
        |> Morph.over Float.Morph.value
