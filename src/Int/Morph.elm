module Int.Morph exposing (float)

import Morph exposing (Morph)
import Value


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


{-| `Int` [`Morph`](#Morph)
-}
int : Value.MorphValue Int
int =
    float
        |> Morph.over number
        |> Morph.over literal
