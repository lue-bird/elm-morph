module Result.Morph exposing (value)

{-| [`Morph`](Morph#Morph) a `Result`


## transform

@docs value

-}

import Morph
import Value exposing (MorphValue)


{-| `Result` [`MorphValue`](Value#MorphValue)
-}
value :
    { ok : MorphValue okValue
    , err : MorphValue error
    }
    -> MorphValue (Result error okValue)
value caseMorphs =
    Morph.choice
        (\ok err narrowResult ->
            case narrowResult of
                Ok value ->
                    value |> ok

                Err error ->
                    error |> err
        )
        |> Value.variant ( Ok, "Ok" ) caseMorphs.ok
        |> Value.variant ( Err, "Err" ) caseMorphs.err
        |> Value.choiceFinish
