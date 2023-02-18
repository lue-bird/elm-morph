module Result.Morph exposing (value)

{-| [`Morph`](Morph#Morph) a `Result`


## transform

@docs value

-}

import Morph
import Value


{-| `Result` [`Value.Morph`](Value#Morph)
-}
value :
    { ok : Value.Morph okValue
    , err : Value.Morph error
    }
    -> Value.Morph (Result error okValue)
value caseMorphs =
    Morph.choice
        (\ok err narrowResult ->
            case narrowResult of
                Ok narrowValue ->
                    narrowValue |> ok

                Err narrowError ->
                    narrowError |> err
        )
        |> Value.variant ( Ok, "Ok" ) caseMorphs.ok
        |> Value.variant ( Err, "Err" ) caseMorphs.err
        |> Value.choiceFinish
