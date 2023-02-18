module Result.Morph exposing (value)

{-| [`Morph`](Morph#Morph) a `Result`


## transform

@docs value

-}

import Choice
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
    Choice.between
        (\ok err narrowResult ->
            case narrowResult of
                Ok narrowValue ->
                    narrowValue |> ok

                Err narrowError ->
                    narrowError |> err
        )
        |> Choice.variantValue ( Ok, "Ok" ) caseMorphs.ok
        |> Choice.variantValue ( Err, "Err" ) caseMorphs.err
        |> Choice.finishValue
