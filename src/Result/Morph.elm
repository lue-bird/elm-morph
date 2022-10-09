module Result.Morph exposing (value)

{-| [`Morph`](Morph#Morph) a `Result`


## transform

@docs value

-}

import Choice
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
    Choice.between
        (\ok err narrowResult ->
            case narrowResult of
                Ok narrowValue ->
                    narrowValue |> ok

                Err narrowError ->
                    narrowError |> err
        )
        |> Choice.tryValue ( Ok, "Ok" ) caseMorphs.ok
        |> Choice.tryValue ( Err, "Err" ) caseMorphs.err
        |> Choice.finishValue
