module Util exposing (recoverTry)

{-| Helpers

Putting them in a separate `module` helps with testing as well as preventing import cycles

@docs recoverTry

-}


{-| Like `Result.andThen` but on `Err` from the attached error
-}
recoverTry :
    (error -> Result errorMapped okValue)
    ->
        (Result error okValue
         -> Result errorMapped okValue
        )
recoverTry errorMapToResult =
    \result ->
        case result of
            Ok ok ->
                ok |> Ok

            Err error ->
                error |> errorMapToResult
