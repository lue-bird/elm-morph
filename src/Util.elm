module Util exposing (restoreTry)

{-| Helpers

Putting them in a separate `module` helps with testing as well as preventing import cycles

@docs restoreTry

-}


{-| Like `Result.andThen` but on `Err` from the attached error
-}
restoreTry :
    (error -> Result errorMapped okValue)
    ->
        (Result error okValue
         -> Result errorMapped okValue
        )
restoreTry errorMapToResult =
    \result ->
        case result of
            Ok ok ->
                ok |> Ok

            Err error ->
                error |> errorMapToResult
