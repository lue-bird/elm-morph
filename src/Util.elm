module Util exposing (restore, restoreTry)

{-| Helpers

Putting them in a separate `module` helps with testing as well as preventing import cycles

@docs restore, restoreTry

-}


{-| Like `Maybe.withDefault` where you can use information from the attached error to create a fallback
-}
restore :
    (error -> okValue)
    ->
        (Result error okValue
         -> okValue
        )
restore errorFallback =
    \result ->
        case result of
            Ok ok ->
                ok

            Err error ->
                error |> errorFallback


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
