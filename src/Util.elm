module Util exposing
    ( onErr
    , resultFromMaybeLazy
    )

{-| Helpers

Putting them in a separate `module` helps with testing as well as preventing import cycles

@docs onErr

-}

import Linear exposing (Direction(..))


{-| Like `Result.andThen` but on `Err` from the attached error
-}
onErr :
    (error -> Result errorMapped okValue)
    ->
        (Result error okValue
         -> Result errorMapped okValue
        )
onErr errorMapToResult result =
    case result of
        Ok ok ->
            Ok ok

        Err error ->
            error |> errorMapToResult


resultFromMaybeLazy : (() -> x) -> Maybe v -> Result x v
resultFromMaybeLazy errOnNothing maybe =
    case maybe of
        Nothing ->
            Err (errOnNothing ())

        Just value ->
            Ok value
