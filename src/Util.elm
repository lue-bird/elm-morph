module Util exposing (recoverTry, justWhen, maybeWhen)

{-| Helpers

Putting them in a separate `module` helps with testing as well as preventing import cycles

@docs recoverTry, justWhen, maybeWhen

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


justWhen : (content -> Bool) -> content -> Maybe content
justWhen passes =
    \content ->
        if content |> passes then
            content |> Just

        else
            Nothing


maybeWhen : (content -> Bool) -> Maybe content -> Maybe content
maybeWhen passes =
    \maybe ->
        case maybe of
            Nothing ->
                Nothing

            Just content ->
                content |> justWhen passes
