module Util exposing
    ( recoverTry
    , stackInit, stackLast
    )

{-| Helpers

Putting them in a separate `module` helps with testing as well as preventing import cycles

@docs recoverTry
@docs stackInit, stackLast

-}

import Emptiable exposing (Emptiable)
import Linear exposing (Direction(..))
import Possibly exposing (Possibly)
import Stack exposing (Stacked)


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


stackLast : Emptiable (Stacked element) Never -> element
stackLast =
    \stack -> stack |> Stack.fold Up (\further _ -> further)


stackInit : Emptiable (Stacked element) Never -> Emptiable (Stacked element) Possibly
stackInit =
    \stack ->
        stack |> Stack.foldFromOne (\_ -> Emptiable.empty) Down Stack.onTopLay
