module Util exposing (listResultsToValuesOrErrors, restoreTry)

{-| Helpers.

Putting them in a separate `module` helps with testing as well as preventing import cycles.

@docs listResultsToValuesOrErrors, restoreTry

-}

import Dict exposing (Dict)
import Morph
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)


type alias StructureLinearInsideExpectation atElement =
    RecordWithoutConstructorFunction
        { elementsAtIndexes : Dict Int atElement }


{-| `Ok` if all values in sequence are `Ok`,
else `Err` with information on at what indexes elements were `Err`.

    import Dict
    import Morph

    [ Ok 3, Ok 4, Ok 5 ]
        |> listResultsToValuesOrErrors
    --> Ok [ 3, 4, 5 ]

    [ Ok 3, Ok 4, Err (Morph.Expected ()), Ok 6 ]
        |> listResultsToValuesOrErrors
    --> Err { elementsAtIndexes = Dict.singleton 2 () }

-}
listResultsToValuesOrErrors :
    List (Result expectation value)
    -> Result (StructureLinearInsideExpectation expectation) (List value)
listResultsToValuesOrErrors =
    \results ->
        results
            |> List.foldr
                (\elementResult { index, collected } ->
                    { collected =
                        case elementResult of
                            Ok elementValue ->
                                collected |> Result.map ((::) elementValue)

                            Err elementError ->
                                { elementsAtIndexes =
                                    case collected of
                                        Ok _ ->
                                            Dict.singleton index elementError

                                        Err { elementsAtIndexes } ->
                                            elementsAtIndexes
                                                |> Dict.insert index elementError
                                }
                                    |> Err
                    , index = index - 1
                    }
                )
                { collected = [] |> Ok
                , index = (results |> List.length) - 1
                }
            |> .collected


{-| Like `Result.andThen` but on `Err` from the attached error.
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
