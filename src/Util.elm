module Util exposing (listResultsToValuesOrErrors)

{-| Helpers.

Putting them in a separate `module` helps with testing as well as preventing import cycles.

@docs listResultsToValuesOrErrors

-}

import Conversion
import Dict exposing (Dict)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)


type alias StructureLinearInsideExpectation atElement =
    RecordWithoutConstructorFunction
        { elementsAtIndexes : Dict Int atElement }


{-| `Ok` if all values in sequence are `Ok`,
else `Err` with information on at what indexes elements were `Err`.

    import Dict
    import Conversion

    [ Ok 3, Ok 4, Ok 5 ]
        |> listResultsToValuesOrErrors
    --> Ok [ 3, 4, 5 ]

    [ Ok 3, Ok 4, Err (Conversion.Expected ()), Ok 6 ]
        |> listResultsToValuesOrErrors
    --> Err { elementsAtIndexes = Dict.singleton 2 () }

-}
listResultsToValuesOrErrors :
    List (Result (Conversion.Error expectation) value)
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

                            Err (Conversion.Expected elementError) ->
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



-- zombie
{- `Ok` if all values in sequence are `Ok`,
   else `Err` with information on at what indexes elements were `Err`.

       import Dict
       import Array
       import Conversion

       Array.fromList [ Ok 3, Ok 4, Ok 5 ]
           |> arrayResultsToValuesOrErrors
       --> Ok (Array.fromList [ 3, 4, 5 ])

       Array.fromList
           [ Ok 3, Ok 4, Err (Conversion.Expected ()), Ok 6 ]
           |> arrayResultsToValuesOrErrors
       --> Err { elementsAtIndexes = Dict.singleton 2 () }


   arrayResultsToValuesOrErrors :
       Array (Result (Conversion.Error expectation) value)
       -> Result (StructureLinearInsideExpectation expectation) (Array value)
   arrayResultsToValuesOrErrors =
       \results ->
           results
               |> Array.foldl
                   (\elementResult { index, collected } ->
                       { collected =
                           case elementResult of
                               Ok elementValue ->
                                   collected |> Result.map (Array.push elementValue)

                               Err (Conversion.Expected elementError) ->
                                   { elementsAtIndexes =
                                       case collected of
                                           Ok _ ->
                                               Dict.singleton index elementError

                                           Err { elementsAtIndexes } ->
                                               elementsAtIndexes
                                                   |> Dict.insert index elementError
                                   }
                                       |> Err
                       , index = index + 1
                       }
                   )
                   { collected = Array.empty |> Ok
                   , index = 0
                   }
               |> .collected
-}
