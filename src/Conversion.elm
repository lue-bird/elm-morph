module Conversion exposing
    ( Conversion
    , Expected(..), expectation, errorExpectationMap
    , validate, expectationMap
    , broaden, narrow
    , Transfer
    , transfer, remain
    , listToArray, arrayToList
    , stringToList, listToString
    , map, unmap
    , lazy, over
    , reverse
    , Tagged(..), TagOrValue(..)
    , IntersectionConversionStep
    , intersection, partEat
    , UnionConversionStep
    , variantUnion, variantEat
    , listElementEachTransfer
    )

{-| Prism, Codec, Converter = ...

@docs Conversion


## fallible

@docs Expected, expectation, errorExpectationMap
@docs validate, expectationMap


## fallible scan

@docs broaden, narrow


### fallible transform


## transferring

@docs Transfer


### `Transfer` create

@docs transfer, remain
@docs listToArray, arrayToList
@docs stringToList, listToString


### `Transfer` scan

@docs map, unmap


## transform

@docs lazy, over


### `Transfer` transform

@docs reverse


## step

@docs Tagged, TagOrValue


### intersections

@docs IntersectionConversionStep
@docs intersection, partEat


### unions

@docs UnionConversionStep
@docs variantUnion, variantEat

-}

import Array exposing (Array)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)


{-| Morph functions to a more general format and back.

👀 type `Conversion narrow broad error`:

  - example: `Conversion Email String (StackFilled DeadEnd)`

  - `broaden : narrow -> broad`
      - Going from a specific type to a general one that possibly can't be turned back easily
      - Can loose information on the way
      - example: `Email -> String`

  - `narrow : broad -> Result error narrow`
      - This is pretty much exactly what a parser looks like!
      - going from a type to a type
      - example: `String -> Result (StackFilled DeadEnd) Email`

It's this abstract. No use-case implied.
Composition, field, variant steps etc. can be defined just as well.

-}
type alias Conversion narrow broad error =
    RecordWithoutConstructorFunction
        { narrow : broad -> Result error narrow
        , broaden : narrow -> broad
        }



--


{-| An expectation that hasn't been met.
-}
type Expected expectation
    = Expected expectation


{-| The expectation that hasn't been met.

    Conversion.Expected 3
        |> Conversion.errorExpectation
    --> 3

-}
expectation : Expected expectation -> expectation
expectation =
    \(Expected expectation_) -> expectation_


{-| Change the expectation that hasn't been met.
-}
errorExpectationMap :
    (expectation -> expectationMapped)
    -> Expected expectation
    -> Expected expectationMapped
errorExpectationMap expectationChange =
    \(Expected expectation_) ->
        Expected (expectation_ |> expectationChange)



--


{-| The function that turns `narrow` into `broad`.
-}
broaden :
    Conversion narrow broad error_
    -> (narrow -> broad)
broaden =
    \conversion -> conversion.broaden


{-| The function that turns `broad` into `narrow` or an `error`.
-}
narrow :
    Conversion specific general error
    -> (general -> Result error specific)
narrow =
    \conversion -> conversion.narrow


{-| Convert values of the arbitrarily chosen types `unmapped -> mapped`.

    "3456" |> map stringToList --> [ '3', '4', '5', '6' ]

-}
map : Transfer unmapped mapped -> (unmapped -> mapped)
map transfer_ =
    \unmapped ->
        unmapped |> broaden transfer_


{-| [`reverse`](#reverse) `|> map` is equivalent.
-}
unmap : Transfer unmapped mapped -> (mapped -> unmapped)
unmap transfer_ =
    \mapped ->
        case mapped |> narrow transfer_ of
            Ok mappedNarrow ->
                mappedNarrow

            Err error ->
                error |> never


{-| Take what the `narrow` function [`Expected`](#Expected) and adapt it.
-}
expectationMap :
    (expectation -> expectationMapped)
    -> Conversion specific general (Expected expectation)
    -> Conversion specific general (Expected expectationMapped)
expectationMap expectationChange =
    \conversion ->
        { broaden = conversion |> broaden
        , narrow =
            \general ->
                general
                    |> (conversion |> narrow)
                    |> Result.mapError
                        (errorExpectationMap expectationChange)
        }



--


{-| Filter specific values.

In general, try to narrow down the type when limiting values:
["Parse, don't validate"](https://elm-radio.com/episode/parse-dont-validate/).
That's a core idea in elm. You'll find lots of legendary resources surrounding this topic.

-}
validate :
    (value -> Result error value)
    -> Conversion value value error
validate narrowConvert =
    { narrow = narrowConvert
    , broaden = identity
    }



--


{-| [`map`](#map) & .
Limits consumed arguments to [`Conversion`](#Conversion)s that can `Never` fail to [`unmap`](#unmap), for example

    stringToListTransfer =
        Conversion.transfer String.toList String.fromList

Don't use `Transfer` to annotate created [`transfer`](#transfer)s!

    Conversion (List Char) String error_

Allows it to be used in more general [`Conversion`](#Conversion) chains where the target value can be an `error_`.

Both type arguments are really equal in "rank",
so choosing one as the `mapped` and one as the `unmapped` is rather arbitrary.

That's the reason it's a good idea to always expose 2 versions: `aToB` & `bToA`.

**!** Information can get lost on the way:

    dictToListConversion :
        Transfer
            (Dict comparableKey value)
            (List ( comparableKey, value ))
    dictToListConversion =
        Conversion.transfer Dict.toList Dict.fromList

Still, there's no parsing to transfer one state to the other.

-}
type alias Transfer unmapped mapped =
    Conversion unmapped mapped Never


{-| A [`Conversion`](#Conversion) that doesn't transform anything.

Same as writing:

  - [`transfer`](#transfer) `identity identity`
  - [`validate`](#validate) `Ok`
  - `{ narrow = Ok, broaden = identity }`

-}
remain : Conversion value value error_
remain =
    transfer identity identity


{-| Mutual `Conversion` → [`Transfer`](#Transfer)
between representations
that have the same structural information
and can be mapped 1:1 into each other.

    stringToListConversion : Conversion (List Char) String error_
    stringToListConversion =
        Conversion.transfer String.toList String.fromList

Included here:

  - [`stringToList`](#stringToList), [`listToString`](#listToString)
  - [`listToArray`](#listToArray), [`arrayToList`](#arrayToList)

-}
transfer :
    (unmapped -> mapped)
    -> (mapped -> unmapped)
    -> Conversion unmapped mapped error_
transfer mapTo unmapFrom =
    { broaden = mapTo
    , narrow = \mapped -> mapped |> unmapFrom |> Ok
    }


{-| `Transfer` each element in a `List`.
-}
listElementEachTransfer :
    Transfer unmapped mapped
    -> Conversion (List unmapped) (List mapped) error_
listElementEachTransfer elementTransfer =
    transfer
        (List.map (map elementTransfer))
        (List.map (unmap elementTransfer))


{-| `Transfer` from `List` to `Array`.

    import Array

    [ 0, 1, 2, 3 ]
        |> (Conversion.listToArray |> Conversion.map)
    --> Array.fromList [ 0, 1, 2, 3 ]

-}
listToArray : Conversion (List element) (Array element) error_
listToArray =
    transfer Array.fromList Array.toList


{-| `Transfer` from `Array` to `List`.

    import Array

    Array.fromList [ 0, 1, 2, 3 ]
        |> (Conversion.arrayToList |> Conversion.map)
    --> [ 0, 1, 2, 3 ]

-}
arrayToList : Conversion (Array element) (List element) error_
arrayToList =
    transfer Array.toList Array.fromList


{-| `Transfer` from `List` to `Array`.

    "0123" |> (Conversion.stringToList |> Conversion.broaden)
    --> [ '0', '1', '2', '3' ]

-}
stringToList : Conversion String (List Char) error_
stringToList =
    transfer String.toList String.fromList


{-| `Transfer` from `List` to `Array`.

    "0123" |> (Conversion.stringToList |> Conversion.broaden)
    --> [ '0', '1', '2', '3' ]

-}
listToString : Conversion (List Char) String error_
listToString =
    stringToList |> reverse



--


{-| A step in building a `Conversion`
between parts & an assembled whole and back.
-}
type alias IntersectionConversionStep narrow tag narrowEat broad fieldValueExpectation =
    RecordWithoutConstructorFunction
        { narrow :
            broad
            ->
                Result
                    (Expected
                        { fieldsAdditional : List tag
                        , fieldValues : List (Tagged tag fieldValueExpectation)
                        }
                    )
                    narrowEat
        , broaden : narrow -> broad
        }


{-| Consume another part.
-}
partEat :
    ( ( whole -> part
      , tag
      )
    , Conversion part partBroad (Expected fieldValueExpectation)
    )
    ->
        IntersectionConversionStep
            whole
            tag
            (part -> wholeAssembleFurther)
            (List (Tagged tag partBroad))
            fieldValueExpectation
    ->
        IntersectionConversionStep
            whole
            tag
            wholeAssembleFurther
            (List (Tagged tag partBroad))
            fieldValueExpectation
partEat ( ( accessPart, partTag ), partConversion ) =
    \wholeAssemblyConversion ->
        { narrow =
            \dictPart ->
                let
                    wholeAssemblyResult =
                        dictPart |> wholeAssemblyConversion.narrow

                    assemblyExpectations =
                        case wholeAssemblyResult of
                            Ok _ ->
                                { fieldValues = []
                                , fieldsAdditional = []
                                }

                            Err (Expected expectations) ->
                                expectations
                in
                case dictPart |> List.filter (\(Tagged tag _) -> tag == partTag) |> List.head of
                    Just (Tagged _ partBroad) ->
                        case partBroad |> (partConversion |> narrow) of
                            Ok partNarrow ->
                                wholeAssemblyResult
                                    |> Result.map
                                        (\eat -> eat partNarrow)

                            Err (Expected innerExpectation) ->
                                Expected
                                    { assemblyExpectations
                                        | fieldValues =
                                            assemblyExpectations.fieldValues
                                                |> (::) (Tagged partTag innerExpectation)
                                    }
                                    |> Err

                    Nothing ->
                        Expected
                            { assemblyExpectations
                                | fieldsAdditional =
                                    assemblyExpectations.fieldsAdditional
                                        |> (::) partTag
                            }
                            |> Err
        , broaden =
            \wholeNarrow ->
                let
                    partBroad =
                        wholeNarrow
                            |> accessPart
                            |> (partConversion |> broaden)
                in
                wholeNarrow
                    |> wholeAssemblyConversion.broaden
                    |> (::) (Tagged partTag partBroad)
        }


{-| A missed expectation on either its tag or its value.
-}
type TagOrValue tagExpectation valueExpectation
    = Tag tagExpectation
    | Value valueExpectation


{-| tag-value pair like a field or a variant.
-}
type Tagged tag value
    = Tagged tag value


{-| Incomplete variant [`Conversion`](#Conversion) builder. See [`variantEat`](#variantEat) & [`variantUnion`](#variantUnion)
-}
type alias UnionConversionStep narrowUnion tag variantValueBroad broaden variantValueExpectation =
    { narrow :
        Tagged tag variantValueBroad
        ->
            Result
                (Expected
                    (TagOrValue
                        { possibilities : List tag }
                        variantValueExpectation
                    )
                )
                narrowUnion
    , broaden : broaden
    }


{-| Building one variant value [`Conversion`](#Conversion)
and feeding it to the [`UnionConversionStep`](#UnionConversionStep).
-}
variantEat :
    ( ( variantValue -> narrowUnion
      , tag
      )
    , Conversion
        variantValue
        variantValueBroad
        (Expected variantValueExpectation)
    )
    ->
        UnionConversionStep
            narrowUnion
            tag
            variantValueBroad
            ((variantValue -> Tagged tag variantValueBroad) -> narrowUnionEat)
            variantValueExpectation
    ->
        UnionConversionStep
            narrowUnion
            tag
            variantValueBroad
            narrowUnionEat
            variantValueExpectation
variantEat ( ( variantValueToUnion, variantTag ), variantValueConversion ) =
    \unionConversionStep ->
        { narrow =
            unionConversionStep.narrow
                |> variantStepNarrow
                    ( variantValueToUnion
                    , variantTag
                    , variantValueConversion |> narrow
                    )
        , broaden =
            unionConversionStep.broaden
                |> variantStepBroaden
                    ( variantTag
                    , variantValueConversion |> broaden
                    )
        }


variantStepBroaden :
    ( tag, variantValue -> variantValueBroad )
    -> ((variantValue -> Tagged tag variantValueBroad) -> narrowUnionEat)
    -> narrowUnionEat
variantStepBroaden ( tag, variantValueBroaden ) =
    let
        broadenValueAndTag : variantValue -> Tagged tag variantValueBroad
        broadenValueAndTag =
            \variantValueNarrow ->
                variantValueNarrow
                    |> variantValueBroaden
                    |> Tagged tag
    in
    (|>) broadenValueAndTag


variantStepNarrow :
    ( variantValue -> narrowUnion
    , tag
    , variantValueBroad
      -> Result (Expected variantValueExpectation) variantValue
    )
    ->
        (Tagged tag variantValueBroad
         ->
            Result
                (Expected
                    (TagOrValue
                        { possibilities : List tag }
                        variantValueExpectation
                    )
                )
                narrowUnion
        )
    ->
        (Tagged tag variantValueBroad
         ->
            Result
                (Expected
                    (TagOrValue
                        { possibilities : List tag }
                        variantValueExpectation
                    )
                )
                narrowUnion
        )
variantStepNarrow ( variantValueToUnion, variantTag, variantValueNarrow ) =
    \unionNarrow ->
        \variantBroad ->
            case variantBroad |> unionNarrow of
                Ok variantNarrow ->
                    variantNarrow |> Ok

                Err (Expected earlierStepsExpectation) ->
                    let
                        (Tagged tagOfBroad valueBroad) =
                            variantBroad
                    in
                    if tagOfBroad == variantTag then
                        case
                            valueBroad
                                |> variantValueNarrow
                                |> Result.map variantValueToUnion
                        of
                            Ok ok ->
                                ok |> Ok

                            Err (Expected innerExpectation) ->
                                Expected (Value innerExpectation)
                                    |> Err

                    else
                        let
                            triedTags =
                                case earlierStepsExpectation of
                                    Value _ ->
                                        []

                                    Tag expectations ->
                                        expectations.possibilities
                        in
                        Expected
                            (Tag
                                { possibilities =
                                    triedTags
                                        |> (::) variantTag
                                }
                            )
                            |> Err


{-| Assemble a combined whole from its [parts](#partEat).
-}
intersection :
    narrowAssemble
    ->
        IntersectionConversionStep
            narrow_
            tag
            narrowAssemble
            (List (Tagged tag partBroad_))
            fieldValueExpectation_
intersection assemble =
    { narrow = \_ -> assemble |> Ok
    , broaden = \_ -> []
    }


{-| Discriminate a union by [variants](Conversion#variantEat).
-}
variantUnion :
    broadenUnion
    ->
        UnionConversionStep
            union_
            tag_
            variantValueBroad_
            broadenUnion
            variantValueExpectation_
variantUnion discriminate =
    { narrow =
        \(Tagged _ _) ->
            Expected (Tag { possibilities = [] }) |> Err
    , broaden = discriminate
    }



--


{-| Useful for recursive structures.
-}
lazy :
    (() -> Conversion specific general error)
    -> Conversion specific general error
lazy conversionLazy =
    { narrow =
        \general ->
            general |> (conversionLazy () |> .narrow)
    , broaden =
        \specific ->
            specific |> (conversionLazy () |> .broaden)
    }


{-| Go over an additional step of [`Conversion`](#Conversion) on a broader type.

    Conversion.transfer Set.toList Set.fromList
        |> Conversion.over
            (Value.list elementConversion)

You might recognize similarities to the concept of `andThen`.

-}
over :
    Conversion narrow broad error
    -> Conversion narrowNarrow narrow error
    -> Conversion narrowNarrow broad error
over specificChange =
    \conversion ->
        { broaden =
            \specific ->
                specific
                    |> conversion.broaden
                    |> specificChange.broaden
        , narrow =
            \general ->
                general
                    |> specificChange.narrow
                    |> Result.andThen conversion.narrow
        }


{-| Reverse the `Transfer a <-> b`
by swapping the functions [`narrow`](#narrow) <-> [`broaden`](#broaden).

    [ 'O', 'h', 'a', 'y', 'o' ]
        |> (Conversion.stringToList
            |> Conversion.reverse
            |> Conversion.map
           )
    --> "Ohayo"

[`unmap`](#unmap) is equivalent to `|> reverse |> map`.

-}
reverse :
    Transfer unmapped mapped
    -> Conversion mapped unmapped error_
reverse =
    \transfer_ ->
        { narrow =
            \unmapped ->
                unmapped |> map transfer_ |> Ok
        , broaden = unmap transfer_
        }
