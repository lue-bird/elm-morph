module Conversion exposing
    ( Conversion
    , Expected(..), expectation, errorExpectationMap
    , validate, expectationMap
    , broaden, narrow
    , Transfer
    , transfer, remain
    , listToArray, stringToList
    , lazy, over
    , reverse
    , Tagged(..), TagOrValue(..)
    , IntersectionConversionStep
    , intersection, partEat
    , UnionConversionStep
    , variantUnion, variantEat
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
@docs listToArray, stringToList


### `Transfer` scan

use [`broaden`](#broaden) or [`reverse`](#reverse) `|> broaden`


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

Tip: In general, try to narrow down the type when limiting values.

-}
validate :
    (value -> Result error value)
    -> Conversion value value error
validate narrowConvert =
    { narrow = narrowConvert
    , broaden = identity
    }



--


{-| Limits arguments to [`Conversion`](#Conversion)s that can `Never` fail while [`narrow`](#narrow)wing, for example

    stringToListConversion : Transfer String (List Char)
    stringToListConversion =
        Conversion.transfer
            ( String.toList, String.fromList )

See [`transfer`](#transfer).

**!** Information can get lost on the way:

    dictToListConversion :
        Transfer
            (Dict comparableKey value)
            (List ( comparableKey, value ))
    dictToListConversion =
        Conversion.transfer
            ( Dict.toList, Dict.fromList )

Still, there's no parsing to transfer one state to the other.

TODO: Its own module and representation independent of `Conversion` for enforceable terminology (`map` instead of `reverse |> broaden`)?

-}
type alias Transfer specific specificMapped =
    Conversion specific specificMapped Never


{-| A [`Conversion`](#Conversion) that doesn't transform anything.

Same as writing:

  - `transfer ( identity, identity )`
  - `validate Ok`
  - `{ narrow = Ok, broaden = identity }`

-}
remain : Conversion value value error_
remain =
    transfer ( identity, identity )


{-| Mutual `Conversion`
between representations
that have the same structural information
and can be mapped 1:1 into each other.

    stringToListConversion : Conversion String (List Char) error_
    stringToListConversion =
        Conversion.transfer
            ( String.toList, String.fromList )

Included here:

  - [`stringToList`](#stringToList)
  - [`listToArray`](#listToArray)

-}
transfer :
    ( specific -> specificMapped
    , specificMapped -> specific
    )
    -> Conversion specificMapped specific error_
transfer ( to, from ) =
    { broaden = from
    , narrow =
        \mapped ->
            mapped |> to |> Ok
    }


{-| `Transfer` from `List` to `Array`.

    import Array

    [ 0, 1, 2, 3 ]
        |> (Conversion.listToArray |> Conversion.broaden)
    --> Array.fromList [ 0, 1, 2, 3 ]

-}
listToArray : Conversion (List element) (Array element) error_
listToArray =
    transfer ( Array.toList, Array.fromList )


{-| `Transfer` from `List` to `Array`.

    "0123" |> (Conversion.stringToList |> Conversion.broaden)
    --> [ '0', '1', '2', '3' ]

-}
stringToList : Conversion String (List Char) error_
stringToList =
    transfer ( String.fromList, String.toList )



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

    ( Set.fromList, Set.toList )
        |> Conversion.transfer
        |> Conversion.over
            (Value.list elementConversion)

You might recognize similarities to the concept of `andThen`.

-}
over :
    Conversion narrow broad error
    -> Conversion broad broadBroad error
    -> Conversion narrow broadBroad error
over specificChange =
    \conversion ->
        { broaden =
            \specific ->
                specific
                    |> specificChange.broaden
                    |> conversion.broaden
        , narrow =
            \general ->
                general
                    |> conversion.narrow
                    |> Result.andThen
                        specificChange.narrow
        }


{-| Reverse the `Transfer a <-> b`
by swapping the functions [`narrow`](#narrow) <-> [`broaden`](#broaden).

    [ 'O', 'h', 'a', 'y', 'o' ]
        |> (Conversion.stringToList
            |> Conversion.reverse
            |> Conversion.broaden
           )
    --> "Ohayo"

-}
reverse :
    Transfer specific specificMapped
    -> Conversion specificMapped specific error_
reverse =
    \transfer_ ->
        { narrow =
            \mapped ->
                mapped
                    |> (transfer_ |> broaden)
                    |> Ok
        , broaden =
            \specific ->
                case specific |> (transfer_ |> narrow) of
                    Ok ok ->
                        ok

                    Err error ->
                        error |> never
        }
