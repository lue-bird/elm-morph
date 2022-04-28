module Conversion exposing
    ( Conversion, ConversionPreserving
    , Error(..)
    , remain, preserving, validate
    , ConversionStep, TagOrValue(..)
    , eatPart, Missing(..), eatVariant
    , broaden, narrow
    , lazy, over
    , expectationMap
    )

{-|

@docs Conversion, ConversionPreserving
@docs Error


## create

@docs remain, preserving, validate


### step

@docs ConversionStep, TagOrValue
@docs eatPart, Missing, eatVariant


## scan

@docs broaden, narrow


## transform

@docs errorMap, lazy, over


## TODO

  - [miniBill/elm-codec](https://github.com/miniBill/elm-codec/blob/main/src/Codec.elm)
  - [MartinSStewart/elm-serialize](https://github.com/MartinSStewart/elm-serialize/blob/master/src/Serialize.elm)
  - [elm/json](https://github.com/elm/json/blob/1.1.3/src/Json/Decode.elm)

-}

import Dict exposing (Dict)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)


{-| Morph functions to a more general format and back.
-}
type alias Conversion narrow broad error =
    RecordWithoutConstructorFunction
        { narrow : broad -> Result error narrow
        , broaden : narrow -> broad
        }


{-| Limits arguments to [`Conversion`]s that can `Never` fail.
-}
type alias ConversionPreserving specific specificMapped =
    Conversion specific specificMapped Never


type Error expectation
    = Expected expectation


broaden :
    Conversion specific general error
    -> (specific -> general)
broaden =
    \conversion -> conversion.broaden


narrow :
    Conversion specific general error
    -> (general -> Result error specific)
narrow =
    \conversion -> conversion.narrow


expectationMap :
    (expectation -> expectationMapped)
    -> Conversion specific general (Error expectation)
    -> Conversion specific general (Error expectationMapped)
expectationMap expectationChange =
    \conversion ->
        { broaden = conversion |> broaden
        , narrow =
            \general ->
                general
                    |> (conversion |> narrow)
                    |> Result.mapError
                        (\(Expected expectation) ->
                            Expected (expectation |> expectationChange)
                        )
        }



--


{-| A `Conversion` that doesn't transform the value,
just brings it to and from elm.
-}
remain : Conversion value value errorCustom_
remain =
    validate Ok


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


{-| Mutual `Conversion`
between representations
that have the same structural information
and can be mapped 1:1 into each other.
-}
preserving :
    ( specific -> specificMapped
    , specificMapped -> specific
    )
    -> Conversion specificMapped specific error
preserving ( to, from ) =
    { broaden = from
    , narrow =
        \unmapped ->
            unmapped |> to |> Ok
    }


{-| A step in building a `Conversion`
between parts & an assembled whole and back.
-}
type alias ConversionStep narrow narrowEat broad expectation =
    RecordWithoutConstructorFunction
        { narrow :
            broad -> Result (Error expectation) narrowEat
        , broaden : narrow -> broad
        }


eatPart :
    ( ( whole -> part
      , tag
      )
    , Conversion part partBroad (Error partValueExpectation)
    )
    ->
        ConversionStep
            whole
            (part -> wholeAssembleFurther)
            (List (Tagged tag partBroad))
            (List (Tagged tag (TagOrValue Missing partValueExpectation)))
    ->
        ConversionStep
            whole
            wholeAssembleFurther
            (List (Tagged tag partBroad))
            (List (Tagged tag (TagOrValue Missing partValueExpectation)))
eatPart ( ( accessPart, partTag ), partConversion ) =
    \wholeAssemblyConversion ->
        { narrow =
            \dictPart ->
                let
                    wholeAssemblyResult =
                        dictPart |> wholeAssemblyConversion.narrow

                    assemblyExpectations =
                        case wholeAssemblyResult of
                            Ok _ ->
                                []

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
                                    (assemblyExpectations
                                        |> (::) (Tagged partTag (Value innerExpectation))
                                    )
                                    |> Err

                    Nothing ->
                        Expected
                            (assemblyExpectations
                                |> (::) (Tagged partTag (Tag Missing))
                            )
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


type TagOrValue tagExpectation valueExpectation
    = Tag tagExpectation
    | Value valueExpectation


type Tagged tag value
    = Tagged tag value


type Missing
    = Missing


type alias UnionConversionStep narrowUnion tag variantValueBroad broaden variantValueExpectation =
    { narrow :
        Tagged tag variantValueBroad
        ->
            Result
                (Error
                    (TagOrValue
                        { oneOf : List tag }
                        variantValueExpectation
                    )
                )
                narrowUnion
    , broaden : broaden
    }


eatVariant :
    ( ( variantValue -> narrowUnion
      , tag
      )
    , Conversion
        variantValue
        variantValueBroad
        (Error variantValueExpectation)
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
eatVariant ( ( variantValueToUnion, variantTag ), variantValueConversion ) =
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
      -> Result (Error variantValueExpectation) variantValue
    )
    ->
        (Tagged tag variantValueBroad
         ->
            Result
                (Error
                    (TagOrValue
                        { oneOf : List tag }
                        variantValueExpectation
                    )
                )
                narrowUnion
        )
    ->
        (Tagged tag variantValueBroad
         ->
            Result
                (Error
                    (TagOrValue
                        { oneOf : List tag }
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
                                        expectations.oneOf
                        in
                        Expected
                            (Tag
                                { oneOf =
                                    triedTags
                                        |> (::) variantTag
                                }
                            )
                            |> Err


{-| Assemble from [parts](Conversion#partEat).
-}
whole :
    narrowAssemble
    ->
        ConversionStep
            narrow_
            narrowAssemble
            (List (Tagged tag partBroad))
            foodValueExpectation_
whole assemble =
    { narrow = \_ -> assemble |> Ok
    , broaden = \_ -> []
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


over :
    Conversion moreSpecific specific error
    -> Conversion specific general error
    -> Conversion moreSpecific general error
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
