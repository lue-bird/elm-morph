module Value exposing
    ( ValueAny, LiteralAny, StructureAny, RecordOfValueAny
    , Valuey(..), Literaly(..), Structurey(..)
    , Origin(..), ModuleOrigin, RepositoryOrigin(..)
    , repositoryElmCoreOrigin
    , unit, char, int, float, posix
    , tuple2, tuple3
    , bool, maybe, result, list, string, array, set, dict
    , group, part, groupFinish, GroupMorph
    , choice, variant, choiceIn, ChoiceMorphInProgress
    , Error, expectationCustomMap
    , DefaultOrCustom(..)
    , Expectation, ExpectationIn(..)
    , LiteralKind
    , StructureExpectation, Tuple2Expectation, Tuple3Expectation, KindOrInsideExpectation, StructureLinearInsideExpectation, RecordInsideExpectation, PartExpectation, VariantTagExpectation(..), VariantInsideExpectation
    )

{-| `case`-able elm values.

json encoders/decoders are too low-level for serialization,
explicitly describing how to serialize individual data types that all have the same shape.

Plus it makes it harder to switch to a different format.


### prior art

  - [`bundsol/`: `Boxed`](https://package.elm-lang.org/packages/bundsol/boxed/2.0.0/Boxed)
      - 👎 no box-unbox morph pairs
  - [`tricycle/elm-storage`: `Storage.Value`](https://dark.elm.dmy.fr/packages/tricycle/elm-storage/latest/Storage-Value)
      - 👎 doesn't expose the `Value` variants
  - [`andre-dietrich/elm-generic`](https://dark.elm.dmy.fr/packages/andre-dietrich/elm-generic/latest/Generic)
      - 👍 multiple broad results: json, xml, yaml
      - 👎 no encode-decode morph pairs
  - [`the-sett/decode-generic`](https://dark.elm.dmy.fr/packages/the-sett/decode-generic/latest/Json-Decode-Generic)
      - 👎 no encode (so also no encode-decode morph pairs)
  - [`miniBill/elm-codec`](https://dark.elm.dmy.fr/packages/miniBill/elm-codec/latest/Codec)
      - 👎 no custom errors
  - [`MartinSStewart/elm-serialize`](https://dark.elm.dmy.fr/packages/MartinSStewart/elm-serialize/latest/)
      - 👍 multiple broad results: json, string (url safe), `Bytes`
      - 👍 custom errors
      - doesn't encode field & variant names
          - 👎 hard to debug
          - 👎 easy to corrupt
          - 👍 little space
  - [`fujiy/elm-json-convert`](https://dark.elm.dmy.fr/packages/fujiy/elm-json-convert/latest/Json-Convert)
      - 👎 no custom errors
      - 👎 no variant converters
  - [`prozacchiwawa/elm-json-codec`](https://dark.elm.dmy.fr/packages/prozacchiwawa/elm-json-codec/latest/JsonCodec)
      - 👎 no custom errors
      - 👎 no variant converters

@docs ValueAny, LiteralAny, StructureAny, RecordOfValueAny
@docs Valuey, Literaly, Structurey


### origins

@docs Origin, ModuleOrigin, RepositoryOrigin
@docs repositoryElmCoreOrigin


## morph

@docs unit, char, int, float, posix
@docs tuple2, tuple3
@docs bool, maybe, result, list, string, array, set, dict
@docs group, part, groupFinish, GroupMorph
@docs choice, variant, choiceIn, ChoiceMorphInProgress


### morphs on common formats

  - [`Json`](Json)

If you feel especially motivated, throw a PR adding

  - `Morph ValueAny Json.Encode.Value ...`
  - `Morph ValueAny String ...`
  - `Morph ValueAny Bytes ...`
  - `Morph ValueAny Yaml ...`
  - `Morph ValueAny Xml ...`
  - `Morph ValueAny Csv ...`
  - ...


## expectations

@docs Error, expectationCustomMap
@docs DefaultOrCustom
@docs Expectation, ExpectationIn
@docs LiteralKind
@docs StructureExpectation, Tuple2Expectation, Tuple3Expectation, KindOrInsideExpectation, StructureLinearInsideExpectation, RecordInsideExpectation, PartExpectation, VariantTagExpectation, VariantInsideExpectation

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Morph exposing (GroupMorphInProgress, Morph, TagOrValue(..), Tagged(..), broaden, narrow)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Set exposing (Set)
import Time exposing (Posix)
import Util exposing (listResultsToValuesOrErrors)


{-| elm structure that can itself contain values.
-}
type Structurey tuple2y tuple3y listy arrayy recordy varianty
    = Tuple2y tuple2y
    | Tuple3y tuple3y
    | Listy listy
    | Arrayy arrayy
    | Recordy recordy
    | Varianty varianty


{-| elm literal that don't itself contain values.
-}
type Literaly unity chary inty floaty stringy posixy
    = Unity unity
    | Chary chary
    | Inty inty
    | Floaty floaty
    | Stringy stringy
    | Posixy posixy


{-| elm value. Either

  - a literal that don't itself contain values
  - a structure that can itself contain values

-}
type Valuey literaly structurey
    = Literaly literaly
    | Structurey structurey


{-| Any value representable in elm.

Is there a structure you'd like to add that can't be converted in a small amount of time? Add it through a PR!

TODO:

  - maybe add `Possible`/`Never` options for each case

-}
type alias ValueAny =
    Valuey LiteralAny StructureAny


{-| A supported elm literal (that don't contain other [values](#ValueAny))
-}
type alias LiteralAny =
    Literaly () Char Int Float String Posix


{-| A structure that can itself contain further values.
-}
type StructureAny
    = StructureAnyIn
        (Structurey
            ( ValueAny, ValueAny )
            ( ValueAny, ValueAny, ValueAny )
            (List ValueAny)
            (Array ValueAny)
            RecordOfValueAny
            (Tagged Origin ValueAny)
        )


{-| The structure of a record that can hold [any](#ValueAny) field value.
-}
type alias RecordOfValueAny =
    List (Tagged String ValueAny)



-- morph


{-| Information on what went wrong while `narrow`ing from a [`ValueAny`](#ValueAny).
-}
type alias Error expectationCustom =
    Morph.Expected (Expectation expectationCustom)


{-| A failed expectation of a different structure kind or an inner part.
-}
type KindOrInsideExpectation insideExpectation
    = Kind
    | Inside insideExpectation


{-| A failed expectation of a value.
-}
type alias Expectation expectationCustom =
    DefaultOrCustom
        (Valuey
            LiteralKind
            (StructureExpectation (ExpectationIn expectationCustom))
        )
        expectationCustom


{-| Wraps failed expectations nested inside a [structure](#StructureExpectation).
-}
type ExpectationIn expectationCustom
    = ExpectationIn (Expectation expectationCustom)


{-| Either a basic library expectation or a custom defined expectation
-}
type DefaultOrCustom default custom
    = Default default
    | Custom custom


{-| A supported elm literal kind (that doesn't contain other [values](#ValueAny))
-}
type alias LiteralKind =
    Literaly () () () () () ()


{-| Failed expectation for the Morph.Expected [structure](#StructureAny)
– [kind or inside](#KindOrInsideExpectation).
-}
type alias StructureExpectation expectation =
    Structurey
        (Tuple2Expectation expectation)
        (Tuple3Expectation expectation)
        (KindOrInsideExpectation (StructureLinearInsideExpectation expectation))
        (KindOrInsideExpectation (StructureLinearInsideExpectation expectation))
        (KindOrInsideExpectation (RecordInsideExpectation expectation))
        (KindOrInsideExpectation (VariantInsideExpectation expectation))


{-| For each part: Was it `Ok ()` or was there an `Err ...`?

If all 2 parts are `Ok ()`, a 2-tuple was Morph.Expected but something else was found!

This expectation could be represented as a choice.
However, a tuple is a lot easier to work with;
you don't lose type information either way :)

-}
type alias Tuple2Expectation expectationAtPart =
    ( Result expectationAtPart ()
    , Result expectationAtPart ()
    )


{-| For each part: Was it `Ok ()` or was there an `Err ...`?

If all 3 parts are `Ok ()`, a 3-tuple was Morph.Expected but something else was found!

This expectation could be represented as a choice.
However, a tuple is a lot easier to work with;
you don't lose type information either way :)

-}
type alias Tuple3Expectation expectationAtPart =
    ( Result expectationAtPart ()
    , Result expectationAtPart ()
    , Result expectationAtPart ()
    )


{-| Failed expectation for the Morph.Expected elements at specific indexes.
-}
type alias StructureLinearInsideExpectation atElement =
    RecordWithoutConstructorFunction
        { elementsAtIndexes : Dict Int atElement }


{-| Failed expectation for the Morph.Expected record field tags or values.
-}
type alias RecordInsideExpectation partValueExpectation =
    List (Tagged String (PartExpectation partValueExpectation))


{-| Failed expectation for the Morph.Expected variant tag or value.
-}
type alias VariantInsideExpectation valueExpectation =
    TagOrValue
        VariantTagExpectation
        (Tagged String valueExpectation)


{-| Failed expectation for the Morph.Expected variant tag.
-}
type VariantTagExpectation
    = VariantTagFromModule ModuleOrigin
    | VariantTagOneIn (List String)


{-| Change the `Custom` [`Expectation`](#Expectation).
-}
expectationCustomMap :
    (expectationCustom -> expectationCustomMapped)
    -> Expectation expectationCustom
    -> Expectation expectationCustomMapped
expectationCustomMap customChange =
    let
        step :
            ExpectationIn expectationCustom
            -> ExpectationIn expectationCustomMapped
        step =
            \(ExpectationIn expectation) ->
                expectation
                    |> expectationCustomMap customChange
                    |> ExpectationIn

        insideStructureMap :
            StructureExpectation (ExpectationIn expectationCustom)
            -> StructureExpectation (ExpectationIn expectationCustomMapped)
        insideStructureMap =
            \structureInside ->
                case structureInside of
                    Tuple2y ( part0Expectation, part1Expectation ) ->
                        ( part0Expectation |> Result.mapError step
                        , part1Expectation |> Result.mapError step
                        )
                            |> Tuple2y

                    Tuple3y ( part0Expectation, part1Expectation, part2Expectation ) ->
                        ( part0Expectation |> Result.mapError step
                        , part1Expectation |> Result.mapError step
                        , part2Expectation |> Result.mapError step
                        )
                            |> Tuple3y

                    Listy (Inside listInside) ->
                        listInside
                            |> arrayInsideMap
                            |> Inside
                            |> Listy

                    Arrayy (Inside arrayInside) ->
                        arrayInside
                            |> arrayInsideMap
                            |> Inside
                            |> Arrayy

                    Recordy (Inside recordExpectation) ->
                        recordExpectation
                            |> recordInsideExpectationMap
                            |> Inside
                            |> Recordy

                    Varianty (Inside variantExpectation) ->
                        variantExpectation
                            |> expectationVariantInsideMap
                            |> Inside
                            |> Varianty

                    Listy Kind ->
                        Listy Kind

                    Arrayy Kind ->
                        Arrayy Kind

                    Recordy Kind ->
                        Recordy Kind

                    Varianty Kind ->
                        Varianty Kind

        arrayInsideMap :
            StructureLinearInsideExpectation (ExpectationIn expectationCustom)
            -> StructureLinearInsideExpectation (ExpectationIn expectationCustomMapped)
        arrayInsideMap =
            \arrayInside ->
                { elementsAtIndexes =
                    arrayInside.elementsAtIndexes
                        |> Dict.map (\_ -> step)
                }

        recordInsideExpectationMap :
            RecordInsideExpectation (ExpectationIn expectationCustom)
            -> RecordInsideExpectation (ExpectationIn expectationCustomMapped)
        recordInsideExpectationMap =
            \recordExpectation ->
                recordExpectation
                    |> List.map
                        (\(Tagged tag partExpectation) ->
                            (case partExpectation of
                                PartExisting ->
                                    PartExisting

                                PartValue value ->
                                    (value |> step) |> PartValue
                            )
                                |> Tagged tag
                        )

        expectationVariantInsideMap :
            VariantInsideExpectation (ExpectationIn expectationCustom)
            -> VariantInsideExpectation (ExpectationIn expectationCustomMapped)
        expectationVariantInsideMap =
            \variantInsideExpectation ->
                case variantInsideExpectation of
                    Tag tag ->
                        Tag tag

                    Value (Tagged tag possibilityValueExpectation) ->
                        (possibilityValueExpectation |> step)
                            |> Tagged tag
                            |> Value
    in
    \expectation ->
        case expectation of
            Custom custom ->
                custom |> customChange |> Custom

            Default (Literaly literalKind) ->
                literalKind |> Literaly |> Default

            Default (Structurey structureInside) ->
                structureInside
                    |> insideStructureMap
                    |> Structurey
                    |> Default



--


{-| `Origin (ModuleOrigin (RepositoryOrigin userName repositoryName)) memberName`
-}
type Origin
    = Origin ModuleOrigin String


{-| `ModuleOrigin (RepositoryOrigin "userName" "repository-name") "Module.Name"`
-}
type ModuleOrigin
    = ModuleOrigin RepositoryOrigin String


{-| `RepositoryOrigin userName repositoryName`, for example

  - [`repositoryElmCoreOrigin`](#repositoryElmCoreOrigin)

-}
type RepositoryOrigin
    = RepositoryOrigin String String


{-| [`RepositoryOrigin`](#RepositoryOrigin) of `elm/core`.
-}
repositoryElmCoreOrigin : RepositoryOrigin
repositoryElmCoreOrigin =
    RepositoryOrigin "elm" "core"



--


literal :
    { kind : () -> LiteralKind
    , morph : Morph literalSpecific LiteralAny ()
    }
    -> Morph literalSpecific ValueAny (Error expectationCustom_)
literal { kind, morph } =
    { broaden =
        \literalSpecific ->
            literalSpecific
                |> (morph |> broaden)
                |> Literaly
    , narrow =
        \valueAny ->
            let
                narrowLiteral =
                    case valueAny of
                        Literaly literalAny ->
                            literalAny
                                |> (morph |> narrow)

                        Structurey _ ->
                            Err ()
            in
            narrowLiteral
                |> Result.mapError
                    (\() ->
                        Morph.Expected
                            (Literaly (kind ()) |> Default)
                    )
    }


{-| `()` [`Morph`](Morph#Morph).
-}
unit : Morph () ValueAny (Error expectationCustom_)
unit =
    literal
        { kind = Unity
        , morph =
            { broaden = Unity
            , narrow =
                \valueAny ->
                    case valueAny of
                        Unity unitValue ->
                            unitValue |> Ok

                        _ ->
                            Err ()
            }
        }


{-| `Char` [`Morph`](Morph#Morph).
-}
char : Morph Char ValueAny (Error expectationCustom_)
char =
    literal
        { kind = Chary
        , morph =
            { broaden = Chary
            , narrow =
                \valueAny ->
                    case valueAny of
                        Chary charValue ->
                            charValue |> Ok

                        _ ->
                            Err ()
            }
        }


{-| `Int` [`Morph`](Morph#Morph).
-}
int : Morph Int ValueAny (Error expectationCustom_)
int =
    literal
        { kind = Inty
        , morph =
            { broaden = Inty
            , narrow =
                \valueAny ->
                    case valueAny of
                        Inty intValue ->
                            intValue |> Ok

                        _ ->
                            Err ()
            }
        }


{-| `Float` [`Morph`](Morph#Morph).
-}
float : Morph Float ValueAny (Error expectationCustom_)
float =
    literal
        { kind = Floaty
        , morph =
            { broaden = Floaty
            , narrow =
                \valueAny ->
                    case valueAny of
                        Floaty floatValue ->
                            floatValue |> Ok

                        _ ->
                            Err ()
            }
        }


{-| `String` [`Morph`](Morph#Morph).
-}
string : Morph String ValueAny (Error expectationCustom_)
string =
    literal
        { kind = Stringy
        , morph =
            { broaden = Stringy
            , narrow =
                \valueAny ->
                    case valueAny of
                        Stringy stringNarrow ->
                            stringNarrow |> Ok

                        _ ->
                            Err ()
            }
        }


{-| `Posix` [`Morph`](Morph#Morph).
-}
posix : Morph Posix ValueAny (Error expectationCustom_)
posix =
    literal
        { kind = Posixy
        , morph =
            { broaden = Posixy
            , narrow =
                \valueAny ->
                    case valueAny of
                        Posixy posixNarrow ->
                            posixNarrow |> Ok

                        _ ->
                            Err ()
            }
        }



--


partNarrowExpectation :
    Result (Error expectation) value_
    -> Result (ExpectationIn expectation) ()
partNarrowExpectation =
    \partNarrow ->
        partNarrow
            |> Result.map (\_ -> ())
            |> Result.mapError
                (\(Morph.Expected expectation) ->
                    expectation |> ExpectationIn
                )


{-| `( ..., ... )` [`Morph`](Morph#Morph).
-}
tuple2 :
    ( Morph part0 ValueAny (Error expectationCustom)
    , Morph part1 ValueAny (Error expectationCustom)
    )
    -> Morph ( part0, part1 ) ValueAny (Error expectationCustom)
tuple2 partMorphs =
    let
        ( part0Morph, part1Morph ) =
            partMorphs
    in
    { broaden =
        \( part0, part1 ) ->
            ( part0 |> (part0Morph |> broaden)
            , part1 |> (part1Morph |> broaden)
            )
                |> Tuple2y
                |> StructureAnyIn
                |> Structurey
    , narrow =
        \valueAny ->
            case valueAny of
                Structurey (StructureAnyIn (Tuple2y ( part0, part1 ))) ->
                    case
                        ( part0 |> (part0Morph |> narrow)
                        , part1 |> (part1Morph |> narrow)
                        )
                    of
                        ( Ok part0Ok, Ok part1Ok ) ->
                            ( part0Ok, part1Ok ) |> Ok

                        ( part0Narrow, part1Narrow ) ->
                            Morph.Expected
                                (( part0Narrow |> partNarrowExpectation
                                 , part1Narrow |> partNarrowExpectation
                                 )
                                    |> Tuple2y
                                    |> Structurey
                                    |> Default
                                )
                                |> Err

                _ ->
                    Morph.Expected
                        (Structurey (Tuple2y ( Ok (), Ok () )) |> Default)
                        |> Err
    }


{-| `( ..., ..., ... )` [`Morph`](Morph#Morph).
-}
tuple3 :
    ( Morph part0 ValueAny (Error expectationCustom)
    , Morph part1 ValueAny (Error expectationCustom)
    , Morph part2 ValueAny (Error expectationCustom)
    )
    ->
        Morph
            ( part0, part1, part2 )
            ValueAny
            (Error expectationCustom)
tuple3 partMorphs =
    let
        ( part0Morph, part1Morph, part2Morph ) =
            partMorphs
    in
    { broaden =
        \( part0, part1, part2 ) ->
            ( part0 |> (part0Morph |> broaden)
            , part1 |> (part1Morph |> broaden)
            , part2 |> (part2Morph |> broaden)
            )
                |> Tuple3y
                |> StructureAnyIn
                |> Structurey
    , narrow =
        \valueAny ->
            case valueAny of
                Structurey (StructureAnyIn (Tuple3y ( part0, part1, part2 ))) ->
                    case
                        ( part0 |> (part0Morph |> narrow)
                        , part1 |> (part1Morph |> narrow)
                        , part2 |> (part2Morph |> narrow)
                        )
                    of
                        ( Ok part0Ok, Ok part1Ok, Ok part2Ok ) ->
                            ( part0Ok, part1Ok, part2Ok ) |> Ok

                        ( part0Narrow, part1Narrow, part2Narrow ) ->
                            Morph.Expected
                                (( part0Narrow |> partNarrowExpectation
                                 , part1Narrow |> partNarrowExpectation
                                 , part2Narrow |> partNarrowExpectation
                                 )
                                    |> Tuple3y
                                    |> Structurey
                                    |> Default
                                )
                                |> Err

                _ ->
                    Morph.Expected
                        (Structurey (Tuple3y ( Ok (), Ok (), Ok () )) |> Default)
                        |> Err
    }


{-| `List` [`Morph`](Morph#Morph).
-}
list :
    Morph element ValueAny (Error expectationCustom)
    -> Morph (List element) ValueAny (Error expectationCustom)
list elementMorph =
    { narrow =
        \broad ->
            case broad of
                Structurey (StructureAnyIn (Listy listOfElementsAny)) ->
                    listOfElementsAny
                        |> List.map (elementMorph |> narrow)
                        |> listResultsToValuesOrErrors
                        |> Result.mapError
                            (\listInsideExpectation ->
                                Morph.Expected
                                    ({ elementsAtIndexes =
                                        listInsideExpectation.elementsAtIndexes
                                            |> Dict.map (\_ -> ExpectationIn)
                                     }
                                        |> Inside
                                        |> Listy
                                        |> Structurey
                                        |> Default
                                    )
                            )

                _ ->
                    Morph.Expected
                        (Structurey (Listy Kind) |> Default)
                        |> Err
    , broaden =
        \listNarrow ->
            listNarrow
                |> List.map (elementMorph |> broaden)
                |> Listy
                |> StructureAnyIn
                |> Structurey
    }


{-| `Array` [`Morph`](Morph#Morph).
-}
array :
    Morph element ValueAny (Error expectationCustom)
    -> Morph (Array element) ValueAny (Error expectationCustom)
array elementMorph =
    { narrow =
        \broad ->
            case broad of
                Structurey (StructureAnyIn (Arrayy arrayOfElementsAny)) ->
                    arrayOfElementsAny
                        |> Array.toList
                        |> List.map (elementMorph |> narrow)
                        |> listResultsToValuesOrErrors
                        |> Result.map Array.fromList
                        |> Result.mapError
                            (\arrayInsideExpectation ->
                                Morph.Expected
                                    ({ elementsAtIndexes =
                                        arrayInsideExpectation.elementsAtIndexes
                                            |> Dict.map (\_ -> ExpectationIn)
                                     }
                                        |> Inside
                                        |> Arrayy
                                        |> Structurey
                                        |> Default
                                    )
                            )

                _ ->
                    Morph.Expected
                        (Structurey (Arrayy Kind) |> Default)
                        |> Err
    , broaden =
        \arrayNarrow ->
            arrayNarrow
                |> Array.map (elementMorph |> broaden)
                |> Arrayy
                |> StructureAnyIn
                |> Structurey
    }



--


{-| Incomplete variant union [`Morph`](Morph#Morph) to [`ValueAny`](#ValueAny)

  - starting from [`choice`](#choice)
  - over [`variant`](#variant)
  - and completed with [`choiceIn`](#choiceIn)

-}
type alias ChoiceMorphInProgress choiceNarrow choiceBroadenFurther variantValueExpectationCustom =
    RecordWithoutConstructorFunction
        { narrow :
            Tagged String ValueAny
            ->
                Result
                    (Morph.Expected
                        (TagOrValue
                            { oneIn : List String }
                            (Tagged String (ExpectationIn variantValueExpectationCustom))
                        )
                    )
                    choiceNarrow
        , broaden : choiceBroadenFurther
        }


{-| Discriminate into [variants](#variant).
-}
choice :
    choiceBroadenByVariant
    ->
        ChoiceMorphInProgress
            choiceNarrow_
            choiceBroadenByVariant
            variantValueExpectationCustom_
choice choiceBroadenDiscriminatedByPossibility =
    { narrow =
        \_ ->
            Morph.Expected (Tag { oneIn = [] }) |> Err
    , broaden = choiceBroadenDiscriminatedByPossibility
    }


{-| Describe another variant value [`Morph`](Morph#Morph) to [`ValueAny`](#ValueAny)
-}
variant :
    ( possibilityNarrow -> choiceNarrow
    , String
    )
    -> Morph possibilityNarrow ValueAny (Error variantValueExpectationCustom)
    ->
        (ChoiceMorphInProgress
            choiceNarrow
            ((possibilityNarrow -> Tagged String ValueAny)
             -> choiceBroadenFurther
            )
            variantValueExpectationCustom
         ->
            ChoiceMorphInProgress
                choiceNarrow
                choiceBroadenFurther
                variantValueExpectationCustom
        )
variant ( possibilityToChoice, possibilityTag ) possibilityMorph =
    \choiceMorphSoFar ->
        { narrow =
            variantStepNarrow
                ( possibilityToChoice, possibilityTag, possibilityMorph.narrow )
                choiceMorphSoFar.narrow
        , broaden =
            choiceMorphSoFar.broaden
                (broaden possibilityMorph
                    >> Tagged possibilityTag
                )
        }


variantStepNarrow :
    ( possibilityNarrow -> narrowChoice
    , String
    , possibilityBroad
      -> Result (Error variantValueExpectationCustom) possibilityNarrow
    )
    ->
        (Tagged String possibilityBroad
         ->
            Result
                (Morph.Expected
                    (TagOrValue
                        { oneIn : List String }
                        (Tagged String (ExpectationIn variantValueExpectationCustom))
                    )
                )
                narrowChoice
        )
    ->
        (Tagged String possibilityBroad
         ->
            Result
                (Morph.Expected
                    (TagOrValue
                        { oneIn : List String }
                        (Tagged String (ExpectationIn variantValueExpectationCustom))
                    )
                )
                narrowChoice
        )
variantStepNarrow ( possibilityToChoice, variantTag, possibilityNarrow ) =
    \choiceNarrowSoFar ->
        \variantBroad ->
            case variantBroad |> choiceNarrowSoFar of
                Ok variantNarrow ->
                    variantNarrow |> Ok

                Err (Morph.Expected earlierStepsExpectation) ->
                    let
                        (Tagged tag valueBroad) =
                            variantBroad
                    in
                    if tag == variantTag then
                        case valueBroad |> possibilityNarrow of
                            Ok ok ->
                                ok |> possibilityToChoice |> Ok

                            Err (Morph.Expected valueExpectation) ->
                                Morph.Expected
                                    (Value
                                        (Tagged variantTag (valueExpectation |> ExpectationIn))
                                    )
                                    |> Err

                    else
                        let
                            tagsTriedSoFar =
                                case earlierStepsExpectation of
                                    Value _ ->
                                        []

                                    Tag tagExpectation ->
                                        tagExpectation.oneIn
                        in
                        Morph.Expected
                            (Tag { oneIn = tagsTriedSoFar |> (::) variantTag })
                            |> Err


{-| Conclude a [`Morph.choice`](Morph#choice) |> [`Morph.possibility`](Morph#possibility) chain.
-}
choiceIn :
    ModuleOrigin
    ->
        (ChoiceMorphInProgress
            choiceNarrow
            (choiceNarrow -> Tagged String ValueAny)
            variantValueExpectationCustom
         ->
            Morph
                choiceNarrow
                ValueAny
                (Error variantValueExpectationCustom)
        )
choiceIn moduleOrigin =
    \choiceMorphComplete ->
        { narrow =
            \value ->
                case value of
                    Structurey (StructureAnyIn (Varianty (Tagged origin possibilityAny))) ->
                        let
                            (Origin variantModuleOrigin variantTag) =
                                origin
                        in
                        if variantModuleOrigin == moduleOrigin then
                            Tagged variantTag possibilityAny
                                |> narrow choiceMorphComplete
                                |> Result.mapError
                                    (Morph.errorExpectationMap
                                        (\expectation ->
                                            (case expectation of
                                                Tag tagExpectation ->
                                                    VariantTagOneIn tagExpectation.oneIn
                                                        |> Tag

                                                Value valueExpectation ->
                                                    valueExpectation |> Value
                                            )
                                                |> Inside
                                                |> Varianty
                                                |> Structurey
                                                |> Default
                                        )
                                    )

                        else
                            Morph.Expected
                                (VariantTagFromModule moduleOrigin
                                    |> Tag
                                    |> Inside
                                    |> Varianty
                                    |> Structurey
                                    |> Default
                                )
                                |> Err

                    _ ->
                        Morph.Expected
                            (Structurey (Varianty Kind) |> Default)
                            |> Err
        , broaden =
            \narrowChoice ->
                let
                    (Tagged tag valueBroad) =
                        narrowChoice |> broaden choiceMorphComplete
                in
                valueBroad
                    |> Tagged (Origin moduleOrigin tag)
                    |> Varianty
                    |> StructureAnyIn
                    |> Structurey
        }


{-| Start a group assembly [`Morph`](Morph#Morph) to [`ValueAny`](#ValueAny).

  - continue with [`part`](#part)
  - finish with [`groupFinish`](#groupFinish)

-}
group :
    groupNarrowAssemble
    ->
        GroupMorphInProgress
            (groupNarrow_ -> RecordOfValueAny)
            groupNarrowAssemble
            RecordOfValueAny
            (Morph.Expected
                (RecordInsideExpectation (ExpectationIn partExpectationCustom_))
            )
group groupNarrowAssemble =
    Morph.group groupNarrowAssemble []


{-| group part failed expectation
-}
type PartExpectation partExpectation
    = PartExisting
    | PartValue partExpectation


{-| possibly incomplete [`Morph`] step from and to a group.
-}
type alias GroupMorph group groupNarrowFurther partExpectationCustom =
    GroupMorphInProgress
        (group -> RecordOfValueAny)
        groupNarrowFurther
        RecordOfValueAny
        (Morph.Expected
            (RecordInsideExpectation (ExpectationIn partExpectationCustom))
        )


{-| Continue a group assembly [`Morph`](Morph#Morph) to [`ValueAny`](#ValueAny).

  - finish with [`groupFinish`](#groupFinish)

-}
part :
    ( group -> partNarrow, String )
    -> Morph partNarrow ValueAny (Error partExpectationCustom)
    ->
        (GroupMorph
            group
            (partNarrow -> groupNarrowFurther)
            partExpectationCustom
         ->
            GroupMorph
                group
                groupNarrowFurther
                partExpectationCustom
        )
part ( accessPart, partTag ) partMorph =
    \groupMorphSoFar ->
        { narrow =
            \groupBroad ->
                let
                    wholeAssemblyResult =
                        groupBroad |> groupMorphSoFar.narrow

                    expectationsSoFar =
                        case wholeAssemblyResult of
                            Ok _ ->
                                []

                            Err (Morph.Expected expectations) ->
                                expectations
                in
                case groupBroad |> List.filter (\(Tagged tag _) -> tag == partTag) |> List.head of
                    Just (Tagged _ partBroad) ->
                        case partBroad |> narrow partMorph of
                            Ok partNarrow ->
                                wholeAssemblyResult
                                    |> Result.map
                                        (\eat -> eat partNarrow)

                            Err (Morph.Expected innerExpectation) ->
                                Morph.Expected
                                    (expectationsSoFar
                                        |> (::)
                                            (Tagged partTag
                                                (PartValue (innerExpectation |> ExpectationIn))
                                            )
                                    )
                                    |> Err

                    Nothing ->
                        Morph.Expected
                            (expectationsSoFar
                                |> (::) (Tagged partTag PartExisting)
                            )
                            |> Err
        , broaden =
            \wholeNarrow ->
                let
                    partBroad =
                        wholeNarrow
                            |> accessPart
                            |> broaden partMorph
                in
                wholeNarrow
                    |> groupMorphSoFar.broaden
                    |> (::) (Tagged partTag partBroad)
        }


{-| Finish the [`group`](#group) |> [`part`](#part) chain.
-}
groupFinish :
    GroupMorph group group expectationCustom
    -> Morph group ValueAny (Error expectationCustom)
groupFinish =
    \groupMorphComplete ->
        { narrow =
            groupNarrowFinish groupMorphComplete.narrow
        , broaden =
            groupMorphComplete.broaden
                >> Recordy
                >> StructureAnyIn
                >> Structurey
        }


groupNarrowFinish :
    (RecordOfValueAny
     ->
        Result
            (Morph.Expected
                (RecordInsideExpectation (ExpectationIn expectationCustom))
            )
            value
    )
    ->
        (ValueAny
         -> Result (Error expectationCustom) value
        )
groupNarrowFinish groupNarrowComplete =
    \broad ->
        case broad of
            Structurey (StructureAnyIn (Recordy fields)) ->
                fields
                    |> groupNarrowComplete
                    |> Result.mapError
                        (Morph.errorExpectationMap
                            (\recordExpectation ->
                                recordExpectation
                                    |> Inside
                                    |> Recordy
                                    |> Structurey
                                    |> Default
                            )
                        )

            _ ->
                Morph.Expected
                    (Kind |> Recordy |> Structurey |> Default)
                    |> Err



--


{-| `Bool` [`Morph`](Morph#Morph).
-}
bool : Morph Bool ValueAny (Error expectationCustom_)
bool =
    choice
        (\true false boolVariantChoiceIsTrue ->
            if boolVariantChoiceIsTrue then
                true ()

            else
                false ()
        )
        |> variant ( \() -> True, "True" ) unit
        |> variant ( \() -> False, "False" ) unit
        |> choiceIn moduleBasicsOrigin


{-| `Basics` [`ModuleOrigin`](#ModuleOrigin)
-}
moduleBasicsOrigin : ModuleOrigin
moduleBasicsOrigin =
    ModuleOrigin repositoryElmCoreOrigin "Basics"


{-| `Maybe` [`Morph`](Morph#Morph).
-}
maybe :
    Morph element ValueAny (Error expectationCustom)
    -> Morph (Maybe element) ValueAny (Error expectationCustom)
maybe contentMorph =
    choice
        (\just nothing narrowMaybe ->
            case narrowMaybe of
                Nothing ->
                    nothing ()

                Just content ->
                    content |> just
        )
        |> variant ( Just, "Just" ) contentMorph
        |> variant ( \() -> Nothing, "Nothing" ) unit
        |> choiceIn moduleMaybeOrigin


{-| `Maybe` [`ModuleOrigin`](#ModuleOrigin)
-}
moduleMaybeOrigin : ModuleOrigin
moduleMaybeOrigin =
    ModuleOrigin repositoryElmCoreOrigin "Maybe"


{-| `Result` [`Morph`](Morph#Morph).
-}
result :
    { ok : Morph okValue ValueAny (Error expectationCustom)
    , err : Morph error ValueAny (Error expectationCustom)
    }
    -> Morph (Result error okValue) ValueAny (Error expectationCustom)
result caseMorphs =
    choice
        (\ok err narrowResult ->
            case narrowResult of
                Ok value ->
                    value |> ok

                Err error ->
                    error |> err
        )
        |> variant ( Ok, "Ok" ) caseMorphs.ok
        |> variant ( Err, "Err" ) caseMorphs.err
        |> choiceIn moduleResultOrigin


{-| `Result` [`ModuleOrigin`](#ModuleOrigin)
-}
moduleResultOrigin : ModuleOrigin
moduleResultOrigin =
    ModuleOrigin repositoryElmCoreOrigin "Result"


{-| `Set` [`Morph`](Morph#Morph).
-}
set :
    Morph comparableElement ValueAny (Error expectationCustom)
    -> Morph (Set comparableElement) ValueAny (Error expectationCustom)
set elementMorph =
    let
        setListMorph =
            Morph.translate Set.toList Set.fromList
                |> Morph.over
                    (list elementMorph)
    in
    choice
        (\setVariant setNarrow -> setVariant setNarrow)
        |> variant ( identity, "Set" ) setListMorph
        |> choiceIn moduleSetOrigin


{-| `Set` [`ModuleOrigin`](#ModuleOrigin)
-}
moduleSetOrigin : ModuleOrigin
moduleSetOrigin =
    ModuleOrigin repositoryElmCoreOrigin "Set"


{-| `Dict` [`Morph`](Morph#Morph).
-}
dict :
    { key : Morph comparableKey ValueAny (Error expectationCustom)
    , value : Morph value ValueAny (Error expectationCustom)
    }
    -> Morph (Dict comparableKey value) ValueAny (Error expectationCustom)
dict nodeMorph =
    let
        dictListMorph =
            Morph.translate Dict.toList Dict.fromList
                |> Morph.over
                    (list
                        (( nodeMorph.key, nodeMorph.value )
                            |> tuple2
                        )
                    )
    in
    choice
        (\dictVariant dictNarrow -> dictVariant dictNarrow)
        |> variant ( identity, "Dict" ) dictListMorph
        |> choiceIn moduleDictOrigin


{-| `Dict` [`ModuleOrigin`](#ModuleOrigin)
-}
moduleDictOrigin : ModuleOrigin
moduleDictOrigin =
    ModuleOrigin repositoryElmCoreOrigin "Dict"
