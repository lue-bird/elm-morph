module Value exposing
    ( LiteralOrStructure(..), Literal(..), Tagged(..), LiteralAny
    , DescriptiveAny, StructureDescriptiveAny(..), RecordAny
    , StructureCompactAny(..), CompactAny
    , unit, char, int, float, posix
    , tuple2, tuple3
    , bool, maybe, result, list, string, array, set, dict
    , record, field, recordFinish, RecordMorph
    , choice, variant, choiceFinish, ChoiceMorph
    , CompactOrDescriptive(..), Morph
    )

{-| generic, `case`-able elm value

json encoders/decoders are too low-level for serialization,
explicitly describing how to serialize individual data types that all have the same shape.

Plus it makes it harder to switch to a different format.


### prior art

  - [`bundsol/`: `Boxed`](https://package.elm-lang.org/packages/bundsol/boxed/2.0.0/Boxed)
      - 👎 no box-unbox conversion pairs
  - [`tricycle/elm-storage`: `Storage.Value`](https://dark.elm.dmy.fr/packages/tricycle/elm-storage/latest/Storage-Value)
      - 👎 doesn't expose the `Value` variants
  - [`andre-dietrich/elm-generic`](https://dark.elm.dmy.fr/packages/andre-dietrich/elm-generic/latest/Generic)
      - 👍 multiple broad results: json, xml, yaml
      - 👎 no encode-decode conversion pairs
  - [`the-sett/decode-generic`](https://dark.elm.dmy.fr/packages/the-sett/decode-generic/latest/Json-Decode-Generic)
      - 👎 no encode (so no encode-decode conversion pairs as well)
  - [`miniBill/elm-codec`](https://dark.elm.dmy.fr/packages/miniBill/elm-codec/latest/Codec)
  - [`MartinSStewart/elm-serialize`](https://dark.elm.dmy.fr/packages/MartinSStewart/elm-serialize/latest/)
      - 👍 multiple broad results: json, string (url safe), `Bytes`
      - 👍 custom errors
      - doesn't encode field & variant names
          - 👎 hard to debug
          - 👎 easy to corrupt
          - 👍 little space
  - [`fujiy/elm-json-convert`](https://dark.elm.dmy.fr/packages/fujiy/elm-json-convert/latest/Json-Convert)
      - 👎 no variant converters
  - [`prozacchiwawa/elm-json-codec`](https://dark.elm.dmy.fr/packages/prozacchiwawa/elm-json-codec/latest/JsonCodec)
      - 👎 no variant converters

@docs CompactOrDescriptivey

@docs LiteralOrStructure, Literal, Morph.Parts, Tagged, LiteralAny


## descriptive

@docs DescriptiveAny, StructureDescriptiveAny, RecordAny


## compact

@docs StructureCompactAny, CompactAny

TODO: add options (Maybe as customizable `Morph (tagged String Value) Json.Encode.Value ...`):

  - tag
  - descriptive
      - field tag = name
      - variant tag = name
      - →
          - readable by humans
          - readable by other tools
          - debuggable
          - shuffling fields [`Morph`](#Morph) order → no change
          - renaming fields → breaking change
  - compact
      - field tag = [field `Morph`](Value#field) index
      - variant tag = [variant `Morph`](Value#variant) index


## morph

@docs unit, char, int, float, posix
@docs tuple2, tuple3
@docs bool, maybe, result, list, string, array, set, dict
@docs record, field, recordFinish, RecordMorph
@docs choice, variant, choiceFinish, ChoiceMorph


### morphs on common formats

  - [`Json`](Json)

If you feel especially motivated, throw a PR adding

  - `Morph ValueAny Json.Encode.Value ...`
  - `Morph ValueAny String ...`
  - `Morph ValueAny Bytes ...`
  - `Morph ValueAny Yaml ...` after [`MaybeJustJames/yaml`](https://github.com/MaybeJustJames/yaml/blob/2.1.1/src/Yaml/Parser.elm)
  - `Morph ValueAny Xml ...`
  - `Morph ValueAny Csv ...`
  - ...


## expectations

@docs Error
@docs DefaultOrCustom
@docs LiteralKind
@docs StructureExpectation, Tuple2Expectation, Tuple3Expectation, KindOrInsideExpectation, StructureLinearInsideExpectation, RecordInsideExpectation, TagOrValue, PartExpectation, TagExpectation, TaggedInsideExpectation

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Dict.Morph
import Emptiable exposing (Emptiable)
import Morph exposing (Morph, MorphIndependently, MorphOrError, NoTry, broadenWith, in_, narrowWith, to)
import Possibly exposing (Possibly(..))
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Set exposing (Set)
import Set.Morph
import Stack exposing (Stacked)
import Time exposing (Posix)


{-| elm structure that can itself contain values
-}
type Structure tuple2 tuple3 list array record variant
    = Tuple2 tuple2
    | Tuple3 tuple3
    | List list
    | Array array
    | Record record
    | Variant variant


{-| elm literal that don't itself contain values
-}
type Literal unit char int float string posix
    = Unit unit
    | Char char
    | Int int
    | Float float
    | String string
    | Posix posix


{-| elm value. Either

  - a literal that don't itself contain values
  - a structure that can itself contain values

-}
type LiteralOrStructure literal structure
    = Literal literal
    | Structure structure


{-| tag-value pair like a field or a variant
-}
type Tagged tag value
    = Tagged tag value


{-| Any value representable in elm.

Is there a structure you'd like to add that can't be converted in a small amount of time? PR!

-}
type alias DescriptiveAny =
    LiteralOrStructure LiteralAny StructureDescriptiveAny


{-| A supported elm literal (that don't contain other [values](#ValueAny))
-}
type alias LiteralAny =
    Literal () Char Int Float String Posix


{-| A structure that can itself contain further values
-}
type StructureDescriptiveAny
    = StructureAny
        (Structure
            ( DescriptiveAny, DescriptiveAny )
            ( DescriptiveAny, DescriptiveAny, DescriptiveAny )
            (List DescriptiveAny)
            (Array DescriptiveAny)
            RecordAny
            (Tagged String DescriptiveAny)
        )


{-| The structure of a record that can hold [any](#ValueAny) field value
-}
type alias RecordAny =
    List (Tagged String DescriptiveAny)


type CompactOrDescriptive compacty descriptivey
    = Compacty compacty
    | Descriptivey descriptivey


type alias CompactAny =
    LiteralOrStructure LiteralAny StructureCompactAny


{-| A structure that can itself contain further values
-}
type StructureCompactAny
    = StructureIndexedAny
        (Structure
            ( DescriptiveAny, DescriptiveAny )
            ( DescriptiveAny, DescriptiveAny, DescriptiveAny )
            (List DescriptiveAny)
            (Array DescriptiveAny)
            Never
            (Tagged Int DescriptiveAny)
        )



-- morph


{-| [`Morph`](Morph#Morph) between a given narrow value
and a generic value [`CompactOrDescriptive`](#CompactOrDescriptive)[`CompactAny`](#DescriptiveAny)[`DescriptiveAny`](#DescriptiveAny)
-}
type alias Morph narrow =
    Morph.MorphIndependently
        (LiteralOrStructure
            LiteralAny
            (CompactOrDescriptive StructureCompactAny StructureDescriptiveAny)
         -> Result Morph.Error narrow
        )
        (narrow -> DescriptiveAny)



-- literal


literal : Morph LiteralAny
literal =
    Morph.value "Literal"
        { broaden = Literal
        , narrow =
            \valueAny ->
                case valueAny of
                    Literal literalAny ->
                        literalAny |> Ok

                    Structure _ ->
                        "Structure" |> Err
        }


literalKindToString : Literal unit_ char_ int_ float_ string_ posix_ -> String
literalKindToString =
    \literal_ ->
        case literal_ of
            Unit _ ->
                "Unit"

            Char _ ->
                "Char"

            Int _ ->
                "Int"

            Float _ ->
                "Float"

            String _ ->
                "String"

            Posix _ ->
                "Posix"


{-| `()` [`Morph`](#Morph)
-}
unit : Morph ()
unit =
    Morph.value "Unit"
        { broaden = Unit
        , narrow =
            \valueAny ->
                case valueAny of
                    Unit unitValue ->
                        unitValue |> Ok

                    literalExceptUnit ->
                        literalExceptUnit |> literalKindToString |> Err
        }
        |> Morph.over literal


{-| `Char` [`Morph`](#Morph)
-}
char : Morph Char
char =
    Morph.value "Char"
        { broaden = Char
        , narrow =
            \valueAny ->
                case valueAny of
                    Char charValue ->
                        charValue |> Ok

                    literalExceptChar ->
                        literalExceptChar |> literalKindToString |> Err
        }
        |> Morph.over literal


{-| `Int` [`Morph`](#Morph)
-}
int : Morph Int
int =
    Morph.value "Int"
        { broaden = Int
        , narrow =
            \valueAny ->
                case valueAny of
                    Int intValue ->
                        intValue |> Ok

                    literalExceptInt ->
                        literalExceptInt |> literalKindToString |> Err
        }
        |> Morph.over literal


{-| `Float` [`Morph`](#Morph)
-}
float : Morph Float
float =
    Morph.value "Float"
        { broaden = Float
        , narrow =
            \valueAny ->
                case valueAny of
                    Float floatValue ->
                        floatValue |> Ok

                    literalExceptFloat ->
                        literalExceptFloat |> literalKindToString |> Err
        }
        |> Morph.over literal


{-| `String` [`Morph`](#Morph)
-}
string : Morph String
string =
    Morph.value "String"
        { broaden = String
        , narrow =
            \valueAny ->
                case valueAny of
                    String stringNarrow ->
                        stringNarrow |> Ok

                    literalExceptString ->
                        literalExceptString |> literalKindToString |> Err
        }
        |> Morph.over literal


{-| `Posix` [`Morph`](#Morph)
-}
posix : Morph Posix
posix =
    Morph.value "Posix"
        { broaden = Posix
        , narrow =
            \valueAny ->
                case valueAny of
                    Posix posixNarrow ->
                        posixNarrow |> Ok

                    literalExceptUnit ->
                        literalExceptUnit |> literalKindToString |> Err
        }
        |> Morph.over literal



-- structure


structure :
    MorphIndependently
        (LiteralOrStructure narrowLiteral_ narrowStructure
         -> Result Morph.Error narrowStructure
        )
        (broadStructure
         -> LiteralOrStructure broadLiteral_ broadStructure
        )
structure =
    Morph.value "Structure"
        { broaden = Structure
        , narrow =
            \valueAny ->
                case valueAny of
                    Structure structure_ ->
                        structure_ |> Ok

                    Literal _ ->
                        "Literal" |> Err
        }


structureKindToString :
    Structure tuple2_ tuple3_ list_ array_ record_ variant_
    -> String
structureKindToString =
    \structure_ ->
        case structure_ of
            Tuple2 _ ->
                "Tuple2"

            Tuple3 _ ->
                "Tuple3"

            List _ ->
                "List"

            Array _ ->
                "Array"

            Record _ ->
                "Record"

            Variant _ ->
                "Variant"


{-| `( ..., ... )` [`Morph`](#Morph)
-}
tuple2 :
    ( Morph part0
    , Morph part1
    )
    -> Morph ( part0, part1 )
tuple2 partMorphs =
    let
        ( part0Morph, part1Morph ) =
            partMorphs
    in
    Morph.value "Tuple2"
        { broaden =
            \( part0, part1 ) ->
                ( part0 |> broadenWith part0Morph
                , part1 |> broadenWith part1Morph
                )
                    |> Tuple2
        , narrow =
            \valueAny ->
                case valueAny of
                    Tuple2 ( part0, part1 ) ->
                        case
                            ( part0 |> narrowWith part0Morph
                            , part1 |> narrowWith part1Morph
                            )
                        of
                            ( Ok part0Ok, Ok part1Ok ) ->
                                ( part0Ok, part1Ok ) |> Ok

                            ( Err part0Error, part1Narrow ) ->
                                Stack.only
                                    { location = 0
                                    , error = part0Error
                                    }
                                    |> (case part1Narrow of
                                            Ok _ ->
                                                identity

                                            Err part1Error ->
                                                Stack.onTopLay
                                                    { location = 1
                                                    , error = part1Error
                                                    }
                                       )
                                    |> Morph.Parts
                                    |> Err

                            ( Ok _, Err part1Error ) ->
                                Stack.only
                                    { location = 1
                                    , error = part1Error
                                    }
                                    |> Morph.Parts
                                    |> Err

                    structureExceptTuple2 ->
                        structureExceptTuple2 |> structureKindToString
        }
        |> Morph.over structure


{-| `( ..., ..., ... )` [`Morph`](#Morph)
-}
tuple3 :
    ( Morph part0
    , Morph part1
    , Morph part2
    )
    -> Morph ( part0, part1, part2 )
tuple3 partMorphs =
    let
        ( part0Morph, part1Morph, part2Morph ) =
            partMorphs
    in
    Morph.value "Tuple3"
        { broaden =
            \( part0, part1, part2 ) ->
                ( part0 |> (part0Morph |> broadenWith)
                , part1 |> (part1Morph |> broadenWith)
                , part2 |> (part2Morph |> broadenWith)
                )
                    |> Tuple3
        , narrow =
            \valueAny ->
                case valueAny of
                    Tuple3 ( part0, part1, part2 ) ->
                        case
                            ( part0 |> narrowWith part0Morph
                            , part1 |> narrowWith part1Morph
                            , part2 |> narrowWith part2Morph
                            )
                        of
                            ( Ok part0Ok, Ok part1Ok, Ok part2Ok ) ->
                                ( part0Ok, part1Ok, part2Ok ) |> Ok

                            ( Err part0Error, part1Narrow, part2Narrow ) ->
                                Stack.only
                                    { location = 0
                                    , error = part0Error
                                    }
                                    |> (case part1Narrow of
                                            Ok _ ->
                                                identity

                                            Err part1Error ->
                                                Stack.onTopLay
                                                    { location = 1
                                                    , error = part1Error
                                                    }
                                       )
                                    |> (case part2Narrow of
                                            Ok _ ->
                                                identity

                                            Err part2Error ->
                                                Stack.onTopLay
                                                    { location = 2
                                                    , error = part2Error
                                                    }
                                       )
                                    |> Morph.Parts
                                    |> Err

                            ( Ok _, Err part1Error, part2Narrow ) ->
                                Stack.only
                                    { location = 1
                                    , error = part1Error
                                    }
                                    |> (case part2Narrow of
                                            Ok _ ->
                                                identity

                                            Err part2Error ->
                                                Stack.onTopLay
                                                    { location = 2
                                                    , error = part2Error
                                                    }
                                       )
                                    |> Morph.Parts
                                    |> Err

                            ( Ok _, Ok _, Err part2Error ) ->
                                Stack.only
                                    { location = 2
                                    , error = part2Error
                                    }
                                    |> Morph.Parts
                                    |> Err

                    structureExceptTODO ->
                        structureExceptTODO |> structureKindToString |> Morph.DeadEnd |> Err
        }
        |> Morph.over structure


{-| `List` [`Morph`](#Morph)
-}
list : Morph element -> Morph (List element)
list elementMorph =
    structure "List"
        { narrow =
            \broad ->
                case broad of
                    List listOfElementsAny ->
                        listOfElementsAny
                            |> List.map (narrowWith elementMorph)
                            |> listResultsToValuesOrErrors
                            |> Result.mapError Morph.Parts

                    structureExceptTODO ->
                        structureExceptTODO |> structureKindToString |> Morph.DeadEnd |> Err
        , broaden =
            \listNarrow ->
                listNarrow
                    |> List.map (elementMorph |> broadenWith)
                    |> List
        }


{-| `Ok` if all values in sequence are `Ok`,
else `Err` with information on at what indexes elements were `Err`
-}
listResultsToValuesOrErrors :
    List (Result error value)
    ->
        Result
            (Emptiable
                (Stacked { location : Int, error : error })
                Never
            )
            (List value)
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
                                (case collected of
                                    Ok _ ->
                                        Emptiable.empty

                                    Err elementsAtIndexes ->
                                        elementsAtIndexes |> Emptiable.emptyAdapt (\_ -> Possible)
                                )
                                    |> Stack.onTopLay
                                        { location = index
                                        , error = elementError
                                        }
                                    |> Err
                    , index = index - 1
                    }
                )
                { collected = [] |> Ok
                , index = (results |> List.length) - 1
                }
            |> .collected


{-| `Array` [`Morph`](#Morph)
-}
array : Morph element -> Morph (Array element)
array elementMorph =
    structure "Array"
        { narrow =
            \broad ->
                case broad of
                    Array arrayOfElementsAny ->
                        case
                            arrayOfElementsAny
                                |> Array.toList
                                |> List.map (elementMorph |> narrowWith)
                                |> listResultsToValuesOrErrors
                        of
                            Ok list_ ->
                                list_ |> Array.fromList |> Ok

                            Err arrayInsideExpectation ->
                                arrayInsideExpectation
                                    |> Morph.Parts
                                    |> Err

                    structureExceptTODO ->
                        structureExceptTODO |> structureKindToString |> Morph.DeadEnd |> Err
        , broaden =
            \arrayNarrow ->
                arrayNarrow
                    |> Array.map (elementMorph |> broadenWith)
                    |> Array
        }



--


{-| Incomplete variant union [`Morph`](#Morph) to [`ValueAny`](#ValueAny)

  - starting from [`choice`](#choice)
  - over [`variant`](#variant)
  - and completed with [`choiceFinish`](#choiceFinish)

-}
type ChoiceMorph choiceNarrow choiceBroadenFurther noStepTag_ noTryPossiblyOrNever
    = ChoiceMorphInProgress
        (MorphIndependently
            (Tagged String DescriptiveAny
             ->
                Result
                    { possibilities :
                        Emptiable
                            (Stacked (Tagged String (TagOrValue () Morph.Error)))
                            noTryPossiblyOrNever
                    }
                    choiceNarrow
            )
            choiceBroadenFurther
        )


type TagOrValue tag value
    = Tag tag
    | Value value


{-| Discriminate into [variants](#variant)
-}
choice :
    choiceBroadenByVariant
    -> ChoiceMorph choiceNarrow_ choiceBroadenByVariant NoTry Possibly
choice choiceBroadenDiscriminatedByPossibility =
    { narrow =
        \_ ->
            { possibilities = Emptiable.empty } |> Err
    , broaden = choiceBroadenDiscriminatedByPossibility
    }
        |> ChoiceMorphInProgress


{-| Describe another variant value [`Morph`](#Morph) to [`ValueAny`](#ValueAny)
-}
variant :
    ( possibilityNarrow -> choiceNarrow
    , String
    )
    -> Morph possibilityNarrow
    ->
        (ChoiceMorph
            choiceNarrow
            ((possibilityNarrow -> Tagged String DescriptiveAny)
             -> choiceBroadenFurther
            )
            NoTry
            noTryPossiblyOrNever_
         ->
            ChoiceMorph
                choiceNarrow
                choiceBroadenFurther
                NoTry
                noTryNever_
        )
variant ( possibilityToChoice, possibilityTag ) possibilityMorph =
    \(ChoiceMorphInProgress choiceMorphSoFar) ->
        { narrow =
            variantStepNarrow
                ( possibilityToChoice, possibilityTag, narrowWith possibilityMorph )
                (narrowWith choiceMorphSoFar)
        , broaden =
            (broadenWith possibilityMorph
                >> Tagged possibilityTag
            )
                |> broadenWith choiceMorphSoFar
        }
            |> ChoiceMorphInProgress


variantStepNarrow :
    ( possibilityNarrow -> narrowChoice
    , String
    , possibilityBroad
      -> Result Morph.Error possibilityNarrow
    )
    ->
        (Tagged String possibilityBroad
         ->
            Result
                (TagOrValue
                    { possibilities : List String }
                    (Tagged String Morph.Error)
                )
                narrowChoice
        )
    ->
        (Tagged String possibilityBroad
         ->
            Result
                (TagOrValue
                    { possibilities : List String }
                    (Tagged String Morph.Error)
                )
                narrowChoice
        )
variantStepNarrow ( possibilityToChoice, variantTag, possibilityNarrow ) =
    \choiceNarrowSoFar ->
        \variantBroad ->
            case variantBroad |> choiceNarrowSoFar of
                Ok variantNarrow ->
                    variantNarrow |> Ok

                Err earlierStepsExpectation ->
                    let
                        (Tagged tag valueBroad) =
                            variantBroad
                    in
                    if tag == variantTag then
                        case valueBroad |> possibilityNarrow of
                            Ok ok ->
                                ok |> possibilityToChoice |> Ok

                            Err valueExpectation ->
                                valueExpectation
                                    |> Tagged variantTag
                                    |> Value
                                    |> Err

                    else
                        let
                            tagsTriedSoFar =
                                case earlierStepsExpectation of
                                    Value _ ->
                                        []

                                    Tag tagExpectation ->
                                        tagExpectation.possibilities
                        in
                        Tag { possibilities = tagsTriedSoFar |> (::) variantTag }
                            |> Err


{-| Conclude a [`Morph.choice`](Morph#choice) |> [`Morph.try`](Morph#try) chain
-}
choiceFinish :
    ChoiceMorph
        choiceNarrow
        (choiceNarrow -> Tagged String DescriptiveAny)
        NoTry
        Never
    -> Morph choiceNarrow
choiceFinish =
    \choiceMorphComplete ->
        Morph.to "variant"
            { narrow =
                \value ->
                    case value of
                        Structure (StructureAny (Variant variant_)) ->
                            variant_
                                |> narrowWith choiceMorphComplete
                                |> Result.mapError
                                    (\error ->
                                        error
                                            |> Morph.Parts
                                    )

                        _ ->
                            ("kind" |> Morph.DeadEnd)
                                |> Err
            , broaden =
                \narrowChoice ->
                    let
                        (Tagged tag valueBroad) =
                            narrowChoice |> broadenWith choiceMorphComplete
                    in
                    valueBroad
                        |> Tagged tag
                        |> Variant
                        |> StructureAny
                        |> Structure
            }


{-| Start a group assembly [`Morph`](#Morph) to [`ValueAny`](#ValueAny).

  - continue with [`part`](#part)
  - finish with [`groupFinish`](#groupFinish)

-}
record :
    groupNarrowAssemble
    ->
        MorphIndependently
            (RecordAny
             ->
                Result
                    (Emptiable
                        (Tagged String (TagOrValue () Morph.Error))
                        Never
                    )
                    groupNarrowAssemble
            )
            (groupNarrow_ -> RecordAny)
record groupNarrowAssemble =
    Morph.group ( groupNarrowAssemble, [] )


{-| possibly incomplete [`Morph`] step from and to a group
-}
type alias RecordMorph groupNarrow groupNarrowFurther =
    MorphIndependently
        (RecordAny
         ->
            Result
                { expected :
                    Emptiable
                        (Stacked
                            { location : String
                            , error : Morph.Error
                            }
                        )
                        Never
                }
                groupNarrowFurther
        )
        (groupNarrow -> RecordAny)


{-| Continue a group assembly [`Morph`](#Morph) to [`ValueAny`](#ValueAny).

  - finish with [`groupFinish`](#groupFinish)

-}
field :
    ( group -> partNarrow
    , String
    )
    -> Morph partNarrow
    ->
        (RecordMorph
            group
            (partNarrow -> groupNarrowFurther)
         -> RecordMorph group groupNarrowFurther
        )
field ( accessPart, partTag ) partMorph =
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

                            Err expectations ->
                                expectations
                in
                case groupBroad |> List.filter (\(Tagged tag _) -> tag == partTag) |> List.head of
                    Just (Tagged _ partBroad) ->
                        case partBroad |> narrowWith partMorph of
                            Ok partNarrow ->
                                wholeAssemblyResult
                                    |> Result.map
                                        (\eat -> eat partNarrow)

                            Err innerExpectation ->
                                (expectationsSoFar
                                    |> (::)
                                        (Tagged partTag
                                            (Value innerExpectation)
                                        )
                                )
                                    |> Err

                    Nothing ->
                        (expectationsSoFar
                            |> (::) (Tagged partTag (Tag ()))
                        )
                            |> Err
        , broaden =
            \wholeNarrow ->
                let
                    partBroad =
                        wholeNarrow
                            |> accessPart
                            |> broadenWith partMorph
                in
                wholeNarrow
                    |> groupMorphSoFar.broaden
                    |> (::) (Tagged partTag partBroad)
        }


{-| Finish the [`group`](#group) |> [`part`](#part) chain
-}
recordFinish :
    RecordMorph group group
    -> Morph group
recordFinish =
    \groupMorphComplete ->
        { narrow =
            recordNarrowFinish groupMorphComplete.narrow
        , broaden =
            groupMorphComplete.broaden
                >> Record
                >> StructureAny
                >> Structure
        }


recordNarrowFinish :
    (RecordAny
     ->
        Result
            { expected :
                Emptiable
                    (Stacked { location : Int, error : Morph.Error })
                    Never
            }
            value
    )
    ->
        (DescriptiveAny
         -> Result value Morph.Error
        )
recordNarrowFinish groupNarrowComplete =
    \broad ->
        case broad of
            Structure (StructureAny (Record fields)) ->
                fields
                    |> groupNarrowComplete
                    |> Result.mapError
                        (\recordExpectation ->
                            recordExpectation.expected
                                |> Morph.Parts
                        )

            _ ->
                ("kind" |> Morph.DeadEnd)
                    |> Err



--


{-| `Bool` [`Morph`](#Morph)
-}
bool : Morph Bool
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
        |> choiceFinish


{-| `Maybe` [`Morph`](#Morph)
-}
maybe : Morph element -> Morph (Maybe element)
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
        |> choiceFinish


{-| `Result` [`Morph`](#Morph)
-}
result :
    { ok : Morph okValue
    , err : Morph error
    }
    -> Morph (Result error okValue)
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
        |> choiceFinish


{-| `Set` [`Morph`](#Morph)
-}
set :
    Morph comparableElement
    -> Morph (Set comparableElement)
set elementMorph =
    let
        setListMorph =
            Set.Morph.list
                |> Morph.over (list elementMorph)
    in
    choice
        (\setVariant setNarrow -> setVariant setNarrow)
        |> variant ( identity, "Set" ) setListMorph
        |> choiceFinish


{-| `Dict` [`Morph`](#Morph)
-}
dict :
    { key : Morph comparableKey
    , value : Morph value
    }
    -> Morph (Dict comparableKey value)
dict entryMorph =
    choice
        (\dictVariant dictNarrow -> dictVariant dictNarrow)
        |> variant ( identity, "Dict" )
            (Dict.Morph.list
                |> Morph.over (list (keyValue entryMorph))
            )
        |> choiceFinish


keyValue :
    { key : Morph comparableKey
    , value : Morph value
    }
    -> Morph { key : comparableKey, value : value }
keyValue entryMorph =
    record (\key value -> { key = key, value = value })
        |> field ( .key, "key" ) entryMorph.key
        |> field ( .value, "value" ) entryMorph.value
        |> recordFinish
