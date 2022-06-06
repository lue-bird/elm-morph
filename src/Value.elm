module Value exposing
    ( ValueAny, LiteralAny, StructureAny, RecordOfValueAny
    , Valuey(..), Literaly(..), Structurey(..)
    , Origin(..), ModuleOrigin, RepositoryOrigin(..)
    , repositoryElmCoreOrigin
    , unit, char, int, float, posix
    , tuple2, tuple3
    , bool, maybe, result, list, string, array, set, dict
    , intersection, unionIn
    , Error, expectationCustomMap
    , DefaultOrCustom(..)
    , Expectation, ExpectationIn(..)
    , LiteralKind
    , StructureExpectation, Tuple2Expectation, Tuple3Expectation, KindOrInsideExpectation, StructureLinearInsideExpectation, RecordInsideExpectation, VariantTagExpectation(..), VariantInsideExpectation
    )

{-| elm values as a `case`-able union.

@docs ValueAny, LiteralAny, StructureAny, RecordOfValueAny
@docs Valuey, Literaly, Structurey


### origins

@docs Origin, ModuleOrigin, RepositoryOrigin
@docs repositoryElmCoreOrigin


## conversion

@docs unit, char, int, float, posix
@docs tuple2, tuple3
@docs bool, maybe, result, list, string, array, set, dict
@docs intersection, unionIn


### conversions on common formats

  - [`Json`](Json)

If you feel especially motivated, throw a PR adding

  - `Conversion ValueAny Json.Encode.Value ...`
  - `Conversion ValueAny String ...`
  - `Conversion ValueAny Bytes ...`
  - `Conversion ValueAny Yaml ...`
  - `Conversion ValueAny Xml ...`
  - `Conversion ValueAny Csv ...`
  - ...


## expectations

@docs Error, expectationCustomMap
@docs DefaultOrCustom
@docs Expectation, ExpectationIn
@docs LiteralKind
@docs StructureExpectation, Tuple2Expectation, Tuple3Expectation, KindOrInsideExpectation, StructureLinearInsideExpectation, RecordInsideExpectation, VariantTagExpectation, VariantInsideExpectation

-}

import Array exposing (Array)
import Conversion exposing (Conversion, TagOrValue(..), Tagged(..), UnionConversionStep, broaden, narrow)
import Dict exposing (Dict)
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


{-| TODO:

  - readd `custom` to preserve functions, opaque data, etc.
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
    Dict String ValueAny



-- conversion


{-| Information on what went wrong while `narrow`ing from a [`ValueAny`](#ValueAny).
-}
type alias Error expectationCustom =
    Conversion.Error (Expectation expectationCustom)


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


{-| Wraps failed part expectations nested inside a [structure](#StructureExpectation).
-}
type ExpectationIn expectationCustom
    = ExpectationIn (Expectation expectationCustom)


{-| TODO: `Default` is a pretty meh name. Considered alternatives: structural?, basic, (usual)
-}
type DefaultOrCustom default custom
    = Default default
    | Custom custom


{-| A supported elm literal kind (that don't contain other [values](#ValueAny))
-}
type alias LiteralKind =
    Literaly () () () () () ()


{-| Failed expectation for the expected [structure](#StructureAny)
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

If all 2 parts are `Ok ()`, a 2-tuple was expected but something else was found!

This expectation could be represented as a union.
However, a tuple is a lot easier to work with;
you don't lose type information either way :)

-}
type alias Tuple2Expectation expectationAtPart =
    ( Result expectationAtPart ()
    , Result expectationAtPart ()
    )


{-| For each part: Was it `Ok ()` or was there an `Err ...`?

If all 3 parts are `Ok ()`, a 3-tuple was expected but something else was found!

This expectation could be represented as a union.
However, a tuple is a lot easier to work with;
you don't lose type information either way :)

-}
type alias Tuple3Expectation expectationAtPart =
    ( Result expectationAtPart ()
    , Result expectationAtPart ()
    , Result expectationAtPart ()
    )


{-| Failed expectation for the expected elements at specific indexes.
-}
type alias StructureLinearInsideExpectation atElement =
    RecordWithoutConstructorFunction
        { elementsAtIndexes : Dict Int atElement }


{-| Failed expectation for the expected record field tags or values.
-}
type alias RecordInsideExpectation atPart =
    RecordWithoutConstructorFunction
        { fieldValues : Dict String atPart
        , fieldsAdditional : Set String
        }


{-| Failed expectation for the expected variant tag or value.
-}
type alias VariantInsideExpectation atValueExpectation =
    TagOrValue VariantTagExpectation atValueExpectation


{-| Failed expectation for the expected variant tag.
-}
type VariantTagExpectation
    = VariantTagFromModule ModuleOrigin
    | VariantTagOneOf (Set String)


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
                { fieldValues =
                    recordExpectation.fieldValues
                        |> Dict.map (\_ -> step)
                , fieldsAdditional =
                    recordExpectation.fieldsAdditional
                }

        expectationVariantInsideMap :
            TagOrValue VariantTagExpectation (ExpectationIn expectationCustom)
            -> TagOrValue VariantTagExpectation (ExpectationIn expectationCustomMapped)
        expectationVariantInsideMap =
            \variantExpectation ->
                case variantExpectation of
                    Tag tagExpectation ->
                        Tag tagExpectation

                    Value variantValue ->
                        Value (variantValue |> step)
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


{-| `ModuleOrigin (RepositoryOrigin userName repositoryName)`
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
    , conversion : Conversion literalSpecific LiteralAny ()
    }
    -> Conversion literalSpecific ValueAny (Error expectationCustom_)
literal { kind, conversion } =
    { broaden =
        \literalSpecific ->
            literalSpecific
                |> (conversion |> broaden)
                |> Literaly
    , narrow =
        \valueAny ->
            let
                narrowLiteral =
                    case valueAny of
                        Literaly literalAny ->
                            literalAny
                                |> (conversion |> narrow)

                        Structurey _ ->
                            Err ()
            in
            narrowLiteral
                |> Result.mapError
                    (\() ->
                        Conversion.Expected
                            (Literaly (kind ()) |> Default)
                    )
    }


{-| `()` [`Conversion`](Conversion#Conversion).
-}
unit : Conversion () ValueAny (Error expectationCustom_)
unit =
    literal
        { kind = Unity
        , conversion =
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


{-| `Char` [`Conversion`](Conversion#Conversion).
-}
char : Conversion Char ValueAny (Error expectationCustom_)
char =
    literal
        { kind = Chary
        , conversion =
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


{-| `Int` [`Conversion`](Conversion#Conversion).
-}
int : Conversion Int ValueAny (Error expectationCustom_)
int =
    literal
        { kind = Inty
        , conversion =
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


{-| `Float` [`Conversion`](Conversion#Conversion).
-}
float : Conversion Float ValueAny (Error expectationCustom_)
float =
    literal
        { kind = Floaty
        , conversion =
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


{-| `String` [`Conversion`](Conversion#Conversion).
-}
string : Conversion String ValueAny (Error expectationCustom_)
string =
    literal
        { kind = Stringy
        , conversion =
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


{-| `Posix` [`Conversion`](Conversion#Conversion).
-}
posix : Conversion Posix ValueAny (Error expectationCustom_)
posix =
    literal
        { kind = Posixy
        , conversion =
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
                (\(Conversion.Expected expectation) ->
                    expectation |> ExpectationIn
                )


{-| `( ..., ... )` [`Conversion`](Conversion#Conversion).
-}
tuple2 :
    ( Conversion part0 ValueAny (Error expectationCustom)
    , Conversion part1 ValueAny (Error expectationCustom)
    )
    -> Conversion ( part0, part1 ) ValueAny (Error expectationCustom)
tuple2 partConversions =
    let
        ( part0Conversion, part1Conversion ) =
            partConversions
    in
    { broaden =
        \( part0, part1 ) ->
            ( part0 |> (part0Conversion |> broaden)
            , part1 |> (part1Conversion |> broaden)
            )
                |> Tuple2y
                |> StructureAnyIn
                |> Structurey
    , narrow =
        \valueAny ->
            case valueAny of
                Structurey (StructureAnyIn (Tuple2y ( part0, part1 ))) ->
                    case
                        ( part0 |> (part0Conversion |> narrow)
                        , part1 |> (part1Conversion |> narrow)
                        )
                    of
                        ( Ok part0Ok, Ok part1Ok ) ->
                            ( part0Ok, part1Ok ) |> Ok

                        ( part0Narrow, part1Narrow ) ->
                            Conversion.Expected
                                (( part0Narrow |> partNarrowExpectation
                                 , part1Narrow |> partNarrowExpectation
                                 )
                                    |> Tuple2y
                                    |> Structurey
                                    |> Default
                                )
                                |> Err

                _ ->
                    Conversion.Expected
                        (Structurey (Tuple2y ( Ok (), Ok () )) |> Default)
                        |> Err
    }


{-| `( ..., ..., ... )` [`Conversion`](Conversion#Conversion).
-}
tuple3 :
    ( Conversion part0 ValueAny (Error expectationCustom)
    , Conversion part1 ValueAny (Error expectationCustom)
    , Conversion part2 ValueAny (Error expectationCustom)
    )
    ->
        Conversion
            ( part0, part1, part2 )
            ValueAny
            (Error expectationCustom)
tuple3 partConversions =
    let
        ( part0Conversion, part1Conversion, part2Conversion ) =
            partConversions
    in
    { broaden =
        \( part0, part1, part2 ) ->
            ( part0 |> (part0Conversion |> broaden)
            , part1 |> (part1Conversion |> broaden)
            , part2 |> (part2Conversion |> broaden)
            )
                |> Tuple3y
                |> StructureAnyIn
                |> Structurey
    , narrow =
        \valueAny ->
            case valueAny of
                Structurey (StructureAnyIn (Tuple3y ( part0, part1, part2 ))) ->
                    case
                        ( part0 |> (part0Conversion |> narrow)
                        , part1 |> (part1Conversion |> narrow)
                        , part2 |> (part2Conversion |> narrow)
                        )
                    of
                        ( Ok part0Ok, Ok part1Ok, Ok part2Ok ) ->
                            ( part0Ok, part1Ok, part2Ok ) |> Ok

                        ( part0Narrow, part1Narrow, part2Narrow ) ->
                            Conversion.Expected
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
                    Conversion.Expected
                        (Structurey (Tuple3y ( Ok (), Ok (), Ok () )) |> Default)
                        |> Err
    }


{-| `List` [`Conversion`](Conversion#Conversion).
-}
list :
    Conversion element ValueAny (Error expectationCustom)
    -> Conversion (List element) ValueAny (Error expectationCustom)
list elementConversion =
    { narrow =
        \broad ->
            case broad of
                Structurey (StructureAnyIn (Listy listOfElementsAny)) ->
                    listOfElementsAny
                        |> List.map (elementConversion |> narrow)
                        |> listResultsToValuesOrErrors
                        |> Result.mapError
                            (\listInsideExpectation ->
                                Conversion.Expected
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
                    Conversion.Expected
                        (Structurey (Listy Kind) |> Default)
                        |> Err
    , broaden =
        \listNarrow ->
            listNarrow
                |> List.map (elementConversion |> broaden)
                |> Listy
                |> StructureAnyIn
                |> Structurey
    }


{-| `Array` [`Conversion`](Conversion#Conversion).
-}
array :
    Conversion element ValueAny (Error expectationCustom)
    -> Conversion (Array element) ValueAny (Error expectationCustom)
array elementConversion =
    { narrow =
        \broad ->
            case broad of
                Structurey (StructureAnyIn (Arrayy arrayOfElementsAny)) ->
                    arrayOfElementsAny
                        |> Array.toList
                        |> List.map (elementConversion |> narrow)
                        |> listResultsToValuesOrErrors
                        |> Result.map Array.fromList
                        |> Result.mapError
                            (\arrayInsideExpectation ->
                                Conversion.Expected
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
                    Conversion.Expected
                        (Structurey (Arrayy Kind) |> Default)
                        |> Err
    , broaden =
        \arrayNarrow ->
            arrayNarrow
                |> Array.map (elementConversion |> broaden)
                |> Arrayy
                |> StructureAnyIn
                |> Structurey
    }



--


{-| Introduce a [`Conversion.union`](Conversion#variantUnion) |> [`Conversion.eatVariant`](Conversion#variantEat) chain.
-}
unionIn :
    ModuleOrigin
    ->
        UnionConversionStep
            narrowUnion
            String
            ValueAny
            (narrowUnion -> Tagged String ValueAny)
            (Expectation expectationCustom)
    ->
        Conversion
            narrowUnion
            ValueAny
            (Error expectationCustom)
unionIn moduleOrigin =
    \conversionStepped ->
        { narrow =
            \value ->
                case value of
                    Structurey (StructureAnyIn (Varianty (Tagged origin variantValueAny))) ->
                        let
                            (Origin variantModuleOrigin variantTag) =
                                origin
                        in
                        if variantModuleOrigin == moduleOrigin then
                            Tagged variantTag variantValueAny
                                |> (conversionStepped |> narrow)
                                |> Result.mapError
                                    (Conversion.errorExpectationMap
                                        (\expectation ->
                                            let
                                                variantInsideExpectation =
                                                    case expectation of
                                                        Tag { oneOf } ->
                                                            VariantTagOneOf (oneOf |> Set.fromList) |> Tag

                                                        Value valueExpectation ->
                                                            valueExpectation |> ExpectationIn |> Value
                                            in
                                            variantInsideExpectation
                                                |> Inside
                                                |> Varianty
                                                |> Structurey
                                                |> Default
                                        )
                                    )

                        else
                            Conversion.Expected
                                (VariantTagFromModule moduleOrigin
                                    |> Tag
                                    |> Inside
                                    |> Varianty
                                    |> Structurey
                                    |> Default
                                )
                                |> Err

                    _ ->
                        Conversion.Expected
                            (Structurey (Varianty Kind) |> Default)
                            |> Err
        , broaden =
            \narrowUnion ->
                let
                    (Tagged tag value) =
                        narrowUnion |> (conversionStepped |> broaden)
                in
                Structurey
                    (Varianty
                        (Tagged (Origin moduleOrigin tag) value)
                        |> StructureAnyIn
                    )
        }


{-| Finish the [`Conversion.intersection`](Conversion#intersection) |> [`Conversion.partEat`](Conversion#partEat) chain.
-}
intersection :
    Conversion.IntersectionConversionStep
        intersection
        String
        intersection
        RecordOfValueAny
        (Expectation expectationCustom)
    -> Conversion intersection ValueAny (Error expectationCustom)
intersection =
    \conversionStepped ->
        { narrow =
            \broad ->
                case broad of
                    Structurey (StructureAnyIn (Recordy fields)) ->
                        fields
                            |> conversionStepped.narrow
                            |> Result.mapError
                                (Conversion.errorExpectationMap
                                    (\recordInside ->
                                        { fieldsAdditional =
                                            recordInside.fieldsAdditional
                                                |> Set.fromList
                                        , fieldValues =
                                            recordInside.fieldValues
                                                |> List.map
                                                    (\(Tagged tag value) ->
                                                        ( tag, value |> ExpectationIn )
                                                    )
                                                |> Dict.fromList
                                        }
                                            |> Inside
                                            |> Recordy
                                            |> Structurey
                                            |> Default
                                    )
                                )

                    _ ->
                        Conversion.Expected
                            (Structurey (Recordy Kind) |> Default)
                            |> Err
        , broaden =
            \narrow ->
                narrow
                    |> conversionStepped.broaden
                    |> Recordy
                    |> StructureAnyIn
                    |> Structurey
        }



--


{-| `Basics` [`ModuleOrigin`](#ModuleOrigin)
-}
moduleBasicsOrigin : ModuleOrigin
moduleBasicsOrigin =
    ModuleOrigin repositoryElmCoreOrigin "Basics"


{-| `Bool` [`Conversion`](Conversion#Conversion).
-}
bool : Conversion Bool ValueAny (Error expectationCustom_)
bool =
    Conversion.variantUnion
        (\true false boolVariantUnionIsTrue ->
            if boolVariantUnionIsTrue then
                true ()

            else
                false ()
        )
        |> Conversion.variantEat ( ( \() -> True, "True" ), unit )
        |> Conversion.variantEat ( ( \() -> False, "False" ), unit )
        |> unionIn moduleBasicsOrigin


{-| `Maybe` [`ModuleOrigin`](#ModuleOrigin)
-}
moduleMaybeOrigin : ModuleOrigin
moduleMaybeOrigin =
    ModuleOrigin repositoryElmCoreOrigin "Maybe"


{-| `Maybe` [`Conversion`](Conversion#Conversion).
-}
maybe :
    Conversion element ValueAny (Error expectationCustom)
    -> Conversion (Maybe element) ValueAny (Error expectationCustom)
maybe contentConversion =
    Conversion.variantUnion
        (\just nothing narrowMaybe ->
            case narrowMaybe of
                Nothing ->
                    nothing ()

                Just content ->
                    content |> just
        )
        |> Conversion.variantEat ( ( Just, "Just" ), contentConversion )
        |> Conversion.variantEat ( ( \() -> Nothing, "Nothing" ), unit )
        |> unionIn moduleMaybeOrigin


{-| `Result` [`ModuleOrigin`](#ModuleOrigin)
-}
moduleResultOrigin : ModuleOrigin
moduleResultOrigin =
    ModuleOrigin repositoryElmCoreOrigin "Result"


{-| `Result` [`Conversion`](Conversion#Conversion).
-}
result :
    { ok : Conversion okValue ValueAny (Error expectationCustom)
    , err : Conversion error ValueAny (Error expectationCustom)
    }
    -> Conversion (Result error okValue) ValueAny (Error expectationCustom)
result caseConversions =
    Conversion.variantUnion
        (\ok err narrowResult ->
            case narrowResult of
                Ok value ->
                    value |> ok

                Err error ->
                    error |> err
        )
        |> Conversion.variantEat ( ( Ok, "Ok" ), caseConversions.ok )
        |> Conversion.variantEat ( ( Err, "Err" ), caseConversions.err )
        |> unionIn moduleResultOrigin


{-| `Set` [`ModuleOrigin`](#ModuleOrigin)
-}
moduleSetOrigin : ModuleOrigin
moduleSetOrigin =
    ModuleOrigin repositoryElmCoreOrigin "Set"


{-| `Set` [`Conversion`](Conversion#Conversion).
-}
set :
    Conversion comparableElement ValueAny (Error expectationCustom)
    -> Conversion (Set comparableElement) ValueAny (Error expectationCustom)
set elementConversion =
    let
        setListConversion =
            Conversion.over
                (( Set.fromList, Set.toList )
                    |> Conversion.transfer
                )
                (list elementConversion)
    in
    Conversion.variantUnion
        (\setVariant setNarrow -> setVariant setNarrow)
        |> Conversion.variantEat
            ( ( identity, "Set" ), setListConversion )
        |> unionIn moduleSetOrigin


{-| `Dict` [`ModuleOrigin`](#ModuleOrigin)
-}
moduleDictOrigin : ModuleOrigin
moduleDictOrigin =
    ModuleOrigin repositoryElmCoreOrigin "Dict"


{-| `Dict` [`Conversion`](Conversion#Conversion).
-}
dict :
    { key : Conversion comparableKey ValueAny (Error expectationCustom)
    , value : Conversion value ValueAny (Error expectationCustom)
    }
    -> Conversion (Dict comparableKey value) ValueAny (Error expectationCustom)
dict nodeConversion =
    let
        dictListConversion =
            Conversion.over
                (( Dict.fromList, Dict.toList )
                    |> Conversion.transfer
                )
                (list
                    (( nodeConversion.key, nodeConversion.value )
                        |> tuple2
                    )
                )
    in
    Conversion.variantUnion
        (\dictVariant dictNarrow -> dictVariant dictNarrow)
        |> Conversion.variantEat
            ( ( identity, "Dict" ), dictListConversion )
        |> unionIn moduleDictOrigin
