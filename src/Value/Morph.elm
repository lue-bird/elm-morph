module Value.Morph exposing
    ( MorphValue
    , unit
    , MorphValueGroupEmptiable, PartsError(..)
    , group, part, groupFinish
    , variant, choiceFinish
    , descriptive, compact, eachTag
    , atom, composed
    )

{-| Morph your types over a [generic, `case`-able elm value](Value)

@docs MorphValue
@docs unit

Basically every `module` here has a [`MorphValue`](#MorphValue),
for example

  - [`Integer.Morph`](Integer-Morph)
  - [`Decimal.Morph`](Decimal-Morph)
  - [`String.Morph`](String-Morph)
  - [`Maybe.Morph`](Maybe-Morph)
  - [`List.Morph`](List-Morph)
  - [`Dict.Morph`](Dict-Morph)
  - [`Set.Morph`](Set-Morph)
  - [`Array.Morph`](Array-Morph)


### grouping

@docs MorphValueGroupEmptiable, PartsError
@docs group, part, groupFinish


### choice

variant union [`Morph`](#MorphValue)

  - starting from [`Morph.choice`](Morph#choice)
  - over [`Value.Morph.variant`](#variant)
  - and completed with [`Value.Morph.choiceFinish`](#choiceFinish)

@docs variant, choiceFinish


## tag

@docs descriptive, compact, eachTag


## spin your own

build on existing ones

    {-| `Posix` `MorphValue`
    -}
    posixValue : MorphValue Posix
    posixValue =
        Morph.oneToOne
            Time.posixToMillis
            Time.millisToPosix
            |> Morph.over Int.Morph.value

or define new atoms, composed structures, ... (↓ are used by [`Json`](Json) for example)

@docs atom, composed


## broad formats

  - [`Json`](Json)
      - [`jsValueMagic`](Json#jsValueMagic)
      - [`string`](Json#string)

Motivated? Explore, PR ↓

  - `Url`/`AppUrl`
  - `Xml`
  - `Yaml`
      - following [`MaybeJustJames/yaml`](https://github.com/MaybeJustJames/yaml/blob/2.1.1/src/Yaml/Parser.elm)
  - ...

-}

import Array
import Emptiable exposing (Emptiable)
import Morph exposing (ChoiceMorphEmptiable, ErrorWithDeadEnd(..), MorphIndependently, oneToOne)
import Possibly exposing (Possibly(..))
import Stack exposing (Stacked)
import Value exposing (Atom(..), AtomOrComposed(..), Composed(..), Index, IndexAndName, IndexOrName(..), Name, Record, Tagged, Value)



-- alter


{-| with readable names. Use in combination with [`eachTag`](#eachTag)

  - field tag = name given to the [`part` `Morph`](#part)
  - variant tag = name given to the [`variant` `Morph`](#variant)
  - →
      - readable by humans
      - readable by other tools
      - debuggable
      - shuffling fields [`Morph`](#MorphValue) order → no change
      - renaming fields → breaking change
      - not [`compact`](#compact)

-}
descriptive :
    MorphIndependently
        (Name -> Result error_ IndexOrName)
        (IndexAndName -> Name)
descriptive =
    oneToOne
        (\tag -> tag.name |> Name)
        (\tag -> { name = tag.name })


{-| With compact indexes. Use in combination with [`eachTag`](#eachTag)

  - field tag = [`field` `Morph`](#part) index index in the builder
  - variant tag = [`variant` `Morph`](#variant) index in the builder
  - →
      - not [`descriptive`](#descriptive)

-}
compact :
    MorphIndependently
        (Index -> Result error_ IndexOrName)
        (IndexAndName -> Index)
compact =
    oneToOne
        (\tag -> tag.index |> Index)
        (\tag -> { index = tag.index })


{-| Reduce the amount of tag information of the [`Value`](Value#Value)
-}
tagMap :
    (tag -> tagMapped)
    -> (Value tag -> Value tagMapped)
tagMap tagChange =
    \value ->
        value |> composedMap (composedTagMap tagChange)


{-| If the [`AtomOrComposed`](Value#AtomOrComposed) is a [`Composed`](Value#AtomOrComposed),
change in a given way
-}
composedMap :
    (composed -> composedMapped)
    ->
        (AtomOrComposed atom composed
         -> AtomOrComposed atom composedMapped
        )
composedMap composedChange =
    \value ->
        case value of
            Atom atom_ ->
                atom_ |> Atom

            Composed composed_ ->
                composed_
                    |> composedChange
                    |> Composed


{-| [`OneToOne`](Morph#OneToOne) a [`Value`](Value#Value)
by reducing the amount of tag information in both directions

For [`Value`](Value#Value), it's

    ...
        |> Morph.over (eachTag compact)

    -- or
    ...
        |> Morph.over (eachTag descriptive)

but the same thing works for [`Json`](Json), ... as well

-}
eachTag :
    MorphIndependently
        (tagBeforeMap -> Result (Morph.ErrorWithDeadEnd Never) tagMapped)
        (tagBeforeUnmap -> tagUnmapped)
    ->
        MorphIndependently
            (Value tagBeforeMap
             -> Result (Morph.ErrorWithDeadEnd never_) (Value tagMapped)
            )
            (Value tagBeforeUnmap -> Value tagUnmapped)
eachTag tagTranslate_ =
    Morph.oneToOneOn ( tagMap, tagMap ) tagTranslate_


composedTagMap :
    (tag -> tagMapped)
    ->
        (Composed tag
         -> Composed tagMapped
        )
composedTagMap tagChange =
    \composedAny_ ->
        case composedAny_ of
            List list_ ->
                list_
                    |> List.map (tagMap tagChange)
                    |> List

            Array array_ ->
                array_
                    |> Array.map (tagMap tagChange)
                    |> Array

            Record fields ->
                fields
                    |> List.map (taggedAnyTagMap tagChange)
                    |> Record

            Variant tagged ->
                tagged
                    |> taggedAnyTagMap tagChange
                    |> Variant


taggedAnyTagMap :
    (tag -> tagMapped)
    -> (Tagged tag -> Tagged tagMapped)
taggedAnyTagMap tagChange =
    \tagged ->
        { tag = tagged.tag |> tagChange
        , value = tagged.value |> tagMap tagChange
        }



-- morph


{-| [`Morph`](Morph#Morph) between a given narrow value
and a [generic value](Value#Value)

The way it's set up, it allows tags that are either

  - [`descriptive`](#descriptive)
  - [`compact`](#compact)

-}
type alias MorphValue narrow =
    Morph.MorphIndependently
        (Value IndexOrName
         -> Result Morph.Error narrow
        )
        (narrow -> Value IndexAndName)



-- atom or composed


{-| [`Morph`](Morph#Morph) to a [`AtomOrComposed`](Value#AtomOrComposed)'s atom if possible
-}
atom :
    Morph.MorphIndependently
        (AtomOrComposed narrowAtom narrowComposed_
         -> Result Morph.Error narrowAtom
        )
        (broadAtom
         -> AtomOrComposed broadAtom broadComposed_
        )
atom =
    Morph.custom "Atom"
        { toBroad = Atom
        , toNarrow =
            \value ->
                case value of
                    Atom atomAny ->
                        atomAny |> Ok

                    Composed _ ->
                        "Composed" |> Err
        }


{-| [`Morph`](Morph#Morph) to a [`AtomOrComposed`](Value#AtomOrComposed)'s composed if possible
-}
composed :
    MorphIndependently
        (AtomOrComposed narrowAtom_ narrowComposed
         -> Result Morph.Error narrowComposed
        )
        (broadComposed
         -> AtomOrComposed broadAtom_ broadComposed
        )
composed =
    Morph.custom "Composed"
        { toBroad = Composed
        , toNarrow =
            \value ->
                case value of
                    Composed composed_ ->
                        composed_ |> Ok

                    Atom _ ->
                        "Atom" |> Err
        }


{-| `()` [`Morph`](#MorphValue)

Often used in when [morphing](Value-Morph#MorphValue) a [variant](#variant)
with 0 attached values

-}
unit : MorphValue ()
unit =
    Morph.custom "Unit"
        { toBroad = Unit
        , toNarrow =
            \value_ ->
                case value_ of
                    Unit unitValue ->
                        unitValue |> Ok

                    atomExceptUnit ->
                        atomExceptUnit |> atomKindToString |> Err
        }
        |> Morph.over atom


{-| Describe the type of [`Atom`](Value#Atom)
-}
atomKindToString : Atom -> String
atomKindToString =
    \atom_ ->
        case atom_ of
            Unit _ ->
                "Unit"

            Number _ ->
                "Decimal"

            String _ ->
                "String"


{-| Start a record assembly [`Morph`](#MorphValue)

Continue with [`field`](#part)

An example translated from [`elm/json`](https://dark.elm.dmy.fr/packages/elm/json/latest/)

    import Decimal exposing (Decimal)
    import Decimal.Morph
    import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
    import String.Morph
    import Value

    type alias Cause =
        RecordWithoutConstructorFunction
            { name : String
            , percent : Decimal
            , per100k : Decimal
            }

    value : MorphValue Cause
    value =
        Value.Morph.group
            (\name percent per100k ->
                { name = name, percent = percent, per100k = per100k }
            )
            |> Value.Morph.part ( .name, "name" ) String.Morph.value
            |> Value.Morph.part ( .percent, "percent" ) Decimal.Morph.value
            |> Value.Morph.part ( .per100k, "per100k" ) Decimal.Morph.value
            |> Value.Morph.groupFinish

Another example for tuples

    {-| `( ..., ... )` `Morph`

    Just use a record with descriptive names instead!

    -}
    tuple2 :
        ( Morph part0
        , Morph part1
        )
        -> Morph ( part0, part1 )
    tuple2 ( part0Morph, part1Morph ) =
        Morph.named "Tuple2"
            (record
                (\part0 part1 -> ( part0, part1 ))
                |> field ( Tuple.first, "part0" ) part0Morph
                |> field ( Tuple.second, "part1" ) part1Morph
                |> recordFinish
            )

    {-| `( ..., ..., ... )` `Morph`

    Just use a record with descriptive names instead!

    -}
    tuple3 :
        ( Morph part0
        , Morph part1
        , Morph part2
        )
        -> Morph ( part0, part1, part2 )
    tuple3 ( part0Morph, part1Morph, part2Morph ) =
        Morph.named "Tuple3"
            (record
                (\part0 part1 part2 -> ( part0, part1, part2 ))
                |> field ( \( part0, _, _ ) -> part0, "part0" ) part0Morph
                |> field ( \( _, part1, _ ) -> part1, "part1" ) part1Morph
                |> field ( \( _, _, part2 ) -> part2, "part2" ) part2Morph
                |> recordFinish
            )

-}
group :
    groupNarrowAssemble
    -> MorphValueGroupEmptiable Possibly groupNarrow_ groupNarrowAssemble
group groupNarrowAssemble =
    Morph.parts ( groupNarrowAssemble, [] )


{-| possibly incomplete step from and to a [`Record`](Value#Record)

building:

  - start with [`group`](#group)
  - continue with [`field`](#part)
  - finish with [`groupFinish`](#groupFinish)

-}
type alias MorphValueGroupEmptiable noPartPossiblyOrNever groupNarrow groupNarrowFurther =
    Morph.PartsMorphEmptiable
        noPartPossiblyOrNever
        (Value.Record IndexOrName
         -> Result PartsError groupNarrowFurther
        )
        (groupNarrow -> Value.Record IndexAndName)


{-| What can go wrong narrowing a [`Record`](Value#Record)
-}
type PartsError
    = TagsMissing (Emptiable (Stacked Int) Never)
    | ValueError { index : Int, error : Morph.Error }


{-| Continue a group assembly [`Morph`](#MorphValue) to [`Value`](Value#Value).

Finish with [`groupFinish`](#groupFinish)

-}
part :
    ( group -> fieldValueNarrow
    , String
    )
    -> MorphValue fieldValueNarrow
    ->
        (MorphValueGroupEmptiable
            noPartPossiblyOrNever_
            group
            (fieldValueNarrow -> groupNarrowFurther)
         ->
            MorphValueGroupEmptiable
                noPartNever_
                group
                groupNarrowFurther
        )
part ( accessFieldValue, fieldName ) fieldValueMorph =
    \groupMorphSoFar ->
        let
            tag : IndexAndName
            tag =
                { index = groupMorphSoFar.description |> Stack.length
                , name = fieldName
                }
        in
        { description =
            groupMorphSoFar.description
                |> Stack.onTopLay
                    { tag = tag.name, value = fieldValueMorph.description }
        , toNarrow =
            \groupBroad ->
                partValueNarrow tag fieldValueMorph groupMorphSoFar.toNarrow groupBroad
        , toBroad =
            \wholeNarrow ->
                let
                    fieldValueBroad : Value IndexAndName
                    fieldValueBroad =
                        wholeNarrow
                            |> accessFieldValue
                            |> Morph.toBroad fieldValueMorph

                    fieldBroad : Tagged IndexAndName
                    fieldBroad =
                        { tag = tag
                        , value = fieldValueBroad
                        }
                in
                wholeNarrow
                    |> groupMorphSoFar.toBroad
                    |> (::) fieldBroad
        }


partValueNarrow :
    IndexAndName
    -> MorphValue fieldValueNarrow
    ->
        (List (Tagged IndexOrName)
         -> Result PartsError (fieldValueNarrow -> groupNarrowFurther)
        )
    ->
        (List (Tagged IndexOrName)
         -> Result PartsError groupNarrowFurther
        )
partValueNarrow tag fieldValueMorph groupSoFarNarrow =
    let
        matches : IndexOrName -> Bool
        matches =
            \tagIndexOrName ->
                case tagIndexOrName of
                    Index index ->
                        index == tag.index

                    Name name ->
                        name == tag.name
    in
    \groupBroad ->
        let
            wholeAssemblyResult :
                Result
                    PartsError
                    (fieldValueNarrow -> groupNarrowFurther)
            wholeAssemblyResult =
                groupBroad |> groupSoFarNarrow
        in
        case groupBroad |> List.filter (.tag >> matches) of
            partBroad :: _ ->
                case partBroad.value |> Morph.toNarrow fieldValueMorph of
                    Ok partNarrow ->
                        wholeAssemblyResult
                            |> Result.map (\eat -> eat partNarrow)

                    Err innerError ->
                        ValueError
                            { index = tag.index
                            , error = innerError
                            }
                            |> Err

            [] ->
                let
                    tagsMissingSoFar : Emptiable (Stacked Int) Possibly
                    tagsMissingSoFar =
                        case wholeAssemblyResult of
                            Err (TagsMissing tagsMissing) ->
                                tagsMissing |> Emptiable.emptyAdapt (\_ -> Possible)

                            Err (ValueError _) ->
                                Emptiable.empty

                            Ok _ ->
                                Emptiable.empty
                in
                TagsMissing (Stack.onTopLay tag.index tagsMissingSoFar) |> Err


{-| Describe the type of [`Composed`](Value#Composed)
-}
composedKindToString : Composed tag_ -> String
composedKindToString =
    \composed_ ->
        case composed_ of
            List _ ->
                "List"

            Array _ ->
                "Array"

            Record _ ->
                "Record"

            Variant _ ->
                "Variant"


{-| Conclude the [`group`](#group) |> [`field`](#part) chain
-}
groupFinish :
    MorphValueGroupEmptiable Never record record
    -> MorphValue record
groupFinish =
    \groupMorphComplete ->
        groupMorphComplete
            |> partsFinish
            |> Morph.over
                (Morph.custom "Record"
                    { toBroad = Record
                    , toNarrow =
                        \composedBroad ->
                            case composedBroad of
                                Record recordNarrow ->
                                    recordNarrow |> Ok

                                composedExceptRecord ->
                                    composedExceptRecord |> composedKindToString |> Err
                    }
                )
            |> Morph.over composed


partsFinish :
    MorphValueGroupEmptiable
        Never
        groupNarrow
        groupNarrow
    ->
        MorphIndependently
            (Record IndexOrName -> Result Morph.Error groupNarrow)
            (groupNarrow -> Record IndexAndName)
partsFinish =
    \groupMorphInProgress ->
        { description =
            groupMorphInProgress.description |> Morph.PartsDescription
        , toNarrow =
            \broad_ ->
                broad_
                    |> groupMorphInProgress.toNarrow
                    |> Result.mapError
                        (\error ->
                            case error of
                                TagsMissing missingTags ->
                                    "missing parts: "
                                        ++ (missingTags |> Stack.toList |> List.map String.fromInt |> String.join ", ")
                                        |> DeadEnd

                                ValueError valueError ->
                                    valueError |> Stack.one |> Morph.PartsError
                        )
        , toBroad = groupMorphInProgress.toBroad
        }


{-| Describe another variant [`Morph`](#MorphValue) to [`Value`](Value#Value)

Done? → [`Value.Morph.choiceFinish`](#choiceFinish)

If a variant doesn't have any value attached, use [`unit`](#unit)

    {-| `Bool` `MorphValue`
    -}
    boolValue : MorphValue Bool
    boolValue =
        Morph.choice
            (\true false bool ->
                case bool of
                    True ->
                        true ()

                    False ->
                        false ()
            )
            |> Value.Morph.variant ( \() -> True, "True" ) unit
            |> Value.Morph.variant ( \() -> False, "False" ) unit
            |> Value.Morph.choiceFinish

-}
variant :
    ( possibilityNarrow -> choiceNarrow
    , String
    )
    -> MorphValue possibilityNarrow
    ->
        (ChoiceMorphEmptiable
            noTryPossiblyOrNever_
            choiceNarrow
            (Tagged IndexOrName)
            ((possibilityNarrow
              -> Tagged IndexAndName
             )
             -> choiceBroadenFurther
            )
            Morph.Error
         ->
            ChoiceMorphEmptiable
                noTryNever_
                choiceNarrow
                (Tagged IndexOrName)
                choiceBroadenFurther
                Morph.Error
        )
variant ( possibilityToChoice, possibilityTag ) possibilityMorph =
    \choiceMorphSoFar ->
        choiceMorphSoFar
            |> Morph.try possibilityToChoice
                (Morph.named possibilityTag
                    { description = possibilityMorph |> Morph.description
                    , toNarrow =
                        variantStepNarrow
                            ( { name = possibilityTag
                              , index = choiceMorphSoFar.description |> Stack.length
                              }
                            , Morph.toNarrow possibilityMorph
                            )
                    , toBroad =
                        \narrowValue ->
                            { tag =
                                { name = possibilityTag
                                , index = choiceMorphSoFar.description |> Stack.length
                                }
                            , value = narrowValue |> Morph.toBroad possibilityMorph
                            }
                    }
                )


variantStepNarrow :
    ( IndexAndName
    , Value IndexOrName
      -> Result Morph.Error possibilityNarrow
    )
    ->
        (Tagged IndexOrName
         -> Result Morph.Error possibilityNarrow
        )
variantStepNarrow ( variantTag, possibilityNarrow ) =
    \variantBroad ->
        case variantBroad.tag of
            Index index ->
                if index == variantTag.index then
                    variantBroad.value |> possibilityNarrow

                else
                    "tag " ++ (index |> String.fromInt) |> Morph.DeadEnd |> Err

            Name name ->
                if name == variantTag.name then
                    variantBroad.value |> possibilityNarrow

                else
                    "tag " ++ name |> Morph.DeadEnd |> Err


{-| Conclude a [`Morph.choice`](Morph#choice) |> [`Value.Morph.variant`](#variant) chain
-}
choiceFinish :
    ChoiceMorphEmptiable
        Never
        choiceNarrow
        (Tagged IndexOrName)
        (choiceNarrow -> Tagged IndexAndName)
        Morph.Error
    -> MorphValue choiceNarrow
choiceFinish =
    \choiceMorphComplete ->
        choiceMorphComplete
            |> Morph.choiceFinish
            |> Morph.over
                (Morph.custom "Variant"
                    { toNarrow =
                        \value ->
                            case value of
                                Variant variant_ ->
                                    variant_ |> Ok

                                composedExceptVariant ->
                                    composedExceptVariant
                                        |> composedKindToString
                                        |> Err
                    , toBroad = Variant
                    }
                )
            |> Morph.over composed
