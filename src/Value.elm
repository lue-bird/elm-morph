module Value exposing
    ( Value, Atom(..), Composed(..), Record, Tagged
    , Morph
    , unit
    , GroupMorphEmpty
    , field, group, groupFinish
    , variant, choiceFinish
    , Name, Index, IndexOrName(..), IndexAndName
    , descriptive, compact, eachTag, tagMap
    , AtomOrComposed(..)
    , atom, composed, composedMap
    , atomKindToString, composedKindToString
    )

{-| Generic, `case`-able elm value

json encoders/decoders are too low-level for serialization,
explicitly describing how to serialize individual data types that all have the same shape

Plus it makes it harder to switch to a different format

@docs Value, Atom, Composed, Record, Tagged


## morph

@docs Morph
@docs unit

Basically every `module` here has a [`Morph`](#Morph),
for example

  - [`Integer`](Integer)
  - [`FloatExplicit`](FloatExplicit)
  - [`String.Morph`](String-Morph)
  - [`Maybe.Morph`](Maybe-Morph)
  - [`Result.Morph`](Result-Morph)
  - [`List.Morph`](List-Morph)
  - [`Dict.Morph`](Dict-Morph)
  - [`Set.Morph`](Set-Morph)
  - [`Array.Morph`](Array-Morph)


### grouping

@docs GroupMorphEmpty
@docs field, group, groupFinish


### choice

variant union [`Morph`](#Morph)

  - starting from [`Morph.choice`](Morph#choice)
  - over [`Value.variant`](#variant)
  - and completed with [`Value.choiceFinish`](#choiceFinish)

@docs variant, choiceFinish


## tag

@docs Name, Index, IndexOrName, IndexAndName

@docs descriptive, compact, eachTag, tagMap


## spin your own

build on existing ones

    {-| `Posix` `Morph`
    -}
    posixValue : Value.Morph Posix
    posixValue =
        Morph.translate
            Time.posixToMillis
            Time.millisToPosix
            |> Morph.over Int.Morph.value

or define new atoms, composed structures, ... (↓ are used by [`Json`](Json) for example)

@docs AtomOrComposed
@docs atom, composed, composedMap
@docs atomKindToString, composedKindToString


## broad formats

  - [`Json`](Json)
      - [`jsValueMagic`](Json#jsValueMagic)
      - [`string`](Json#string)

Motivated? Explore, PR ↓

  - `Yaml`
      - following [`MaybeJustJames/yaml`](https://github.com/MaybeJustJames/yaml/blob/2.1.1/src/Yaml/Parser.elm)
  - `Xml`
  - `Url`
  - `Bytes`
  - ...


## prior art

  - [`bundsol/`: `Boxed`](https://package.elm-lang.org/packages/bundsol/boxed/2.0.0/Boxed)
      - 👎 no box-unbox conversion pairs
  - [`tricycle/elm-storage`: `Storage.Value`](https://dark.elm.dmy.fr/packages/tricycle/elm-storage/latest/Storage-Value)
      - 👎 doesn't expose the `Value` variants
  - [`andre-dietrich/elm-generic`](https://dark.elm.dmy.fr/packages/andre-dietrich/elm-generic/latest/Generic)
      - 👍 multiple broad formats: json, xml, yaml
      - 👎 no encode-decode conversion pairs
  - [`the-sett/decode-generic`](https://dark.elm.dmy.fr/packages/the-sett/decode-generic/latest/Json-Decode-Generic)
      - 👎 no encode (so no encode-decode conversion pairs as well)
  - [`miniBill/elm-codec`](https://dark.elm.dmy.fr/packages/miniBill/elm-codec/latest/Codec)
  - [`MartinSStewart/elm-serialize`](https://dark.elm.dmy.fr/packages/MartinSStewart/elm-serialize/latest/)
      - 👍 multiple broad formats: json, string (url safe), `Bytes`
      - custom errors
      - doesn't encode field & variant names
          - 👎 hard to debug
          - 👎 easy to corrupt
          - 👍 little space
  - [`fujiy/elm-json-convert`](https://dark.elm.dmy.fr/packages/fujiy/elm-json-convert/latest/Json-Convert)
      - 👎 no variant converters
  - [`prozacchiwawa/elm-json-codec`](https://dark.elm.dmy.fr/packages/prozacchiwawa/elm-json-codec/latest/JsonCodec)
      - 👎 no variant converters

-}

import Array exposing (Array)
import ArraySized
import Decimal.Internal exposing (Decimal)
import Emptiable exposing (Emptiable, fill, filled)
import Linear exposing (Direction(..))
import Morph exposing (ChoiceMorphEmpty, Morph, MorphIndependently, MorphOrError, broadenFrom, narrowTo, to, translate)
import Possibly exposing (Possibly)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Stack exposing (Stacked)


{-| An elm literal (that doesn't itself contain other [values](#Value))
-}
type Atom
    = Unit ()
    | Number Decimal
    | String String


{-| elm value. Either

  - a atom that don't itself contain values
  - a composed that can itself contain values

-}
type AtomOrComposed atom composed
    = Atom atom
    | Composed composed


{-| Generic representation of any value representable in elm

Use to the [`Morph`](#Morph)s present in most `module`s of this package
to construct a [`Value`](#Value).

PR if you'd like to see a structure or atom (like `Dict String ...` or `Random.Seed`)
that can't be converted in constant time
(turning tuples into records would only take O(1) time for example)

-}
type alias Value tag =
    AtomOrComposed Atom (Composed tag)


{-| elm structure that can itself contain values

Note: To not have overly complex recursive types,
a [`Value.Composed`](#Composed) _can_ describe an invalid elm value,
for example

    List [ Unit (), String "huh" ]

-}
type Composed tag
    = List (List (Value tag))
    | Array (Array (Value tag))
    | Record (Record tag)
    | Variant (Tagged tag)


{-| [tag](#tag)-[`Value`](#Value) pair used to represent a field or a variant
-}
type alias Tagged tag =
    RecordWithoutConstructorFunction
        { tag : tag, value : Value tag }


{-| The composed of a record that can hold [any](#Value) field value
-}
type alias Record tag =
    Emptiable (Stacked (Tagged tag)) Possibly


{-| EIther [`Index`](#Index) or [`Name`](#Name)

Used as the narrow argument of a [`Morph`](#Morph)

-}
type IndexOrName
    = Index Int
    | Name String


{-| Only its index identifies the variant or field
-}
type alias Index =
    RecordWithoutConstructorFunction
        { index : Int }


{-| Only its name identifies the variant or field
-}
type alias Name =
    RecordWithoutConstructorFunction
        { name : String }


{-| Both [`Index`](#Index) and [`Name`](#Name)

Used as the broad result of a [`Morph`](#Morph)

-}
type alias IndexAndName =
    RecordWithoutConstructorFunction
        { index : Int, name : String }



-- alter


{-| with readable names

  - field tag = name given to the [`field` `Morph`](#field)
  - variant tag = name given to the [`variant` `Morph`](#variant)
  - →
      - readable by humans
      - readable by other tools
      - debuggable
      - shuffling fields [`Morph`](#Morph) order → no change
      - renaming fields → breaking change
      - not [`compact`](#compact)

-}
descriptive :
    MorphIndependently
        (Name -> Result error_ IndexOrName)
        (IndexAndName -> Name)
descriptive =
    translate
        (\tag -> tag.name |> Name)
        (\tag -> { name = tag.name })


{-| With compact indexes

  - field tag = [`field` `Morph`](#field) index index in the builder
  - variant tag = [`variant` `Morph`](#variant) index in the builder
  - →
      - not [`descriptive`](#descriptive)

-}
compact :
    MorphIndependently
        (Index -> Result error_ IndexOrName)
        (IndexAndName -> Index)
compact =
    translate
        (\tag -> tag.index |> Index)
        (\tag -> { index = tag.index })


{-| [`Translate`](Morph#Translate) a [`Value`](#Value)
by reducing the amount of tag information in both directions

For [`Value`](#Value), it's

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
    translate
        (tagMap (Morph.mapTo tagTranslate_))
        (tagMap (Morph.broadenFrom tagTranslate_))


{-| Reduce the amount of tag information of the [`Value`](#Value)
-}
tagMap :
    (tag -> tagMapped)
    -> (Value tag -> Value tagMapped)
tagMap tagChange =
    \value ->
        value |> composedMap (composedTagMap tagChange)


{-| If the [`AtomOrComposed`](#AtomOrComposed) is a [`Composed`](#AtomOrComposed),
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
                    |> Stack.map (\_ -> taggedAnyTagMap tagChange)
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
and a [generic value](#Value)

The way it's set up, it allows tags that are either

  - [`descriptive`](#descriptive)
  - [`compact`](#compact)

-}
type alias Morph narrow =
    Morph.MorphIndependently
        (Value IndexOrName
         -> Result Morph.Error narrow
        )
        (narrow -> Value IndexAndName)



-- atom or composed


{-| [`Morph`](Morph#Morph) to a [`AtomOrComposed`](#AtomOrComposed)'s atom if possible
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
    Morph.value "Atom"
        { broaden = Atom
        , narrow =
            \value ->
                case value of
                    Atom atomAny ->
                        atomAny |> Ok

                    Composed _ ->
                        "Composed" |> Err
        }


{-| [`Morph`](Morph#Morph) to a [`AtomOrComposed`](#AtomOrComposed)'s composed if possible
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
    Morph.value "Composed"
        { broaden = Composed
        , narrow =
            \value ->
                case value of
                    Composed composed_ ->
                        composed_ |> Ok

                    Atom _ ->
                        "Atom" |> Err
        }


{-| Describe the type of [`Atom`](#Atom)
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


{-| Describe the type of [`Composed`](#Composed)
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


{-| `()` [`Morph`](#Morph)

Often used in when [morphing](Value#Morph) a [variants](Choice#variantValue)
with 0 attached values

-}
unit : Morph ()
unit =
    Morph.value "Unit"
        { broaden = Unit
        , narrow =
            \value_ ->
                case value_ of
                    Unit unitValue ->
                        unitValue |> Ok

                    atomExceptUnit ->
                        atomExceptUnit |> atomKindToString |> Err
        }
        |> Morph.over atom


{-| Start a record assembly [`Morph`](#Morph)

Continue with [`field`](#field)

    {-| `( ..., ... )` `Morph`

    Just use a record with descriptive names instead!

    -}
    tuple2 :
        ( Morph part0
        , Morph part1
        )
        -> Morph ( part0, part1 )
    tuple2 ( part0Morph, part1Morph ) =
        Morph.to "Tuple2"
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
        Morph.to "Tuple3"
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
    -> GroupMorphEmpty Possibly groupNarrow_ groupNarrowAssemble
group groupNarrowAssemble =
    Morph.parts ( groupNarrowAssemble, Emptiable.empty )


{-| possibly incomplete step from and to a [`Record`](Value#Record)

building:

  - start with [`group`](#value)
  - continue with [`field`](#field)
  - finish with [`groupFinish`](#groupFinish)

-}
type alias GroupMorphEmpty noPartPossiblyOrNever groupNarrow groupNarrowFurther =
    Morph.PartsMorphEmpty
        noPartPossiblyOrNever
        (Record IndexOrName
         ->
            Result
                (Morph.PartsError Morph.Error)
                groupNarrowFurther
        )
        (groupNarrow -> Record IndexAndName)


{-| Continue a group assembly [`Morph`](#Morph) to [`Value`](#Value).

  - finish with [`groupFinish`](#groupFinish)

-}
field :
    ( group -> fieldValueNarrow
    , String
    )
    -> Morph fieldValueNarrow
    ->
        (GroupMorphEmpty
            noPartPossiblyOrNever_
            group
            (fieldValueNarrow -> groupNarrowFurther)
         ->
            GroupMorphEmpty
                noPartNever_
                group
                groupNarrowFurther
        )
field ( accessFieldValue, fieldName ) fieldValueMorph =
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
                    (Morph.to tag.name fieldValueMorph
                        |> Morph.description
                    )
        , narrow =
            \groupBroad ->
                partValueNarrow tag fieldValueMorph groupMorphSoFar.narrow groupBroad
        , broaden =
            \wholeNarrow ->
                let
                    fieldValueBroad : Value IndexAndName
                    fieldValueBroad =
                        wholeNarrow
                            |> accessFieldValue
                            |> Morph.broadenFrom fieldValueMorph

                    fieldBroad : Tagged IndexAndName
                    fieldBroad =
                        { tag = tag
                        , value = fieldValueBroad
                        }
                in
                wholeNarrow
                    |> groupMorphSoFar.broaden
                    |> Stack.onTopLay fieldBroad
        }


partValueNarrow :
    IndexAndName
    -> Morph fieldValueNarrow
    ->
        (Emptiable (Stacked (Tagged IndexOrName)) possiblyOrNever_
         ->
            Result
                (Morph.PartsError Morph.Error)
                (fieldValueNarrow -> groupNarrowFurther)
        )
    ->
        (Emptiable (Stacked (Tagged IndexOrName)) possiblyOrNever_
         -> Result (Morph.PartsError Morph.Error) groupNarrowFurther
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
                    (Morph.PartsError Morph.Error)
                    (fieldValueNarrow -> groupNarrowFurther)
            wholeAssemblyResult =
                groupBroad |> groupSoFarNarrow

            errorsSoFar : () -> Emptiable (Stacked { index : Int, error : Morph.Error }) Possibly
            errorsSoFar () =
                case wholeAssemblyResult of
                    Ok _ ->
                        Emptiable.empty

                    Err expectations ->
                        expectations |> Emptiable.emptyAdapt never
        in
        case groupBroad |> Stack.toList |> List.filter (.tag >> matches) of
            partBroad :: _ ->
                case partBroad.value |> Morph.narrowTo fieldValueMorph of
                    Ok partNarrow ->
                        wholeAssemblyResult
                            |> Result.map (\eat -> eat partNarrow)

                    Err innerError ->
                        errorsSoFar ()
                            |> Stack.onTopLay
                                { index = tag.index
                                , error = innerError
                                }
                            |> Err

            [] ->
                errorsSoFar ()
                    |> Stack.onTopLay
                        { index = tag.index
                        , error = (tag.name ++ " missing") |> Morph.DeadEnd
                        }
                    |> Err


{-| Conclude the [`group`](#group) |> [`field`](#field) chain
-}
groupFinish :
    GroupMorphEmpty Never record record
    -> Morph record
groupFinish =
    \groupMorphComplete ->
        groupMorphComplete
            |> Morph.partsFinish
            |> Morph.over
                (Morph.value "Record"
                    { broaden = Record
                    , narrow =
                        \composedBroad ->
                            case composedBroad of
                                Record recordNarrow ->
                                    recordNarrow |> Ok

                                composedExceptRecord ->
                                    composedExceptRecord |> composedKindToString |> Err
                    }
                )
            |> Morph.over composed


{-| Describe another variant value [`Morph`](#Morph) to [`Value`](#Value)

Done? → [`Value.choiceFinish`](#choiceFinish)

If a variant doesn't have any value attached, use [`unit`](Value#unit)

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
            |> Value.variant ( \() -> True, "True" ) unit
            |> Value.variant ( \() -> False, "False" ) unit
            |> Value.choiceFinish

-}
variant :
    ( possibilityNarrow -> choiceNarrow
    , String
    )
    -> Morph possibilityNarrow
    ->
        (ChoiceMorphEmpty
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
            ChoiceMorphEmpty
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
                (Morph.to possibilityTag
                    { description = { custom = Emptiable.empty, inner = Emptiable.empty }
                    , narrow =
                        variantStepNarrow
                            ( { name = possibilityTag
                              , index = choiceMorphSoFar.description |> Stack.length
                              }
                            , Morph.narrowTo possibilityMorph
                            )
                    , broaden =
                        \narrowValue ->
                            { tag =
                                { name = possibilityTag
                                , index = choiceMorphSoFar.description |> Stack.length
                                }
                            , value = narrowValue |> Morph.broadenFrom possibilityMorph
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


{-| Conclude a [`Morph.choice`](Morph#Morph.choice) |> [`Value.variant`](#variant) chain
-}
choiceFinish :
    ChoiceMorphEmpty
        Never
        choiceNarrow
        (Tagged IndexOrName)
        (choiceNarrow -> Tagged IndexAndName)
        Morph.Error
    -> Morph choiceNarrow
choiceFinish =
    \choiceMorphComplete ->
        choiceMorphComplete
            |> Morph.choiceFinish
            |> Morph.over
                (Morph.value "Variant"
                    { narrow =
                        \value ->
                            case value of
                                Variant variant_ ->
                                    variant_ |> Ok

                                composedExceptVariant ->
                                    composedExceptVariant
                                        |> composedKindToString
                                        |> Err
                    , broaden = Variant
                    }
                )
            |> Morph.over composed
