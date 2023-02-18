module Value exposing
    ( Value, Atom(..), Composed(..), Record, Tagged
    , Morph
    , unit
    , GroupMorphNoPart
    , field, group, groupFinish
    , Name, Index, IndexOrName(..), IndexAndName
    , descriptive, compact, tagTranslate, tagMap
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

Basically every `module` here has a [`Value.Morph`](#Morph),
for example

  - [`Int.Morph`](Int-Morph)
  - [`FloatExplicit`](Float-Morph)
  - [`String.Morph`](String-Morph)
  - [`Maybe.Morph`](Maybe-Morph)
  - [`Result.Morph`](Result-Morph)
  - [`List.Morph`](List-Morph)
  - [`Dict.Morph`](Dict-Morph)
  - [`Set.Morph`](Set-Morph)
  - [`Array.Morph`](Array-Morph)


### record

@docs GroupMorphNoPart
@docs field, group, groupFinish


### Choice.between

variant union [`Value.Morph`](#Value.Morph)

  - starting from [`Choice.between`](Choice#between)
  - over [`Choice.variantValue`](Choice#variantValue)
  - and completed with [`Choice.finishValue`](Choice#finishValue)


## tag

@docs Name, Index, IndexOrName, IndexAndName

@docs descriptive, compact, tagTranslate, tagMap


## spin your own

build on existing ones

    {-| `Posix` `Value.Morph`
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
      - after [`MaybeJustJames/yaml`](https://github.com/MaybeJustJames/yaml/blob/2.1.1/src/Yaml/Parser.elm)
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
import Morph exposing (Morph, MorphIndependently, MorphOrError, broadenFrom, narrowTo, to, translate)
import N exposing (Up)
import Possibly exposing (Possibly(..))
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Stack exposing (Stacked)


{-| A supported elm atom (that don't contain other [values](#Value))
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


{-| Any value representable in elm

Like to see a composed or atom
that can't be converted in a small amount of runtime
(for example treating tuples the same as records is just O(1))?
→ PR!

Please use to the [`Value.Morph`](#Value.Morph)s present in most `module`s
to construct a [`Value`](#Value),
as you can for example construct ↓ using the exposed(..) variants

    Value.List [ Value.Unit (), Value.String "huh" ]

which isn't a valid elm value

-}
type alias Value tag =
    AtomOrComposed Atom (Composed tag)


{-| elm composed that can itself contain values
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

Used as the narrow argument of a [`Value.Morph`](#Value.Morph)

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

Used as the broad result of a [`Value.Morph`](#Value.Morph)

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
        |> Morph.over (Value.tagTranslate Value.compact)

    -- or
    ...
        |> Morph.over (Value.tagTranslate Value.descriptive)

but the same thing works for [`Json`](Json), ... as well

-}
tagTranslate :
    MorphIndependently
        (tagBeforeMap -> Result (Morph.ErrorWithDeadEnd Never) tagMapped)
        (tagBeforeUnmap -> tagUnmapped)
    ->
        MorphIndependently
            (Value tagBeforeMap
             -> Result (Morph.ErrorWithDeadEnd never_) (Value tagMapped)
            )
            (Value tagBeforeUnmap -> Value tagUnmapped)
tagTranslate tagTranslate_ =
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


{-| Start a record assembly [`Value.Morph`](#Value.Morph)

Continue with [`field`](#field)

    {-| `( ..., ... )` `Value.Morph`

    Just use a record with descriptive names instead!

    -}
    tuple2 :
        ( Value.Morph part0
        , Value.Morph part1
        )
        -> Value.Morph ( part0, part1 )
    tuple2 ( part0Morph, part1Morph ) =
        Morph.to "Tuple2"
            (record
                (\part0 part1 -> ( part0, part1 ))
                |> field ( Tuple.first, "part0" ) part0Morph
                |> field ( Tuple.second, "part1" ) part1Morph
                |> recordFinish
            )

    {-| `( ..., ..., ... )` `Value.Morph`

    Just use a record with descriptive names instead!

    -}
    tuple3 :
        ( Value.Morph part0
        , Value.Morph part1
        , Value.Morph part2
        )
        -> Value.Morph ( part0, part1, part2 )
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
    -> GroupMorphNoPart Possibly groupNarrow_ groupNarrowAssemble
group groupNarrowAssemble =
    Morph.groupToFrom ( groupNarrowAssemble, Emptiable.empty )


{-| possibly incomplete step from and to a [`Value.Record`](Value#Record)

building:

  - start with [`Value.group`](#value)
  - continue with [`Value.field`](#field)
  - finish with [`Value.groupFinish`](#groupFinish)

-}
type alias GroupMorphNoPart noPartPossiblyOrNever groupNarrow groupNarrowFurther =
    Morph.MorphNoPart
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
        (GroupMorphNoPart
            noPartPossiblyOrNever_
            group
            (fieldValueNarrow -> groupNarrowFurther)
         ->
            GroupMorphNoPart
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


{-| Conclude the [`Value.group`](#group) |> [`Value.field`](#field) chain
-}
groupFinish :
    GroupMorphNoPart Never record record
    -> Morph record
groupFinish =
    \groupMorphComplete ->
        groupMorphComplete
            |> Morph.groupFinish
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
