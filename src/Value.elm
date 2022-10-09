module Value exposing
    ( Value, Literal(..), Structure(..), Record, Tagged
    , MorphValue
    , Name, Index, IndexOrName(..), IndexAndName
    , descriptive, compact, tagTranslate, tagMap
    , LiteralOrStructure(..)
    , literal, structure, structureMap
    )

{-| Generic, `case`-able elm value

json encoders/decoders are too low-level for serialization,
explicitly describing how to serialize individual data types that all have the same shape

Plus it makes it harder to switch to a different format

@docs Value, Literal, Structure, Record, Tagged


## morph

@docs MorphValue

Basically every `module` here has a [`MorphValue`](#MorphValue),
for example

  - [`Unit`](Unit)
  - [`Int.Morph`](Int-Morph)
  - [`Float.Morph`](Float-Morph)
  - [`String.Morph`](String-Morph)
  - [`Maybe.Morph`](Maybe-Morph)
  - [`Result.Morph`](Result-Morph)
  - [`List.Morph`](List-Morph)
  - [`Dict.Morph`](Dict-Morph)
  - [`Set.Morph`](Set-Morph)
  - [`Array.Morph`](Array-Morph)


### record

[`Group.MorphValue`](Group#MorphValue)


### Choice.between

variant union [`MorphValue`](#MorphValue)

  - starting from [`Choice.between`](Choice#between)
  - over [`Choice.tryValue`](Choice#tryValue)
  - and completed with [`Choice.finishValue`](Choice#finishValue)


## tag

@docs Name, Index, IndexOrName, IndexAndName

@docs descriptive, compact, tagTranslate, tagMap


## spin your own

build on existing ones

    {-| `Posix` `MorphValue`
    -}
    posixValue : MorphValue Posix
    posixValue =
        Morph.translate
            Time.posixToMillis
            Time.millisToPosix
            |> Morph.over Int.Morph.value

or define new literals, structures, ... (↓ are used by [`Json`](Json) for example)

@docs LiteralOrStructure
@docs literal, structure, structureMap


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
import Emptiable exposing (Emptiable, fill, filled)
import Linear exposing (Direction(..))
import Morph exposing (Morph, MorphIndependently, MorphOrError, broadenWith, narrowWith, to, translate)
import N exposing (Up)
import Possibly exposing (Possibly(..))
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Stack exposing (Stacked)


{-| A supported elm literal (that don't contain other [values](#Value))
-}
type Literal
    = Unit ()
    | Float Float
    | String String


{-| elm value. Either

  - a literal that don't itself contain values
  - a structure that can itself contain values

-}
type LiteralOrStructure literal structure
    = Literal literal
    | Structure structure


{-| Any value representable in elm

Like to see a structure or literal
that can't be converted in a small amount of runtime
(for example treating tuples the same as records is just O(1))?
→ PR!

Please use to the [`MorphValue`](#MorphValue)s present in most `module`s
to construct a [`Value`](#Value),
as you can for example construct ↓ using the exposed(..) variants

    Value.List [ Value.Float 3, Value.String "huh" ]

which isn't a valid elm value

-}
type alias Value tag =
    LiteralOrStructure Literal (Structure tag)


{-| elm structure that can itself contain values
-}
type Structure tag
    = List (List (Value tag))
    | Array (Array (Value tag))
    | Record (Record tag)
    | Variant (Tagged tag)


{-| [tag](#tag)-[`Value`](#Value) pair used to represent a field or a variant
-}
type alias Tagged tag =
    RecordWithoutConstructorFunction
        { tag : tag, value : Value tag }


{-| The structure of a record that can hold [any](#Value) field value
-}
type alias Record tag =
    Emptiable (Stacked (Tagged tag)) Possibly


{-| EIther [`Index`](#Index) or [`Name`](#Name)

Used as the narrow argument of a [`MorphValue`](#MorphValue)

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

Used as the broad result of a [`MorphValue`](#MorphValue)

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
        (tagMap (Morph.mapWith tagTranslate_))
        (tagMap (Morph.broadenWith tagTranslate_))


{-| Reduce the amount of tag information of the [`Value`](#Value)
-}
tagMap :
    (tag -> tagMapped)
    -> (Value tag -> Value tagMapped)
tagMap tagChange =
    \value ->
        value |> structureMap (structureTagMap tagChange)


{-| If the [`LiteralOrStructure`](#LiteralOrStructure) is a [`Structure`](#LiteralOrStructure),
change in a given way
-}
structureMap :
    (structure -> structureMapped)
    ->
        (LiteralOrStructure literal structure
         -> LiteralOrStructure literal structureMapped
        )
structureMap structureChange =
    \value ->
        case value of
            Literal literal_ ->
                literal_ |> Literal

            Structure structure_ ->
                structure_
                    |> structureChange
                    |> Structure


structureTagMap :
    (tag -> tagMapped)
    ->
        (Structure tag
         -> Structure tagMapped
        )
structureTagMap tagChange =
    \structureAny_ ->
        case structureAny_ of
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
type alias MorphValue narrow =
    Morph.MorphIndependently
        (Value IndexOrName
         -> Result Morph.Error narrow
        )
        (narrow -> Value IndexAndName)



-- literal or structure


{-| [`Morph`](Morph#Morph) to a [`LiteralOrStructure`](#LiteralOrStructure)'s literal if possible
-}
literal :
    Morph.MorphIndependently
        (LiteralOrStructure narrowLiteral narrowStructure_
         -> Result Morph.Error narrowLiteral
        )
        (broadLiteral
         -> LiteralOrStructure broadLiteral broadStructure_
        )
literal =
    Morph.value "Literal"
        { broaden = Literal
        , narrow =
            \value ->
                case value of
                    Literal literalAny ->
                        literalAny |> Ok

                    Structure _ ->
                        "Structure" |> Err
        }


{-| [`Morph`](Morph#Morph) to a [`LiteralOrStructure`](#LiteralOrStructure)'s structure if possible
-}
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
            \value ->
                case value of
                    Structure structure_ ->
                        structure_ |> Ok

                    Literal _ ->
                        "Literal" |> Err
        }
