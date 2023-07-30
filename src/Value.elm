module Value exposing
    ( Value, Atom(..), Composed(..), Record, Tagged
    , atomKindToString, composedKindToString
    , Name, Index, IndexOrName(..), IndexAndName
    , tagMap
    , AtomOrComposed(..)
    , composedMap, atomMap
    )

{-| Generic, `case`-able elm value

json encoders/decoders are pretty low-level which makes them mildly unpleasant for serialization,
explicitly describing **how** to serialize individual data types.
Data that has same shape could at the low level be coded differently,
which means you have to spell it out explicitly each time.

Switching to a different format would also be a lot of work;
some low-level primitives like bools might not be supported etc.

This module has all the types and operations for these generic [`Value`](#Value)s
while [`Value.Morph`](Value-Morph) and most other modules contain morphs to convert your types.

@docs Value, Atom, Composed, Record, Tagged
@docs atomKindToString, composedKindToString


## tag

@docs Name, Index, IndexOrName, IndexAndName
@docs tagMap


## an abstract concept: atom or composed

@docs AtomOrComposed
@docs composedMap, atomMap


## oh look! other projects do similar things

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

import Decimal exposing (Decimal)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)


{-| An elm literal (that doesn't itself contain other [values](#Value))
-}
type Atom
    = Unit ()
    | Number Decimal
    | String String


{-| Either

  - an atom that don't itself contain values
  - a composed structure that can itself contain recursive values

-}
type AtomOrComposed atom composed
    = Atom atom
    | Composed composed


{-| Generic representation of any value representable in elm

Use the [`MorphValue`](Value-Morph#MorphValue)s present in most `module`s of this package
to convert a [`Value`](#Value).

-}
type alias Value tag =
    AtomOrComposed Atom (Composed tag)


{-| elm structure that can itself contain values

Other kinds of structures should be converted to one of these.
A tuple for example can be represented as a record.

Note: To not have overly complex recursive-ish types,
a [`Value.Composed`](#Composed) _can_ describe an invalid elm value,
for example

    List [ Unit (), String "huh" ]

-}
type Composed tag
    = List (List (Value tag))
    | Record (Record tag)
    | Variant (Tagged tag)


{-| [tag](#tag)-[`Value`](#Value) pair used to represent a field or a variant
-}
type alias Tagged tag =
    RecordWithoutConstructorFunction
        { tag : tag, value : Value tag }


{-| The [structure](#Composed) of a record which can hold multiple [tagged field](#Tagged) [value](#Value)s
-}
type alias Record tag =
    List (Tagged tag)


{-| Either [`Index`](#Index) or [`Name`](#Name)

Used as the narrow argument of a [`MorphValue`](Value-Morph#MorphValue)

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

Used as the broad result of a [`MorphValue`](Value-Morph#MorphValue)

-}
type alias IndexAndName =
    RecordWithoutConstructorFunction
        { index : Int, name : String }



-- alter


{-| Reduce the amount of tag information of the [`Value`](#Value)
-}
tagMap :
    (tag -> tagMapped)
    -> (Value tag -> Value tagMapped)
tagMap tagChange =
    \value ->
        value |> composedMap (composedTagMap tagChange)


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


{-| If the [`AtomOrComposed`](#AtomOrComposed) is an [`Atom`](#AtomOrComposed),
change in a given way
-}
atomMap :
    (atom -> atomMapped)
    ->
        (AtomOrComposed atom composed
         -> AtomOrComposed atomMapped composed
        )
atomMap atomChange =
    \value ->
        case value of
            Atom atom_ ->
                atom_ |> atomChange |> Atom

            Composed composed_ ->
                composed_ |> Composed


{-| Describe the type of [`Atom`](#Atom)
-}
atomKindToString : Atom -> String
atomKindToString =
    \atom_ ->
        case atom_ of
            Unit () ->
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

            Record _ ->
                "Record"

            Variant _ ->
                "Variant"
