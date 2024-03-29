module Value.Morph exposing
    ( MorphValue
    , unit
    , MorphValueGroupEmptiable, PartsError(..)
    , group, part, groupFinish
    , variant, choiceFinish
    , eachTag, descriptive
    , toAtom, toComposed
    , bits, json
    )

{-| [Morph](Morph#Morph) your types over a [generic, `case`-able elm value](Value)

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

of course you can combine them to reach other structures,
like

    Natural.Morph.integer
        |> Morph.over Integer.Morph.value

for a [`Natural`](Natural#Natural)


## grouping

@docs MorphValueGroupEmptiable, PartsError
@docs group, part, groupFinish


## choice

variant union [`MorphValue`](#MorphValue)

  - start with [`Morph.choice`](Morph#choice)
  - add each [`|> Value.Morph.variant`](#variant)
  - complete with [`Value.Morph.choiceFinish`](#choiceFinish)

@docs variant, choiceFinish


## tag

@docs eachTag, descriptive


## spin your own

build on existing ones

    {-| `Posix` `MorphValue`
    -}
    posixValue : MorphValue Posix
    posixValue =
        Morph.named "posix"
            (Morph.oneToOne
                Time.posixToMillis
                Time.millisToPosix
                |> Morph.over Int.Morph.integer
                |> Morph.over Integer.Morph.value
            )

or define new atoms, composed structures, ... (↓ are used by [`Json`](Json) for example)

@docs toAtom, toComposed


## supported default broad formats

@docs bits, json

from [`Json`](Json)

  - [`Json.Morph.jsValueMagic`](Json-Morph#jsValueMagic)
  - [`Json.Morph.string`](Json-Morph#string)

-}

import ArraySized.Morph
import Bit exposing (Bit)
import Bit.Morph
import Bytes
import Decimal exposing (Decimal)
import Decimal.Morph
import Emptiable exposing (Emptiable)
import Int.Morph
import Integer
import Json exposing (Json)
import List.Morph
import Morph exposing (ChoiceMorphEmptiable, ErrorWithDeadEnd(..), MorphIndependently, MorphOrError, MorphRow, MorphRowIndependently)
import N.Local exposing (n32)
import N.Morph
import Natural.Morph
import Possibly exposing (Possibly(..))
import Stack exposing (Stacked)
import String.Morph
import Utf8CodePoint
import Value exposing (IndexAndName, IndexOrName(..), Record, Tagged, Value)
import Value.Morph.Internal



-- alter


{-| with readable names. Use in combination with [`eachTag`](#eachTag)

  - part tag = name given to the [`part` `MorphValue`](#part)
  - variant tag = name given to the [`variant` `MorphValue`](#variant)
  - →
      - readable by humans
      - readable by other tools
      - debuggable
      - shuffling [`Value.Morph.part`](#part) order → no change
      - renaming [`Value.Morph.part`](#part)s → breaking change

Another option to morph tags is [`Json.Morph.compact`](Json-Morph#compact)

-}
descriptive :
    MorphIndependently
        (String -> Result error_ IndexOrName)
        (IndexAndName -> String)
descriptive =
    Morph.oneToOne Name .name


{-| [`Morph.OneToOne`](Morph#OneToOne) the tags of a [`Value`](Value#Value).

  - From one side, a tag will contain all the information that is available,
    namely [`IndexAndName`](Value#IndexAndName). Your specified morph can choose one,
    maybe even formatted in some way

  - From the other side, a tag will come as what you chose/formatted.
    Now it's your jon to recreate either an [`IndexOrName`](Value#IndexOrName)

The simplest such tag morph is [`descriptive`](#descriptive), which is simply

    Morph.oneToOne Value.Name .name

We keep only the `.name` and we reconstruct any tag as a name.

A more complex example is [`Json.Morph.compact`](Json-Morph#compact)

    Morph.oneToOne
        (\tag ->
            case tag |> String.uncons of
                Just ( 'a', tagAfterA ) ->
                    case tagAfterA |> String.toInt of
                        Just index ->
                            index |> Value.Index
                        ...
        )
        (\tag -> "a" ++ (tag.index |> String.fromInt))

We keep the `.index` but prefixed with "a" to make it a field name.
When recovering, we have to drop the "a" and then extract the index again.
For all the cases where extracting fails, it's usually nice to just return them as a `Value.Name`
since we do actually know how to decode only based on name.

An example chain that uses [`eachTag`](#eachTag) to decode a project to a compact [`Json.Encode.Value`](Json-Morph#JsValueMagic):

    Project.Morph.value
        |> Morph.over (Value.Morph.eachTag Json.Morph.compact)
        |> Morph.over Value.Morph.json
        |> Morph.over Json.Morph.jsValueMagic

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
    Morph.oneToOneOn Value.tagMap Value.tagMap tagTranslate_



-- morph


{-| [`Morph`](Morph#Morph) between a given narrow value
and a [generic value](Value#Value)

The way it's set up, it allows build up tags as both [`IndexAndName`](Value#IndexAndName)
which means you can sometimes choose which one yuo want to display for a more broad format. See [`eachTag`](#eachTag)

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
toAtom :
    Morph.MorphIndependently
        (Value.AtomOrComposed narrowAtom narrowComposed_
         -> Result Morph.Error narrowAtom
        )
        (broadAtom
         -> Value.AtomOrComposed broadAtom broadComposed_
        )
toAtom =
    Value.Morph.Internal.toAtom


{-| [`Morph`](Morph#Morph) to a [`AtomOrComposed`](Value#AtomOrComposed)'s composed if possible
-}
toComposed :
    MorphIndependently
        (Value.AtomOrComposed narrowAtom_ narrowComposed
         -> Result Morph.Error narrowComposed
        )
        (broadComposed
         -> Value.AtomOrComposed broadAtom_ broadComposed
        )
toComposed =
    Value.Morph.Internal.toComposed


{-| `()` [`MorphValue`](#MorphValue)

Used in when [morphing](Value-Morph#MorphValue) a [variant](#variant)
with 0 attached values.

-}
unit : MorphValue ()
unit =
    Value.Morph.Internal.unit


{-| Start assembling multiple parts.

Continue with [`|> part`](#part) and after you're done, [`|> groupFinish`](#groupFinish).

An example translated from [`elm/json`](https://dark.elm.dmy.fr/packages/elm/json/latest/)

    import Decimal exposing (Decimal)
    import Decimal.Morph
    import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
    import String.Morph
    import Value

    type alias Cause =
        -- from lue-bird/elm-no-record-type-alias-constructor-function
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

    {-| `( ..., ... )` `MorphValue`

    Just use a record with descriptive names instead!

    -}
    tuple2 :
        ( Morph part0
        , Morph part1
        )
        -> Morph ( part0, part1 )
    tuple2 ( part0Morph, part1Morph ) =
        Morph.named "2-tuple"
            (Value.Morph.group
                (\part0 part1 -> ( part0, part1 ))
                |> Value.Morph.part ( Tuple.first, "part0" ) part0Morph
                |> Value.Morph.part ( Tuple.second, "part1" ) part1Morph
                |> Value.Morph.groupFinish
            )

    {-| `( ..., ..., ... )` `MorphValue`

    Just use a record with descriptive names instead!

    -}
    tuple3 :
        ( Morph part0
        , Morph part1
        , Morph part2
        )
        -> Morph ( part0, part1, part2 )
    tuple3 ( part0Morph, part1Morph, part2Morph ) =
        Morph.named "3-tuple"
            (Value.Morph.group
                (\part0 part1 part2 -> ( part0, part1, part2 ))
                |> Value.Morph.part ( \( part0, _, _ ) -> part0, "part0" ) part0Morph
                |> Value.Morph.part ( \( _, part1, _ ) -> part1, "part1" ) part1Morph
                |> Value.Morph.part ( \( _, _, part2 ) -> part2, "part2" ) part2Morph
                |> Value.Morph.groupFinish
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
  - continue with [`|> part`](#part)
  - finish with [`|> groupFinish`](#groupFinish)

-}
type alias MorphValueGroupEmptiable noPartPossiblyOrNever groupNarrow groupNarrowFurther =
    Morph.PartsMorphEmptiable
        noPartPossiblyOrNever
        (Value.Record IndexOrName
         -> Result PartsError groupNarrowFurther
        )
        (groupNarrow -> Value.Record IndexAndName)


{-| What can go wrong while narrowing to a [`Record`](Value#Record)
-}
type PartsError
    = TagsMissing (Emptiable (Stacked Int) Never)
    | ValueError { index : Int, error : Morph.Error }


{-| Continue a group assembly [`MorphValue`](#MorphValue).

For example to morph a name String field, add

    |> Value.Morph.part ( .name, "name" ) String.Morph.value

Once you've assembled all parts, end the builder with [`groupFinish`](#groupFinish).

-}
part :
    ( group -> partValueNarrow
    , String
    )
    -> MorphValue partValueNarrow
    ->
        (MorphValueGroupEmptiable
            noPartPossiblyOrNever_
            group
            (partValueNarrow -> groupNarrowFurther)
         ->
            MorphValueGroupEmptiable
                noPartNever_
                group
                groupNarrowFurther
        )
part ( accessPartValue, partName ) partValueMorph =
    \groupMorphSoFar ->
        let
            tag : IndexAndName
            tag =
                { index = groupMorphSoFar.description |> Stack.length
                , name = partName
                }
        in
        { description =
            groupMorphSoFar.description
                |> Stack.onTopLay
                    { tag = tag.name, value = partValueMorph.description }
        , toNarrow =
            \groupBroad ->
                partValueNarrow tag partValueMorph groupMorphSoFar.toNarrow groupBroad
        , toBroad =
            \wholeNarrow ->
                let
                    partValueBroad : Value IndexAndName
                    partValueBroad =
                        wholeNarrow
                            |> accessPartValue
                            |> Morph.toBroad partValueMorph

                    partBroad : Tagged IndexAndName
                    partBroad =
                        { tag = tag
                        , value = partValueBroad
                        }
                in
                wholeNarrow
                    |> groupMorphSoFar.toBroad
                    |> (::) partBroad
        }


partValueNarrow :
    IndexAndName
    -> MorphValue partValueNarrow
    ->
        (List (Tagged IndexOrName)
         -> Result PartsError (partValueNarrow -> groupNarrowFurther)
        )
    ->
        (List (Tagged IndexOrName)
         -> Result PartsError groupNarrowFurther
        )
partValueNarrow tag partValueMorph groupSoFarNarrow =
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
                    (partValueNarrow -> groupNarrowFurther)
            wholeAssemblyResult =
                groupBroad |> groupSoFarNarrow
        in
        case groupBroad |> List.filter (.tag >> matches) of
            partBroad :: _ ->
                case partBroad.value |> Morph.toNarrow partValueMorph of
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


{-| Conclude the [`Value.Morph.group`](#group) [`|> Value.Morph.part`](#part) builder
-}
groupFinish :
    MorphValueGroupEmptiable Never record record
    -> MorphValue record
groupFinish =
    \groupMorphComplete ->
        groupMorphComplete
            |> partsFinish
            |> Morph.over Value.Morph.Internal.composedToRecord
            |> Morph.over toComposed


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


{-| Describe another variant [`MorphValue`](#MorphValue)

When you're done, end the builder with [`|> Value.Morph.choiceFinish`](#choiceFinish)

    type User
        = Guest GuestData
        | SignedIn SignedInData

    value : MorphValue User
    value =
        Morph.choice
            (\variantGuest variantSignedIn user ->
                case user of
                    Guest guest ->
                        variantGuest guest

                    SignedIn signedIn ->
                        variantSignedIn signedIn
            )
            |> Value.Morph.variant ( Guest, "guest" ) guestValue
            |> Value.Morph.variant ( SignedIn, "signed in" ) signedInValue
            |> Value.Morph.choiceFinish

where `guestValue : MorphValue GuestData` and `signedInValue : MorphValue SignedInData`.

If a variant has no attached thing, use [`Value.Morph.unit`](#unit)

    signValue : MorphValue Sign
    signValue =
        Morph.choice
            (\positive negative sign ->
                case sign of
                    Positive ->
                        positive ()

                    Negative ->
                        negative ()
            )
            |> Value.Morph.variant ( \() -> Positive, "Positive" ) Value.Morph.unit
            |> Value.Morph.variant ( \() -> Negative, "Negative" ) Value.Morph.unit
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
             -> choiceToBroadFurther
            )
            Morph.Error
         ->
            ChoiceMorphEmptiable
                noTryNever_
                choiceNarrow
                (Tagged IndexOrName)
                choiceToBroadFurther
                Morph.Error
        )
variant ( possibilityToChoice, possibilityTag ) possibilityMorph =
    \choiceMorphSoFar ->
        choiceMorphSoFar
            |> Value.Morph.Internal.variant ( possibilityToChoice, possibilityTag ) possibilityMorph


{-| Conclude a [`Morph.choice`](Morph#choice) [`|> Value.Morph.variant`](#variant) builder.
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
        choiceMorphComplete |> Value.Morph.Internal.choiceFinish


{-| [`Morph.OneToOne`](Morph#OneToOne) from [`Json`](Json#Json)

[Inverse](Morph#invert) of [`Json.Morph.value`](Json-Morph#value)

-}
json : MorphOrError (Value.Value String) Json error_
json =
    Morph.oneToOne Json.toValue Json.fromValue


{-| [`MorphRow`](Morph#MorphRow) from [`Bit`](https://dark.elm.dmy.fr/packages/lue-bird/elm-bits/latest/Bit)s.

Example chain converting to [`Bytes`](https://dark.elm.dmy.fr/packages/elm/bytes/latest/)

    yourTypeMorphValue
        |> Morph.overRow Value.Morph.bits
        |> Morph.over List.Morph.bytes

-}
bits : MorphRowIndependently (Value IndexOrName) (Value IndexAndName) Bit
bits =
    eachTag (Morph.oneToOne Value.Index .index)
        |> Morph.overRow intTaggedBits


intTaggedBits : MorphRow (Value Int) Bit
intTaggedBits =
    Morph.recursive "generic value"
        (\step ->
            Morph.choice
                (\atomVariant composedVariant atomOrComposed ->
                    case atomOrComposed of
                        Value.Atom atomValue ->
                            atomVariant atomValue

                        Value.Composed composedValue ->
                            composedVariant composedValue
                )
                |> Morph.rowTry Value.Atom
                    (Morph.narrow (\atom_ -> atom_)
                        |> Morph.match (Bit.Morph.only Bit.O |> Morph.one)
                        |> Morph.grab (\atom_ -> atom_) atomBits
                    )
                |> Morph.rowTry Value.Composed
                    (Morph.narrow (\composed_ -> composed_)
                        |> Morph.match (Bit.Morph.only Bit.I |> Morph.one)
                        |> Morph.grab (\composed_ -> composed_) (composedBits step)
                    )
                |> Morph.choiceFinish
        )


listUnnamedBits :
    MorphRowIndependently beforeToBroad narrow Bit
    -> MorphRowIndependently (List beforeToBroad) (List narrow) Bit
listUnnamedBits step =
    List.Morph.arraySized
        |> Morph.overRow
            (ArraySized.Morph.exactlyWith
                (N.Morph.natural
                    |> Morph.overRow (Natural.Morph.bits Bytes.BE n32)
                )
                step
            )


stringBits : MorphRow String Bit
stringBits =
    Morph.named "string"
        (String.Morph.list
            |> Morph.overRow (listUnnamedBits Utf8CodePoint.charBits)
        )


atomBits : MorphRow Value.Atom Bit
atomBits =
    Morph.choice
        (\unitVariant numberVariant stringVariant atomChoice ->
            case atomChoice of
                Value.Unit unitValue ->
                    unitVariant unitValue

                Value.Number numberValue ->
                    numberVariant numberValue

                Value.String stringValue ->
                    stringVariant stringValue
        )
        |> Morph.rowTry Value.Unit
            (Morph.narrow ()
                |> Morph.match (Bit.Morph.only Bit.O |> Morph.one)
                |> Morph.match (Bit.Morph.only Bit.O |> Morph.one)
            )
        |> Morph.rowTry Value.Number
            (Morph.narrow (\number_ -> number_)
                |> Morph.match (Bit.Morph.only Bit.O |> Morph.one)
                |> Morph.match (Bit.Morph.only Bit.I |> Morph.one)
                |> Morph.grab (\number_ -> number_) numberBits
            )
        |> Morph.rowTry Value.String
            (Morph.narrow (\string_ -> string_)
                |> Morph.match (Bit.Morph.only Bit.I |> Morph.one)
                |> Morph.grab (\string_ -> string_) stringBits
            )
        |> Morph.choiceFinish


numberBits : MorphRow Decimal Bit
numberBits =
    Morph.named "number" Decimal.Morph.bitsVariableCount


composedBits :
    MorphRow (Value Int) Bit
    -> MorphRow (Value.Composed Int) Bit
composedBits step =
    Morph.choice
        (\listVariant recordVariant taggedVariant composedChoice ->
            case composedChoice of
                Value.List listValue ->
                    listVariant listValue

                Value.Record recordValue ->
                    recordVariant recordValue

                Value.Variant taggedValue ->
                    taggedVariant taggedValue
        )
        |> Morph.rowTry Value.List (listBits step)
        |> Morph.rowTry Value.Record (recordBits step)
        |> Morph.rowTry Value.Variant (variantBits step)
        |> Morph.choiceFinish


listBits :
    MorphRowIndependently beforeToBroad narrow Bit
    -> MorphRowIndependently (List beforeToBroad) (List narrow) Bit
listBits step =
    Morph.named "list" (listUnnamedBits step)


taggedBits : MorphRow (Value Int) Bit -> MorphRow (Tagged Int) Bit
taggedBits step =
    Morph.narrow (\tag value -> { tag = tag, value = value })
        |> Morph.grab .tag
            (Morph.named "tag"
                (Int.Morph.integer
                    -- to save bits
                    |> Morph.over (Morph.oneToOne Integer.fromNatural Integer.absolute)
                    |> Morph.overRow Natural.Morph.bitsVariableCount
                )
            )
        |> Morph.grab .value (Morph.named "value" step)


recordBits : MorphRow (Value Int) Bit -> MorphRow (Record Int) Bit
recordBits step =
    Morph.named "record"
        (listUnnamedBits (taggedBits step))


variantBits : MorphRow (Value Int) Bit -> MorphRow (Tagged Int) Bit
variantBits step =
    Morph.named "variant" (taggedBits step)
