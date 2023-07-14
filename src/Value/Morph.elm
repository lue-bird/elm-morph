module Value.Morph exposing
    ( MorphValue
    , unit
    , MorphValueGroupEmptiable, PartsError(..)
    , group, part, groupFinish
    , variant, choiceFinish
    , descriptive, compact, eachTag
    , atom, composed
    , bits
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

of course you can combine them to reach other structures,
like

    Natural.Morph.integer
        |> Morph.over Integer.Morph.value

for a [`Natural`](Natural#Natural)


### grouping

@docs MorphValueGroupEmptiable, PartsError
@docs group, part, groupFinish


### choice

variant union [`MorphValue`](#MorphValue)

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


## supported default broad formats

[`Json`](Json)

  - [`Json.Morph.jsValueMagic`](Json-Morph#jsValueMagic)
  - [`Json.Morph.string`](Json-Morph#string)

@docs bits

-}

import ArraySized.Morph
import Bit exposing (Bit)
import Bit.Morph
import Bytes
import Decimal exposing (Decimal)
import Decimal.Morph
import Emptiable exposing (Emptiable)
import List.Morph
import Morph exposing (ChoiceMorphEmptiable, ErrorWithDeadEnd(..), MorphIndependently, MorphRow, MorphRowIndependently)
import N.Local exposing (n32)
import N.Morph
import Natural.Morph
import Possibly exposing (Possibly(..))
import Stack exposing (Stacked)
import String.Morph
import Utf8CodePoint
import Value exposing (Index, IndexAndName, IndexOrName(..), Name, Record, Tagged, Value)
import Value.Morph.Internal



-- alter


{-| with readable names. Use in combination with [`eachTag`](#eachTag)

  - field tag = name given to the [`part` `MorphValue`](#part)
  - variant tag = name given to the [`variant` `MorphValue`](#variant)
  - →
      - readable by humans
      - readable by other tools
      - debuggable
      - shuffling fields [`MorphValue`](#MorphValue) order → no change
      - renaming fields → breaking change
      - not [`compact`](#compact)

-}
descriptive :
    MorphIndependently
        (Name -> Result error_ IndexOrName)
        (IndexAndName -> Name)
descriptive =
    Morph.oneToOne
        (\tag -> tag.name |> Name)
        (\tag -> { name = tag.name })


{-| With compact indexes. Use in combination with [`eachTag`](#eachTag)

  - part tag = [`part` `MorphValue`](#part) index index in the builder
  - variant tag = [`variant` `MorphValue`](#variant) index in the builder
  - →
      - not [`descriptive`](#descriptive)

-}
compact :
    MorphIndependently
        (Index -> Result error_ IndexOrName)
        (IndexAndName -> Index)
compact =
    Morph.oneToOne
        (\tag -> tag.index |> Index)
        (\tag -> { index = tag.index })


{-| [`Morph.OneToOne`](Morph#OneToOne) from a [`Value`](Value#Value),
reducing the amount of tag information in both directions

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
    Morph.oneToOneOn ( Value.tagMap, Value.tagMap ) tagTranslate_



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
        (Value.AtomOrComposed narrowAtom narrowComposed_
         -> Result Morph.Error narrowAtom
        )
        (broadAtom
         -> Value.AtomOrComposed broadAtom broadComposed_
        )
atom =
    Value.Morph.Internal.atom


{-| [`Morph`](Morph#Morph) to a [`AtomOrComposed`](Value#AtomOrComposed)'s composed if possible
-}
composed :
    MorphIndependently
        (Value.AtomOrComposed narrowAtom_ narrowComposed
         -> Result Morph.Error narrowComposed
        )
        (broadComposed
         -> Value.AtomOrComposed broadAtom_ broadComposed
        )
composed =
    Value.Morph.Internal.composed


{-| `()` [`MorphValue`](#MorphValue)

Used in when [morphing](Value-Morph#MorphValue) a [variant](#variant)
with 0 attached values.

-}
unit : MorphValue ()
unit =
    Value.Morph.Internal.unit


{-| Start assembling multiple part [`MorphValue`](#MorphValue)s

Continue with [`part`](#part)

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


{-| What can go wrong while narrowing to a [`Record`](Value#Record)
-}
type PartsError
    = TagsMissing (Emptiable (Stacked Int) Never)
    | ValueError { index : Int, error : Morph.Error }


{-| Continue a group assembly [`MorphValue`](#MorphValue).

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


{-| Conclude the [`group`](#group) |> [`field`](#part) chain
-}
groupFinish :
    MorphValueGroupEmptiable Never record record
    -> MorphValue record
groupFinish =
    \groupMorphComplete ->
        groupMorphComplete
            |> partsFinish
            |> Morph.over Value.Morph.Internal.recordComposed
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


{-| Describe another variant [`MorphValue`](#MorphValue)

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
            |> Value.Morph.Internal.variant ( possibilityToChoice, possibilityTag ) possibilityMorph


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
        choiceMorphComplete |> Value.Morph.Internal.choiceFinish


{-| [`MorphRow`](Morph#MorphRow) from `Bit`s to a [`Value`](Value#Value).

Since it takes a `Value String`, you use it
with [`compact`](#compact) or [`descriptive`](#descriptive)
to chain it with a [`MorphValue`](#MorphValue):

    yourTypeMorphValue
        |> Morph.over (Value.Morph.eachTag Value.Morph.compact)
        |> Morph.overRow Value.Morph.bits

-}
bits : MorphRow (Value String) Bit
bits =
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
                |> Morph.tryRow Value.Atom
                    (Morph.succeed (\atom_ -> atom_)
                        |> Morph.match (Bit.Morph.only Bit.O |> Morph.one)
                        |> Morph.grab (\atom_ -> atom_) atomBits
                    )
                |> Morph.tryRow Value.Composed
                    (Morph.succeed (\composed_ -> composed_)
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
        |> Morph.tryRow Value.Unit
            (Morph.succeed ()
                |> Morph.match (Bit.Morph.only Bit.O |> Morph.one)
                |> Morph.match (Bit.Morph.only Bit.O |> Morph.one)
            )
        |> Morph.tryRow Value.Number
            (Morph.succeed (\number_ -> number_)
                |> Morph.match (Bit.Morph.only Bit.O |> Morph.one)
                |> Morph.match (Bit.Morph.only Bit.I |> Morph.one)
                |> Morph.grab (\number_ -> number_) numberBits
            )
        |> Morph.tryRow Value.String
            (Morph.succeed (\string_ -> string_)
                |> Morph.match (Bit.Morph.only Bit.I |> Morph.one)
                |> Morph.grab (\string_ -> string_) stringBits
            )
        |> Morph.choiceFinish


numberBits : MorphRow Decimal Bit
numberBits =
    Morph.named "number" Decimal.Morph.bitsVariableCount


composedBits :
    (() -> MorphRow (Value String) Bit)
    -> MorphRow (Value.Composed String) Bit
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
        |> Morph.tryRow Value.List (listBits (step ()))
        |> Morph.tryRow Value.Record (recordBits (step ()))
        |> Morph.tryRow Value.Variant (variantBits (step ()))
        |> Morph.choiceFinish


listBits :
    MorphRowIndependently beforeToBroad narrow Bit
    -> MorphRowIndependently (List beforeToBroad) (List narrow) Bit
listBits step =
    Morph.named "list" (listUnnamedBits step)


taggedBits :
    MorphRow (Value String) Bit
    -> MorphRow (Tagged String) Bit
taggedBits step =
    Morph.succeed (\tag value -> { tag = tag, value = value })
        |> Morph.grab .tag (Morph.named "tag" stringBits)
        |> Morph.grab .value (Morph.named "value" step)


recordBits :
    MorphRow (Value String) Bit
    -> MorphRow (Record String) Bit
recordBits step =
    Morph.named "record"
        (listUnnamedBits (taggedBits step))


variantBits :
    MorphRow (Value String) Bit
    -> MorphRow (Tagged String) Bit
variantBits step =
    Morph.named "variant" (taggedBits step)
