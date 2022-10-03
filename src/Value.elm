module Value exposing
    ( Value, Literal(..), Structure(..), Record, Tagged
    , MorphValue
    , RecordMorph, record, field, recordFinish
    , variant, choiceFinish
    , Name, Index, IndexOrName(..), IndexAndName
    , descriptive, compact, tagTranslate, tagMap
    , LiteralOrStructure(..)
    , literalOrStructure, literal, structure, structureMap
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


### records

@docs RecordMorph, record, field, recordFinish


### choices

variant union [`MorphValue`](#MorphValue)

  - starting from [`Morph.choice`](Morph#choice)
  - over [`variant`](#variant)
  - and completed with [`choiceFinish`](#choiceFinish)

@docs variant, choiceFinish


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
@docs literalOrStructure, literal, structure, structureMap


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
import Morph exposing (ChoiceMorph(..), GroupError, GroupMorph(..), Morph, MorphIndependently, MorphOrError, NoPart, NoTry, broadenWith, choice, in_, narrowWith, to, translate)
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

-}
type alias Value tag =
    LiteralOrStructure Literal (Structure tag)


{-| elm structure that can itself contain values

TODO include both array and list in narrow

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


type IndexOrName
    = Index Int
    | Name String


type alias Index =
    RecordWithoutConstructorFunction
        { index : Int }


type alias Name =
    RecordWithoutConstructorFunction
        { name : String }


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


{-| Change in a given way
if the [`LiteralOrStructure`](#LiteralOrStructure) is a [`Structure`](#LiteralOrStructure)
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


{-| [`Morph`](Morph#Morph) depending on whether it's a `Literal` or `Structure`
in given ways
-}
literalOrStructure :
    { literal :
        MorphIndependently
            (beforeNarrow
             -> Result (Morph.ErrorWithDeadEnd deadEnd) narrowLiteral
            )
            (literal -> broad)
    , structure :
        MorphIndependently
            (beforeNarrow
             -> Result (Morph.ErrorWithDeadEnd deadEnd) narrowStructure
            )
            (structure -> broad)
    }
    ->
        MorphIndependently
            (beforeNarrow
             ->
                Result
                    (Morph.ErrorWithDeadEnd deadEnd)
                    (LiteralOrStructure narrowLiteral narrowStructure)
            )
            (LiteralOrStructure literal structure -> broad)
literalOrStructure =
    \literalOrStructureMorphs ->
        Morph.choice
            (\variantLiteral variantStructure literalOrStructureUnion ->
                case literalOrStructureUnion of
                    Literal literalValue ->
                        variantLiteral literalValue

                    Structure structureValue ->
                        variantStructure structureValue
            )
            |> Morph.try Literal
                (Morph.to "Literal" literalOrStructureMorphs.literal)
            |> Morph.try Structure
                (Morph.to "Structure" literalOrStructureMorphs.structure)
            |> Morph.choiceFinish


literal : MorphValue Literal
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


structureKindToString : Structure tag_ -> String
structureKindToString =
    \structure_ ->
        case structure_ of
            List _ ->
                "List"

            Array _ ->
                "Array"

            Record _ ->
                "Record"

            Variant _ ->
                "Variant"



--


{-| Describe another variant value [`Morph`](#Morph) to [`Value`](#Value)

Done? → [`Value.choiceFinish`](#choiceFinish)

If a variant doesn't have any value attached, use [`Unit.value`](Unit#value)

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
            |> Value.variant ( \() -> True, "True" ) Unit.value
            |> Value.variant ( \() -> False, "False" ) Unit.value
            |> Value.choiceFinish

-}
variant :
    ( possibilityNarrow -> choiceNarrow
    , String
    )
    -> MorphValue possibilityNarrow
    ->
        (ChoiceMorph
            choiceNarrow
            (Tagged IndexOrName)
            ((possibilityNarrow
              -> Tagged IndexAndName
             )
             -> choiceBroadenFurther
            )
            Morph.Error
            NoTry
            noTryPossiblyOrNever_
         ->
            ChoiceMorph
                choiceNarrow
                (Tagged IndexOrName)
                choiceBroadenFurther
                Morph.Error
                NoTry
                noTryNever_
        )
variant ( possibilityToChoice, possibilityTag ) possibilityMorph =
    \choiceMorphSoFar ->
        choiceMorphSoFar
            |> Morph.try possibilityToChoice
                (let
                    (Morph.ChoiceMorphInProgress inProgress) =
                        choiceMorphSoFar
                 in
                 Morph.to possibilityTag
                    { description = { custom = Emptiable.empty, inner = Emptiable.empty }
                    , narrow =
                        variantStepNarrow
                            ( { name = possibilityTag
                              , index = inProgress.description |> Stack.length
                              }
                            , narrowWith possibilityMorph
                            )
                    , broaden =
                        \narrowValue ->
                            { tag =
                                { name = possibilityTag
                                , index = inProgress.description |> Stack.length
                                }
                            , value = narrowValue |> broadenWith possibilityMorph
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


{-| Conclude a [`Morph.choice`](Morph#choice) |> [`Value.variant`](#variant) chain
-}
choiceFinish :
    ChoiceMorph
        choiceNarrow
        (Tagged IndexOrName)
        (choiceNarrow -> Tagged IndexAndName)
        Morph.Error
        NoTry
        Never
    -> MorphValue choiceNarrow
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

                                structureExceptVariant ->
                                    structureExceptVariant
                                        |> structureKindToString
                                        |> Err
                    , broaden = Variant
                    }
                )
            |> Morph.over structure


{-| Start a record assembly [`MorphValue`](#MorphValue)

Continue with [`field`](#field)

    {-| `( ..., ... )` `MorphValue`

    Just use a record with descriptive names instead!

    -}
    tuple2 :
        ( MorphValue part0
        , MorphValue part1
        )
        -> MorphValue ( part0, part1 )
    tuple2 ( part0Morph, part1Morph ) =
        Morph.to "Tuple2"
            (record
                (\part0 part1 -> ( part0, part1 ))
                |> field ( Tuple.first, "part0" ) part0Morph
                |> field ( Tuple.second, "part1" ) part1Morph
                |> recordFinish
            )

    {-| `( ..., ..., ... )` `MorphValue`

    Just use a record with descriptive names instead!

    -}
    tuple3 :
        ( MorphValue part0
        , MorphValue part1
        , MorphValue part2
        )
        -> MorphValue ( part0, part1, part2 )
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
record :
    groupNarrowAssemble
    -> RecordMorph groupNarrow_ groupNarrowAssemble NoPart Possibly
record groupNarrowAssemble =
    Morph.group ( groupNarrowAssemble, Emptiable.empty )


{-| possibly incomplete [`MorphValue`](#MorphValue) step from and to a record

building:

  - start with [`record`](#record)
  - continue with [`field`](#field)
  - finish with [`recordFinish`](#recordFinish)

-}
type alias RecordMorph groupNarrow groupNarrowFurther noPartTag noPartPossiblyOrNever =
    GroupMorph
        (Record IndexOrName
         ->
            Result
                (GroupError Morph.Error)
                groupNarrowFurther
        )
        (groupNarrow -> Record IndexAndName)
        noPartTag
        noPartPossiblyOrNever


{-| Continue a group assembly [`Morph`](#Morph) to [`Value`](#Value).

  - finish with [`groupFinish`](#groupFinish)

-}
field :
    ( group -> fieldValueNarrow
    , String
    )
    -> MorphValue fieldValueNarrow
    ->
        (RecordMorph
            group
            (fieldValueNarrow -> groupNarrowFurther)
            NoPart
            noPartPossiblyOrNever_
         ->
            RecordMorph
                group
                groupNarrowFurther
                NoPart
                noPartNever_
        )
field ( accessFieldValue, fieldName ) fieldValueMorph =
    \(GroupMorphInProgress groupMorphSoFar) ->
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
                fieldNarrow tag fieldValueMorph groupMorphSoFar.narrow groupBroad
        , broaden =
            \wholeNarrow ->
                let
                    fieldValueBroad : Value IndexAndName
                    fieldValueBroad =
                        wholeNarrow
                            |> accessFieldValue
                            |> broadenWith fieldValueMorph

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
            |> GroupMorphInProgress


fieldNarrow :
    IndexAndName
    -> MorphValue fieldValueNarrow
    ->
        (Emptiable (Stacked (Tagged IndexOrName)) possiblyOrNever_
         ->
            Result
                (GroupError Morph.Error)
                (fieldValueNarrow -> groupNarrowFurther)
        )
    ->
        (Emptiable (Stacked (Tagged IndexOrName)) possiblyOrNever_
         -> Result (GroupError Morph.Error) groupNarrowFurther
        )
fieldNarrow tag fieldValueMorph groupSoFarNarrow =
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
                    (GroupError Morph.Error)
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
                case partBroad.value |> narrowWith fieldValueMorph of
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


{-| Finish the [`group`](#group) |> [`field`](#field) chain
-}
recordFinish :
    RecordMorph group group NoPart Never
    -> MorphValue group
recordFinish =
    \groupMorphComplete ->
        groupMorphComplete
            |> Morph.groupFinish
            |> Morph.over
                (Morph.value "Record"
                    { narrow =
                        \structureBroad ->
                            case structureBroad of
                                Record recordNarrow ->
                                    recordNarrow |> Ok

                                structureNotRecord ->
                                    structureNotRecord |> structureKindToString |> Err
                    , broaden = Record
                    }
                )
            |> Morph.over structure
