module Value.Morph.Internal exposing
    ( MorphValue
    , MorphValueGroupEmptiable
    , PartsError(..)
    , choiceFinish
    , composedToRecord
    , group
    , groupFinish
    , part
    , toAtom
    , toComposed
    , unit
    , variant
    )

{-| [`Morph`](Morph#Morph) to a [`AtomOrComposed`](Value#AtomOrComposed)'s atom if possible
-}

import Emptiable exposing (Emptiable)
import Morph exposing (ChoiceMorphEmptiable, MorphIndependently)
import Possibly exposing (Possibly(..))
import Stack exposing (Stacked)
import Value exposing (Atom(..), AtomOrComposed(..), Composed(..), IndexAndName, IndexOrName(..), Record, Tagged, Value)


type alias MorphValue narrow =
    Morph.MorphIndependently
        (Value IndexOrName
         -> Result Morph.Error narrow
        )
        (narrow -> Value IndexAndName)


toAtom :
    Morph.MorphIndependently
        (AtomOrComposed narrowAtom narrowComposed_
         -> Result Morph.Error narrowAtom
        )
        (broadAtom
         -> AtomOrComposed broadAtom broadComposed_
        )
toAtom =
    { description = Morph.CustomDescription
    , toBroad = Atom
    , toNarrow =
        \value ->
            case value of
                Atom atomAny ->
                    atomAny |> Ok

                Composed _ ->
                    "composed" |> Morph.DeadEnd |> Err
    }


toComposed :
    MorphIndependently
        (AtomOrComposed narrowAtom_ narrowComposed
         -> Result Morph.Error narrowComposed
        )
        (broadComposed
         -> AtomOrComposed broadAtom_ broadComposed
        )
toComposed =
    { description = Morph.CustomDescription
    , toBroad = Composed
    , toNarrow =
        \value ->
            case value of
                Composed composed_ ->
                    composed_ |> Ok

                Atom _ ->
                    "atom" |> Morph.DeadEnd |> Err
    }


unit : MorphValue ()
unit =
    { description = Morph.CustomDescription
    , toBroad = Unit
    , toNarrow =
        \value_ ->
            case value_ of
                Unit unitValue ->
                    unitValue |> Ok

                atomExceptUnit ->
                    atomExceptUnit |> Value.atomKindToString |> Morph.DeadEnd |> Err
    }
        |> Morph.over toAtom


group :
    groupNarrowAssemble
    -> MorphValueGroupEmptiable Possibly groupNarrow_ groupNarrowAssemble
group groupNarrowAssemble =
    Morph.parts ( groupNarrowAssemble, [] )


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


{-| Conclude the [`group`](#group) |> [`field`](#part) chain
-}
groupFinish :
    MorphValueGroupEmptiable Never record record
    -> MorphValue record
groupFinish =
    \groupMorphComplete ->
        groupMorphComplete
            |> partsFinish
            |> Morph.over composedToRecord
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
                                        |> Morph.DeadEnd

                                ValueError valueError ->
                                    valueError |> Stack.one |> Morph.PartsError
                        )
        , toBroad = groupMorphInProgress.toBroad
        }


composedToRecord :
    MorphIndependently
        (Composed IndexOrName -> Result Morph.Error (Record IndexOrName))
        (Record IndexAndName -> Composed IndexAndName)
composedToRecord =
    Morph.custom "record"
        { toBroad = Record
        , toNarrow =
            \composedBroad ->
                case composedBroad of
                    Record recordNarrow ->
                        recordNarrow |> Ok

                    composedExceptRecord ->
                        composedExceptRecord |> Value.composedKindToString |> Err
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
            |> Morph.try possibilityToChoice
                (variantTry
                    { name = possibilityTag
                    , index = choiceMorphSoFar.description |> Stack.length
                    }
                    possibilityMorph
                )


variantTry :
    IndexAndName
    -> MorphValue possibilityNarrow
    ->
        MorphIndependently
            (Tagged IndexOrName -> Result Morph.Error possibilityNarrow)
            (possibilityNarrow -> Tagged IndexAndName)
variantTry indexAndName possibilityMorph =
    Morph.named indexAndName.name
        { description = possibilityMorph |> Morph.description
        , toNarrow =
            variantStepNarrow
                ( indexAndName
                , Morph.toNarrow possibilityMorph
                )
        , toBroad =
            \narrowValue ->
                { tag = indexAndName
                , value = narrowValue |> Morph.toBroad possibilityMorph
                }
        }


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
            |> Morph.over variantComposed
            |> Morph.over toComposed


variantComposed :
    MorphIndependently
        (Composed IndexOrName -> Result Morph.Error (Tagged IndexOrName))
        (Tagged IndexAndName -> Composed IndexAndName)
variantComposed =
    Morph.custom "variant"
        { toNarrow =
            \value ->
                case value of
                    Variant variant_ ->
                        variant_ |> Ok

                    composedExceptVariant ->
                        composedExceptVariant
                            |> Value.composedKindToString
                            |> Err
        , toBroad = Variant
        }
