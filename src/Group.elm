module Group exposing
    ( MorphNoPart
    , toFrom, part, finish
    , grab, skip
    , MorphValueNoPart, value, fieldValue, finishValue
    )

{-| [`Morph`](Morph#Morph) a thing that can be split into smaller pieces

@docs MorphNoPart


## [`Morph`]

@docs toFrom, part, finish


## [`MorphRow`](Morph#MorphRow)

Start with [`Morph.succeed`](Morph#succeed), then chain

@docs grab, skip


## [`MorphValue`](Value#MorphValue)

@docs MorphValueNoPart, value, fieldValue, finishValue

-}

import ArraySized
import Emptiable exposing (Emptiable)
import Linear exposing (Direction(..))
import Morph
import Possibly exposing (Possibly)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Stack exposing (Stacked)
import Value
import Value.PackageInternal



-- Morph


{-| [`Morph`](#Morph) on groups in progress.
Start with [`group`](#group), complete with [`part`](#part), finally [`groupFinish`](#groupFinish)
-}
type alias MorphNoPart noPartPossiblyOrNever narrow broaden =
    RecordWithoutConstructorFunction
        { description :
            -- parts
            Emptiable (Stacked Morph.Description) noPartPossiblyOrNever
        , narrow : narrow
        , broaden : broaden
        }


{-| Word in a [`GroupMorph`](#GroupMorph) in progress. For example

    choiceFinish :
        Value.GroupMorph (N (In N0 N9)) Char (N (In N0 N9) -> Char) NoPart Never
        -> Morph (N (In N0 N9)) Char

-}
type NoPart
    = NoPartTag Never


{-| Assemble a group from narrow and broad [`part`](#part)s

Use [`Group.toFrom`](Group#toFrom)
when each broad, narrow [`part`](#part) always has their respective counterpart

    ( "4", "5" )
        |> Morph.narrowTo
            (Group.toFrom
                ( \x y -> { x = x, y = y }
                , \x y -> ( x, y )
                )
                |> Group.part ( .x, Tuple.first )
                    (Integer.Morph.toInt
                        |> Morph.overRow Integer.Morph.rowChar
                        |> Morph.rowFinish
                    )
                |> Group.part ( .y, Tuple.second )
                    (Integer.Morph.toInt
                        |> Morph.over (Integer.Morph.bitSizeAtMost n32)
                        |> Morph.overRow Integer.Morph.rowChar
                        |> Morph.rowFinish
                    )
                |> Group.finish
            )
    --> Ok { x = 4, y = 5 }

-}
toFrom :
    ( narrowAssemble
    , broadAssemble
    )
    ->
        MorphNoPart
            Possibly
            (broad_
             -> Result error_ narrowAssemble
            )
            (groupNarrow_ -> broadAssemble)
toFrom ( narrowAssemble, broadAssemble ) =
    { description = Emptiable.empty
    , narrow = \_ -> narrowAssemble |> Ok
    , broaden = \_ -> broadAssemble
    }


{-| The [`Morph`](#Morph) of the next part in a [`group`](#group).

    Group.build
        ( \nameFirst nameLast email ->
            { nameFirst = nameFirst, nameLast = nameLast, email = email }
        , \nameFirst nameLast email ->
            { nameFirst = nameFirst, nameLast = nameLast, email = email }
        )
        |> Group.part ( .nameFirst, .nameFirst ) remain
        |> Group.part ( .nameLast, .nameLast ) remain
        |> Group.part ( .email, .email ) emailMorph
        |> Group.finish

-}
part :
    ( groupNarrow -> partNarrow
    , groupBroad -> partBroad
    )
    -> Morph.MorphOrError partNarrow partBroad partError
    ->
        (MorphNoPart
            noPartPossiblyOrNever_
            (groupBroad
             ->
                Result
                    (Morph.PartsError partError)
                    (partNarrow -> groupNarrowFurther)
            )
            (groupNarrow -> (partBroad -> groupBroadenFurther))
         ->
            MorphNoPart
                noPartNever_
                (groupBroad
                 ->
                    Result
                        (Morph.PartsError partError)
                        groupNarrowFurther
                )
                (groupNarrow -> groupBroadenFurther)
        )
part ( narrowPartAccess, broadPartAccess ) partMorph =
    \groupMorphSoFar ->
        { description =
            groupMorphSoFar.description
                |> Stack.onTopLay partMorph.description
        , narrow =
            groupMorphSoFar.narrow
                |> narrowPart
                    (groupMorphSoFar.description |> Stack.length)
                    broadPartAccess
                    (Morph.narrowTo partMorph)
        , broaden =
            groupMorphSoFar.broaden
                |> broadenPart narrowPartAccess (Morph.broadenFrom partMorph)
        }


broadenPart :
    (groupNarrow -> partNarrow)
    -> (partNarrow -> partBroad)
    ->
        ((groupNarrow -> (partBroad -> groupBroadenFurther))
         -> (groupNarrow -> groupBroadenFurther)
        )
broadenPart narrowPartAccess broadenPartMorph =
    \groupMorphSoFarBroaden ->
        \groupNarrow ->
            (groupNarrow |> groupMorphSoFarBroaden)
                (groupNarrow
                    |> narrowPartAccess
                    |> broadenPartMorph
                )


narrowPart :
    Int
    -> (groupBroad -> partBroad)
    -> (partBroad -> Result partError partNarrow)
    ->
        ((groupBroad
          ->
            Result
                (Morph.PartsError partError)
                (partNarrow -> groupNarrowFurther)
         )
         ->
            (groupBroad
             ->
                Result
                    (Morph.PartsError partError)
                    groupNarrowFurther
            )
        )
narrowPart index broadPartAccess narrowPartMorph =
    \groupMorphSoFarNarrow ->
        \groupBroad ->
            let
                narrowPartOrError : Result partError partNarrow
                narrowPartOrError =
                    groupBroad
                        |> broadPartAccess
                        |> narrowPartMorph
            in
            case ( groupBroad |> groupMorphSoFarNarrow, narrowPartOrError ) of
                ( Ok groupMorphSoFarEat, Ok partNarrow ) ->
                    groupMorphSoFarEat partNarrow |> Ok

                ( Ok _, Err partError ) ->
                    { index = index, error = partError }
                        |> Stack.one
                        |> Err

                ( Err partsSoFarErrors, Ok _ ) ->
                    partsSoFarErrors |> Err

                ( Err partsSoFarErrors, Err partError ) ->
                    partsSoFarErrors
                        |> Stack.onTopLay { index = index, error = partError }
                        |> Err


{-| Conclude a [`Group.build`](#group) |> [`Group.part`](#part) chain
-}
finish :
    MorphNoPart
        Never
        (beforeNarrow
         ->
            Result
                (Morph.PartsError (Morph.ErrorWithDeadEnd deadEnd))
                narrowed
        )
        (beforeBroaden -> broadened)
    ->
        Morph.MorphIndependently
            (beforeNarrow
             -> Result (Morph.ErrorWithDeadEnd deadEnd) narrowed
            )
            (beforeBroaden -> broadened)
finish =
    \groupMorphInProgress ->
        { description =
            case groupMorphInProgress.description |> Emptiable.fill of
                Stack.TopBelow ( part0, part1 :: parts2Up ) ->
                    { inner =
                        ArraySized.l2 part0 part1
                            |> ArraySized.attachMin Up
                                (parts2Up |> ArraySized.fromList)
                            |> Morph.Group
                            |> Emptiable.filled
                    , custom = Emptiable.empty
                    }

                Stack.TopBelow ( partOnly, [] ) ->
                    partOnly
        , narrow =
            \broad ->
                broad
                    |> groupMorphInProgress.narrow
                    |> Result.mapError Morph.Parts
        , broaden = groupMorphInProgress.broaden
        }



-- MorphRow


{-| Take what we get from [converting](#MorphRow) the next section
and channel it back up to the [`Morph.succeed`](#Morph.succeed) grouping
-}
grab :
    (groupNarrow -> partNextNarrow)
    -> Morph.MorphRow partNextNarrow broadElement
    ->
        (Morph.MorphRowIndependently
            groupNarrow
            (partNextNarrow -> groupNarrowFurther)
            broadElement
         ->
            Morph.MorphRowIndependently
                groupNarrow
                groupNarrowFurther
                broadElement
        )
grab partAccess grabbedNextMorphRow =
    \groupMorphRowSoFar ->
        { description = groupMorphRowSoFar |> Morph.description
        , narrow =
            \broad_ ->
                broad_
                    |> Morph.narrowTo groupMorphRowSoFar
                    |> Result.andThen
                        (\result ->
                            result.broad
                                |> Morph.narrowTo grabbedNextMorphRow
                                |> Result.map
                                    (\nextParsed ->
                                        { narrow = result.narrow nextParsed.narrow
                                        , broad = nextParsed.broad
                                        }
                                    )
                        )
        , broaden =
            \groupNarrow ->
                groupNarrow
                    |> partAccess
                    |> grabbedNextMorphRow.broaden
                    |> Stack.attach Down
                        (groupNarrow
                            |> groupMorphRowSoFar.broaden
                        )
        }


{-| Require values to be matched next to continue but ignore the result.

    import String.Morph exposing (text)
    import Morph exposing (Morph.succeed, atLeast, take, drop)

    -- parse a simple email, but we're only interested in the username
    "user@example.com"
        |> Text.narrowTo
            (Morph.succeed (\userName -> { username = userName })
                |> grab .username (atLeast n1 aToZ)
                |> skip (one '@')
                |> skip
                    (Text.fromList
                        |> Morph.overRow (atLeast n1 aToZ)
                        |> broad "example"
                    )
                |> skip (text ".com")
            )
    --> Ok { username = "user" }

[`broad`](#broad) `... |>` [`Morph.overRow`](MorphRow#over) is cool:
when multiple kinds of input can be dropped,
it allows choosing a default possibility for building.

-}
skip :
    Morph.MorphRow () broadElement
    ->
        (Morph.MorphRowIndependently groupNarrow narrow broadElement
         -> Morph.MorphRowIndependently groupNarrow narrow broadElement
        )
skip ignoredNext =
    \groupMorphRowSoFar ->
        { description = groupMorphRowSoFar |> Morph.description
        , narrow =
            \broad_ ->
                broad_
                    |> Morph.narrowTo groupMorphRowSoFar
                    |> Result.andThen
                        (\result ->
                            result.broad
                                |> Morph.narrowTo ignoredNext
                                |> Result.map
                                    (\nextParsed ->
                                        { narrow = result.narrow
                                        , broad = nextParsed.broad
                                        }
                                    )
                        )
        , broaden =
            \groupNarrow ->
                (() |> ignoredNext.broaden)
                    |> Stack.attach Down
                        (groupNarrow |> groupMorphRowSoFar.broaden)
        }



-- MorphValue


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
value :
    groupNarrowAssemble
    -> MorphValueNoPart Possibly groupNarrow_ groupNarrowAssemble
value groupNarrowAssemble =
    toFrom ( groupNarrowAssemble, Emptiable.empty )


{-| possibly incomplete step from and to a [`Value.Record`](Value#Record)

building:

  - start with [`Group.value`](#value)
  - continue with [`Group.fieldValue`](#fieldValue)
  - finish with [`Group.finishValue`](#finishValue)

-}
type alias MorphValueNoPart noPartPossiblyOrNever groupNarrow groupNarrowFurther =
    MorphNoPart
        noPartPossiblyOrNever
        (Value.Record Value.IndexOrName
         ->
            Result
                (Morph.PartsError Morph.Error)
                groupNarrowFurther
        )
        (groupNarrow -> Value.Record Value.IndexAndName)


{-| Continue a group assembly [`Morph`](#Morph) to [`Value`](#Value).

  - finish with [`groupFinish`](#groupFinish)

-}
fieldValue :
    ( group -> fieldValueNarrow
    , String
    )
    -> Value.MorphValue fieldValueNarrow
    ->
        (MorphValueNoPart
            noPartPossiblyOrNever_
            group
            (fieldValueNarrow -> groupNarrowFurther)
         ->
            MorphValueNoPart
                noPartNever_
                group
                groupNarrowFurther
        )
fieldValue ( accessFieldValue, fieldName ) fieldValueMorph =
    \groupMorphSoFar ->
        let
            tag : Value.IndexAndName
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
                    fieldValueBroad : Value.Value Value.IndexAndName
                    fieldValueBroad =
                        wholeNarrow
                            |> accessFieldValue
                            |> Morph.broadenFrom fieldValueMorph

                    fieldBroad : Value.Tagged Value.IndexAndName
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
    Value.IndexAndName
    -> Value.MorphValue fieldValueNarrow
    ->
        (Emptiable (Stacked (Value.Tagged Value.IndexOrName)) possiblyOrNever_
         ->
            Result
                (Morph.PartsError Morph.Error)
                (fieldValueNarrow -> groupNarrowFurther)
        )
    ->
        (Emptiable (Stacked (Value.Tagged Value.IndexOrName)) possiblyOrNever_
         -> Result (Morph.PartsError Morph.Error) groupNarrowFurther
        )
partValueNarrow tag fieldValueMorph groupSoFarNarrow =
    let
        matches : Value.IndexOrName -> Bool
        matches =
            \tagIndexOrName ->
                case tagIndexOrName of
                    Value.Index index ->
                        index == tag.index

                    Value.Name name ->
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


{-| Conclude the [`Group.value`](#value) |> [`Group.fieldValue`](#fieldValue) chain
-}
finishValue :
    MorphValueNoPart Never group group
    -> Value.MorphValue group
finishValue =
    \groupMorphComplete ->
        groupMorphComplete
            |> finish
            |> Morph.over
                (Morph.value "Record"
                    { narrow =
                        \composedBroad ->
                            case composedBroad of
                                Value.Record recordNarrow ->
                                    recordNarrow |> Ok

                                composedExceptRecord ->
                                    composedExceptRecord
                                        |> Value.PackageInternal.composedKindToString
                                        |> Err
                    , broaden = Value.Record
                    }
                )
            |> Morph.over Value.composed
