module Choice exposing
    ( MorphNoTry, between
    , try, finish
    , MorphRowNoTry, tryRow, finishRow
    , tryValue, finishValue
    )

{-| [`Morph`](Morph#Morph) a tagged union `type`

@docs MorphNoTry, between


## [`Morph`](Morph#Morph)

@docs try, finish


## [`MorphRow`](Morph#MorphRow)

@docs MorphRowNoTry, tryRow, finishRow


## [`MorphValue`](Value#MorphValue)

@docs tryValue, finishValue

-}

import ArraySized
import Emptiable exposing (Emptiable)
import Linear exposing (Direction(..))
import Morph
import Possibly exposing (Possibly)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Stack exposing (Stacked)
import Util exposing (restoreTry)
import Value
import Value.PackageInternal



-- Morph


{-| Possibly incomplete [`Morph`](#Morph) to and from a Choice.between.
See [`Choice.between`](#Choice.between), [`possibility`](#try), [`choiceFinish`](#choiceFinish)
-}
type alias MorphNoTry noTryPossiblyOrNever choiceNarrow choiceBroad choiceBroaden error =
    RecordWithoutConstructorFunction
        { description :
            -- possibilities
            Emptiable (Stacked Morph.Description) noTryPossiblyOrNever
        , narrow :
            choiceBroad
            ->
                Result
                    (-- tries
                     Emptiable (Stacked error) noTryPossiblyOrNever
                    )
                    choiceNarrow
        , broaden : choiceBroaden
        }


{-| Word in an incomplete morph in progress. For example

    Choice.finish :
        Choice.MorphNoTry
            Never
            (N (InFixed N0 N9)))
            Char
            (N (InFixed N0 N9)) -> Char)
        -> Morph (N (InFixed N0 N9)) Char

-}
type NoTry
    = NoTryTag Never


{-| Discriminate into possibilities

    {-| Invisible spacing character
    -}
    type Blank
        = Space
        | Tab
        | Return Line.Return
        | FormFeed

    blankChar : Morph Blank Char (Morph.Error Char)
    blankChar =
        Morph.to "blank"
            (Choice.between
                (\spaceVariant tabVariant returnVariant formFeedVariant blankNarrow ->
                    case blankNarrow of
                        Space ->
                            spaceVariant ()

                        Tab ->
                            tabVariant ()

                        Return return_ ->
                            returnVariant return_

                        FormFeed ->
                            formFeedVariant ()
                )
                |> Choice.try (\() -> Space) (Char.Morph.only ' ')
                |> Choice.try (\() -> Tab) (Char.Morph.only '\t')
                |> Choice.try Return Line.returnChar
                |> Choice.try (\() -> FormFeed)
                    -- \f
                    (Char.Morph.only '\u{000C}')
                |> Choice.finish
            )

    {-| Line break character
    -}
    type Return
        = NewLine
        | CarriageReturn

    {-| Match a line break character: Either

      - new line `'\n'`
      - carriage return `'\r'`

    > ℹ️ Equivalent regular expression: `[\n\r]`

        import Morph.Error
        import String.Morph as Text

        -- match a blank
        "\n\t abc" |> Text.narrowWith blank --> Ok '\n'

        -- anything else makes it fail
        "abc"
            |> Text.narrowWith blank
            |> Result.mapError Morph.Error.textMessage
        --> Err "1:1: I was expecting a blank space or new line. I got stuck when I got 'a'."

    -}
    returnChar : Morph Return Char (Morph.Error Char)
    returnChar =
        Choice.between
            (\newLineVariant carriageReturnVariant returnNarrow ->
                case returnNarrow of
                    NewLine ->
                        newLineVariant ()

                    CarriageReturn ->
                        carriageReturnVariant ()
            )
            |> Choice.try (\() -> NewLine)
                (Char.Morph.only '\n')
            |> Choice.try (\() -> CarriageReturn)
                -- \r
                (Char.Morph.only '\u{000D}')
            |> Choice.finish

    {-| The end of a text line:
    either a [return character](Return#Return) or the end of the whole text.
    -}
    type LineEnd
        = InputEnd
        | Return Return

    {-| Consume the end of the current line or Morph.succeed if there are
    no more remaining characters in the input text.

    > ℹ️ Equivalent regular expression: `$`

    -}
    endText : MorphRow Char LineEnd
    endText =
        Choice.between
            (\returnVariant inputEndVariant maybeChoice ->
                case maybeChoice of
                    Return returnValue ->
                        returnVariant returnValue

                    InputEnd ->
                        inputEndVariant ()
            )
            |> Choice.tryRow Return
                (returnChar |> Morph.one)
            |> Choice.tryRow (\() -> InputEnd)
                Morph.end
            |> Choice.finishRow

-}
between :
    choiceBroadenByPossibility
    ->
        MorphNoTry
            Possibly
            choiceNarrow_
            choiceBroad_
            choiceBroadenByPossibility
            error_
between choiceBroadenDiscriminatedByPossibility =
    { description = Emptiable.empty
    , narrow =
        \_ ->
            Emptiable.empty |> Err
    , broaden = choiceBroadenDiscriminatedByPossibility
    }



-- Morph


{-| If the previous [`possibility`](#try) fails
try this [`Morph`](#Morph).

> ℹ️ Equivalent regular expression: `|`

    import Char.Morph as Char
    import Morph.Error
    import AToZ exposing (AToZ)

    type UnderscoreOrLetter
        = Underscore
        | Letter Char

    underscoreOrLetter : Morph UnderscoreOrLetter Char
    underscoreOrLetter =
        Choice.between
            (\underscore letter underscoreOrLetter ->
                case underscoreOrLetter of
                    Underscore ->
                        underscore ()

                    Letter aToZ ->
                        letter aToZ
            )
            |> try Underscore (Char.Morph.only '_')
            |> try Letter AToZ.char

    -- try the first possibility
    "_" |> Text.narrowWith (underscoreOrLetter |> one)
    --> Ok Underscore

    -- if it fails, try the next
    "a" |> Text.narrowWith (underscoreOrLetter |> one)
    --> Ok 'a'

    -- if none work, we get the error from all possible steps
    "1"
        |> Text.narrowWith (underscoreOrLetter |> one)
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:1: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '1'."

-}
try :
    (possibilityNarrow -> narrowChoice)
    ->
        Morph.MorphIndependently
            (possibilityBeforeNarrow
             -> Result error possibilityNarrow
            )
            (possibilityBeforeBroaden -> possibilityBroad)
    ->
        (MorphNoTry
            noTryPossiblyOrNever_
            narrowChoice
            possibilityBeforeNarrow
            ((possibilityBeforeBroaden -> possibilityBroad)
             -> choiceBroadenFurther
            )
            error
         ->
            MorphNoTry
                noTryNever_
                narrowChoice
                possibilityBeforeNarrow
                choiceBroadenFurther
                error
        )
try possibilityToChoice possibilityMorph =
    \choiceMorphSoFar ->
        { description =
            choiceMorphSoFar.description
                |> Stack.onTopLay possibilityMorph.description
        , narrow =
            \broadValue ->
                broadValue
                    |> choiceMorphSoFar.narrow
                    |> restoreTry
                        (\soFarExpectationPossibilities ->
                            case broadValue |> Morph.narrowWith possibilityMorph of
                                Ok possibilityNarrow ->
                                    possibilityNarrow
                                        |> possibilityToChoice
                                        |> Ok

                                Err possibilityExpectation ->
                                    soFarExpectationPossibilities
                                        |> Stack.onTopLay possibilityExpectation
                                        |> Err
                        )
        , broaden =
            choiceMorphSoFar.broaden
                (Morph.broadenWith possibilityMorph)
        }


{-| Conclude a [`Choice.between`](#Choice.between) |> [`Choice.try`](#try) chain
-}
finish :
    MorphNoTry
        Never
        narrow
        beforeNarrow
        broaden
        (Morph.ErrorWithDeadEnd deadEnd)
    ->
        Morph.MorphIndependently
            (beforeNarrow
             -> Result (Morph.ErrorWithDeadEnd deadEnd) narrow
            )
            broaden
finish =
    \choiceMorphComplete ->
        { description =
            case choiceMorphComplete.description |> Emptiable.fill of
                Stack.TopDown variantOnly [] ->
                    variantOnly

                Stack.TopDown variant0 (variant1 :: variants2Up) ->
                    { custom = Emptiable.empty
                    , inner =
                        ArraySized.l2 variant0 variant1
                            |> ArraySized.glueMin Up
                                (variants2Up |> ArraySized.fromList)
                            |> Morph.Choice
                            |> Emptiable.filled
                    }
        , narrow =
            choiceMorphComplete.narrow
                >> Result.mapError Morph.Possibilities
        , broaden =
            choiceMorphComplete.broaden
        }



-- MorphRow


{-| Possibly incomplete [`MorphRow`](#MorphRow) to and from a Choice.between.
See [`Choice.between`](#Choice.between), [`Choice.tryRow`](#try), [`MorphRow.choiceFinish`](#choiceFinish)
-}
type alias MorphRowNoTry noTryPossiblyOrNever broadElement choiceNarrow choiceBroaden =
    MorphNoTry
        noTryPossiblyOrNever
        { narrow : choiceNarrow
        , broad : Emptiable (Stacked broadElement) Possibly
        }
        (Emptiable (Stacked broadElement) Possibly)
        choiceBroaden
        Morph.Error


{-| If the previous [`possibility`](#try) fails
try this [`MorphRow`](#MorphRow).

> ℹ️ Equivalent regular expression: `|`

    import Morph
    import Char.Morph as Char
    import Morph.Error

    type UnderscoreOrLetter
        = Underscore
        | Letter Char

    underscoreOrLetter : MorphRow Char UnderscoreOrLetter
    underscoreOrLetter =
        Choice.between
            (\underscoreVariant letterVariant underscoreOrLetterNarrow ->
                case underscoreOrLetterNarrow of
                    Underscore ->
                        underscoreVariant ()

                    Letter letter ->
                        letterVariant letter
            )
            |> try (\() -> Underscore) (Char.Morph.only '_')
            |> try Letter AToZ.caseAny
            |> choiceFinish

    -- try the first possibility
    "_"
        |> Text.narrowWith underscoreOrLetter
    --> Ok Underscore

    -- if it fails, try the next
    "a"
        |> Text.narrowWith underscoreOrLetter
    --> Ok 'a'

    -- if none work, we get the error from all possible steps
    "1"
        |> Text.narrowWith (onFailDown [ one '_', AToZ.Morph.char ])
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:1: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '1'."


### example: fallback step if the previous step fails

    import Morph
    import Char.Morph as Char
    import Morph.Error

    type AlphaNum
        = Digits (List (N (InFixed N0 N9)))
        | Letters String

    alphaNum : MorphRow Char AlphaNum
    alphaNum =
        Choice.between
            (\digit letter alphaNum ->
                case alphaNum of
                    Digits int ->
                        digit int

                    Letters char ->
                        letter char
            )
            |> Choice.try Letter
                (map String.Morph.fromList
                    (atLeast n1 AToZ.Morph.char)
                )
            |> Choice.try Digit
                (atLeast n1 Digit.n0To9)
            |> MorphRow.choiceFinish

    -- try letters, or else give me some digits
    "abc"
        |> Text.narrowWith alphaNum
    --> Ok "abc"

    -- we didn't get letters, but we still got digits
    "123"
        |> Text.narrowWith alphaNum
    --> Ok "123"

    -- but if we still fail, give the expectations of all steps
    "_"
        |> Text.narrowWith alphaNum
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:1: I was expecting at least 1 digit [0-9]. I got stuck when I got the character '_'."

-}
tryRow :
    (possibilityNarrow -> choiceNarrow)
    -> Morph.MorphRow broadElement possibilityNarrow
    ->
        (MorphRowNoTry
            noTryPossiblyOrNever_
            broadElement
            choiceNarrow
            ((possibilityNarrow -> Emptiable (Stacked broadElement) Possibly)
             -> choiceBroadenFurther
            )
         ->
            MorphRowNoTry
                never_
                broadElement
                choiceNarrow
                choiceBroadenFurther
        )
tryRow possibilityToChoice possibilityMorph =
    \choiceMorphSoFar ->
        { description =
            choiceMorphSoFar.description
                |> Stack.onTopLay (possibilityMorph |> Morph.description)
        , narrow =
            \choiceBroad ->
                choiceBroad
                    |> choiceMorphSoFar.narrow
                    |> restoreTry
                        (\soFarErrorPossibilities ->
                            case choiceBroad |> Morph.narrowWith possibilityMorph of
                                Ok possibilityParsed ->
                                    { broad = possibilityParsed.broad
                                    , narrow =
                                        possibilityParsed.narrow
                                            |> possibilityToChoice
                                    }
                                        |> Ok

                                Err possibilityExpectation ->
                                    soFarErrorPossibilities
                                        |> Stack.onTopLay possibilityExpectation
                                        |> Err
                        )
        , broaden =
            choiceMorphSoFar.broaden
                (Morph.broadenWith possibilityMorph)
        }


{-| Always the last step in a [`Choice.between`](#Choice.between) `|>` [`Choice.tryRow`](#try) build process
-}
finishRow :
    MorphRowNoTry
        Never
        broadElement
        choiceNarrow
        (choiceNarrow -> Emptiable (Stacked broadElement) Possibly)
    -> Morph.MorphRow broadElement choiceNarrow
finishRow =
    \choiceMorphRowComplete ->
        { description =
            case choiceMorphRowComplete.description |> Emptiable.fill of
                Stack.TopDown descriptionOnly [] ->
                    descriptionOnly

                Stack.TopDown description0 (description1 :: description2Up) ->
                    { custom = Emptiable.empty
                    , inner =
                        ArraySized.l2 description0 description1
                            |> ArraySized.glueMin Up
                                (description2Up |> ArraySized.fromList)
                            |> Morph.Group
                            |> Emptiable.filled
                    }
        , narrow =
            \broad_ ->
                broad_
                    |> choiceMorphRowComplete.narrow
                    |> Result.mapError
                        (\errorPossibilities ->
                            { startDown = broad_ |> Stack.length
                            , error = errorPossibilities |> Morph.Possibilities
                            }
                                |> Morph.Row
                        )
        , broaden = choiceMorphRowComplete.broaden
        }



-- MorphValue


{-| Describe another variant value [`Morph`](#Morph) to [`Value`](#Value)

Done? → [`Choice.finishValue`](#choiceFinish)

If a variant doesn't have any value attached, use [`Unit.value`](Unit#value)

    {-| `Bool` `MorphValue`
    -}
    boolValue : MorphValue Bool
    boolValue =
        Choice.between
            (\true false bool ->
                case bool of
                    True ->
                        true ()

                    False ->
                        false ()
            )
            |> Choice.tryValue ( \() -> True, "True" ) Unit.value
            |> Choice.tryValue ( \() -> False, "False" ) Unit.value
            |> Choice.finishValue

-}
tryValue :
    ( possibilityNarrow -> choiceNarrow
    , String
    )
    -> Value.MorphValue possibilityNarrow
    ->
        (MorphNoTry
            noTryPossiblyOrNever_
            choiceNarrow
            (Value.Tagged Value.IndexOrName)
            ((possibilityNarrow
              -> Value.Tagged Value.IndexAndName
             )
             -> choiceBroadenFurther
            )
            Morph.Error
         ->
            MorphNoTry
                noTryNever_
                choiceNarrow
                (Value.Tagged Value.IndexOrName)
                choiceBroadenFurther
                Morph.Error
        )
tryValue ( possibilityToChoice, possibilityTag ) possibilityMorph =
    \choiceMorphSoFar ->
        choiceMorphSoFar
            |> try possibilityToChoice
                (Morph.to possibilityTag
                    { description = { custom = Emptiable.empty, inner = Emptiable.empty }
                    , narrow =
                        variantStepNarrow
                            ( { name = possibilityTag
                              , index = choiceMorphSoFar.description |> Stack.length
                              }
                            , Morph.narrowWith possibilityMorph
                            )
                    , broaden =
                        \narrowValue ->
                            { tag =
                                { name = possibilityTag
                                , index = choiceMorphSoFar.description |> Stack.length
                                }
                            , value = narrowValue |> Morph.broadenWith possibilityMorph
                            }
                    }
                )


variantStepNarrow :
    ( Value.IndexAndName
    , Value.Value Value.IndexOrName
      -> Result Morph.Error possibilityNarrow
    )
    ->
        (Value.Tagged Value.IndexOrName
         -> Result Morph.Error possibilityNarrow
        )
variantStepNarrow ( variantTag, possibilityNarrow ) =
    \variantBroad ->
        case variantBroad.tag of
            Value.Index index ->
                if index == variantTag.index then
                    variantBroad.value |> possibilityNarrow

                else
                    "tag " ++ (index |> String.fromInt) |> Morph.DeadEnd |> Err

            Value.Name name ->
                if name == variantTag.name then
                    variantBroad.value |> possibilityNarrow

                else
                    "tag " ++ name |> Morph.DeadEnd |> Err


{-| Conclude a [`Choice.between`](Morph#Choice.between) |> [`Choice.tryValue`](#variant) chain
-}
finishValue :
    MorphNoTry
        Never
        choiceNarrow
        (Value.Tagged Value.IndexOrName)
        (choiceNarrow -> Value.Tagged Value.IndexAndName)
        Morph.Error
    -> Value.MorphValue choiceNarrow
finishValue =
    \choiceMorphComplete ->
        choiceMorphComplete
            |> finish
            |> Morph.over
                (Morph.value "Variant"
                    { narrow =
                        \value ->
                            case value of
                                Value.Variant variant_ ->
                                    variant_ |> Ok

                                structureExceptVariant ->
                                    structureExceptVariant
                                        |> Value.PackageInternal.structureKindToString
                                        |> Err
                    , broaden = Value.Variant
                    }
                )
            |> Morph.over Value.structure
