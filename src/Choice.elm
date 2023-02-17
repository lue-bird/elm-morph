module Choice exposing
    ( equivalent
    , MorphNoVariant, toFrom, variant, finishToFrom
    , between
    , MorphNoTry, try, finish
    , MorphRowNoTry, tryRow, finishRow
    , variantValue, finishValue
    )

{-| [`Morph`](Morph#Morph) a tagged union `type`

@docs equivalent


## morph by variant

@docs MorphNoVariant, toFrom, variant, finishToFrom

@docs between


## [`Morph`](Morph#Morph)

@docs MorphNoTry, try, finish


## [`MorphRow`](Morph#MorphRow)

@docs MorphRowNoTry, tryRow, finishRow


## [`MorphValue`](Value#MorphValue)

@docs variantValue, finishValue

-}

import ArraySized
import Emptiable exposing (Emptiable)
import Linear exposing (Direction(..))
import Morph exposing (MorphIndependently)
import Possibly exposing (Possibly)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Stack exposing (Stacked)
import Util exposing (restoreTry)
import Value
import Value.PackageInternal



-- Morph


{-| Possibly incomplete [`Morph`](Morph#Morph) a choice from a [`Value`](Value#Value).
See [`Choice.between`](#Choice.between), [`variantValue`](#variantValue), [`finishValue`](#finishValue)
-}
type alias MorphNoTry noTryPossiblyOrNever choiceNarrow choiceBeforeNarrow choiceBroaden error =
    MorphNoVariant
        noTryPossiblyOrNever
        (choiceBeforeNarrow
         ->
            Result
                (-- tries
                 Emptiable (Stacked error) noTryPossiblyOrNever
                )
                choiceNarrow
        )
        choiceBroaden


{-| Word in an incomplete morph in progress. For example

    Choice.finish :
        Choice.MorphNoTry
            Never
            (N (In N0 N9)))
            Char
            (N (In N0 N9)) -> Char)
        -> Morph (N (In N0 N9)) Char

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
        "\n\t abc" |> Text.narrowTo blank --> Ok '\n'

        -- anything else makes it fail
        "abc"
            |> Text.narrowTo blank
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
    broadenByPossibility
    ->
        MorphNoTry
            Possibly
            choiceNarrow_
            choiceBroad_
            broadenByPossibility
            error_
between choiceBroadenDiscriminatedByPossibility =
    { description = Emptiable.empty
    , narrow =
        \_ ->
            Emptiable.empty |> Err
    , broaden = choiceBroadenDiscriminatedByPossibility
    }


{-| Offer alternative [`Morph`](Morph#Morph) possibilities to a given preferred one.
Functionally, it's the same as [`Choice.equivalent`](#equivalent) with an optimization
as shown in ["Fast parsing of String Sets in Elm" by Marcelo Lazaroni](https://lazamar.github.io/fast-parsing-of-string-sets-in-elm/)
published as [`dict-parser`](https://dark.elm.dmy.fr/packages/lazamar/dict-parser/latest/Parser-Dict)

Usually, you'll be better off with a [`Choice.between`](#between)
an explicit custom tagged union
because you'll have the option to preserve what was [narrowed](Morph#narrowTo).
(Remember: you can always discard that info and set a preferred option with [`Morph.broad`](Morph#broad))

Go [`Choice.equivalent`](Choice#equivalent) if you have a dynamic list of aliases/morphs to treat equally.
An example is defined variable names

    import Order

    Choice.equivalentRow String.Morph.only
        { broad = "∨"
        , alternatives = [ "|", "or" ]
        , order = Order.string { case_ = Order.lowerUpper }
        }

    Choice.equivalentRow String.Morph.only
        { broad = "±"
        , alternatives = [ "pm", "plusminus" ]
        , order = Order.string { case_ = Order.lowerUpper }
        }

TODO: optimize

-}
equivalentRow :
    (broadElement
     ->
        MorphIndependently
            (beforeNarrow
             -> Result (Morph.ErrorWithDeadEnd deadEnd) narrow
            )
            broaden
    )
    ->
        { broad : Emptiable (Stacked broadElement) broadPossibilityEmptyPossiblyOrNever_
        , alternatives :
            List
                (Emptiable (Stacked broadElement) alternativePossibilityEmptyPossiblyOrNever_)
        , order : broadElement -> broadElement -> Order
        }
    ->
        MorphIndependently
            (beforeNarrow
             -> Result (Morph.ErrorWithDeadEnd deadEnd) narrow
            )
            broaden
equivalentRow possibilityMorph possibilitiesOrdered =
    Debug.todo ""


{-| Offer alternative [`Morph`](Morph#Morph) possibilities to a given preferred one.

Usually, you'll be better off with a [`Choice.between`](#between)
an explicit custom tagged union
because you'll have the option to preserve what was [narrowed](Morph#narrowTo).
(Remember: you can always discard that info and set a preferred option with [`Morph.broad`](Morph#broad))

Go [`Choice.equivalent`](Choice#equivalent) if you have a dynamic list of aliases/morphs to treat equally.
An example is defined variable names

    Choice.equivalent Char.Morph.only { broad = '∨', alternatives = [ '|' ] }

Use [`Choice.equivalentRow`](#equivalentRow) for strings etc.

-}
equivalent :
    (element
     ->
        MorphIndependently
            (beforeNarrow
             -> Result (Morph.ErrorWithDeadEnd deadEnd) narrow
            )
            broaden
    )
    ->
        { broad : element
        , alternatives : List element
        }
    ->
        MorphIndependently
            (beforeNarrow
             -> Result (Morph.ErrorWithDeadEnd deadEnd) narrow
            )
            broaden
equivalent traversePossibility possibilities =
    case possibilities.alternatives of
        [] ->
            traversePossibility possibilities.broad

        alternative0 :: alternatives1Up ->
            { description =
                { custom = Emptiable.empty
                , inner =
                    ArraySized.l2 possibilities.broad alternative0
                        |> ArraySized.attachMin Up (alternatives1Up |> ArraySized.fromList)
                        |> ArraySized.map traversePossibility
                        |> ArraySized.map Morph.description
                        |> Morph.Choice
                        |> Emptiable.filled
                }
            , narrow =
                \beforeNarrow ->
                    beforeNarrow
                        |> equivalentTryNarrow traversePossibility
                            (Stack.topBelow possibilities.broad
                                (alternative0 :: alternatives1Up)
                            )
            , broaden =
                (traversePossibility possibilities.broad).broaden
            }


equivalentTryNarrow :
    (element
     ->
        MorphIndependently
            (beforeNarrow
             -> Result (Morph.ErrorWithDeadEnd deadEnd) narrow
            )
            broaden
    )
    -> Emptiable (Stacked element) Never
    ->
        (beforeNarrow
         -> Result (Morph.ErrorWithDeadEnd deadEnd) narrow
        )
equivalentTryNarrow traverseTry tries =
    \beforeNarrow ->
        tries
            |> Stack.removeTop
            |> Stack.foldFrom
                (beforeNarrow
                    |> Morph.narrowTo
                        (traverseTry (tries |> Stack.top))
                    |> Result.mapError Stack.one
                )
                Up
                (\elementForMorph resultSoFar ->
                    resultSoFar
                        |> restoreTry
                            (\errorsSoFar ->
                                beforeNarrow
                                    |> Morph.narrowTo (traverseTry elementForMorph)
                                    |> Result.mapError
                                        (\error -> errorsSoFar |> Stack.onTopLay error)
                            )
                )
            |> Result.mapError Morph.Tries



-- Morph


{-| Builder for a [`Morph`](#Morph) to a choice. Possibly incomplete

Initialize with [`Choice.toFrom`](#toFrom)

-}
type alias MorphNoVariant noTryPossiblyOrNever narrow broaden =
    RecordWithoutConstructorFunction
        { description : Emptiable (Stacked Morph.Description) noTryPossiblyOrNever
        , narrow : narrow
        , broaden : broaden
        }



-- maybe variant


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
    "_" |> Text.narrowTo (underscoreOrLetter |> one)
    --> Ok Underscore

    -- if it fails, try the next
    "a" |> Text.narrowTo (underscoreOrLetter |> one)
    --> Ok 'a'

    -- if none work, we get the error from all possible steps
    "1"
        |> Text.narrowTo (underscoreOrLetter |> one)
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
                        (\soFarTryErrors ->
                            case broadValue |> Morph.narrowTo possibilityMorph of
                                Ok possibilityNarrow ->
                                    possibilityNarrow
                                        |> possibilityToChoice
                                        |> Ok

                                Err tryError ->
                                    soFarTryErrors
                                        |> Stack.onTopLay tryError
                                        |> Err
                        )
        , broaden =
            choiceMorphSoFar.broaden
                (Morph.broadenFrom possibilityMorph)
        }



-- each variant


{-| Initialize a [choice morph](Choice#MorphNoTry)
by discriminating `(` the broad`,` the narrow `)` choices,
then `|>` [`Choice.try`](Choice#try)ing each possibility,
concluding the builder with [`Choice.finish`](#finish)

A use case is [morphing](Morph#Morph) from and to an internal type

    absoluteInternal : MorphOrError Absolute Decimal.Internal.Absolute error_
    absoluteInternal =
        Choice.toFrom
            ( \variantFraction variantAtLeast1 decimal ->
                case decimal of
                    Decimal.Internal.Fraction fractionValue ->
                        variantFraction fractionValue

                    Decimal.Internal.AtLeast1 atLeast1Value ->
                        variantAtLeast1 atLeast1Value
            , \variantFraction variantAtLeast1 decimal ->
                case decimal of
                    Fraction fractionValue ->
                        variantFraction fractionValue

                    AtLeast1 atLeast1Value ->
                        variantAtLeast1 atLeast1Value
            )
            |> Choice.tryToFrom ( Fraction, Decimal.Internal.Fraction ) fractionInternal
            |> Choice.tryToFrom ( AtLeast1, Decimal.Internal.AtLeast1 ) atLeast1Internal
            |> Choice.finish

For morphing choices with simple variants without values (enums),
a simple [`translate`](Morph#translate) also does the job

    signInternal : MorphOrError Sign Sign.Internal.Sign error_
    signInternal =
        Morph.translate
            (\signInternalBeforeNarrow ->
                case signInternalBeforeNarrow of
                    Sign.Internal.Negative ->
                        Sign.Negative

                    Sign.Internal.Positive ->
                        Sign.Positive
            )
            (\signBeforeBroaden ->
                case signBeforeBroaden of
                    Sign.Negative ->
                        Sign.Internal.Negative

                    Sign.Positive ->
                        Sign.Internal.Positive
            )

-}
toFrom :
    ( narrowByPossibility
    , broadenByPossibility
    )
    ->
        MorphNoVariant
            Possibly
            narrowByPossibility
            broadenByPossibility
toFrom ( narrowByPossibility, broadenByPossibility ) =
    { description = Emptiable.empty
    , narrow = narrowByPossibility
    , broaden = broadenByPossibility
    }


{-| [`Morph`](Morph#Morph) the next variant value.
Finish with [`Choice.finishToFrom`](#finishToFrom)
-}
variant :
    ( narrowVariantValue -> narrowChoice
    , possibilityBroad -> broadChoice
    )
    ->
        MorphIndependently
            (beforeNarrowVariantValue
             -> Result error narrowVariantValue
            )
            (beforeBroadVariantValue -> possibilityBroad)
    ->
        (MorphNoVariant
            noTryPossiblyOrNever_
            ((beforeNarrowVariantValue
              -> Result error narrowChoice
             )
             -> narrowChoiceFurther
            )
            ((beforeBroadVariantValue -> broadChoice)
             -> broadenChoiceFurther
            )
         ->
            MorphNoVariant
                noTryNever_
                narrowChoiceFurther
                broadenChoiceFurther
        )
variant ( possibilityToChoice, possibilityFromChoice ) possibilityMorph =
    \choiceMorphSoFar ->
        { description =
            choiceMorphSoFar.description
                |> Stack.onTopLay possibilityMorph.description
        , narrow =
            choiceMorphSoFar.narrow
                (\broad ->
                    broad
                        |> Morph.narrowTo possibilityMorph
                        |> Result.map possibilityToChoice
                )
        , broaden =
            choiceMorphSoFar.broaden
                (\narrow ->
                    narrow
                        |> Morph.broadenFrom possibilityMorph
                        |> possibilityFromChoice
                )
        }


{-| Conclude a [`Choice.toFrom`](Choice#toFrom) `|>` [`Choice.tryToFrom`](Choice#tryToFrom) builder
-}
finishToFrom :
    MorphNoVariant Never narrow broaden
    -> MorphIndependently narrow broaden
finishToFrom =
    \choiceMorphComplete ->
        { description =
            case choiceMorphComplete.description |> Emptiable.fill of
                Stack.TopBelow ( variantOnly, [] ) ->
                    variantOnly

                Stack.TopBelow ( variant0, variant1 :: variants2Up ) ->
                    { custom = Emptiable.empty
                    , inner =
                        ArraySized.l2 variant0 variant1
                            |> ArraySized.attachMin Up
                                (variants2Up |> ArraySized.fromList)
                            |> Morph.Choice
                            |> Emptiable.filled
                    }
        , narrow = choiceMorphComplete.narrow
        , broaden = choiceMorphComplete.broaden
        }



-- MorphRow


{-| Possibly incomplete [`MorphRow`](#MorphRow) to and from a Choice.between.
See [`Choice.between`](#Choice.between), [`Choice.tryRow`](#try), [`MorphRow.choiceFinish`](#choiceFinish)
-}
type alias MorphRowNoTry noTryPossiblyOrNever choiceNarrow choiceBroaden broadElement =
    MorphNoVariant
        noTryPossiblyOrNever
        (Emptiable (Stacked broadElement) Possibly
         ->
            Result
                (-- tries
                 Emptiable (Stacked Morph.Error) noTryPossiblyOrNever
                )
                { narrow : choiceNarrow
                , broad : Emptiable (Stacked broadElement) Possibly
                }
        )
        choiceBroaden


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
        |> Text.narrowTo underscoreOrLetter
    --> Ok Underscore

    -- if it fails, try the next
    "a"
        |> Text.narrowTo underscoreOrLetter
    --> Ok 'a'

    -- if none work, we get the error from all possible steps
    "1"
        |> Text.narrowTo (onFailDown [ one '_', AToZ.char ])
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:1: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '1'."


### example: fallback step if the previous step fails

    import Morph
    import Char.Morph as Char
    import Morph.Error

    type AlphaNum
        = Digits (List (N (In N0 N9)))
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
                    (atLeast n1 AToZ.char)
                )
            |> Choice.try Digit
                (atLeast n1 Digit.n0To9)
            |> MorphRow.choiceFinish

    -- try letters, or else give me some digits
    "abc"
        |> Text.narrowTo alphaNum
    --> Ok "abc"

    -- we didn't get letters, but we still got digits
    "123"
        |> Text.narrowTo alphaNum
    --> Ok "123"

    -- but if we still fail, give the expectations of all steps
    "_"
        |> Text.narrowTo alphaNum
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:1: I was expecting at least 1 digit [0-9]. I got stuck when I got the character '_'."

-}
tryRow :
    (possibilityNarrow -> choiceNarrow)
    -> Morph.MorphRow possibilityNarrow broadElement
    ->
        (MorphRowNoTry
            noTryPossiblyOrNever_
            choiceNarrow
            ((possibilityNarrow -> Emptiable (Stacked broadElement) Possibly)
             -> choiceBroadenFurther
            )
            broadElement
         ->
            MorphRowNoTry
                never_
                choiceNarrow
                choiceBroadenFurther
                broadElement
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
                            case choiceBroad |> Morph.narrowTo possibilityMorph of
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
                (Morph.broadenFrom possibilityMorph)
        }


{-| Always the last step in a [`Choice.between`](#Choice.between) `|>` [`Choice.tryRow`](#try) build process
-}
finishRow :
    MorphRowNoTry
        Never
        choiceNarrow
        (choiceNarrow
         -> Emptiable (Stacked broadElement) Possibly
        )
        broadElement
    -> Morph.MorphRow choiceNarrow broadElement
finishRow =
    \choiceMorphRowComplete ->
        { description =
            case choiceMorphRowComplete.description |> Emptiable.fill of
                Stack.TopBelow ( descriptionOnly, [] ) ->
                    descriptionOnly

                Stack.TopBelow ( description0, description1 :: description2Up ) ->
                    { custom = Emptiable.empty
                    , inner =
                        ArraySized.l2 description0 description1
                            |> ArraySized.attachMin Up
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
                            , error = errorPossibilities |> Morph.Tries
                            }
                                |> Morph.Row
                        )
        , broaden = choiceMorphRowComplete.broaden
        }



-- MorphValue


{-| Describe another variant value [`Morph`](#Morph) to [`Value`](#Value)

Done? → [`Choice.finishValue`](#choiceFinish)

If a variant doesn't have any value attached, use [`Value.unit`](Value#unit)

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
            |> Choice.variantValue ( \() -> True, "True" ) Value.unit
            |> Choice.variantValue ( \() -> False, "False" ) Value.unit
            |> Choice.finishValue

-}
variantValue :
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
variantValue ( possibilityToChoice, possibilityTag ) possibilityMorph =
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


{-| Conclude a [`Choice.between`](Choice#between) `|>` [`Choice.try`](Choice#try) builder
-}
finish :
    MorphNoTry
        Never
        choiceNarrow
        choiceBeforeNarrow
        (choiceBeforeBroaden -> choiceBroad)
        (Morph.ErrorWithDeadEnd deadEnd)
    ->
        MorphIndependently
            (choiceBeforeNarrow
             -> Result (Morph.ErrorWithDeadEnd deadEnd) choiceNarrow
            )
            (choiceBeforeBroaden -> choiceBroad)
finish =
    \choiceMorphComplete ->
        { description =
            case choiceMorphComplete.description |> Emptiable.fill of
                Stack.TopBelow ( variantOnly, [] ) ->
                    variantOnly

                Stack.TopBelow ( variant0, variant1 :: variants2Up ) ->
                    { custom = Emptiable.empty
                    , inner =
                        ArraySized.l2 variant0 variant1
                            |> ArraySized.attachMin Up
                                (variants2Up |> ArraySized.fromList)
                            |> Morph.Choice
                            |> Emptiable.filled
                    }
        , narrow =
            choiceMorphComplete.narrow
                >> Result.mapError Morph.Tries
        , broaden =
            choiceMorphComplete.broaden
        }


{-| Conclude a [`Choice.between`](Morph#Choice.between) |> [`Choice.variantValue`](#variant) chain
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

                                composedExceptVariant ->
                                    composedExceptVariant
                                        |> Value.PackageInternal.composedKindToString
                                        |> Err
                    , broaden = Value.Variant
                    }
                )
            |> Morph.over Value.composed
