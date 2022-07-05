module MorphRow exposing
    ( MorphRow
    , SuccessExpectation, Error, InputEndOrSuccess(..)
    , succeed, expect, fail
    , end, oneAny, one
    , specific
    , skip, grab, GroupMorphRowInProgress
    , over, next
    , possibility, ChoiceMorphRowInProgress, choiceFinish
    , sequence
    , atLeast, between, exactly, maybe
    , separatedBy
    , before, until, while
    )

{-| A simple, easy to use, general-purpose parser-builder with good error messages.

Heavily inspired by

  - [`elm/parser`](https://package.elm-lang.org/packages/elm/parser/latest/)
  - [`dasch/parser`](https://package.elm-lang.org/packages/dasch/parser/latest/)
  - especially [`lambda-phi/parser`](https://dark.elm.dmy.fr/packages/lambda-phi/parser/latest/)


## example: 2D point

    import MorphRow exposing (MorphRow, atLeast, drop, into, succeed, take, one)
    import Morph.Char as Char
    import Morph.TextRow as Text exposing (number)
    import MorphRow.Error

    type alias Point =
        -- makes `Point` function unavailable:
        -- https://dark.elm.dmy.fr/packages/lue-bird/elm-no-record-type-alias-constructor-function/latest/
        RecordWithoutConstructorFunction
            { x : Float
            , y : Float
            }

    -- successful parsing looks like
    "(2.71, 3.14)" |> narrow (listToString |> over point)
    --> Ok { x = 2.71, y = 3.14 }

    -- building always works
    { x = 2.71, y = 3.14 } |> broad (listToString |> over point)
    --> "( 2.71, 3.14 )"

    point : MorphRow Point
    point =
        into "Point"
            (succeed (\x y -> { x = x, y = y })
                |> skip (Morph.Text.specific "(")
                |> skip
                    (broadenFrom [ Char.Space ]
                        |> MorphRow.over (atLeast 0 (Char.blank |> one))
                    )
                |> grab number
                |> skip
                    (broadenFrom []
                        |> MorphRow.over (atLeast 0 (Char.blank |> one))
                    )
                |> skip (Morph.Text.specific ",")
                |> skip
                    (broadenFrom [ Char.Space ]
                        |> MorphRow.over (atLeast 0 (Char.blank |> one))
                    )
                |> grab .x Number.Morph.text
                |> skip
                    (broadenFrom [ Char.Space ]
                        |> MorphRow.over (atLeast 0 (Char.blank |> one))
                    )
                |> skip (Morph.Text.specific ")")
            )

    -- we can get a nice error message if it fails
    "(2.71, x)"
        |> Text.narrowWith point
        |> Result.mapError (MorphRow.Error.dump "filename.txt")
    --> Err
    -->     [ "[ERROR] filename.txt: line 1:8: I was expecting a digit [0-9]. I got stuck when I got 'x'."
    -->     , "  in Point at line 1:1"
    -->     , ""
    -->     , "1|(2.71, x)"
    -->     , "  ~~~~~~~^"
    -->     ]


## usage

  - [`MorphRow`](MorphRow):
    creating, chaining [`Morph`](Morph#Morph)s over rows of input

  - [`Morph.Char`](Morph-Char), [`Morph.Text`](Morph-Text), [`Integer.Morph`](Integer-Morph), [`Number.Morph`](Number-Morph), ...
    include [`Morph`](Morph#Morph)s over characters

  - [`MorphRow.Error`](MorphRow-Error):
    error reporting and formatting

Note before we start:
`MorphRow` _always backtracks_ and never commits to a specific path!

  - 👍 improves readability
  - 👍 provides more information on what could be possible to the user
  - 👎 performs worse as there's more [possibilities](#possibility) to parse to know it failed

@docs MorphRow
@docs SuccessExpectation, Error, InputEndOrSuccess


## scan

Simply use [`Morph.narrow`](Morph#narrow), [`Morph.broaden`](Morph#broaden)


## create

@docs succeed, expect, fail
@docs end, oneAny, one
@docs specific


## chain

@docs skip, grab, GroupMorphRowInProgress
@docs over, next


### choice

@docs possibility, ChoiceMorphRowInProgress, choiceFinish


## sequences

@docs sequence
@docs atLeast, between, exactly, maybe
@docs separatedBy


### loop

[`while`](#while), [`until`](#until) are powerful ways to recurse over [`MorphRow`](#MorphRow)s:

situation: One [`possibility`](#possibility) matches, [`next`](#next) the new argument is taken to call the whole [`MorphRow`](#MorphRow) recursively.

This grows the stack, so you cannot do it indefinitely.
[`while`](#while), [`until`](#until) enable tail-call elimination so you can have as many repeats you want.

@docs before, until, while

-}

import Hand exposing (Empty, Hand, filled)
import Morph exposing (Expected(..), GroupMorphInProgress, Morph, broaden, broadenFrom, choice, narrow, translate, validate)
import Possibly exposing (Possibly(..))
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Stack exposing (StackTopBelow, Stacked, topDown)
import Util exposing (restoreTry)



-- types


{-| A function that takes an inputs `List` of "atoms"
and returns either an [`Error`](#Error),
or a value with the next state.

    {-| [`MorphRow`](#MorphRow) on input characters
    -}
    type alias TextMorphRow narrow =
        MorphRow Char narrow

-}
type alias MorphRow atom narrow expectedCustom =
    Morph
        narrow
        (Hand (Stacked atom) Possibly Empty)
        (Error atom narrow expectedCustom)


{-| Incomplete [`MorphRow`](#MorphRow) for a thing composed of multiple parts = group.
It's what you supply during a [`succeed`](#succeed)`|>`[`grab`](#grab)/[`skip`](#skip) build.
-}
type alias GroupMorphRowInProgress atom groupNarrow groupNarrowAssemble expectedCustom =
    GroupMorphInProgress
        (groupNarrow
         -> Hand (Stacked atom) Possibly Empty
        )
        groupNarrowAssemble
        (Hand (Stacked atom) Possibly Empty)
        (Error atom groupNarrowAssemble expectedCustom)


{-| The description of an error.
-}
type alias SuccessExpectation atom description =
    RecordWithoutConstructorFunction
        { expected :
            Morph.ExpectationWith { startingAtDown : Int } atom description
        , description : Hand (Stacked description) Possibly Empty
        , startingAtDown : Int
        }


{-| Complete description of a situation that's considered a failure,
expecting either [`InputEndOrSuccess`](#InputEndOrSuccess)
-}
type alias Error atom narrow expectedCustom =
    Morph.Expected (InputEndOrSuccess atom narrow expectedCustom)


{-| `expectedCustom` is used to provide details on what you [`expect`](#expect) to parse.
-}
type InputEndOrSuccess atom narrow expectedCustom
    = NoMoreInputRemaining
        { remainingAtomCount : Hand (Stacked atom) Never Empty
        , narrow : narrow
        }
    | Success (SuccessExpectation atom expectedCustom)



--


onRemainingOk :
    Result (Error element narrow expectedCustom) narrow
    ->
        Result
            (SuccessExpectation element expectedCustom)
            { narrow : narrow
            , broad : Hand (Stacked element) Possibly Empty
            }
onRemainingOk state =
    case state of
        Ok narrowed ->
            { narrow = narrowed, broad = Hand.empty } |> Ok

        Err (Expected (NoMoreInputRemaining inputRemaining)) ->
            { narrow = inputRemaining.narrow
            , broad =
                inputRemaining.remainingAtomCount
                    |> Hand.emptyAdapt (\_ -> Possible)
            }
                |> Ok

        Err (Expected (Success expectationMiss)) ->
            expectationMiss |> Err


narrowStep :
    GroupMorphRowInProgress atom buildable_ narrow expectedCustom
    ->
        (Hand (Stacked atom) Possibly Empty
         ->
            Result
                (SuccessExpectation atom expectedCustom)
                { narrow : narrow
                , broad : Hand (Stacked atom) Possibly Empty
                }
        )
narrowStep morphRow =
    \broad ->
        broad
            |> morphRow.narrow
            |> onRemainingOk


onRemainingErr :
    Result
        (SuccessExpectation element expectedCustom)
        { narrow : narrow
        , broad : Hand (Stacked element) Possibly Empty
        }
    -> Result (Error element narrow expectedCustom) narrow
onRemainingErr state =
    case state of
        Ok stateOk ->
            case stateOk.broad of
                Hand.Empty _ ->
                    stateOk.narrow |> Ok

                Hand.Filled stacked ->
                    Expected
                        (NoMoreInputRemaining
                            { narrow = stateOk.narrow
                            , remainingAtomCount = stacked |> filled
                            }
                        )
                        |> Err

        Err expectationMiss ->
            Expected (Success expectationMiss)
                |> Err


{-| Describe the context to improve error messages.

    import MorphRow.Error
    import Morph.Char as Char
    import Morph.TextRow as Text

    -- we can redefine an error message if something goes wrong
    "123"
        |> Text.narrowWith
            (expect "a name consisting of letters"
                (atLeast 1 Char.letter)
            )
        |> Result.mapError MorphRow.Error.textMessage
    --> Err "1:1: I was expecting a name consisting of letters. I got stuck when I got '1'."


    import MorphRow exposing (take, drop, succeed, expect, one)
    import Morph.TextRow as Text

    type alias Point =
        -- makes `Point` function unavailable:
        -- https://dark.elm.dmy.fr/packages/lue-bird/elm-no-record-type-alias-constructor-function/latest/
        RecordWithoutConstructorFunction
            { x : Float
            , y : Float
            }

    -- we can use `expect` to have more context when an error happens
    point : MorphRow Point
    point =
        expect "point"
            (succeed (\x y -> { x = x, y = y })
                |> skip (one '(')
                |> grab .x Text.number
                |> skip (one ',')
                |> grab .y Text.number
                |> skip (one ')')
            )

    "(12,34)" |> narrow (map Text.fromList point)
    --> Ok { x = 12, y = 34 }

    -- we can get the error context stack as well as where they started matching
    "(a,b)" |> narrow (map Text.fromList point)
        |> Result.mapError .expected
    --> Err [ ExpectedCustom "point" ]

-}
expect :
    expectedCustom
    -> MorphRow atom narrow expectedCustom
    -> MorphRow atom narrow expectedCustom
expect contextDescription morphRow =
    { narrow =
        \beforeInput ->
            beforeInput
                |> narrowStep morphRow
                |> Result.mapError
                    (\context ->
                        { context
                            | description =
                                context.description
                                    |> Stack.onTopLay contextDescription
                        }
                    )
                |> Result.andThen
                    (\after ->
                        after.broad
                            |> narrowStep (after.narrow |> succeed)
                    )
                |> onRemainingErr
    , broaden = broaden morphRow
    }


{-| Take what we get from [converting](#MorphRow) the next section
and channel it back up to the [`succeed`](#succeed) grouping.
-}
grab :
    (groupNarrow -> partNextNarrow)
    -> MorphRow atom partNextNarrow expectedCustom
    ->
        (GroupMorphRowInProgress
            atom
            groupNarrow
            (partNextNarrow -> groupNarrowFurther)
            expectedCustom
         ->
            GroupMorphRowInProgress
                atom
                groupNarrow
                groupNarrowFurther
                expectedCustom
        )
grab partAccess grabbedNextMorphRow =
    \groupMorphRowSoFar ->
        { narrow =
            \broad ->
                broad
                    |> narrowStep groupMorphRowSoFar
                    |> Result.andThen
                        (\result ->
                            result.broad
                                |> narrowStep grabbedNextMorphRow
                                |> Result.map
                                    (\nextParsed ->
                                        { narrow = result.narrow nextParsed.narrow
                                        , broad = nextParsed.broad
                                        }
                                    )
                        )
                    |> onRemainingErr
        , broaden =
            \groupNarrow ->
                groupNarrow
                    |> partAccess
                    |> grabbedNextMorphRow.broaden
                    |> Stack.onTopStack
                        (groupNarrow
                            |> groupMorphRowSoFar.broaden
                        )
        }



-- basic


{-| Require values to be matched next to continue but ignore the result.

    import Morph.TextRow exposing (text)
    import MorphRow exposing (atom, succeed, atLeast, take, drop)

    -- parse a simple email, but we're only interested in the username
    "user@example.com"
        |> Text.narrowWith
            (succeed (\userName -> { username = userName })
                |> grab .username (atLeast 1 aToZ)
                |> skip (one '@')
                |> skip
                    (Text.fromList
                        |> MorphRow.over (atLeast 1 aToZ)
                        |> broadenFrom "example"
                    )
                |> skip (text ".com")
            )
    --> Ok { username = "user" }

[`broadenFrom`](Morph#broadenFrom) `... |>` [`MorphRow.over`](MorphRow#over) is cool:
when multiple kinds of input can be dropped,
it allows choosing a default possibility for building.

-}
skip :
    MorphRow atom () expectedCustom
    ->
        (GroupMorphRowInProgress atom groupNarrow narrow expectedCustom
         -> GroupMorphRowInProgress atom groupNarrow narrow expectedCustom
        )
skip ignoredNext =
    \groupMorphRowSoFar ->
        { narrow =
            \broad ->
                broad
                    |> narrowStep groupMorphRowSoFar
                    |> Result.andThen
                        (\result ->
                            result.broad
                                |> narrowStep ignoredNext
                                |> Result.map
                                    (\nextParsed ->
                                        { narrow = result.narrow
                                        , broad = nextParsed.broad
                                        }
                                    )
                        )
                    |> onRemainingErr
        , broaden =
            \groupNarrow ->
                (() |> ignoredNext.broaden)
                    |> Stack.onTopStack
                        (groupNarrow |> groupMorphRowSoFar.broaden)
        }


{-| Match any single character.

> ℹ️ Equivalent regular expression: `.`

    import MorphRow exposing (oneAny)
    import MorphRow.Error
    import Morph.TextRow as Text

    -- can match any character
    "abc" |> Text.narrowWith oneAny --> Ok 'a'
    "#hashtag" |> Text.narrowWith oneAny --> Ok '#'

    -- only fails if we run out of inputs
    ""
        |> Text.narrowWith oneAny
        |> Result.mapError MorphRow.Error.textMessage
    --> Err "1:0: I was expecting a character. I reached the end of the input."

-}
oneAny : MorphRow atom atom expectedCustom_
oneAny =
    { narrow =
        \broad ->
            case broad of
                Hand.Empty _ ->
                    Morph.Expected
                        (Success
                            { expected = Morph.MoreInput
                            , startingAtDown = 0
                            , description = Hand.empty
                            }
                        )
                        |> Err

                Hand.Filled (Stack.TopDown nextAtom tail) ->
                    tail |> Stack.fromList |> narrow (nextAtom |> succeed)
    , broaden = Stack.only
    }


{-| Match only a specific sequence of single broad input elements.

    import MorphRow exposing (atom)
    import MorphRow.Error
    import Morph.TextRow as Text

    -- Match a specific character, case sensitive
    "abc" |> Text.narrowWith (specific [ 'a' ]) --> Ok 'a'

    -- It fails if it's not _exactly_ the same
    "A"
        |> Text.narrowWith (specific [ 'a' ])
        |> Result.mapError MorphRow.Error.textMessage
    --> Err "1:1: I was expecting the character 'a'. I got stuck when I got the character 'A'."

-}
specific : List atom -> MorphRow atom () expectedCustom_
specific expectedConstantInput =
    broadenFrom
        (List.repeat (expectedConstantInput |> List.length) ())
        |> over
            (expectedConstantInput
                |> List.map (Morph.specific >> one)
                |> sequence
            )


{-| [`MorphRow`](#MorphRow) from and to a single broad input.

Short for [`MorphRow.over`](#over) [`oneAny`](#oneAny)

-}
one :
    Morph narrow element (Morph.Error element expectCustom)
    -> MorphRow element narrow expectCustom
one =
    over oneAny


{-| Always succeed with the given value.

    import MorphRow exposing (succeed)

    "no matter the input text"
        |> Text.narrowWith (succeed "abc")
    --> Ok "abc"

For anything composed of multiple parts,

> `succeed` is the key to success
> – folks from [elm radio](https://elm-radio.com/)

first declaratively describing what you expect to get in the end,
then [taking](#grab) and [dropping](#skip) what you need to parse.

    import MorphRow exposing (succeed, one)
    import Morph.TextRow exposing (integer)

    type alias Point =
        -- makes `Point` function unavailable:
        -- https://dark.elm.dmy.fr/packages/lue-bird/elm-no-record-type-alias-constructor-function/latest/
        RecordWithoutConstructorFunction
            { x : Int
            , y : Int
            }

    point : MorphRow Char Point
    point =
        succeed (\x y -> { x = x, y = y })
            |> grab .x integer
            |> skip (one ',')
            |> grab .y integer

    "12,34" |> Text.narrowWith point
    --> Ok { x = 12, y = 34 }


### `succeed` anti-patterns

One example you'll run into when using other parsers is using

    succeed identity
        |> skip ...
        |> skip ...
        |> grab ...
        |> skip ...

it get's pretty hard to read as you have to jump around the code to know what you're actually producing.

    succeed (\sum -> sum) |> ...

is already nicer.

-}
succeed :
    narrow
    -> GroupMorphRowInProgress atom_ narrowNarrowBroadenable_ narrow expectedCustom_
succeed narrowConstant =
    { narrow =
        \broad ->
            { narrow = narrowConstant
            , broad = broad
            }
                |> Ok
                |> onRemainingErr
    , broaden =
        \_ -> Hand.empty
    }



-- chain


{-| Always fail on this branch. Make sure you've explained what to [`expect`](#expect).

    import MorphRow.Error
    import Morph.TextRow

    ""
        |> Morph.TextRow.narrowWith
            (expect "nothing, this always fails"
                fail
            )
        |> Result.mapError MorphRow.Error.textMessage
    --> Err "1:0: I was expecting nothing, this always fails. I reached the end of the broad."

This is often used in combination with [`next`](#next).

TODO: remove?

-}
fail : MorphRow atom_ narrow_ expectedCustom_
fail =
    { narrow =
        \broad ->
            Expected
                (Success
                    { expected = Morph.NoFail
                    , startingAtDown = broad |> Stack.length
                    , description = Hand.empty
                    }
                )
                |> Err
    , broaden = \_ -> Hand.empty
    }


{-| Possibly incomplete [`MorphRow`](#MorphRow) to and from a choice.
See [`Morph.choice`](Morph#choice), [`MorphRow.possibility`](#possibility), [`MorphRow.choiceFinish`](#choiceFinish)
-}
type alias ChoiceMorphRowInProgress atom choiceNarrow choiceBroaden expectedCustom =
    Morph.ChoiceMorphInProgress
        { narrow : choiceNarrow
        , broad : Hand (Stacked atom) Possibly Empty
        }
        (Hand (Stacked atom) Possibly Empty)
        choiceBroaden
        (SuccessExpectation atom expectedCustom)


{-| If the previous [`possibility`](#possibility) fails
try this [`MorphRow`](#MorphRow).

> ℹ️ Equivalent regular expression: `|`

    import MorphRow exposing (atom)
    import Morph.Char as Char
    import MorphRow.Error

    type UnderscoreOrLetter
        = Underscore
        | Letter Char

    underscoreOrLetter : MorphRow Char UnderscoreOrLetter
    underscoreOrLetter =
        choice
            (\underscoreVariant letterVariant underscoreOrLetterNarrow ->
                case underscoreOrLetterNarrow of
                    Underscore ->
                        underscoreVariant ()

                    Letter letter ->
                        letterVariant letter
            )
            |> possibility (\() -> Underscore) (Morph.specific '_')
            |> possibility Letter Char.aToZ
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
        |> Text.narrowWith (onFailDown [ one '_', Char.letter ])
        |> Result.mapError MorphRow.Error.textMessage
    --> Err "1:1: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '1'."


### example: fallback step if the previous step fails

    import Morph
    import Morph.Char as Char
    import MorphRow.Error

    type AlphaNum
        = Digits (List Digit.Morph.N0To9)
        | Letters String

    alphaNum : MorphRow Char AlphaNum
    alphaNum =
        Morph.choice
            (\digit letter alphaNum ->
                case alphaNum of
                    Digits int ->
                        digit int

                    Letters char ->
                        letter char
            )
            |> Morph.possibility Letter
                (map Morph.Text.fromList
                    (atLeast 1 Char.letter)
                )
            |> Morph.possibility Digit
                (atLeast 1 Digit.Morph.n0To9)
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
        |> Result.mapError MorphRow.Error.textMessage
    --> Err "1:1: I was expecting at least 1 digit [0-9]. I got stuck when I got the character '_'."

-}
possibility :
    (possibilityNarrow -> choiceNarrow)
    ->
        MorphRow
            atom
            possibilityNarrow
            possibilityExpectation
    ->
        (ChoiceMorphRowInProgress
            atom
            choiceNarrow
            ((possibilityNarrow -> Hand (Stacked atom) Possibly Empty)
             -> choiceBroadenFurther
            )
            possibilityExpectation
         ->
            ChoiceMorphRowInProgress
                atom
                choiceNarrow
                choiceBroadenFurther
                possibilityExpectation
        )
possibility possibilityToChoice possibilityMorph =
    \choiceMorphSoFar ->
        { narrow =
            \choiceBroad ->
                choiceBroad
                    |> choiceMorphSoFar.narrow
                    |> restoreTry
                        (\(Morph.Expected soFarExpectationMiss) ->
                            case choiceBroad |> narrow possibilityMorph |> onRemainingOk of
                                Ok possibilityParsed ->
                                    { broad = possibilityParsed.broad
                                    , narrow =
                                        possibilityParsed.narrow
                                            |> possibilityToChoice
                                    }
                                        |> Ok

                                Err possibilityExpectation ->
                                    Expected
                                        { possibilities =
                                            soFarExpectationMiss.possibilities
                                                |> Stack.onTopLay possibilityExpectation
                                        }
                                        |> Err
                        )
        , broaden =
            choiceMorphSoFar.broaden
                (broaden possibilityMorph)
        }


{-| Always the last step in a [`Morph.choice`](Morph#choice) `|>` [`MorphRow.possibility`](#possibility) build process.
-}
choiceFinish :
    ChoiceMorphRowInProgress
        atom
        choiceNarrow
        (choiceNarrow -> Hand (Stacked atom) Possibly Empty)
        expectedCustom
    -> MorphRow atom choiceNarrow expectedCustom
choiceFinish =
    \choiceMorphRowComplete ->
        { narrow =
            \broad ->
                broad
                    |> choiceMorphRowComplete.narrow
                    |> Result.mapError
                        (\(Morph.Expected choiceExpectation) ->
                            { expected =
                                Morph.OneIn choiceExpectation.possibilities
                            , startingAtDown = broad |> Stack.length
                            , description = Hand.empty
                            }
                        )
                    |> onRemainingErr
        , broaden = choiceMorphRowComplete.broaden
        }


{-| After [converting](#MorphRow) the previous section,
form another [morph](#MorphRow) with the value we got.

It allows using the last value for the next [`MorphRow`](#MorphRow) like a backreference.

But!

  - try to keep [`next`](#next) filters/validations to a minimum to get
      - a better error description out of the box
      - a more descriptive and correct type
      - building invalid values becomes impossible
  - [loop](#loop) instead of recursively recursively [`next`](#next)
  - try to know what to morph by tracking context,
    independent of what narrow result the last morph gave
      - for example don't use [`next`](#next) for versioning etc.
        Use [`choice`](Morph#choice) where each [`possibility`](#possibility) expects a specific number

TODO: add example
TODO: remove?

-}
next :
    (narrow -> broad)
    -> (broad -> MorphRow atom narrow expectedCustom)
    ->
        (MorphRow atom broad expectedCustom
         -> MorphRow atom narrow expectedCustom
        )
next narrowNarrowToNarrow morphNarrowNarrow =
    \morphRowBefore ->
        { narrow =
            narrowStep morphRowBefore
                >> Result.andThen
                    (\result ->
                        result.broad
                            |> narrowStep
                                (morphNarrowNarrow result.narrow)
                    )
                >> onRemainingErr
        , broaden =
            \narrowNarrow ->
                let
                    narrowed =
                        narrowNarrow |> narrowNarrowToNarrow
                in
                (narrowed |> broaden morphRowBefore)
                    |> Stack.onTopStack
                        (narrowNarrow
                            |> broaden (morphNarrowNarrow narrowed)
                        )
        }



-- transform


errorFromMorph :
    { startingAtDown : Int }
    -> Morph.Error atom description
    -> SuccessExpectation atom description
errorFromMorph location =
    \morphError ->
        { expected =
            morphError.expected |> expectationFromMorph location
        , description = morphError.description
        , startingAtDown = location.startingAtDown
        }


{-| Describe how to reach an even broader type.

    import Morph exposing (listToString)
    import MorphRow exposing (map, atLeast)
    import Morph.Char as Char
    import Morph.TextRow as Text

    -- get some letters, make them lowercase
    "ABC"
        |> Text.narrowWith
            (atLeast 1 Char.letter
                |> map listToString
                |> map (translate identity String.toLower)
            )
    --> Ok "abc"

-}
over :
    MorphRow atom narrow expectedCustom
    ->
        (Morph narrowNarrow narrow (Morph.Error atom expectedCustom)
         -> MorphRow atom narrowNarrow expectedCustom
        )
over morphRowBeforeMorph =
    \narrowMorph ->
        { narrow =
            \broad ->
                broad
                    |> narrowStep morphRowBeforeMorph
                    |> Result.andThen
                        (\narrowed ->
                            narrowed.narrow
                                |> narrow narrowMorph
                                |> Result.map
                                    (\narrowNarrow ->
                                        { narrow = narrowNarrow
                                        , broad = narrowed.broad
                                        }
                                    )
                                |> Result.mapError
                                    (errorFromMorph { startingAtDown = broad |> Stack.length })
                        )
                    >> onRemainingErr
        , broaden =
            broaden narrowMorph
                >> broaden morphRowBeforeMorph
        }


expectationFromMorph :
    { startingAtDown : Int }
    -> Morph.Expectation specific description
    -> Morph.ExpectationWith { startingAtDown : Int } specific description
expectationFromMorph location =
    \morphExpectation ->
        case morphExpectation of
            Morph.NoFail ->
                Morph.NoFail

            Morph.MoreInput ->
                Morph.MoreInput

            Morph.NoMoreInput ->
                Morph.NoMoreInput

            Morph.Specific specificAtom ->
                Morph.Specific specificAtom

            Morph.OneIn possibleErrors ->
                Morph.OneIn
                    (possibleErrors
                        |> Stack.map (\_ -> errorFromMorph location)
                    )



-- sequence


{-| Match a sequence of parsers in order, and gets the result as a `List`.

    import MorphRow exposing (map, one)
    import Morph.Char as Char
    import Morph.TextRow as Text

    -- all parsers must be of the same type
    "bau5"
        |> Text.narrowWith
            (sequence [ Char.aToZLower, Char.digit ])
    --> Ok [ 'b', 'a', 'u', Digit.Morph.N5 ]

Don't try to be clever with this.
The usual [`succeed`](#succeed)`(\... -> ...) |>`[`grab`](#grab)-[`skip`](#skip) chain
is often more explicit and descriptive.

-}
sequence :
    List (MorphRow atom narrow expectedCustom)
    -> MorphRow atom (List narrow) expectedCustom
sequence morphRowsInSequence =
    let
        step :
            MorphRow atom narrow expectedCustom
            ->
                { narrow : List narrow
                , broad : Hand (Stacked atom) Possibly Empty
                }
            ->
                Result
                    (SuccessExpectation atom expectedCustom)
                    { narrow : List narrow
                    , broad : Hand (Stacked atom) Possibly Empty
                    }
        step stepMorphRow =
            \soFar ->
                soFar.broad
                    |> narrowStep stepMorphRow
                    |> Result.map
                        (\stepParsed ->
                            { broad = stepParsed.broad
                            , narrow =
                                soFar.narrow
                                    |> (::) stepParsed.narrow
                            }
                        )
    in
    { narrow =
        \initialInput ->
            morphRowsInSequence
                |> List.foldr
                    (\stepMorphRow ->
                        Result.andThen (step stepMorphRow)
                    )
                    ({ narrow = [], broad = initialInput } |> Ok)
                |> onRemainingErr
    , broaden =
        \narrowSequence ->
            List.map2
                (\morphInSequence -> broaden morphInSequence)
                morphRowsInSequence
                narrowSequence
                |> List.concatMap Stack.toList
                |> Stack.fromList
    }


{-| Match an optional value and returns it as a `Maybe`.

> ℹ️ Equivalent regular expression: `?`

    import Morph.Char exposing (letter)
    import Morph.TextRow as Text

    -- maybe we get `Just` a letter
    "abc" |> Text.narrowWith (maybe Char.letter)
    --> Ok (Just 'a')

    -- maybe we get `Nothing`
    "123abc" |> Text.narrowWith (maybe Char.letter)
    --> Ok Nothing

-}
maybe :
    MorphRow atom contentNarrow expectedCustom
    -> MorphRow atom (Maybe contentNarrow) expectedCustom
maybe contentMorphRow =
    choice
        (\justVariant nothingVariant maybeNarrow ->
            case maybeNarrow of
                Just justValue ->
                    justVariant justValue

                Nothing ->
                    nothingVariant ()
        )
        |> possibility Just contentMorphRow
        |> possibility (\() -> Nothing) (succeed ())
        |> choiceFinish


{-| Match a value `exactly` a number of times and returns them as a `List`.

> ℹ️ Equivalent regular expression: `{n}`

    import MorphRow.Error
    import Morph.Char as Char
    import Morph.TextRow as Text

    -- we want `exactly 3` letters
    "abcdef" |> narrow (map Text.fromList (exactly 3 Char.letter))
    --> Ok [ 'a', 'b', 'c' ]

    -- not 2 or 4, we want 3
    "ab_def"
        |> narrow (map Text.fromList (exactly 3 Char.letter))
        |> Result.mapError MorphRow.Error.textMessage
    --> Err "1:3: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '_'."

-}
exactly :
    Int
    -> MorphRow atom narrow expectedCustom
    -> MorphRow atom (List narrow) expectedCustom
exactly howOften repeatedMorphRow =
    sequence (List.repeat howOften repeatedMorphRow)


loopWhile :
    (List goOnElement
     -> MorphRow atom goOnElement expectedCustom
    )
    -> MorphRow atom (List goOnElement) expectedCustom
loopWhile whileStep =
    let
        loopStep =
            \before_ ->
                choice
                    (\goOn commit loopStepNarrow ->
                        case loopStepNarrow of
                            GoOn goOnELement ->
                                goOn goOnELement

                            Commit commitElement ->
                                commit commitElement
                    )
                    |> possibility GoOn (before_ |> whileStep)
                    |> possibility Commit (succeed ())
                    |> choiceFinish
    in
    { broaden =
        let
            broadenStepBack :
                ()
                ->
                    (List goOnElement
                     -> Hand (Stacked atom) Possibly Empty
                    )
            broadenStepBack () =
                \toStep ->
                    case toStep of
                        [] ->
                            Hand.empty

                        top :: tail ->
                            (top |> GoOn)
                                |> broaden (tail |> loopStep)
                                |> Stack.onTopStack (tail |> broadenStepBack ())
        in
        \commitResultNarrow ->
            commitResultNarrow
                |> List.reverse
                |> broadenStepBack ()
    , narrow =
        let
            loopNarrowStep :
                ()
                ->
                    (List goOnElement
                     ->
                        (Hand (Stacked atom) Possibly Empty
                         ->
                            Result
                                (SuccessExpectation atom expectedCustom)
                                { narrow : List goOnElement
                                , broad : Hand (Stacked atom) Possibly Empty
                                }
                        )
                    )
            loopNarrowStep () =
                \before_ ->
                    narrowStep (before_ |> loopStep)
                        >> Result.andThen
                            (\stepped ->
                                case stepped.narrow of
                                    Commit () ->
                                        { broad = stepped.broad
                                        , narrow = before_ |> List.reverse
                                        }
                                            |> Ok

                                    GoOn goOnElement ->
                                        stepped.broad
                                            |> (before_ |> (::) goOnElement |> loopNarrowStep ())
                            )
        in
        ([] |> loopNarrowStep ())
            >> onRemainingErr
    }


{-| Match a value at least a number of times and returns them as a `List`.

> ℹ️ Equivalent regular expression: `{min,}`

    import MorphRow.Error
    import Morph.Char as Char
    import Morph.TextRow as Text

    -- we want at least three letters, we are okay with more than three
    "abcdef"
        |> Text.narrowWith (atLeast 3 Char.letter)
    --> Ok [ 'a', 'b', 'c', 'd', 'e', 'f' ]

    -- but not two, that's sacrilegious
    "ab_def"
        |> Text.narrowWith (atLeast 3 Char.letter)
        |> Result.mapError MorphRow.Error.textMessage
    --> Err "1:3: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '_'."


## `arLeast 0`

> ℹ️ Equivalent regular expression: `*`

    import Morph.Char as Char
    import Morph.TextRow as Text

    -- We want as many letters as there are.
    "abc" |> Text.narrowWith (atLeast 0 Char.letter)
    --> Ok [ 'a', 'b', 'c' ]

    "abc123" |> Text.narrowWith (atLeast 0 Char.letter)
    --> Ok [ 'a', 'b', 'c' ]

    -- even zero letters is okay
    "123abc" |> Text.narrowWith (atLeast 0 Char.letter)
    --> Ok []


### `atLeast 1`

> ℹ️ Equivalent regular expression: `+`

    import MorphRow.Error
    import Morph.Char as Char
    import Morph.TextRow as Text

    -- we want as many letters as there are
    "abc" |> Text.narrowWith (atLeast 1 Char.letter)
    --> Ok [ 'a', 'b', 'c' ]

    "abc123" |> Text.narrowWith (atLeast 1 Char.letter)
    --> Ok [ 'a', 'b', 'c' ]

    -- but we want at least one
    "123abc"
        |> Text.narrowWith (atLeast 1 Char.letter)
        |> Result.mapError MorphRow.Error.textMessage
    --> Err "1:1: I was expecting a letter a|..|z or A|...|Z. I got stuck when I got the character '1'."

-}
atLeast :
    Int
    -> MorphRow atom narrow expectedCustom
    -> MorphRow atom (List narrow) expectedCustom
atLeast minimum elementStepMorphRow =
    succeed (\minimumList overMinimum -> minimumList ++ overMinimum)
        |> grab (List.take minimum)
            (exactly minimum elementStepMorphRow)
        |> grab (List.drop minimum)
            (loopWhile (\_ -> elementStepMorphRow))


{-| How are [`atLeast`](#atLeast), ... defined?
The [section loop](#loop) explains why using [`next`](#next) recursively is a bad idea.

    import Morph exposing (choice, validate)
    import MorphRow exposing (MorphRow, one, succeed, atLeast, take, drop, while)
    import Morph.Char
    import Morph.Text
    import Number.Morph

    sumWhileLessThan : Float -> MorphRow Char (List Number)
    sumWhileLessThan max =
        while
            (\stepped ->
                let
                    floats =
                        stepped |> Stack.map (\_ -> Morph.map Number.Morph.toFloat)
                in
                if (floats |> Stack.sum) < max then
                    Ok ()
                else
                    Err ()
            )
            (succeed (\n -> n)
                |> grab (\n -> n) Number.Morph.text
                |> skip (atLeast 0 (Morph.Char.blank |> one))
            )

    -- stops before we reach a maximum of 6 in the sum
    "2 3 4"
        |> narrow
            (Morph.Text.fromList
                |> MorphRow.over
                    (succeed (\numbers -> numbers)
                        |> grab (\numbers -> numbers) (sumWhileLessThan 6)
                        |> skip (Text.Morph.specific "4")
                    )
            )
    --> Ok 5

-}
while :
    (Hand (Stacked element) Never Empty -> Result () ())
    -> MorphRow atom element possibilityExpectationCustom
    -> MorphRow atom (List element) possibilityExpectationCustom
while elementValidate elementStepMorphRowInLoopToGoOn =
    loopWhile
        (\soFar ->
            validate
                (\element ->
                    case soFar |> Stack.topDown element |> elementValidate of
                        Ok () ->
                            element |> Ok

                        Err () ->
                            Morph.failure
                )
                |> over elementStepMorphRowInLoopToGoOn
        )


{-| [Morph](#MorphRow) multiple elements from now to when `end` matches.

    decoderNameSubject : MorphRow String Char expectationCustom_
    decoderNameSubject =
        Text.fromList
            |> MorphRow.over
                (MorphRow.before
                    { end =
                        MorphRow.succeed ()
                            |> skip (Morph.Text.specific "Decoder")
                            |> skip MorphRow.end
                    , goOn = MorphRow.oneAny
                    }
                )

You might think: Why not use

    decoderNameSubject : MorphRow String Char expectationCustom_
    decoderNameSubject =
        MorphRow.succeed (\subject -> subject)
            |> grab (\subject -> subject) (atLeast 0 MorphRow.oneAny)
            |> skip (Morph.Text.specific "Decoder")
            |> skip MorphRow.end

Problem is: This will never succeed.
`atLeast 0 MorphRow.oneAny` always goes on.
We never reach the necessary [`skip`](#skip)ped things.

-}
before :
    { end : MorphRow atom () expectedCustom
    , goOn : MorphRow atom goOnElement expectedCustom
    }
    -> MorphRow atom (List goOnElement) expectedCustom
before untilStep =
    until
        { commit =
            translate
                (\before_ -> { before = before_, end = () })
                .before
        , end = untilStep.end
        , goOn = untilStep.goOn
        }


{-| How are [`between`](#between), ... defined?
The [section loop](#loop) explains why using [`next`](#next) recursively is a bad idea.

    decoderNameSubject : MorphRow String Char expectationCustom_
    decoderNameSubject =
        Text.fromList
            |> MorphRow.over
                (MorphRow.until
                    { commit =
                        translate
                            (\before -> { before = before, end = () })
                            .before
                    , end =
                        MorphRow.succeed ()
                            |> skip (Morph.Text.specific "Decoder")
                            |> skip MorphRow.end
                    , goOn = MorphRow.oneAny
                    }
                )

↑ can be simplified with [`before`](#before)

Any kind of structure validation that if it fails should proceed to `goOn`
must be in `commit`

-}
until :
    { commit :
        Morph
            commitResult
            { end : endElement
            , before : List goOnElement
            }
            (Morph.Error atom expectedCustom)
    , end : MorphRow atom endElement expectedCustom
    , goOn : MorphRow atom goOnElement expectedCustom
    }
    -> MorphRow atom commitResult expectedCustom
until untilStep =
    let
        loopStep =
            choice
                (\commit goOn loopStepNarrow ->
                    case loopStepNarrow of
                        Commit commitElement ->
                            commit commitElement

                        GoOn goOnELement ->
                            goOn goOnELement
                )
                |> possibility Commit untilStep.end
                |> possibility GoOn untilStep.goOn
                |> choiceFinish
    in
    { broaden =
        let
            broadenStepBack :
                ()
                ->
                    (List goOnElement
                     -> Hand (Stacked atom) Possibly Empty
                    )
            broadenStepBack () =
                \toStep ->
                    case toStep of
                        [] ->
                            Hand.empty

                        top :: tail ->
                            (top |> GoOn)
                                |> broaden loopStep
                                |> Stack.onTopStack
                                    (tail |> broadenStepBack ())
        in
        \commitResultNarrow ->
            let
                committedBack =
                    commitResultNarrow
                        |> broaden untilStep.commit
            in
            (committedBack.before
                |> List.reverse
                |> broadenStepBack ()
            )
                |> Stack.onTopStack
                    ((committedBack.end |> Commit)
                        |> broaden loopStep
                    )
    , narrow =
        let
            loopNarrowStep :
                ()
                ->
                    (List goOnElement
                     ->
                        (Hand (Stacked atom) Possibly Empty
                         ->
                            Result
                                (SuccessExpectation atom expectedCustom)
                                { narrow : commitResult
                                , broad : Hand (Stacked atom) Possibly Empty
                                }
                        )
                    )
            loopNarrowStep () =
                \before_ ->
                    narrowStep loopStep
                        >> Result.andThen
                            (\stepped ->
                                case stepped.narrow of
                                    Commit committed ->
                                        case { before = before_, end = committed } |> narrow untilStep.commit of
                                            Err error ->
                                                error
                                                    |> errorFromMorph
                                                        { startingAtDown = stepped.broad |> Stack.length }
                                                    |> Err

                                            Ok commitResult ->
                                                { broad = stepped.broad
                                                , narrow = commitResult
                                                }
                                                    |> Ok

                                    GoOn goOnElement ->
                                        stepped.broad
                                            |> (before_ |> (::) goOnElement |> loopNarrowStep ())
                            )
        in
        ([] |> loopNarrowStep ())
            >> onRemainingErr
    }


{-| Match a value between a range of times and returns them as a `List`.

> ℹ️ Equivalent regular expression: `{min,max}`

    import MorphRow.Error
    import Morph.Char as Char
    import Morph.TextRow as Text

    -- we want between two and four letters
    "abcdef" |> Text.narrowWith (between 2 4 Char.letter)
    --> Ok [ 'a', 'b', 'c', 'd' ]

    "abc_ef" |> Text.narrowWith (between 2 4 Char.letter)
    --> Ok [ 'a', 'b', 'c' ]

    "ab_def" |> Text.narrowWith (between 2 4 Char.letter)
    --> Ok [ 'a', 'b' ]


    -- but less than that is not cool
    "i_am_here"
        |> Text.narrowWith (between 2 3 letter)
        |> Result.mapError MorphRow.Error.textMessage
    --> Err "1:2: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '_'."


### example: `between 0 1`

Alternative to [`maybe`](#maybe) which instead returns a `List`.

> ℹ️ Equivalent regular expression: `?`

    import Morph.Char as Char
    import Morph.TextRow as Text

    -- we want one letter, optionally
    "abc" |> Text.narrowWith (between 0 1 Char.letter)
    --> Ok [ 'a' ]

    -- if we don't get any, that's still okay
    "123abc" |> Text.narrowWith (between 0 1 Char.letter)
    --> Ok []


### example: at most

> ℹ️ Equivalent regular expression: `{0,max}`

    import MorphRow exposing (atom)
    import Morph.Char as Char
    import Morph.TextRow as Text

    -- we want a maximum of three letters
    "abcdef" |> Text.narrowWith (between 0 3 Char.letter)
    --> Ok [ 'a', 'b', 'c' ]

    -- less than that is also okay
    "ab_def" |> Text.narrowWith (between 0 3 Char.letter)
    --> Ok [ 'a', 'b' ]

    -- even zero letters are fine
    "_underscore" |> Text.narrowWith (between 0 3 Char.letter)
    --> Ok []

    -- make sure we don't consume more than three letters
    "abcdef"
        |> Text.narrowWith
            (succeed (\letters -> letters)
                |> grab (between 0 3 Char.letter)
                |> skip (one 'd')
            )
    --> Ok [ 'a', 'b', 'c' ]

-}
between :
    Int
    -> Int
    -> MorphRow atom narrow expectedCustom
    -> MorphRow atom (List narrow) expectedCustom
between minimum maximum repeatedMorphRow =
    succeed (\minimumList overMinimum -> minimumList ++ overMinimum)
        |> grab
            (List.take minimum)
            (exactly minimum repeatedMorphRow)
        |> grab
            (List.drop maximum)
            (atMost (maximum - minimum) repeatedMorphRow)


{-| Match a value at most a number of times and returns them as a `List`.

**Shouldn't be exposed**

-}
atMost :
    Int
    -> MorphRow atom narrow expectedCustom
    -> MorphRow atom (List narrow) expectedCustom
atMost maximum elementStepMorphRow =
    until
        { commit =
            validate
                (\soFar ->
                    if (soFar |> List.length) >= maximum then
                        soFar |> Ok

                    else
                        Morph.failure
                )
                |> Morph.over
                    (translate
                        (\before_ -> { end = (), before = before_ })
                        .before
                    )
        , end = succeed ()
        , goOn = elementStepMorphRow
        }


{-| How to continue this `loop`.
Either continue with a partial result or return with a complete value.
-}
type LoopStep partial complete
    = GoOn partial
    | Commit complete


{-| [Morph](#MorphRow) parts and interspersed separators into a `Stack`.

    import Stack
    import MorphRow exposing (separatedBy, atLeast, one)
    import Morph.TextRow as Text exposing (text)
    import Morph.Char as Char

    -- note that both values and separators must be of the same type
    "a,bc,def"
        |> Text.narrowWith
            (separatedBy
                ( atLeast 0, Morph.Text.specific "," )
                (atLeast 0 (Morph.Char.aToZLower |> one))
            )
    --> Ok
    -->     (Stack.topDown
    -->         [ 'a' ]
    -->         [ { separator = (), part = [ 'b', 'c' ] }
    -->         , { separator = (), part = [ 'd', 'e', 'f' ] }
    -->         ]
    -->     )

    -- leading/trailing separators are valid and give empty parts
    ",a,,"
        |> Text.narrowWith
            (separatedBy
                ( atLeast 0, Morph.Text.specific "," )
                (atLeast 0 (Morph.Char.aToZLower |> one))
            )
    --> Ok
    -->     (Stack.topDown
    -->         []
    -->         [ { separator = (), part = [ 'a' ] }
    -->         , { separator = (), part = [] }
    -->         , { separator = (), part = [] }
    -->         ]
    -->     )

    -- an empty input text gives a single element from an empty string
    ""
        |> Text.narrowWith
            (separatedBy
                ( atLeast 0, Morph.Text.specific "," )
                (atLeast 0 (Morph.Char.aToZLower |> one))
            )
    --> Ok (topDown [] [])

Attention on separated part [`MorphRow`](#MorphRow)s:

    separatedBy
        ( atLeast 0, Morph.Text.specific "," )
        (atLeast 0 oneAny)

would only parse 1 part until the end
because `atLeast 0 oneAny` always [`succeed`](#succeed)s.
No separator would ever be parsed.

-}
separatedBy :
    ( MorphRow atom { separator : separator, part : part } expectedCustom
      -> MorphRow atom (List { separator : separator, part : part }) expectedCustom
    , MorphRow atom separator expectedCustom
    )
    -> MorphRow atom part expectedCustom
    ->
        MorphRow
            atom
            (Hand
                (StackTopBelow
                    part
                    { separator : separator, part : part }
                )
                Never
                Empty
            )
            expectedCustom
separatedBy ( separatorsToSequence, separatorMorphRow ) partMorphRow =
    succeed topDown
        |> grab Stack.top partMorphRow
        |> grab (Stack.topRemove >> Stack.toList)
            (separatorsToSequence
                (succeed
                    (\separator part ->
                        { separator = separator, part = part }
                    )
                    |> grab .separator separatorMorphRow
                    |> grab .part partMorphRow
                )
            )


{-| Only matches when there's no further broad input afterwards.

This is not required for [`narrow`](Morph#narrow)ing to succeed.

It can, however simplify checking for specific endings:

    decoderNameSubject : MorphRow String Char expectationCustom_
    decoderNameSubject =
        Text.fromList
            |> MorphRow.over
                (MorphRow.until
                    { commit =
                        translate
                            (\before -> { before = before, end = () })
                            .before
                    , end =
                        MorphRow.succeed ()
                            |> skip (Morph.Text.specific "Decoder")
                            |> skip MorphRow.end
                    , goOn = MorphRow.oneAny
                    }
                )

-}
end : MorphRow atom_ () expectationCustom_
end =
    { narrow =
        \broad ->
            case broad of
                Hand.Empty _ ->
                    Hand.empty |> narrow (succeed ())

                Hand.Filled stacked ->
                    Morph.Expected
                        (Success
                            { expected = Morph.NoMoreInput
                            , startingAtDown = stacked |> filled |> Stack.length
                            , description = Hand.empty
                            }
                        )
                        |> Err
    , broaden =
        \() -> Hand.empty
    }
