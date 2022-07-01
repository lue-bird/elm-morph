module MorphRow exposing
    ( MorphRow
    , SuccessExpectation, Error, InputEndOrSuccess(..)
    , succeed, expect, fail
    , atomAny, atom
    , specific
    , skip, grab, GroupMorphRowInProgress
    , over, next
    , possibility, ChoiceMorphRowInProgress, choiceFinish
    , sequence
    , atLeast, between, exactly, maybe
    , split
    , loop, LoopStep(..)
    )

{-| A simple, easy to use, general-purpose parser-builder with good error messages.

Heavily inspired by

  - [`elm/parser`](https://package.elm-lang.org/packages/elm/parser/latest/)
  - [`dasch/parser`](https://package.elm-lang.org/packages/dasch/parser/latest/)
  - especially [`lambda-phi/parser`](https://dark.elm.dmy.fr/packages/lambda-phi/parser/latest/)


## example: 2D point

    import MorphRow exposing (MorphRow, atLeast, drop, into, succeed, take, atom)
    import Morph.CharRow as Char
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
                        |> MorphRow.over (atLeast 0 (Char.blank |> atom))
                    )
                |> grab number
                |> skip
                    (broadenFrom []
                        |> MorphRow.over (atLeast 0 (Char.blank |> atom))
                    )
                |> skip (Morph.Text.specific ",")
                |> skip
                    (broadenFrom [ Char.Space ]
                        |> MorphRow.over (atLeast 0 (Char.blank |> atom))
                    )
                |> grab .x Number.Morph.text
                |> skip
                    (broadenFrom [ Char.Space ]
                        |> MorphRow.over (atLeast 0 (Char.blank |> atom))
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
@docs atomAny, atom
@docs specific


## chain

@docs skip, grab, GroupMorphRowInProgress
@docs over, next


### choices

@docs possibility, ChoiceMorphRowInProgress, choiceFinish


## sequences

@docs sequence
@docs atLeast, between, exactly, maybe
@docs split


### looping

@docs loop, LoopStep

-}

import Hand exposing (Empty, Hand, filled)
import Morph exposing (Expected(..), GroupMorphInProgress, Morph, broaden, broadenFrom, choice, narrow, translate)
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
    import Morph.CharRow as Char
    import Morph.TextRow as Text

    -- we can redefine an error message if something goes wrong
    "123"
        |> Text.narrowWith
            (expect "a name consisting of letters"
                (atLeast 1 Char.letter)
            )
        |> Result.mapError MorphRow.Error.textMessage
    --> Err "1:1: I was expecting a name consisting of letters. I got stuck when I got '1'."


    import MorphRow exposing (take, drop, succeed, expect, atom)
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
                |> skip (atom '(')
                |> grab .x Text.number
                |> skip (atom ',')
                |> grab .y Text.number
                |> skip (atom ')')
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
                |> skip (atom '@')
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

    import MorphRow exposing (atomAny)
    import MorphRow.Error
    import Morph.TextRow as Text

    -- can match any character
    "abc" |> Text.narrowWith atomAny --> Ok 'a'
    "#hashtag" |> Text.narrowWith atomAny --> Ok '#'

    -- only fails if we run out of inputs
    ""
        |> Text.narrowWith atomAny
        |> Result.mapError MorphRow.Error.textMessage
    --> Err "1:0: I was expecting a character. I reached the end of the input."

-}
atomAny : MorphRow atom atom expectedCustom_
atomAny =
    expectDefault Morph.MoreInput
        { narrow =
            \broad ->
                case broad of
                    Hand.Empty _ ->
                        Hand.empty |> narrow fail

                    Hand.Filled (Stack.TopDown nextAtom tail) ->
                        narrow (nextAtom |> succeed) (Stack.fromList tail)
        , broaden = Stack.only
        }


{-| [Expect](Morph#Expectation) something basic. Use [`expect`](#expect) for custom failure states.
-}
expectDefault :
    Morph.ExpectationWith { startingAtDown : Int } atom expectedCustom
    -> MorphRow atom narrow expectedCustom
    -> MorphRow atom narrow expectedCustom
expectDefault expected morphRow =
    { narrow =
        \beforeInput ->
            beforeInput
                |> narrowStep morphRow
                |> Result.mapError
                    (\error -> { error | expected = expected })
                |> Result.andThen
                    (\after ->
                        after.broad
                            |> narrowStep (after.narrow |> succeed)
                    )
                |> onRemainingErr
    , broaden = broaden morphRow
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
                |> List.map (Morph.specific >> atom)
                |> sequence
            )


{-| [`MorphRow`](#MorphRow) from and to a single broad input = atom.

Short for [`MorphRow.over`](#over) [`atomAny`](#atomAny)

-}
atom :
    Morph narrow atom (Morph.Error atom expectCustom)
    -> MorphRow atom narrow expectCustom
atom =
    over atomAny


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

    import MorphRow exposing (succeed, atom)
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
            |> skip (atom ',')
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

TODO: avoidable?

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
    import Morph.CharRow as Char
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
        |> Text.narrowWith (onFailDown [ atom '_', Char.letter ])
        |> Result.mapError MorphRow.Error.textMessage
    --> Err "1:1: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '1'."


### example: fallback step if the previous step fails

    import Morph
    import Morph.CharRow as Char
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

This can be used to narrow the previous value further,
or to use the last value for the next [morph](#MorphRow) like a backreference.

    integerWithLeading0s : MorphRow Char Int
    integerWithLeading0s =
        next
            identity
            (\int ->
                case int of
                    0 ->
                        Text.integer

                    intSigned ->
                        intSigned |> succeed
            )
            Text.integer

Try to keep `next` filters to a minimum. Instead of

    printable : MorphRow Char Char
    printable =
        next identity
            (\char ->
                if List.member char [ '!', '#', '$', '%', '&', '*', '_', '-' ] then
                    char |> succeed

                else
                    fail
            )
            atomAny

do

    printable : MorphRow Char LocalSymbolPrintable
    printable =
        choice
            (\exclamationMark numberSign dollarSign percentSign ampersand asterisk lowLine hyphenMinus printable ->
                case printable of
                    ExclamationMark ->
                        exclamationMark ()

                    NumberSign ->
                        numberSign ()

                    DollarSign ->
                        dollarSign ()

                    PercentSign ->
                        percentSign ()

                    Ampersand ->
                        ampersand ()

                    Asterisk ->
                        asterisk ()

                    LowLine ->
                        lowLine ()

                    HyphenMinus ->
                        hyphenMinus ()
            )
            (possibility (\() -> ExclamationMark) (atom '!')
                >> possibility (\() -> NumberSign) (atom '#')
                >> possibility (\() -> DollarSign) (atom '$')
                >> possibility (\() -> PercentSign) (atom '%')
                >> possibility (\() -> Ampersand) (atom '&')
                >> possibility (\() -> Asterisk) (atom '*')
                >> possibility (\() -> LowLine) (atom '_')
                >> possibility (\() -> HyphenMinus) (atom '-')
            )

This gives you

  - a better error description out of the box
  - a more descriptive and correct type
  - building invalid values is impossible!

If you're using `next` recursively, see if you can [`loop`](#loop) instead.

-}
next :
    (narrowNarrow -> narrow)
    -> (narrow -> MorphRow atom narrowNarrow expectedCustom)
    ->
        (MorphRow atom narrow expectedCustom
         -> MorphRow atom narrowNarrow expectedCustom
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
    import Morph.CharRow as Char
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
    -> Morph.Expectation atom description
    -> Morph.ExpectationWith { startingAtDown : Int } atom description
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

    import MorphRow exposing (map, atom)
    import Morph.CharRow as Char
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

    import Morph.CharRow exposing (letter)
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
    import Morph.CharRow as Char
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


{-| Match a value at least a number of times and returns them as a `List`.

> ℹ️ Equivalent regular expression: `{min,}`

    import MorphRow.Error
    import Morph.CharRow as Char
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

    import Morph.CharRow as Char
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
    import Morph.CharRow as Char
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
atLeast minimum stepMorphRow =
    succeed (\minimumList overMinimum -> minimumList ++ overMinimum)
        |> grab (List.take minimum)
            (exactly minimum stepMorphRow)
        |> grab (List.drop minimum)
            (untilFailure stepMorphRow)


untilFailure :
    MorphRow atom narrow expectedCustom
    -> MorphRow atom (List narrow) expectedCustom
untilFailure stepMorphRow =
    loop
        { initial = []
        , step =
            \soFar ->
                choice
                    (\goOn commit stepNarrow ->
                        case stepNarrow of
                            GoOn goOnValue ->
                                goOn goOnValue

                            Commit commitValue ->
                                commit commitValue
                    )
                    |> possibility GoOn
                        (translate
                            Stack.top
                            (\stepped -> Stack.topDown stepped soFar)
                            |> over stepMorphRow
                        )
                    |> possibility Commit
                        ((soFar |> List.reverse) |> succeed)
        , goOnBroaden = Stack.toList
        , goOnBack =
            \stack ->
                case stack |> Stack.topRemove of
                    Hand.Empty _ ->
                        Nothing

                    Hand.Filled downStacked ->
                        downStacked |> filled |> Just
        , commitBack =
            \list ->
                case list |> Stack.fromList of
                    Hand.Empty _ ->
                        Nothing

                    Hand.Filled stacked ->
                        stacked |> filled |> Stack.reverse |> Just
        }


{-| Match a value between a range of times and returns them as a `List`.

> ℹ️ Equivalent regular expression: `{min,max}`

    import MorphRow.Error
    import Morph.CharRow as Char
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

    import Morph.CharRow as Char
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
    import Morph.CharRow as Char
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
                |> skip (atom 'd')
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
atMost maximum =
    \step ->
        loop
            { initial = []
            , step =
                \soFar ->
                    choice
                        (\goOn commit loopStep ->
                            case loopStep of
                                GoOn goOnValue ->
                                    goOn goOnValue

                                Commit commitValue ->
                                    commit commitValue
                        )
                        |> possibility GoOn
                            (if (soFar |> List.length) >= maximum then
                                fail

                             else
                                translate
                                    Stack.top
                                    (\top -> topDown top soFar)
                                    |> over step
                            )
                        |> possibility Commit
                            (soFar |> List.reverse |> succeed)
            , goOnBroaden = Stack.toList
            , goOnBack =
                \stack ->
                    case stack |> Stack.topRemove of
                        Hand.Empty _ ->
                            Nothing

                        Hand.Filled downStacked ->
                            downStacked |> filled |> Just
            , commitBack =
                \list ->
                    case list of
                        [] ->
                            Nothing

                        last :: before ->
                            Stack.topDown last before |> Stack.reverse |> Just
            }


{-| How to continue this [`loop`](#loop).
Either continue with a partial result or return with a complete value.
-}
type LoopStep partial complete
    = GoOn partial
    | Commit complete


{-| A powerful way to recurse over [`MorphRow`](#MorphRow)s.

Double-check that `goOnBack` can give `Nothing` in actual situations.
Otherwise, [`broaden`](Morph#broaden) runs infinitely.


### example: fold

    import MorphRow exposing (MorphRow, atom, succeed, onFailDown, fail, atLeast, take, drop, loop)
    import Morph.CharRow as Char
    import Morph.TextRow as Text
    import Morph.TextRow exposing (number)

    sumWhileLessThan : Float -> MorphRow Char Float
    sumWhileLessThan max =
        loop
            { initial = 0
            , step =
                \soFarTotal ->
                    choice
                        (\goOn commit loopStep ->
                            case loopStep of
                                MorphRow.GoOn goOnValue ->
                                    goOn goOnValue

                                MorphRow.Commit commitValue ->
                                    commit commitValue
                        )
                        |> possibility MorphRow.GoOn
                            (validate
                                (\n ->
                                    if soFarTotal + n >= max then
                                        () |> Err
                                    else
                                        (soFarTotal + n) |> Ok
                                )
                                |> MorphRow.over
                                    (succeed (\n -> n)
                                        |> grab (\n -> n) number
                                        |> skip (atLeast 0 Char.blank)
                                    )
                            )
                        |> possibility MorphRow.Commit
                            (soFarTotal |> succeed)
            , goOnBroaden = identity
            }

    -- stops before we reach a maximum of 6 in the sum
    "2 3 4"
        |> Text.narrowWith
            (succeed (\sum -> sum)
                |> grab (sumWhileLessThan 6)
                |> skip (atom '4')
            )
    --> Ok 5

TODO: fix example

-}
loop :
    { initial : beforeStep
    , step :
        beforeStep
        ->
            ChoiceMorphRowInProgress
                atom
                (LoopStep goOnValue commitValue)
                (LoopStep goOnValue commitValue
                 -> Hand (Stacked atom) Possibly Empty
                )
                expectedCustom
    , goOnBroaden : goOnValue -> beforeStep
    , goOnBack : goOnValue -> Maybe goOnValue
    , commitBack : commitValue -> Maybe goOnValue
    }
    -> MorphRow atom commitValue expectedCustom
loop loop_ =
    -- ↓ This  is what hell looks like FYI
    let
        loopStep =
            \before ->
                before |> loop_.step |> choiceFinish
    in
    { broaden =
        let
            goOnBack : Maybe goOnValue -> beforeStep
            goOnBack =
                \steppedBack ->
                    case steppedBack of
                        Nothing ->
                            loop_.initial

                        Just goOnBefore ->
                            goOnBefore |> loop_.goOnBroaden

            broadenStepBack : () -> (Maybe goOnValue -> Hand (Stacked atom) Possibly Empty)
            broadenStepBack () =
                \narrowGoOnValue ->
                    case narrowGoOnValue of
                        Nothing ->
                            Hand.empty

                        Just goOnValue ->
                            let
                                steppedBack =
                                    goOnValue |> loop_.goOnBack
                            in
                            (goOnValue |> GoOn)
                                |> broaden (steppedBack |> goOnBack |> loopStep)
                                |> Stack.onTopStack
                                    (steppedBack |> broadenStepBack ())
        in
        \commitValueNarrow ->
            let
                committedBack =
                    commitValueNarrow
                        |> loop_.commitBack
            in
            (committedBack |> broadenStepBack ())
                |> Stack.onTopStack
                    ((commitValueNarrow |> Commit)
                        |> broaden (committedBack |> goOnBack |> loopStep)
                    )
    , narrow =
        let
            loopNarrowStep :
                ()
                ->
                    (beforeStep
                     ->
                        (Hand (Stacked atom) Possibly Empty
                         ->
                            Result
                                (SuccessExpectation atom expectedCustom)
                                { narrow : commitValue
                                , broad : Hand (Stacked atom) Possibly Empty
                                }
                        )
                    )
            loopNarrowStep () =
                \before ->
                    narrowStep (before |> loopStep)
                        >> Result.andThen
                            (\stepped ->
                                case stepped.narrow of
                                    Commit committed ->
                                        { broad = stepped.broad
                                        , narrow = committed
                                        }
                                            |> Ok

                                    GoOn goOnValue ->
                                        stepped.broad
                                            |> (goOnValue
                                                    |> loop_.goOnBroaden
                                                    |> loopNarrowStep ()
                                               )
                            )
        in
        \broad ->
            broad
                |> (loop_.initial |> loopNarrowStep ())
                |> onRemainingErr
    }


{-| [convert](#MorphRow) parts and interspersed separators into a `Stack`.

    import MorphRow exposing (map, split)
    import Morph.TextRow as Text exposing (text)
    import Morph.CharRow as Char

    -- note that both values and separators must be of the same type
    "a,bc,def"
        |> Text.narrowWith
            (split ( atLeast 0, text "," ) (atLeast 0 Char.aToZLower))
    --> Ok
    -->     (topDown
    -->         [ 'a' ]
    -->         [ { separator = (), part = [ 'b', 'c' ] }
    -->         , { separator = (), part = [ 'd', 'e', 'f' ] }
    -->         ]
    -->     )

    -- leading/trailing separators are valid and give empty parts
    ",a,,"
        |> Text.narrowWith
            (split ( atLeast 0, text "," ) (atLeast 0 Char.aToZLower))
    --> Ok
    -->     (topDown
    -->         []
    -->         [ { separator = (), part = [ 'a' ] }
    -->         , { separator = (), part = [] }
    -->         , { separator = (), part = [] }
    -->         ]
    -->     )

    -- an empty input text gives a single element from an empty string
    ""
        |> Text.narrowWith
            (split ( atLeast 0, text "," ) (atLeast 0 Char.aToZLower))
    --> Ok (topDown [] [])

Attention on separated part [`MorphRow`](#MorphRow)s:

    split ( atLeast 0, text "," ) (atLeast 0 atomAny)

would only parse 1 part until the end
because `atLeast 0 atomAny` always [`succeed`](#succeed)s.
No separator would ever be parsed.

-}
split :
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
split ( splitSequence, separatorMorphRow ) partMorphRow =
    succeed topDown
        |> grab Stack.top partMorphRow
        |> grab (Stack.topRemove >> Stack.toList)
            (splitSequence
                (succeed
                    (\separator part ->
                        { separator = separator, part = part }
                    )
                    |> grab .separator separatorMorphRow
                    |> grab .part partMorphRow
                )
            )
