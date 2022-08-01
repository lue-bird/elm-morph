module MorphRow exposing
    ( MorphRow, Error
    , succeed, expect, fail
    , end, oneAny, one
    , specific
    , skip, grab, GroupMorphRowInProgress
    , over, next
    , possibility, ChoiceMorphRowInProgress, choiceFinish
    , atLeast, between, exactly, maybe
    , separatedBy
    , before, until
    , whileAccumulate
    , finish
    )

{-| Simple, easy to use, general-purpose parser-builder with good error messages

Inspired by

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

    crucial so we don't experience reports like

    > "If it compiles it runs"
    >
    > Unless you are writing a parser.
    >
    > The parser doesn't care.
    >
    > The parser will compile and then murder you for breakfast.

    – xarvh (Francesco Orsenigo)

  - 👍 error messages will always show all options and why they failed,
    showing those that came the furthest first

  - 👎 performs worse as there's more [possibilities](#possibility) to parse to know it failed

@docs MorphRow, Error


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


## sequence

@docs atLeast, between, exactly, maybe
@docs separatedBy


### loop

[`while`](#while), [`until`](#until) are powerful ways to recurse over [`MorphRow`](#MorphRow)s:

situation: One [`possibility`](#possibility) matches, [`next`](#next) the new argument is taken to call the whole [`MorphRow`](#MorphRow) recursively.

This grows the stack, so you cannot do it indefinitely.
[`while`](#while), [`until`](#until) enable tail-call elimination so you can have as many repeats you want.

@docs before, until
@docs whileAccumulate


### transform

@docs finish

-}

import ArraySized exposing (ArraySized)
import ArraySized.Morph
import Emptiable exposing (Emptiable, filled)
import Linear exposing (DirectionLinear(..))
import Morph exposing (Morph, MorphInProgress, broaden, broadenFrom, choice, narrow, translate, validate)
import N exposing (Down, Exactly, Fixed, In, Min, N, N0, To, Up, n0, n1)
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
    type alias MorphText narrow =
        MorphRow Char narrow


#### Q: Why no custom error support?

Non-default structured errors are mostly useful for recovery based on what went wrong.
In these cases,

    : MorphRow atom (Result error narrow)
    succeed (Err ...)
    succeed (Ok ...)

The benefits of introducing a new custom error type variable
are diminished by the decreased simplicity.
Think otherwise? → issue

-}
type alias MorphRow atom narrow =
    MorphInProgress
        { narrow :
            Emptiable (Stacked atom) Possibly
            ->
                Result
                    (Error atom)
                    { narrow : narrow
                    , broad : Emptiable (Stacked atom) Possibly
                    }
        , broaden :
            narrow -> Emptiable (Stacked atom) Possibly
        }


{-| Incomplete [`MorphRow`](#MorphRow) for a thing composed of multiple parts = group.
It's what you supply during a [`succeed`](#succeed)`|>`[`grab`](#grab)/[`skip`](#skip) build.
-}
type alias GroupMorphRowInProgress atom groupNarrow groupNarrowAssemble =
    MorphInProgress
        { narrow :
            Emptiable (Stacked atom) Possibly
            ->
                Result
                    (Error atom)
                    { narrow : groupNarrowAssemble
                    , broad : Emptiable (Stacked atom) Possibly
                    }
        , broaden :
            groupNarrow
            -> Emptiable (Stacked atom) Possibly
        }


{-| Complete description of a situation that's considered a failure.
-}
type alias Error atom =
    Morph.ErrorWith { startingAtDown : Int } atom



--


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
    String
    -> MorphRow atom narrow
    -> MorphRow atom narrow
expect contextDescription morphRow =
    { narrow =
        \beforeInput ->
            beforeInput
                |> narrow morphRow
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
                            |> narrow (after.narrow |> succeed)
                    )
    , broaden = broaden morphRow
    }


{-| Take what we get from [converting](#MorphRow) the next section
and channel it back up to the [`succeed`](#succeed) grouping.
-}
grab :
    (groupNarrow -> partNextNarrow)
    -> MorphRow atom partNextNarrow
    ->
        (GroupMorphRowInProgress
            atom
            groupNarrow
            (partNextNarrow -> groupNarrowFurther)
         ->
            GroupMorphRowInProgress
                atom
                groupNarrow
                groupNarrowFurther
        )
grab partAccess grabbedNextMorphRow =
    \groupMorphRowSoFar ->
        { narrow =
            \broad ->
                broad
                    |> narrow groupMorphRowSoFar
                    |> Result.andThen
                        (\result ->
                            result.broad
                                |> narrow grabbedNextMorphRow
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
    MorphRow atom ()
    ->
        (GroupMorphRowInProgress atom groupNarrow narrow
         -> GroupMorphRowInProgress atom groupNarrow narrow
        )
skip ignoredNext =
    \groupMorphRowSoFar ->
        { narrow =
            \broad ->
                broad
                    |> narrow groupMorphRowSoFar
                    |> Result.andThen
                        (\result ->
                            result.broad
                                |> narrow ignoredNext
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
oneAny : MorphRow atom atom
oneAny =
    { narrow =
        \broad ->
            case broad of
                Emptiable.Empty _ ->
                    { expected = Morph.MoreInput
                    , location = { startingAtDown = 0 }
                    , description = Emptiable.empty
                    }
                        |> Err

                Emptiable.Filled (Stack.TopDown nextAtom tail) ->
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
specific : List atom -> MorphRow atom ()
specific expectedConstantInput =
    broadenFrom
        (List.repeat (expectedConstantInput |> List.length) ())
        |> over
            (expectedConstantInput
                |> List.map (Morph.specific >> one)
                |> sequence
            )


{-| **Should not be exposed!**

A `List` of `MorphRow`s in order

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
    List (MorphRow atom narrow)
    -> MorphRow atom (List narrow)
sequence morphRowsInSequence =
    let
        step stepMorphRow =
            \soFar ->
                soFar.broad
                    |> narrow stepMorphRow
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
    , broaden =
        \narrowSequence ->
            List.map2
                (\morphInSequence -> broaden morphInSequence)
                morphRowsInSequence
                narrowSequence
                |> List.concatMap Stack.toList
                |> Stack.fromList
    }


{-| [`MorphRow`](#MorphRow) from and to a single broad input.

Short for [`MorphRow.over`](#over) [`oneAny`](#oneAny)

-}
one :
    Morph narrow element (Morph.Error element)
    -> MorphRow element narrow
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
    -> GroupMorphRowInProgress atom_ narrowNarrowBroadenable_ narrow
succeed narrowConstant =
    { narrow =
        \broad ->
            { narrow = narrowConstant
            , broad = broad
            }
                |> Ok
    , broaden =
        \_ -> Emptiable.empty
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
fail : MorphRow atom_ narrow
fail =
    { narrow =
        \broad ->
            { expected = Morph.NoFail
            , location = { startingAtDown = broad |> Stack.length }
            , description = Emptiable.empty
            }
                |> Err
    , broaden = \_ -> Emptiable.empty
    }


{-| Possibly incomplete [`MorphRow`](#MorphRow) to and from a choice.
See [`Morph.choice`](Morph#choice), [`MorphRow.possibility`](#possibility), [`MorphRow.choiceFinish`](#choiceFinish)
-}
type alias ChoiceMorphRowInProgress atom choiceNarrow choiceBroaden =
    Morph.MorphInProgress
        { narrow :
            Emptiable (Stacked atom) Possibly
            ->
                Result
                    { possibilities :
                        Emptiable (Stacked (Error atom)) Possibly
                    }
                    { narrow : choiceNarrow
                    , broad : Emptiable (Stacked atom) Possibly
                    }
        , broaden : choiceBroaden
        }


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
    ->
        (ChoiceMorphRowInProgress
            atom
            choiceNarrow
            ((possibilityNarrow -> Emptiable (Stacked atom) Possibly)
             -> choiceBroadenFurther
            )
         ->
            ChoiceMorphRowInProgress
                atom
                choiceNarrow
                choiceBroadenFurther
        )
possibility possibilityToChoice possibilityMorph =
    \choiceMorphSoFar ->
        { narrow =
            \choiceBroad ->
                choiceBroad
                    |> choiceMorphSoFar.narrow
                    |> restoreTry
                        (\soFarExpectationMiss ->
                            case choiceBroad |> narrow possibilityMorph of
                                Ok possibilityParsed ->
                                    { broad = possibilityParsed.broad
                                    , narrow =
                                        possibilityParsed.narrow
                                            |> possibilityToChoice
                                    }
                                        |> Ok

                                Err possibilityExpectation ->
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
        (choiceNarrow -> Emptiable (Stacked atom) Possibly)
    -> MorphRow atom choiceNarrow
choiceFinish =
    \choiceMorphRowComplete ->
        { narrow =
            \broad ->
                broad
                    |> choiceMorphRowComplete.narrow
                    |> Result.mapError
                        (\choiceExpectation ->
                            { expected =
                                Morph.OneOf choiceExpectation.possibilities
                            , location = { startingAtDown = broad |> Stack.length }
                            , description = Emptiable.empty
                            }
                        )
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

-}
next :
    (narrow -> broad)
    -> (broad -> MorphRow atom narrow)
    ->
        (MorphRow atom broad
         -> MorphRow atom narrow
        )
next narrowNarrowToNarrow morphNarrowNarrow =
    \morphRowBefore ->
        { narrow =
            narrow morphRowBefore
                >> Result.andThen
                    (\result ->
                        result.broad
                            |> narrow
                                (morphNarrowNarrow result.narrow)
                    )
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
    ->
        (Morph.Error atom
         -> Error atom
        )
errorFromMorph location =
    \morphError ->
        { expected =
            morphError.expected |> expectationFromMorph location
        , description = morphError.description
        , location = location
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
    MorphRow atom narrow
    ->
        (Morph narrowNarrow narrow (Morph.Error atom)
         -> MorphRow atom narrowNarrow
        )
over morphRowBeforeMorph =
    \narrowMorph ->
        { narrow =
            \broad ->
                broad
                    |> narrow morphRowBeforeMorph
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
        , broaden =
            broaden narrowMorph
                >> broaden morphRowBeforeMorph
        }


expectationFromMorph :
    { startingAtDown : Int }
    -> Morph.Expectation specific
    -> Morph.ExpectationWith { startingAtDown : Int } specific
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

            Morph.OneOf possibleErrors ->
                Morph.OneOf
                    (possibleErrors
                        |> Stack.map (\_ -> errorFromMorph location)
                    )



-- sequence


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
    MorphRow atom contentNarrow
    -> MorphRow atom (Emptiable contentNarrow Possibly)
maybe contentMorphRow =
    choice
        (\justVariant nothingVariant maybeNarrow ->
            case maybeNarrow of
                Emptiable.Filled justValue ->
                    justVariant justValue

                Emptiable.Empty _ ->
                    nothingVariant ()
        )
        |> possibility filled contentMorphRow
        |> possibility (\() -> Emptiable.empty) (succeed ())
        |> choiceFinish


{-| Match a value `exactly` a number of times
and return them as a [`ArraySized`](https://package.elm-lang.org/packages/lue-bird/elm-typesafe-array/latest/ArraySized)

> ℹ️ Equivalent regular expression: `{n}`

    import MorphRow.Error
    import Morph.Char as Char
    import Morph.TextRow as Text
    import N exposing (n3)

    -- we want `exactly 3` letters
    "abcdef" |> narrow (map Text.fromList (exactly n3 Char.letter))
    --> Ok [ 'a', 'b', 'c' ]

    -- not 2 or 4, we want 3
    "ab_def"
        |> narrow (map Text.fromList (exactly n3 Char.letter))
        |> Result.mapError MorphRow.Error.textMessage
    --> Err "1:3: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '_'."

-}
exactly :
    N (Exactly howMany)
    -> MorphRow atom element
    ->
        MorphRow
            atom
            (ArraySized (Exactly howMany) element)
exactly howOften repeatedMorphRow =
    { narrow =
        let
            narrowRepeatStep :
                { soFar : ArraySized (Min (Fixed N0)) element }
                -> Emptiable (Stacked atom) Possibly
                ->
                    Result
                        (Error atom)
                        { narrow : ArraySized (Exactly howMany) element
                        , broad : Emptiable (Stacked atom) Possibly
                        }
            narrowRepeatStep { soFar } =
                \broad ->
                    case soFar |> ArraySized.hasAtLeast (howOften |> N.maxUp n1) of
                        Ok arraySizedAtLeastHowOften ->
                            { narrow =
                                arraySizedAtLeastHowOften
                                    |> ArraySized.take ( Up, howOften, { atLeast = howOften } )
                            , broad = broad
                            }
                                |> Ok

                        Err _ ->
                            case broad |> narrow repeatedMorphRow of
                                Err error ->
                                    error |> Err

                                Ok parsed ->
                                    -- does this blow the stack?
                                    narrowRepeatStep
                                        { soFar =
                                            ArraySized.minDown n1
                                                (ArraySized.minPush parsed.narrow soFar)
                                        }
                                        parsed.broad
        in
        narrowRepeatStep { soFar = ArraySized.empty |> ArraySized.maxNo }
    , broaden =
        \repeated ->
            repeated
                |> ArraySized.toList
                |> Stack.fromList
                |> Stack.map (\_ -> broaden repeatedMorphRow)
                |> Stack.flatten
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


## `atLeast 0`

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

    import N exposing (n1)
    import MorphRow.Error
    import Morph.Char as Char
    import Morph.Text as Text

    -- we want as many letters as there are
    "abc" |> Text.narrowWith (atLeast n1 Char.letter)
    --> Ok [ 'a', 'b', 'c' ]

    "abc123" |> Text.narrowWith (atLeast n1 Char.letter)
    --> Ok [ 'a', 'b', 'c' ]

    -- but we want at least one
    "123abc"
        |> Text.narrowWith (atLeast n1 Char.letter)
        |> Result.mapError MorphRow.Error.textMessage
    --> Err "1:1: I was expecting a letter a|..|z or A|...|Z. I got stuck when I got the character '1'."

-}
atLeast :
    N (Exactly lowerLimitMin)
    -> MorphRow atom narrow
    ->
        MorphRow
            atom
            (ArraySized (Min (Fixed lowerLimitMin)) narrow)
atLeast =
    \minimum elementStepMorphRow ->
        succeed
            (\minimumArraySized overMinimum ->
                minimumArraySized
                    |> ArraySized.minGlue Up
                        (overMinimum |> ArraySized.min n0)
            )
            |> grab (ArraySized.take ( Up, minimum, { atLeast = minimum } ))
                (exactly minimum elementStepMorphRow)
            |> grab (ArraySized.minDrop ( Up, minimum ))
                (ArraySized.Morph.toList
                    |> over (untilFail elementStepMorphRow)
                )


untilFail :
    MorphRow atom element
    -> MorphRow atom (List element)
untilFail elementStepMorphRow =
    whileAccumulate
        { initial = ()
        , step = \_ () -> () |> Ok
        , element = elementStepMorphRow
        }


{-| [Morph](#MorphRow) multiple elements from now to when `end` matches.

    decoderNameSubject : MorphRow String Char expectationCustom
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

    decoderNameSubject : MorphRow String Char expectationCustom
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
    { end : MorphRow atom ()
    , goOn : MorphRow atom goOnElement
    }
    -> MorphRow atom (List goOnElement)
before untilStep =
    until
        { commit =
            translate .before
                (\before_ -> { before = before_, end = () })
        , end = untilStep.end
        , goOn = untilStep.goOn
        }


{-| How are [`between`](#between), ... defined?
The [section loop](#loop) explains why using [`next`](#next) recursively is a bad idea.

    decoderNameSubject : MorphRow String Char expectationCustom
    decoderNameSubject =
        Text.fromList
            |> MorphRow.over
                (MorphRow.until
                    { commit =
                        translate .before
                            (\before -> { before = before, end = () })
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
            (Morph.Error atom)
    , end : MorphRow atom endElement
    , goOn : MorphRow atom goOnElement
    }
    -> MorphRow atom commitResult
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
                     -> Emptiable (Stacked atom) Possibly
                    )
            broadenStepBack () =
                \toStep ->
                    case toStep of
                        [] ->
                            Emptiable.empty

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
                        (Emptiable (Stacked atom) Possibly
                         ->
                            Result
                                (Error atom)
                                { narrow : commitResult
                                , broad : Emptiable (Stacked atom) Possibly
                                }
                        )
                    )
            loopNarrowStep () =
                \before_ ->
                    narrow loopStep
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
        [] |> loopNarrowStep ()
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
    "abcdef" |> Text.narrowWith (between ( n0, n3 ) Char.letter)
    --> Ok [ 'a', 'b', 'c' ]

    -- less than that is also okay
    "ab_def" |> Text.narrowWith (between ( n0, n3 ) Char.letter)
    --> Ok [ 'a', 'b' ]

    -- even zero letters are fine
    "_underscore" |> Text.narrowWith (between ( n0, n3 ) Char.letter)
    --> Ok []

    -- make sure we don't consume more than three letters
    "abcdef"
        |> Text.narrowWith
            (succeed (\letters -> letters)
                |> grab (between ( n0, n3 ) Char.letter)
                |> skip (one 'd')
            )
    --> Ok [ 'a', 'b', 'c' ]

-}
between :
    ( N (Exactly lowerLimitMin)
    , N (In (Fixed lowerLimitMin) (Fixed upperLimitMax))
    )
    -> MorphRow atom element
    ->
        MorphRow
            atom
            (ArraySized
                (In (Fixed lowerLimitMin) (Fixed upperLimitMax))
                element
            )
between =
    \( lowerLimit, upperLimit ) repeatedElementMorphRow ->
        succeed
            (\minimumList overMinimum ->
                minimumList
                    |> ArraySized.minGlue Up (overMinimum |> ArraySized.min n0)
                    |> ArraySized.take
                        ( Up
                        , upperLimit |> N.min lowerLimit
                        , { atLeast = lowerLimit }
                        )
            )
            |> grab
                (ArraySized.take ( Up, lowerLimit, { atLeast = lowerLimit } ))
                (exactly lowerLimit repeatedElementMorphRow)
            |> grab
                (ArraySized.minDrop ( Up, lowerLimit )
                    >> ArraySized.maxNo
                )
                (atMostLoop
                    ((upperLimit |> N.toInt) - (lowerLimit |> N.toInt))
                    repeatedElementMorphRow
                )


{-| Match a value at less or equal a number of times.

**Shouldn't be exposed**

-}
atMostLoop :
    Int
    -> MorphRow atom narrow
    -> MorphRow atom (ArraySized (Min (Fixed N0)) narrow)
atMostLoop upperLimit elementStepMorphRow =
    ArraySized.Morph.toList
        |> over
            (whileAccumulate
                { initial = { length = 0 }
                , step =
                    \_ soFar ->
                        if soFar.length >= upperLimit then
                            Err ()

                        else
                            { length = soFar.length + 1 } |> Ok
                , element = elementStepMorphRow
                }
            )


{-| How are [`atLeast`](#atLeast), ... defined?
The [section loop](#loop) explains why using [`next`](#next) recursively is a bad idea.

    import Morph exposing (choice, validate)
    import MorphRow exposing (MorphRow, one, succeed, atLeast, take, drop, whileAccumulate)
    import Morph.Char
    import Morph.Text
    import Number.Morph

    sumWhileLessThan : Float -> MorphRow Char (List Number)
    sumWhileLessThan max =
        whileAccumulate
            { initial = 0
            , step =
                \element stepped ->
                    let
                        floats =
                            stepped + (element |> Morph.map Number.Morph.toFloat)
                    in
                    if floats >= max then
                        Err ()
                    else
                        floats |> Ok
            , element =
                succeed (\n -> n)
                    |> grab (\n -> n) Number.Morph.text
                    |> skip (atLeast n0 (Morph.Char.blank |> one))
            }

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
whileAccumulate :
    { initial : accumulationValue
    , step :
        goOnElement
        -> accumulationValue
        -> Result () accumulationValue
    , element : MorphRow atom goOnElement
    }
    -> MorphRow atom (List goOnElement)
whileAccumulate { initial, step, element } =
    { broaden =
        -- TODO: check if order is correct
        List.map (broaden element)
            >> Stack.fromList
            >> Stack.flatten
    , narrow =
        let
            loopNarrowStep :
                { accumulationValue : accumulationValue }
                ->
                    (Emptiable (Stacked atom) Possibly
                     ->
                        Result
                            (Error atom)
                            { narrow : List goOnElement
                            , broad : Emptiable (Stacked atom) Possibly
                            }
                    )
            loopNarrowStep { accumulationValue } =
                \broad ->
                    broad
                        |> narrow element
                        |> Result.andThen
                            (\stepped ->
                                case accumulationValue |> step stepped.narrow of
                                    Err () ->
                                        { broad = broad
                                        , narrow = []
                                        }
                                            |> Ok

                                    Ok accumulationValueAltered ->
                                        stepped.broad
                                            |> loopNarrowStep
                                                { accumulationValue = accumulationValueAltered }
                                            |> Result.map
                                                (\tail ->
                                                    { broad = tail.broad
                                                    , narrow =
                                                        tail.narrow
                                                            |> (::) stepped.narrow
                                                    }
                                                )
                            )
        in
        loopNarrowStep { accumulationValue = initial }
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
    ( MorphRow atom { separator : separator, part : part }
      -> MorphRow atom afterFirst
    , MorphRow atom separator
    )
    -> MorphRow atom part
    ->
        MorphRow
            atom
            { first : part
            , afterFirst : afterFirst
            }
separatedBy ( separatorsToSequence, separatorMorphRow ) partMorphRow =
    succeed (\first afterFirst -> { first = first, afterFirst = afterFirst })
        |> grab .first partMorphRow
        |> grab .afterFirst
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

    decoderNameSubject : MorphRow String Char expectationCustom
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
end : MorphRow atom_ ()
end =
    { narrow =
        \broad ->
            case broad of
                Emptiable.Empty _ ->
                    Emptiable.empty |> narrow (succeed ())

                Emptiable.Filled stacked ->
                    { expected = Morph.NoMoreInput
                    , location =
                        { startingAtDown = stacked |> filled |> Stack.length }
                    , description = Emptiable.empty
                    }
                        |> Err
    , broaden =
        \() -> Emptiable.empty
    }



-- transform


{-| Final step before running a [`MorphRow`](#MorphRow),
transforming it into a [`Morph`](Morph#Morph) on the full stack of input elements.

    point
        |> MorphRow.finish
        |> Morph.over Stack.Morph.toText

-}
finish :
    MorphRow atom narrow
    -> Morph narrow (Emptiable (Stacked atom) Possibly) (Error atom)
finish =
    \morphRow ->
        { narrow =
            narrow (morphRow |> skip end)
                >> Result.map .narrow
        , broaden =
            broaden morphRow
        }
