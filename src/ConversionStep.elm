module ConversionStep exposing
    ( ConversionStep
    , Expected(..), Context, ExpectationMiss, Error(..)
    , narrowWith, build
    , succeed, expect, fail
    , atomAny, atom
    , lazy
    , take
    , drop, buildFrom
    , map, next
    , ChoiceConversionStep, choice, possibility
    , sequence
    , atLeast, between, exactly, maybe
    , until, split
    , loop, LoopStep(..)
    )

{-| A simple, easy to use, general-purpose parser with good error messages.

Heavily inspired by

  - [`elm/parser`](https://package.elm-lang.org/packages/elm/parser/latest/)
  - [`dasch/parser`](https://package.elm-lang.org/packages/dasch/parser/latest/)
  - and especially [`lambda-phi/parser`](https://dark.elm.dmy.fr/packages/lambda-phi/parser/latest/)


## example: 2D point

Define the data type you want to parse into, then build a parser for that data type.

    import ConversionStep exposing (ConversionStep, atLeast, drop, into, succeed, take, atom)
    import Char.ConversionStep as Char
    import Text.ConversionStep as Text exposing (number)
    import ConversionStep.Error

    type alias Point =
        -- makes `Point` function unavailable:
        -- https://dark.elm.dmy.fr/packages/lue-bird/elm-no-record-type-alias-constructor-function/latest/
        RecordWithoutConstructorFunction
            { x : Float
            , y : Float
            }

    -- a successful parse looks like
    "(2.71, 3.14)" |> Text.narrowWith point --> Ok { x = 2.71, y = 3.14 }

    -- build always works
    { x = 2.71, y = 3.14 } |> Text.build point --> "( 2.71, 3.14 )"

    point : ConversionStep Point
    point =
        into "Point"
            (succeed (\x y -> { x = x, y = y })
                |> drop (atom '(')
                |> drop (atLeast 0 Char.blank |> buildFrom [ () ])
                |> take number
                |> drop (atLeast 0 Char.blank)
                |> drop (atom ',')
                |> drop (atLeast 0 Char.blank |> buildFrom [ () ])
                |> take number
                |> drop (atLeast 0 Char.blank |> buildFrom [ () ])
                |> drop (atom ')')
            )

    -- we can get a nice error message if it fails
    "(2.71, x)"
        |> Text.narrowWith point
        |> Result.mapError (ConversionStep.Error.dump "filename.txt")
    --> Err
    -->     [ "[ERROR] filename.txt: line 1:8: I was expecting a digit [0-9]. I got stuck when I got 'x'."
    -->     , "  in Point at line 1:1"
    -->     , ""
    -->     , "1|(2.71, x)"
    -->     , "  ~~~~~~~^"
    -->     ]


## usage

  - [`ConversionStep`](ConversionStep): creating and chaining parsers

  - [`Char.ConversionStep`](Char-ConversionStep): parsing characters

  - [`Text.ConversionStep`](Text-ConversionStep): parsing text

  - [`ConversionStep.Error`](ConversionStep-Error):
    error reporting and formatting

Note before we start:
`ConversionStep` always backtracks and never commits to a specific path!

  - 👍 improves parser code readability
  - 👍 provides more information on what could be possible to the user
  - 👎 performs worse as there's more to parse to know it failed

@docs ConversionStep
@docs Expected, Context, ExpectationMiss, Error


## basic

@docs narrowWith, build


## create

@docs succeed, expect, fail
@docs atomAny, atom


## recursion

@docs lazy


## chain

@docs take
@docs drop, buildFrom
@docs map, next
@docs ChoiceConversionStep, choice, possibility


## sequences

@docs sequence
@docs atLeast, between, exactly, maybe
@docs until, split


### looping

@docs loop, LoopStep

-}

import Conversion exposing (Transfer, transfer)
import Hand exposing (Empty, Hand, filled)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Stack exposing (StackTopBelow(..), Stacked, topDown)



-- types


{-| A function that takes an inputs `List` of "atoms"
and returns either an [`Error`](#Error),
or a value with the next state.

    {-| [`ConversionStep`](#ConversionStep) on input characters
    -}
    type alias TextConversionStep narrow =
        ConversionStep Char narrow

-}
type alias ConversionStep atom narrow =
    SequenceConversionStep atom narrow narrow


{-| Possibly incomplete [`ConversionStep`](#ConversionStep) for a thing composed of multiple parts.
It's what you get after you [`take`](#take) or [`drop`](#drop) steps.
-}
type alias SequenceConversionStep atom sequenceBuildable sequence =
    RecordWithoutConstructorFunction
        { narrow :
            List atom
            ->
                Result
                    (ExpectationMiss atom)
                    { narrow : sequence
                    , input : List atom
                    }
        , broaden : sequenceBuildable -> List atom
        }


{-| Details: What do we [`expect`](#expect) to parse here?
-}
type alias Context atom =
    RecordWithoutConstructorFunction
        { expected : Expected atom
        , startingAtDown : Int
        }


{-| The description of an error as its [`Context`](#Context) stack.
-}
type alias ExpectationMiss atom =
    List (Context atom)


{-| What went wrong.
-}
type Expected atom
    = Fail
    | ExpectedCustom String
    | ExpectedSpecifically atom
    | Expected1In (List (ExpectationMiss atom))


type Error atom narrow
    = InputRemaining
        { narrow : narrow
        , input : Hand (Stacked atom) Never Empty
        }
    | ExpectationMiss (ExpectationMiss atom)



--


narrowStepFrom :
    List atom
    -> SequenceConversionStep atom buildable_ narrow
    ->
        Result
            (ExpectationMiss atom)
            { narrow : narrow
            , input : List atom
            }
narrowStepFrom state =
    \parser -> parser.narrow state


{-| Parse an input text, and get either an [`Error`](#Error)
or the parsed value as a `Result`.

    import Char.ConversionStep as Char
    import Text.ConversionStep exposing (number)
    import ConversionStep.Error

    -- consumes a single letter, then "bc" are still remaining
    "abc" |> Text.narrowWith Char.letter --> Ok 'a'

    -- we can also parse text into other data types like numbers
    "3.14" |> Text.narrowWith number --> Ok 3.14

    -- we get an error message if the parser doesn't match
    "123"
        |> Text.narrowWith Char.letter
        |> Result.mapError ConversionStep.Error.textMessage
    --> Err "1:1: I was expecting a letter [a-zA-Z]. I got stuck when I got '1'."

-}
narrowWith :
    ConversionStep atom narrow
    ->
        (List atom
         -> Result (Error atom narrow) narrow
        )
narrowWith parser =
    \input ->
        case parser |> narrowStepFrom input of
            Err expectationMiss ->
                ExpectationMiss expectationMiss |> Err

            Ok parsed ->
                case parsed.input |> Stack.fromList of
                    Hand.Empty _ ->
                        parsed.narrow |> Ok

                    Hand.Filled inputStacked ->
                        InputRemaining
                            { narrow = parsed.narrow
                            , input = inputStacked |> filled
                            }
                            |> Err


build : SequenceConversionStep atom complete narrow_ -> (complete -> List atom)
build builder =
    \narrow -> narrow |> builder.broaden


{-| Describe the context to improve error messages.

    import ConversionStep.Error
    import Char.ConversionStep as Char
    import Text.ConversionStep as Text

    -- we can redefine an error message if something goes wrong
    "123"
        |> Text.narrowWith
            (expect "a name consisting of letters"
                (atLeast 1 Char.letter)
            )
        |> Result.mapError ConversionStep.Error.textMessage
    --> Err "1:1: I was expecting a name consisting of letters. I got stuck when I got '1'."


    import ConversionStep exposing (take, drop, succeed, expect, atom)
    import Text.ConversionStep as Text

    type alias Point =
        { x : Float
        , y : Float
        }

    -- we can use `expect` to have more context when an error happens
    point : ConversionStep Point
    point =
        expect "point"
            (succeed (\x y -> { x = x, y = y })
                |> drop (atom '(')
                |> take Text.number
                |> drop (atom ',')
                |> take Text.number
                |> drop (atom ')')
            )

    "(12,34)" |> Text.narrowWith point
    --> Ok { x = 12, y = 34 }

    -- we can get the error context stack as well as where they started matching
    "(a,b)" |> Text.narrowWith point
        |> Result.mapError .expected
    --> Err [ ExpectedCustom "point" ]

-}
expect :
    String
    -> ConversionStep atom narrow
    -> ConversionStep atom narrow
expect contextDescription parser =
    expectInternal (ExpectedCustom contextDescription) parser


expectInternal :
    Expected atom
    -> ConversionStep atom narrow
    -> ConversionStep atom narrow
expectInternal expected parser =
    { narrow =
        \beforeInput ->
            parser
                |> narrowStepFrom beforeInput
                |> Result.mapError
                    (\context ->
                        context
                            |> (::)
                                { expected = expected
                                , startingAtDown = beforeInput |> List.length
                                }
                    )
                |> Result.andThen
                    (\after ->
                        succeed after.narrow
                            |> narrowStepFrom after.input
                    )
    , broaden = \_ -> []
    }



-- basic


{-| Takes a parsed value and feeds it to the return value of the parser.
-}
take :
    (complete -> partNext)
    -> SequenceConversionStep atom partNext partNextNarrow
    ->
        (SequenceConversionStep atom complete (partNextNarrow -> applied)
         -> SequenceConversionStep atom complete applied
        )
take partAccess grabbedNext =
    \parser ->
        { narrow =
            \input ->
                parser
                    |> narrowStepFrom input
                    |> Result.andThen
                        (\result ->
                            grabbedNext
                                |> narrowStepFrom result.input
                                |> Result.map
                                    (\nextParsed ->
                                        { narrow = result.narrow nextParsed.narrow
                                        , input = nextParsed.input
                                        }
                                    )
                        )
        , broaden =
            \complete ->
                (complete |> build parser)
                    ++ (complete |> partAccess |> build grabbedNext)
        }


{-| Require values to be matched next to continue but ignore the result.

    import Text.ConversionStep exposing (text)
    import ConversionStep exposing (atom, succeed, atLeast, take, drop)

    -- parse a simple email, but we're only interested in the username
    "user@example.com"
        |> Text.narrowWith
            (succeed (\userName -> { username = userName })
                |> take .username (atLeast 1 letter)
                |> drop (atom '@')
                |> drop
                    (atLeast 1 letter
                        |> map listToSting
                        |> buildFrom "example"
                    )
                |> drop (text ".com")
            )
    --> Ok { username = "user" }

[`buildFrom`](#buildFrom) is cool:
when multiple kinds of input can be dropped,
it allows choosing a default possibility for building.

-}
drop :
    SequenceConversionStep atom () nextNarrow_
    ->
        (SequenceConversionStep atom complete narrow
         -> SequenceConversionStep atom complete narrow
        )
drop ignoredNext =
    \parser ->
        { narrow =
            \input ->
                parser
                    |> narrowStepFrom input
                    |> Result.andThen
                        (\result ->
                            ignoredNext
                                |> narrowStepFrom result.input
                                |> Result.map
                                    (\nextParsed ->
                                        { narrow = result.narrow
                                        , input = nextParsed.input
                                        }
                                    )
                        )
        , broaden =
            \complete ->
                (complete |> build parser)
                    ++ (() |> build ignoredNext)
        }


{-| Matches a parser _lazily_.
This allows to create self-referential parsers for recursive definitions.

    import Char.ConversionStep as Char
    import Text.ConversionStep exposing (text, token)

    type LazyList
        = End
        | Next Char LazyList

    lazyList : ConversionStep LazyList
    lazyList =
        onFailDown [ lazyListEnd, lazyListNext ]

    lazyListEnd : ConversionStep LazyList
    lazyListEnd =
        succeed End
            |> drop (text "[]")

    lazyListNext : ConversionStep LazyList
    lazyListNext =
        succeed Next
            |> take atomAny
            |> drop (token (text "::"))
            |> take (lazy (\() -> lazyList))

    "[]" |> Text.narrowWith lazyList
    --> Ok End

    "a :: []" |> Text.narrowWith lazyList
    --> Ok (Next 'a' End)

    "a :: b :: []" |> Text.narrowWith lazyList
    --> Ok (Next 'a' (Next 'b' End))

Without `lazy`, you would get an error like:

>     The `lazyList` definition is causing a very tricky infinite loop.
>
>     The `lazyList` value depends on itself through the following chain of
>     definitions:
>
>           ┌─────┐
>           │    lazyList
>           │     ↓
>           │    lazyListNext
>           └─────┘

-}
lazy :
    (() -> ConversionStep atom narrow)
    -> ConversionStep atom narrow
lazy makeConversionStep =
    { narrow =
        \input -> makeConversionStep () |> narrowStepFrom input
    , broaden =
        \narrow -> narrow |> build (makeConversionStep ())
    }


{-| Matches any single character.

> ℹ️ Equivalent regular expression: `.`

    import ConversionStep exposing (atomAny)
    import ConversionStep.Error
    import Text.ConversionStep as Text

    -- can match any character
    "abc" |> Text.narrowWith atomAny --> Ok 'a'
    "#hashtag" |> Text.narrowWith atomAny --> Ok '#'

    -- only fails if we run out of inputs
    ""
        |> Text.narrowWith atomAny
        |> Result.mapError ConversionStep.Error.textMessage
    --> Err "1:0: I was expecting a character. I reached the end of the input."

-}
atomAny : ConversionStep atom atom
atomAny =
    expect "at least some input"
        { narrow =
            \input ->
                case input of
                    [] ->
                        fail
                            |> narrowStepFrom input

                    nextAtom :: tail ->
                        succeed nextAtom
                            |> narrowStepFrom tail
        , broaden = \narrow -> [ narrow ]
        }


{-| Matches a specific single input (= atom).

    import ConversionStep exposing (atom)
    import ConversionStep.Error
    import Text.ConversionStep as Text

    -- Match a specific character, case sensitive
    "abc" |> Text.narrowWith (atom 'a') --> Ok 'a'

    -- It fails if it's not _exactly_ the same
    "A"
        |> Text.narrowWith  (atom 'a')
        |> Result.mapError ConversionStep.Error.textMessage
    --> Err "1:1: I was expecting the character 'a'. I got stuck when I got the character 'A'."

-}
atom : atom -> ConversionStep atom ()
atom expectedAtom =
    next
        (\() -> expectedAtom)
        (\actualAtom ->
            if actualAtom == expectedAtom then
                succeed ()

            else
                fail
        )
        atomAny
        |> expectInternal
            (ExpectedSpecifically expectedAtom)


{-| Always succeed with the given value.

`succeed` is the key to success – folks from [elm radio]()

    import ConversionStep exposing (succeed)

    "no matter the input text"
        |> Text.narrowWith (succeed "abc")
    --> Ok "abc"

For anything composed of multiple parts,

> `succeed` is the key to success
> – folks from [elm radio]()

first declaratively describing what you expect to get in the end,
then [taking](#take) and [dropping](#drop) what you need to parse.

    import ConversionStep exposing (succeed, atom)
    import Text.ConversionStep exposing (integer)

    type alias Point =
        { x : Int
        , y : Int
        }

    point : ConversionStep Char Point
    point =
        succeed (\x y -> { x = x, y = y })
            |> take .x integer
            |> drop (atom ',')
            |> take .y integer

    "12,34" |> Text.narrowWith point
    --> Ok { x = 12, y = 34 }


### `succeed` anti-patterns

One example you'll run into when using other parsers is using

    succeed identity
        |> drop ...
        |> drop ...
        |> take ...
        |> drop ...

it get's pretty hard to read as you have to jump around the code to know what you're actually producing.

    succeed (\sum -> sum) |> ...

is already nicer, perfect would be

    succeed .sum
        |> drop ...
        |> drop ...
        |> take (map (\sum -> { sum = sum }) ...)
        |> drop ...

-}
succeed : narrow -> SequenceConversionStep atom_ complete_ narrow
succeed narrow =
    { narrow =
        \input ->
            { narrow = narrow
            , input = input
            }
                |> Ok
    , broaden =
        \_ -> []
    }



-- chain


{-| Always fail. Make sure you've told me what you [`expect`](#expect).

    import ConversionStep.Error
    import Text.ConversionStep

    ""
        |> Text.ConversionStep.narrowWith
            (expect "nothing, this always fails"
                fail
            )
        |> Result.mapError ConversionStep.Error.textMessage
    --> Err "1:0: I was expecting nothing, this always fails. I reached the end of the input."

-}
fail : ConversionStep atom_ narrow_
fail =
    { narrow =
        \input ->
            Err
                [ { startingAtDown = input |> List.length
                  , expected = Fail
                  }
                ]
    , broaden = \_ -> []
    }


buildFrom :
    narrow
    ->
        (ConversionStep atom narrow
         -> SequenceConversionStep atom () narrow
        )
buildFrom narrowBuildingSeed =
    \parser ->
        { narrow = parser.narrow
        , broaden = \() -> narrowBuildingSeed |> build parser
        }


{-| Returns the value of the first parser that matches.
It tries to match the parsers in order.

If none of the parsers match, it keeps the error message from the last parser.

> ℹ️ Equivalent regular expression: `|`

    import ConversionStep exposing (atom)
    import Char.ConversionStep as Char
    import ConversionStep.Error

    type UnderscoreOrLetter
        = Underscore
        | Letter Char

    underscoreOrLetter : ConversionStep Char UnderscoreOrLetter
    underscoreOrLetter =
        choice
            (\underscore letter first ->
                case first of
                    Underscore ->
                        underscore ()

                    Letter char ->
                        letter char
            )
            (possibility (\() -> Underscore) (atom '_')
                >> possibility Letter Char.letter
            )

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
        |> Result.mapError ConversionStep.Error.textMessage
    --> Err "1:1: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '1'."


### example: fallback step if the previous step fails

    import Conversion
    import Char.ConversionStep as Char
    import ConversionStep.Error

    type AlphaNums
        = Digits (List Int)
        | Letters String

    alphaNum : ConversionStep Char AlphaNum
    alphaNum =
        choice
            (\digit letter alphaNum ->
                case alphaNum of
                    Digit int ->
                        digit int

                    Letter char ->
                        letter char
            )
            (possibility Letter
                (atLeast 1 Char.letter
                    |> map Conversion.listToString
                )
                >> possibility Digit (atLeast 1 Char.digit)
            )

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
        |> Result.mapError ConversionStep.Error.textMessage
    --> Err "1:1: I was expecting at least 1 digit [0-9]. I got stuck when I got the character '_'."

-}
possibility :
    (possibilityNarrow -> variantUnion)
    -> SequenceConversionStep atom possibilityBuildable possibilityNarrow
    ->
        (ChoiceConversionStep
            atom
            ((possibilityBuildable -> List atom) -> discriminateFurther)
            variantUnion
         ->
            ChoiceConversionStep
                atom
                discriminateFurther
                variantUnion
        )
possibility possibilityTagToUnion possibilityConversionStep =
    \soFar ->
        { narrow =
            \input ->
                case soFar.narrow input of
                    Ok soFarOk ->
                        soFarOk |> Ok

                    Err soFarErrors ->
                        case possibilityConversionStep.narrow input of
                            Ok possibilityParsed ->
                                { input = possibilityParsed.input
                                , narrow = possibilityParsed.narrow |> possibilityTagToUnion
                                }
                                    |> Ok

                            Err failure ->
                                soFarErrors
                                    |> (::) failure
                                    |> Err
        , broaden =
            let
                discriminatedFurther =
                    soFar.broaden possibilityConversionStep.broaden
            in
            discriminatedFurther
        }


type alias ChoiceConversionStep atom discriminate variantUnion =
    RecordWithoutConstructorFunction
        { narrow :
            List atom
            ->
                Result
                    (List (ExpectationMiss atom))
                    { narrow : variantUnion
                    , input : List atom
                    }
        , broaden : discriminate
        }


choice :
    discriminate
    ->
        (ChoiceConversionStep atom discriminate variantUnion
         -> ChoiceConversionStep atom (variantUnion -> List atom) variantUnion
        )
    -> ConversionStep atom variantUnion
choice discriminate possibilities =
    let
        possible =
            possibilities
                { narrow = \_ -> Err []
                , broaden = discriminate
                }
    in
    { narrow =
        \input ->
            input
                |> possible.narrow
                |> Result.mapError
                    (\possibleErrors ->
                        [ { expected = Expected1In possibleErrors
                          , startingAtDown = input |> List.length
                          }
                        ]
                    )
    , broaden = possible.broaden
    }


{-| Parse one value, then after we got that value,
form another parser with this info.

This can be used to narrow the previous value,
or to use the last value for the next parser like a backreference.

TODO: example

-}
next :
    (narrowNarrow -> narrow)
    -> (narrow -> ConversionStep atom narrowNarrow)
    ->
        (ConversionStep atom narrow
         -> ConversionStep atom narrowNarrow
        )
next narrowNarrowToNarrow parserNarrowNarrow =
    \parser ->
        { narrow =
            \state ->
                parser
                    |> narrowStepFrom state
                    |> Result.andThen
                        (\result ->
                            parserNarrowNarrow result.narrow
                                |> narrowStepFrom result.input
                        )
        , broaden =
            \narrowNarrow ->
                let
                    narrow =
                        narrowNarrow |> narrowNarrowToNarrow
                in
                (narrow |> build parser)
                    ++ (narrowNarrow
                            |> build (parserNarrowNarrow narrow)
                       )
        }



-- transform


{-| Transform the narrow result value.

    import Conversion exposing (listToString)
    import ConversionStep exposing (map, atLeast)
    import Char.ConversionStep as Char
    import Text.ConversionStep as Text

    -- Get some letters, and make them lowercase.
    "ABC"
        |> Text.narrowWith
            (atLeast 1 Char.letter
                |> map listToString
                |> map (transfer { map = String.toLower, unmap = identity })
            )
    --> Ok "abc"

-}
map :
    Transfer narrow mappedNarrow
    -> ConversionStep atom narrow
    -> ConversionStep atom mappedNarrow
map narrowTransfer =
    \parser ->
        { narrow =
            \state ->
                parser
                    |> narrowStepFrom state
                    |> Result.map
                        (\result ->
                            { input = result.input
                            , narrow =
                                result.narrow
                                    |> Conversion.map narrowTransfer
                            }
                        )
        , broaden =
            \result ->
                result |> Conversion.unmap narrowTransfer |> build parser
        }



-- sequence


{-| Matches a sequence of parsers in order, and gets the result as a `List`.

    import ConversionStep exposing (map, atom)
    import Char.ConversionStep as Char
    import Text.ConversionStep as Text

    -- all parsers must be of the same type
    "_A5"
        |> Text.narrowWith
            (sequence [ atom '_', Char.letter, Char.digit ])
    --> Ok [ '_', 'A', '5' ]

    "3.14"
        |> Text.narrowWith
            (sequence
                [ atLeast 1 Char.digit     -- [ '3' ]
                , between 0 1 (atom '.') -- [ '.' ]
                , atLeast 0 Char.digit     -- [ '1', '4' ]
                ]
                |> map List.concat
            )
    --> Ok [ '3', '.', '1', '4' ]

Don't try to be clever with this.
The usual [`succeed`](#succeed)`(\... -> ...) |>`[`take`](#take)-[`drop`](#drop) chain
is often more explicit and descriptive.

-}
sequence : List (ConversionStep atom narrow) -> ConversionStep atom (List narrow)
sequence parsersInSequence =
    let
        step :
            ConversionStep atom narrow
            ->
                { narrow : List narrow
                , input : List atom
                }
            ->
                Result
                    (ExpectationMiss atom)
                    { narrow : List narrow
                    , input : List atom
                    }
        step stepConversionStep =
            \soFar ->
                stepConversionStep
                    |> narrowStepFrom soFar.input
                    |> Result.map
                        (\stepParsed ->
                            { input = stepParsed.input
                            , narrow =
                                soFar.narrow |> (::) stepParsed.narrow
                            }
                        )
    in
    { narrow =
        \initialInput ->
            parsersInSequence
                |> List.foldr
                    (\stepConversionStep ->
                        Result.andThen (step stepConversionStep)
                    )
                    ({ narrow = [], input = initialInput } |> Ok)
    , broaden =
        \narrowSequence ->
            List.map2
                (\parserInSequence narrowInSequence ->
                    narrowInSequence |> build parserInSequence
                )
                parsersInSequence
                narrowSequence
                |> List.concat
    }


{-| Matches an optional value and returns it as a `Maybe`.

> ℹ️ Equivalent regular expression: `?`

    import Char.ConversionStep exposing (letter)
    import Text.ConversionStep as Text

    -- maybe we get `Just` a letter
    "abc" |> Text.narrowWith (maybe Char.letter)
    --> Ok (Just 'a')

    -- maybe we get `Nothing`
    "123abc" |> Text.narrowWith (maybe Char.letter)
    --> Ok Nothing

-}
maybe :
    ConversionStep atom contentNarrow
    -> ConversionStep atom (Maybe contentNarrow)
maybe parser =
    choice
        (\just nothing maybe_ ->
            case maybe_ of
                Just content ->
                    just content

                Nothing ->
                    nothing ()
        )
        (possibility Just parser
            >> possibility (\() -> Nothing) (succeed ())
        )


{-| Matches a value `exactly` a number of times and returns them as a `List`.

> ℹ️ Equivalent regular expression: `{n}`

    import ConversionStep.Error
    import Char.ConversionStep as Char
    import Text.ConversionStep as Text

    -- we want `exactly` three letters
    "abcdef" |> Text.narrowWith (exactly 3 Char.letter)
    --> Ok [ 'a', 'b', 'c' ]

    -- not two or four, we want three
    "ab_def"
        |> Text.narrowWith (exactly 3 Char.letter)
        |> Result.mapError ConversionStep.Error.textMessage
    --> Err "1:3: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '_'."

-}
exactly :
    Int
    -> ConversionStep atom narrow
    -> ConversionStep atom (List narrow)
exactly howOften parser =
    sequence (List.repeat howOften parser)


{-| Matches a value at least a number of times and returns them as a `List`.

> ℹ️ Equivalent regular expression: `{min,}`

    import ConversionStep.Error
    import Char.ConversionStep as Char
    import Text.ConversionStep as Text

    -- we want at least three letters, we are okay with more than three
    "abcdef"
        |> Text.narrowWith (atLeast 3 Char.letter)
    --> Ok [ 'a', 'b', 'c', 'd', 'e', 'f' ]

    -- but not two, that's sacrilegious
    "ab_def"
        |> Text.narrowWith (atLeast 3 Char.letter)
        |> Result.mapError ConversionStep.Error.textMessage
    --> Err "1:3: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '_'."


## `arLeast 0`

> ℹ️ Equivalent regular expression: `*`

    import Char.ConversionStep as Char
    import Text.ConversionStep as Text

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

    import ConversionStep.Error
    import Char.ConversionStep as Char
    import Text.ConversionStep as Text

    -- we want as many letters as there are
    "abc" |> Text.narrowWith (atLeast 1 Char.letter)
    --> Ok [ 'a', 'b', 'c' ]

    "abc123" |> Text.narrowWith (atLeast 1 Char.letter)
    --> Ok [ 'a', 'b', 'c' ]

    -- but we want at least one
    "123abc"
        |> Text.narrowWith (atLeast 1 Char.letter)
        |> Result.mapError ConversionStep.Error.textMessage
    --> Err "1:1: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '1'."

-}
atLeast :
    Int
    -> ConversionStep atom narrow
    -> ConversionStep atom (List narrow)
atLeast minimum parser =
    succeed (\minimumList overMinimum -> minimumList ++ overMinimum)
        |> take (List.take minimum) (exactly minimum parser)
        |> take (List.drop minimum) (atLeast 0 parser)


{-| Matches a value between a range of times and returns them as a `List`.

> ℹ️ Equivalent regular expression: `{min,max}`

    import ConversionStep.Error
    import Char.ConversionStep as Char
    import Text.ConversionStep as Text

    -- we want between two and four letters
    "abcdef" |> Text.narrowWith (between 2 4 Char.letter)
    --> Ok [ 'a', 'b', 'c', 'd' ]

    "abc_ef" |> Text.narrowWith (between 2 4 Char.letter)
    --> Ok [ 'a', 'b', 'c' ]

    "ab_def" |> Text.narrowWith (between 2 4 Char.letter)
    --> Ok [ 'a', 'b' ]


    -- but less than that is not cool
    "a_cdef"
        |> Text.narrowWith (between 2 3 letter)
        |> Result.mapError ConversionStep.Error.textMessage
    --> Err "1:2: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '_'."


### example: `between 0 1`

ALternative to [`maybe`](#maybe) which instead returns a `List`.

> ℹ️ Equivalent regular expression: `?`

    import Char.ConversionStep as Char
    import Text.ConversionStep as Text

    -- we want one letter, optionally
    "abc" |> Text.narrowWith (between 0 1 Char.letter)
    --> Ok [ 'a' ]

    -- if we don't get any, that's still okay
    "123abc" |> Text.narrowWith (between 0 1 Char.letter)
    --> Ok []


### example: at most

> ℹ️ Equivalent regular expression: `{0,max}`

    import ConversionStep exposing (atom)
    import Char.ConversionStep as Char
    import Text.ConversionStep as Text

    -- we want a maximum of three letters
    "abcdef" |> Text.narrowWith (between 0 3 Char.letter)
    --> Ok [ 'a', 'b', 'c' ]

    -- less than that is also okay
    "ab_def" |> Text.narrowWith (between 0 3 Char.letter)
    --> Ok [ 'a', 'b' ]

    -- even zero letters are fine
    "_bcdef" |> Text.narrowWith (between 0 3 Char.letter)
    --> Ok []

    -- make sure we don't consume more than three letters
    "abcdef"
        |> Text.narrowWith
            (succeed (\letters -> letters)
                |> take (between 0 3 Char.letter)
                |> drop (atom 'd')
            )
    --> Ok [ 'a', 'b', 'c' ]

-}
between : Int -> Int -> ConversionStep atom narrow -> ConversionStep atom (List narrow)
between minimum maximum parser =
    succeed (\minimumList overMinimum -> minimumList ++ overMinimum)
        |> take
            (List.take minimum)
            (exactly minimum parser)
        |> take
            (List.drop maximum)
            (atMost (maximum - minimum) parser)


{-| Matches a value at most a number of times and returns them as a `List`.

**Shouldn't be exposed**

-}
atMost :
    Int
    -> ConversionStep atom narrow
    -> ConversionStep atom (List narrow)
atMost maximum =
    \step ->
        loop
            { initial = []
            , goOnBroaden = Stack.toList
            , step =
                \soFar ->
                    possibility GoOn
                        (if (soFar |> List.length) >= maximum then
                            fail

                         else
                            step
                                |> map
                                    (transfer
                                        (\parsed -> topDown parsed soFar)
                                        Stack.top
                                    )
                        )
                        >> possibility Commit
                            (soFar |> List.reverse |> succeed)
            }


{-| Matches a values repeatedly until a delimiter parser matches.
The delimiter marks the end of the sequence, and it is consumed.

    import ConversionStep exposing (drop, map, succeed, take, atom, end)
    import ConversionStep.Error
    import Char.ConversionStep as Char
    import Text.ConversionStep as Text

    "abcdef"
        |> Text.narrowWith (until (atom 'd') Char.letter)
    --> Ok { before = [ 'a', 'b', 'c' ], delimiter = 'd' }

    -- the delimiter _must_ be present
    "abc123"
        |> Text.narrowWith (until (atom 'd') Char.letter)
        |> Result.toMaybe
    --> Nothing

    -- the delimiter is consumed
    "<abc>"
        |> Text.narrowWith
            (succeed (\tag -> tag)
                |> drop (atom '<')
                |> take
                    (until (atom '>') Char.letter
                        |> map .before
                        |> map String.fromList
                    )
                |> drop end
            )
    --> Ok "abc"

-}
until :
    ConversionStep atom end
    -> ConversionStep atom before
    -> ConversionStep atom { before : before, end : end }
until endConversionStep beforeConversionStep =
    { narrow =
        \input ->
            (loop
                { initial = { before = [] }
                , goOnBroaden =
                    \{ before } ->
                        { before = before |> Stack.toList }
                , step =
                    \{ before } ->
                        possibility GoOn
                            (succeed (\before_ -> { before = before_ })
                                |> take .before
                                    (atomAny
                                        |> map
                                            (transfer (\top -> topDown top before) Stack.top)
                                    )
                            )
                            >> possibility Commit
                                (endConversionStep
                                    |> map
                                        (transfer
                                            (\end ->
                                                { end = end
                                                , before = before |> List.reverse
                                                }
                                            )
                                            .end
                                        )
                                )
                }
            ).narrow
                input
                |> Result.andThen
                    (\parsed ->
                        parsed.narrow.before
                            |> beforeConversionStep.narrow
                            |> Result.map
                                (\beforeParsed ->
                                    { input = beforeParsed.input
                                    , narrow =
                                        { end = parsed.narrow.end
                                        , before = beforeParsed.narrow
                                        }
                                    }
                                )
                    )
    , broaden =
        \parsed ->
            (parsed.before |> build beforeConversionStep)
                ++ (parsed.end |> build endConversionStep)
    }


type LoopStep partial committed
    = GoOn partial
    | Commit committed


{-| A powerful way to recurse over [`ConversionStep`](#ConversionStep)s.


### example: fold

    import ConversionStep exposing (ConversionStep, atom, succeed, onFailDown, expected, atLeast, take, drop, loop)
    import Char.ConversionStep as Char
    import Text.ConversionStep as Text
    import Text.ConversionStep exposing (number)

    sumWhileLessThan : Float -> ConversionStep Char Float
    sumWhileLessThan max =
        loop
            { initial = { total = 0 }
            , step =
                \soFar ->
                    onFailDown
                        [ andThen
                            (\n ->
                                let
                                    total =
                                        soFar.total + n
                                in
                                if total >= max then
                                    expected ""
                                else
                                    succeed ({ total = total } |> ConversionStep.GoOn)
                            )
                            (succeed (\n -> n)
                                |> take number
                                |> drop (atLeast 0 Char.blank)
                            )
                        , succeed (soFar.total |> ConversionStep.Commit)
                        ]
            }

    -- The fold stops before we reach a maximum of 6 in the sum
    "2 3 4" |> Text.narrowWith (sumWhileLessThan 6) --> Ok 5

    -- Make sure we didn't consume too many numbers
    "2 3 4"
        |> Text.narrowWith
            (succeed (\sum -> sum)
                |> take (sumWhileLessThan 6)
                |> drop (atom '4')
            )
            |> parse
    --> Ok 5

-}
loop :
    { initial : beforeStep
    , goOnBroaden : goingOn -> beforeStep
    , step :
        beforeStep
        ->
            (ChoiceConversionStep
                atom
                ((goingOn -> List atom)
                 -> (committed -> List atom)
                 -> LoopStep goingOn committed
                 -> List atom
                )
                (LoopStep goingOn committed)
             ->
                ChoiceConversionStep
                    atom
                    (LoopStep goingOn committed -> List atom)
                    (LoopStep goingOn committed)
            )
    }
    -> ConversionStep atom committed
loop loop_ =
    -- ✨ isn't this beautiful ✨
    let
        step : () -> beforeStep -> ConversionStep atom committed
        step () =
            \beforeStep ->
                choice
                    (\goOn commit stepped ->
                        case stepped of
                            GoOn goingOn ->
                                goOn goingOn

                            Commit committed ->
                                commit committed
                    )
                    (loop_.step beforeStep)
                    |> next Commit
                        (\parsed ->
                            case parsed of
                                Commit committed ->
                                    committed |> succeed

                                GoOn goingOn ->
                                    goingOn |> loop_.goOnBroaden |> step ()
                        )
    in
    loop_.initial |> step ()


{-| Splits the input text by a _separator_ parser into a `List` of `String`s.
The separators cannot overlap,
and are interleaved alongside the values in the order found.

    import ConversionStep exposing (map, split)
    import Text.ConversionStep as Text exposing (text)

    -- note that both values and separators must be of the same type
    "a,bc,def"
        |> Text.narrowWith
            (split ( atLeast 0, text "," ) (atLeast 0 atomAny))
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
            (split ( atLeast 0, text "," ) (atLeast 0 atomAny))
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
            (split ( atLeast 0, text "," ) (atLeast 0 atomAny))
    --> Ok (topDown [] [])

-}
split :
    ( ConversionStep atom { separator : separator, part : part }
      -> ConversionStep atom (List { separator : separator, part : part })
    , ConversionStep atom separator
    )
    -> ConversionStep atom part
    ->
        ConversionStep
            atom
            (Hand
                (StackTopBelow
                    part
                    { separator : separator, part : part }
                )
                Never
                Empty
            )
split ( splitSequence, separatorConversionStep ) partConversionStep =
    succeed topDown
        |> take Stack.top partConversionStep
        |> take (Stack.topRemove >> Stack.toList)
            (splitSequence
                (succeed
                    (\separator part ->
                        { separator = separator, part = part }
                    )
                    |> take .separator separatorConversionStep
                    |> take .part partConversionStep
                )
            )
