module Parser exposing
    ( Parser
    , Expected(..), ExpectationMiss, Error(..)
    , narrowWith
    , into
    , succeed, expecting, atomAny, atom
    , lazy
    , take, drop
    , expected, onFailDown, andThen, except, map
    , atLeast, between, exactly, maybe, sequence
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

    import Parser exposing (Parser, atLeast, drop, into, succeed, take, atom)
    import Char.Parser as Char
    import Text.Parser as Text exposing (number)
    import Parser.Error

    type alias Point =
        { x : Float
        , y : Float
        }

    -- a successful parse looks like
    "(2.71, 3.14)" |> Text.narrowWith point --> Ok { x = 2.71, y = 3.14 }

    point : Parser Point
    point =
        into "Point"
            (succeed (\x y -> { x = x, y = y })
                |> drop (atom '(')
                |> drop (atLeast 0 Char.blank)
                |> take number
                |> drop (atLeast 0 Char.blank)
                |> drop (atom ',')
                |> drop (atLeast 0 Char.blank)
                |> take number
                |> drop (atLeast 0 Char.blank)
                |> drop (atom ')')
            )

    -- we can get a nice error message if it fails
    "(2.71, x)"
        |> Text.narrowWith point
        |> Result.mapError (Parser.Error.dump "filename.txt")
    --> Err
    -->     [ "[ERROR] filename.txt: line 1:8: I was expecting a digit [0-9]. I got stuck when I got 'x'."
    -->     , "  in Point at line 1:1"
    -->     , ""
    -->     , "1|(2.71, x)"
    -->     , "  ~~~~~~~^"
    -->     ]


## usage

  - [`Parser`](Parser): creating and chaining parsers

  - [`Char.Parser`](Char-Parser): parsing characters

  - [`Text.Parser`](Text-Parser): parsing text

  - [`Parser.Error`](Parser-Error):
    error reporting and formatting

Note before we start:
`Parser` always backtracks and never commits to a specific path!

  - 👍 improves parser code readability
  - 👍 provides more information on what could be possible to the user
  - 👎 performs worse as there's more to parse to know it failed

@docs Parser
@docs Expected, ExpectationMiss, Error


## basic

@docs narrowWith
@docs into


## create

@docs succeed, expecting, atomAny, atom


## recursion

@docs lazy


## chain

@docs take, drop
@docs expected, onFailDown, andThen, except, map


## sequences

@docs atLeast, between, exactly, maybe, sequence
@docs until, split


### looping

@docs loop, LoopStep

-}

import Hand exposing (Empty, Hand, filled)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Stack exposing (Stacked)



-- types


{-| A function that takes an [input state](#RunningState)
and returns either an [`Error`](#Error),
or a value with the next state.

    {-| [`Parser`](#Parser) on input characters
    -}
    type alias TextParser narrow =
        Parser Char narrow

-}
type alias Parser atom narrow =
    RecordWithoutConstructorFunction
        { narrow :
            List atom
            ->
                Result
                    (ExpectationMiss atom)
                    { narrow : narrow
                    , input : List atom
                    }
        }


{-| Details: What are we trying to parse here?
-}
type alias Context =
    RecordWithoutConstructorFunction
        { description : String
        , fromLast : Int
        }


{-| Contains the description of an error.
This includes an error message, the position, and the context stack.
-}
type alias ExpectationMiss atom =
    RecordWithoutConstructorFunction
        { expected : Expected atom
        , stuckAtFromLast : Int
        , context : List Context
        }


{-| What went wrong.
-}
type Expected atom
    = ExpectedSpecifically atom
    | Expected1In (List (ExpectationMiss atom))
    | ExpectedCustom String


type Error atom narrow
    = InputRemaining
        { narrow : narrow
        , input : Hand (Stacked atom) Never Empty
        }
    | ExpectationMiss (ExpectationMiss atom)



--


narrowStepFrom :
    List atom
    -> Parser atom narrow
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

    import Char.Parser as Char
    import Text.Parser exposing (number)
    import Parser.Error

    -- consumes a single letter, then "bc" are still remaining
    "abc" |> Text.narrowWith Char.letter --> Ok 'a'

    -- we can also parse text into other data types like numbers
    "3.14" |> Text.narrowWith number --> Ok 3.14

    -- we get an error message if the parser doesn't match
    "123"
        |> Text.narrowWith Char.letter
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:1: I was expecting a letter [a-zA-Z]. I got stuck when I got '1'."

-}
narrowWith :
    Parser atom narrow
    -> List atom
    -> Result (Error atom narrow) narrow
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


{-| Starts a parsing pipeline to parse into a data type.

You should also specify a name for the context,
this helps to give better error messages.

    import Parser exposing (take, drop, succeed, into, atom)
    import Text.Parser exposing (number)

    type alias Point =
        { x : Float
        , y : Float
        }

    -- We can use `into` to have more context when an error happens.
    point : Parser Point
    point =
        into "Point"
            (succeed (\x y -> { x = x, y = y })
                |> drop (atom '(')
                |> take number
                |> drop (atom ',')
                |> take number
                |> drop (atom ')')
            )

    "(12,34)" |> Text.narrowWith point --> Ok { x = 12, y = 34 }

    -- We can get the error context stack as well as where they started matching.
    "(a,b)" |> Text.narrowWith point
        |> Result.mapError (\error -> error.context |> List.map .name)
    --> Err [ "Point" ]

-}
into :
    String
    -> Parser atom narrow
    -> Parser atom narrow
into contextDescription parser =
    { narrow =
        \beforeInput ->
            parser
                |> narrowStepFrom beforeInput
                |> Result.mapError
                    (\error ->
                        { error
                            | context =
                                error.context
                                    |> (::)
                                        { description = contextDescription
                                        , fromLast = beforeInput |> List.length
                                        }
                        }
                    )
                |> Result.andThen
                    (\after ->
                        succeed after.narrow
                            |> narrowStepFrom after.input
                    )
    }



-- basic


{-| Takes a parsed value and feeds it to the return value of the parser.
-}
take :
    Parser atom next
    -> Parser atom (next -> applied)
    -> Parser atom applied
take next =
    andThen (\eat -> next |> map eat)


{-| Ignores a parsed value, but it still must match to continue.

    import Text.Parser exposing (text)
    import Parser exposing (atom, succeed, atLeast, take, drop)

    -- parse a simple email, but we're only interested in the username
    "user@example.com"
        |> Text.narrowWith
            (succeed (\userName -> userName)
                |> take (atLeast 1 letter)
                |> drop (atom '@')
                |> drop (atLeast 1 letter)
                |> drop (text ".com")
            )
    --> Ok "user"

-}
drop :
    Parser atom next_
    -> Parser atom narrow
    -> Parser atom narrow
drop next =
    andThen
        (\narrow ->
            next |> map (\_ -> narrow)
        )


{-| Matches a parser _lazily_.
This allows to create self-referential parsers for recursive definitions.

    import Char.Parser as Char
    import Text.Parser exposing (text, token)

    type LazyList
        = End
        | Next Char LazyList

    lazyList : Parser LazyList
    lazyList =
        onFailDown [ lazyListEnd, lazyListNext ]

    lazyListEnd : Parser LazyList
    lazyListEnd =
        succeed End
            |> drop (text "[]")

    lazyListNext : Parser LazyList
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
    (() -> Parser atom narrow)
    -> Parser atom narrow
lazy makeParser =
    { narrow =
        \state -> makeParser () |> narrowStepFrom state
    }


{-| Matches any character `except` the parser provided.

> ℹ️ It's a good idea to use [`Parser.expecting`](Parser#expecting) alongside this function
> to improve the error messages.

    import Parser.Error
    import Text.Parser as Text

    -- anything except a letter is okay
    "123" |> Text.narrowWith (except letter) --> Ok '1'
    "-123" |> Text.narrowWith (except letter) --> Ok '-'

    -- but a letter is not
    abc"
        |> Text.narrowWith (except letter)
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:1: I was expecting a different character. I got stuck when I got the character 'a'."

-}
except :
    Parser atom atom
    -> Parser atom atom
except parser =
    { narrow =
        \input ->
            case parser |> narrowStepFrom input of
                Ok exception ->
                    expected "a different character"
                        |> narrowStepFrom exception.input

                Err _ ->
                    atomAny |> narrowStepFrom input
    }


{-| Matches any single character.

> ℹ️ Equivalent regular expression: `.`

    import Parser exposing (atomAny)
    import Parser.Error
    import Text.Parser as Text

    -- can match any character
    "abc" |> Text.narrowWith atomAny --> Ok 'a'
    "#hashtag" |> Text.narrowWith atomAny --> Ok '#'

    -- only fails if we run out of inputs
    ""
        |> Text.narrowWith atomAny
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:0: I was expecting a character. I reached the end of the input."

-}
atomAny : Parser atom atom
atomAny =
    { narrow =
        \input ->
            case input of
                [] ->
                    expected "at least some input"
                        |> narrowStepFrom input

                next :: tail ->
                    succeed next
                        |> narrowStepFrom tail
    }


{-| **should not be exposed**
-}
expectingInternal :
    (Expected atom -> Expected atom)
    -> Parser atom narrow
    -> Parser atom narrow
expectingInternal expectedMap =
    \parser ->
        { narrow =
            \input ->
                parser
                    |> narrowStepFrom input
                    |> Result.mapError
                        (\error ->
                            { error
                                | expected = error.expected |> expectedMap
                            }
                        )
        }


{-| Matches a specific single input (= atom).

    import Parser exposing (atom)
    import Parser.Error
    import Text.Parser as Text

    -- Match a specific character, case sensitive
    "abc" |> Text.narrowWith (atom 'a') --> Ok 'a'

    -- It fails if it's not _exactly_ the same
    "A"
        |> Text.narrowWith  (atom 'a')
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:1: I was expecting the character 'a'. I got stuck when I got the character 'A'."

-}
atom : atom -> Parser atom atom
atom expectedSingle =
    andThen
        (\actualSingle ->
            if actualSingle == expectedSingle then
                succeed actualSingle

            else
                expected ""
        )
        atomAny
        |> expectingInternal
            (\_ -> ExpectedSpecifically expectedSingle)


{-| A parser that always succeeds with the given value.

    import Parser exposing (succeed, atom)
    import Text.Parser exposing (int)

    -- Always succeed with "abc" no matter the input text
    "" |> Text.narrowWith (succeed "abc") --> Ok "abc"

    -- This is usually used to start parser pipelines
    type alias Pair =
        { x : Int
        , y : Int
        }

    pair : Parser Char Pair
    pair =
        succeed (\x y -> { x = x, y = y })
            |> take int
            |> drop (atom ',')
            |> take int

    "12,34" |> Text.narrowWith pair --> Ok { x = 12, y = 34 }

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
succeed : narrow -> Parser atom_ narrow
succeed narrow =
    { narrow =
        \state ->
            { narrow = narrow
            , input = state
            }
                |> Ok
    }



-- chain


{-| Always fail with a given error message.

    import Parser.Error
    import Text.Parser

    ""
        |> Text.Parser.narrowWith
            (expected "nothing, this always fails")
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:0: I was expecting nothing, this always fails. I reached the end of the input."

-}
expected : String -> Parser atom_ narrow_
expected description =
    expectedInternal (ExpectedCustom description)


{-| **should not be exposed**
-}
expectedInternal :
    Expected atom
    -> Parser atom narrow_
expectedInternal expectation =
    { narrow =
        \input ->
            Err
                { expected = expectation
                , stuckAtFromLast = input |> List.length
                , context = []
                }
    }


{-| Returns the value of the first parser that matches.
It tries to match the parsers in order.

If none of the parsers match, it keeps the error message from the last parser.

> ℹ️ It's a good idea to use [`expecting`](#expecting) alongside this function
> to improve the error messages.

> ℹ️ Equivalent regular expression: `|`

    import Parser exposing (atom)
    import Char.Parser as Char
    import Parser.Error

    -- try the first parser
    "_abc"
        |> Text.narrowWith (onFailDown [ atom '_', Char.letter ])
    --> Ok '_'

    -- if it doesn't work, try the next
    "abc"
        |> Text.narrowWith (onFailDown [ atom '_', Char.letter ])
    --> Ok 'a'

    -- if none of them work, we get the error from the last parser
    "123"
        |> Text.narrowWith (onFailDown [ atom '_', Char.letter ])
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:1: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '1'."


### example: fallback parser if the previous parser fails

    import Char.Parser as Char
    import Parser.Error

    -- try letters, or else give me some digits
    "abc"
        |> Text.narrowWith
            (onFailDown
                [ atLeast 1 Char.letter
                , atLeast 1 Char.digit
                ]
                |> map String.fromList
            )
    --> Ok "abc"

    -- we didn't get letters, but we still got digits
    "123"
        |> Text.narrowWith
            (onFailDown
                [ atLeast 1 Char.letter
                , atLeast 1 Char.digit
                ]
                |> map String.fromList
            )
    --> Ok "123"

    -- but if we still fail, give the error message of the fallback parser
    "_"
        |> Text.narrowWith
            (onFailDown
                [ atLeast 1 Char.letter
                , atLeast 1 Char.digit
                ]
                |> map String.fromList
            )
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:1: I was expecting at least 1 digit [0-9]. I got stuck when I got the character '_'."

-}
onFailDown :
    List (Parser atom narrow)
    -> Parser atom narrow
onFailDown optionParsers =
    { narrow =
        \input ->
            optionParsers
                |> List.foldl
                    (\parser soFar ->
                        case soFar of
                            Ok soFarOk ->
                                soFarOk |> Ok

                            Err soFarErrors ->
                                case parser.narrow input of
                                    Ok ok ->
                                        ok |> Ok

                                    Err failure ->
                                        soFarErrors |> (::) failure |> Err
                    )
                    (Err [])
                |> Result.mapError
                    (\failedExpectations ->
                        { context = []
                        , stuckAtFromLast = input |> List.length
                        , expected = Expected1In failedExpectations
                        }
                    )
    }


{-| Parse one value `andThen` do something with that value,
which results in another parser.

This can be used to validate the last thing we parsed,
or transform the last value,
or to use the last value for the next parser like a backreference.

    import Parser exposing (atLeast, atomAny)
    import Parser.Error

    -- Get some characters `andThen` interpret them as an `Int`.
    "123"
        |> Text.narrowWith
            ((atLeast 1 atomAny |> map String.fromList)
                |> andThen
                    (\chars ->
                        case String.toInt chars of
                            Just n ->
                                n |> succeed
                            Nothing ->
                                expected "an integer, better use Text.Parser.int"
                    )
            )
    --> Ok 123

-}
andThen :
    (narrow -> Parser atom result)
    -> Parser atom narrow
    -> Parser atom result
andThen parserFromNarrow parser =
    { narrow =
        \state ->
            parser
                |> narrowStepFrom state
                |> Result.andThen
                    (\result ->
                        parserFromNarrow result.narrow
                            |> narrowStepFrom result.input
                    )
    }


{-| If there is an error, this replaces the error message.
This helps create more descriptive error messages instead of the more generic
ones.

If you want to use the value that failed some validation in the error message,
consider using a more relaxed parser and using [`andThen`](#andThen) to do the
validation.
It's a little longer, but that way you get access to the potentially invalid
parsed value.

    import Parser.Error
    import Char.Parser as Char
    import Text.Parser as Text

    -- We can redefine an error message if something goes wrong.
    "123"
        |> Text.narrowWith
            (atLeast 1 Char.letter
                |> expecting "a name consisting of letters"
            )
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:1: I was expecting a name consisting of letters. I got stuck when I got the character '1'."

-}
expecting : String -> Parser atom narrow -> Parser atom narrow
expecting description =
    expectingInternal (\_ -> ExpectedCustom description)



-- transform


{-| Transform the narrow result value.

    import Parser exposing (map, atLeast)
    import Char.Parser as Char
    import Text.Parser as Text

    -- Get some letters, and make them lowercase.
    "ABC"
        |> Text.narrowWith
            (atLeast 1 Char.letter
                |> map String.fromList
                |> map String.toLower
            )
    --> Ok "abc"

-}
map :
    (narrow -> mappedNarrow)
    -> Parser atom narrow
    -> Parser atom mappedNarrow
map narrowMap =
    andThen (\narrow -> narrow |> narrowMap |> succeed)



-- sequence


{-| Matches a sequence of parsers in order, and gets the result as a `List`.

    import Parser exposing (map, atom)
    import Char.Parser as Char
    import Text.Parser as Text

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
sequence : List (Parser atom narrow) -> Parser atom (List narrow)
sequence parsers =
    let
        step :
            Parser atom narrow
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
        step stepParser =
            \soFar ->
                stepParser
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
            parsers
                |> List.foldr
                    (\stepParser ->
                        Result.andThen (step stepParser)
                    )
                    ({ narrow = [], input = initialInput } |> Ok)
    }


{-| Matches an optional value and returns it as a `Maybe`.

> ℹ️ Equivalent regular expression: `?`

    import Char.Parser exposing (letter)
    import Text.Parser as Text

    -- Maybe we get `Just` a letter
    "abc" |> Text.narrowWith (maybe Char.letter) --> Ok (Just 'a')

    -- Or maybe we get `Nothing`
    "123abc" |> Text.narrowWith (maybe Char.letter) --> Ok Nothing

-}
maybe :
    Parser atom narrow
    -> Parser atom (Maybe narrow)
maybe parser =
    onFailDown
        [ map Just parser
        , succeed Nothing
        ]


{-| Matches a value `exactly` a number of times and returns them as a `List`.

> ℹ️ Equivalent regular expression: `{n}`

    import Parser.Error
    import Char.Parser as Char
    import Text.Parser as Text

    -- we want `exactly` three letters
    "abcdef" |> Text.narrowWith (exactly 3 Char.letter)
    --> Ok [ 'a', 'b', 'c' ]

    -- not two or four, we want three
    "ab_def"
        |> Text.narrowWith (exactly 3 Char.letter)
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:3: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '_'."

-}
exactly :
    Int
    -> Parser atom narrow
    -> Parser atom (List narrow)
exactly n parser =
    sequence (List.repeat n parser)


{-| Matches a value at least a number of times and returns them as a `List`.

> ℹ️ Equivalent regular expression: `{min,}`

    import Parser.Error
    import Char.Parser as Char
    import Text.Parser as Text

    -- we want at least three letters, we are okay with more than three
    "abcdef"
        |> Text.narrowWith (atLeast 3 Char.letter)
    --> Ok [ 'a', 'b', 'c', 'd', 'e', 'f' ]

    -- but not two, that's sacrilegious
    "ab_def"
        |> Text.narrowWith (atLeast 3 Char.letter)
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:3: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '_'."


## `arLeast 0`

> ℹ️ Equivalent regular expression: `*`

    import Char.Parser as Char
    import Text.Parser as Text

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

    import Parser.Error
    import Char.Parser as Char
    import Text.Parser as Text

    -- we want as many letters as there are
    "abc" |> Text.narrowWith (atLeast 1 Char.letter)
    --> Ok [ 'a', 'b', 'c' ]

    "abc123" |> Text.narrowWith (atLeast 1 Char.letter)
    --> Ok [ 'a', 'b', 'c' ]

    -- but we want at least one
    "123abc"
        |> Text.narrowWith (atLeast 1 Char.letter)
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:1: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '1'."

-}
atLeast :
    Int
    -> Parser atom narrow
    -> Parser atom (List narrow)
atLeast minimum parser =
    succeed (\minimumList overMinimum -> minimumList ++ overMinimum)
        |> take (exactly minimum parser)
        |> take (atLeast 0 parser)


{-| Matches a value between a range of times and returns them as a `List`.

> ℹ️ Equivalent regular expression: `{min,max}`

    import Parser.Error
    import Char.Parser as Char
    import Text.Parser as Text

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
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:2: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '_'."


### example: `between 0 1`

ALternative to [`maybe`](#maybe) which instead returns a `List`.

> ℹ️ Equivalent regular expression: `?`

    import Char.Parser as Char
    import Text.Parser as Text

    -- we want one letter, optionally
    "abc" |> Text.narrowWith (between 0 1 Char.letter)
    --> Ok [ 'a' ]

    -- if we don't get any, that's still okay
    "123abc" |> Text.narrowWith (between 0 1 Char.letter)
    --> Ok []


### example: at most

> ℹ️ Equivalent regular expression: `{0,max}`

    import Parser exposing (atom)
    import Char.Parser as Char
    import Text.Parser as Text

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
between : Int -> Int -> Parser atom narrow -> Parser atom (List narrow)
between min max parser =
    succeed (\minimum overMinimum -> minimum ++ overMinimum)
        |> take (exactly min parser)
        |> take (atMost (max - min) parser)


{-| Matches a value at most a number of times and returns them as a `List`.

**Shouldn't be exposed**

-}
atMost :
    Int
    -> Parser atom narrow
    -> Parser atom (List narrow)
atMost maximum =
    \stepParser ->
        loop
            { initial = []
            , step =
                \soFar ->
                    onFailDown
                        [ if (soFar |> List.length) >= maximum then
                            expected ""

                          else
                            stepParser
                                |> andThen
                                    (\parsed ->
                                        succeed (GoOn (parsed :: soFar))
                                    )
                        , succeed (Commit (soFar |> List.reverse))
                        ]
            }


{-| Matches a values repeatedly until a delimiter parser matches.
The delimiter marks the end of the sequence, and it is consumed.

    import Parser exposing (drop, map, succeed, take, atom, end)
    import Parser.Error
    import Char.Parser as Char
    import Text.Parser as Text

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
    Parser atom delimiter
    -> Parser atom before
    -> Parser atom { before : List before, delimiter : delimiter }
until delimiterParser beforeParser =
    loop
        { initial = { before = [] }
        , step =
            \{ before } ->
                onFailDown
                    [ delimiterParser
                        |> map
                            (\delimiter ->
                                Commit { before = before, delimiter = delimiter }
                            )
                    , beforeParser
                        |> map
                            (\anotherBefore ->
                                GoOn { before = before |> (::) anotherBefore }
                            )
                    ]
        }


type LoopStep partial done
    = GoOn partial
    | Commit done


{-| A powerful way to recurse over [`Parser`](#Parser)s.


### example: fold

    import Parser exposing (Parser, atom, succeed, onFailDown, expected, atLeast, take, drop, loop)
    import Char.Parser as Char
    import Text.Parser as Text
    import Text.Parser exposing (number)

    sumWhileLessThan : Float -> Parser Char Float
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
                                    succeed ({ total = total } |> Parser.GoOn)
                            )
                            (succeed (\n -> n)
                                |> take number
                                |> drop (atLeast 0 Char.blank)
                            )
                        , succeed (soFar.total |> Parser.Commit)
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
    { initial : partial
    , step : partial -> Parser atom (LoopStep partial done)
    }
    -> Parser atom done
loop parser =
    { narrow =
        let
            step partial =
                \input ->
                    parser.step partial
                        |> narrowStepFrom input
                        |> Result.andThen
                            (\parsed ->
                                case parsed.narrow of
                                    Commit narrow ->
                                        succeed narrow |> narrowStepFrom parsed.input

                                    GoOn goOnPartial ->
                                        parsed.input |> step goOnPartial
                            )
        in
        step parser.initial
    }


{-| Splits the input text by a _separator_ parser into a `List` of `String`s.
The separators cannot overlap,
and are interleaved alongside the values in the order found.

    import Parser exposing (map, split)
    import Text.Parser as Text exposing (text)

    -- note that both values and separators must be of the same type
    "a,bc,def"
        |> Text.narrowWith (split (text ","))
        --> Ok
    -->     { initial =
    -->         [ { part = "a", separator = "," }
    -->         , { part = "bc", separator = "," }
    -->         , { part = "def", separator = "," }
    -->     , last = ""
    -->     }

    -- leading/trailing separators are valid and give empty values
    ",a,,"
        |> Text.narrowWith (split (text ","))
    --> Ok
    -->     { initial =
    -->         [ { part = "", separator = "," }
    -->         , { part = "a", separator = "," }
    -->         , { part = "", separator = "," }
    -->     , last = ""
    -->     }

    -- an empty input text gives a single element from an empty string
    "" |> Text.narrowWith (split (text ","))
    --> Ok { initial = [], last = [] }

-}
split :
    Parser atom separator
    ->
        Parser
            atom
            { initial : List { part : List atom, separator : separator }
            , last : List atom
            }
split separator =
    succeed (\before last -> { initial = before, last = last })
        |> take
            -- Zero or more value-separator pairs
            (atLeast 0
                (until separator atomAny
                    |> map
                        (\narrow ->
                            { separator = narrow.delimiter
                            , part = narrow.before
                            }
                        )
                )
            )
        |> take
            -- Last value with whatever is left
            (atLeast 0 atomAny)



-- zombie
{-

-}
