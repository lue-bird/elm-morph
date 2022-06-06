module Parser exposing
    ( Parser, StateRunning, Error, Expected(..)
    , narrowWith, parse
    , into
    , succeed, expecting, atomAny, atom
    , beginning, end
    , lazy
    , take, drop
    , expected, oneOf, andThen, except, map
    , atLeast, between, exactly, foldWhile, maybe, sequence, until
    , followedBy, notFollowedBy
    )

{-| A simple, easy to use, general-purpose parser with good error messages out of the box.

Heavily inspired by

  - [`elm/parser`](https://package.elm-lang.org/packages/elm/parser/latest/)
  - [`dasch/parser`](https://package.elm-lang.org/packages/dasch/parser/latest/)
  - and especially [`lambda-phi/parser`](https://dark.elm.dmy.fr/packages/lambda-phi/parser/latest/)

but simplified where possible

Define the data type you want to parse into, then build a parser for that data type.


## example: 2D point

    import Parser exposing (Parser, atLeast, drop, into, parse, succeed, take, atom)
    import Char.Parser as Char
    import Text.Parser exposing (number)
    import Parser.Error

    type alias Point =
        { x : Float
        , y : Float
        }

    -- a successful parse looks like
    parse "(2.71, 3.14)" point --> Ok { x = 2.71, y = 3.14 }

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

    -- And we can get a nice error message if it fails.
    point
        |> parse "(2.71, x)"
        |> Result.mapError (Parser.Error.dump "filename.txt")
    --> Err
    -->     [ "[ERROR] filename.txt:1:8: I was expecting a digit [0-9]. I got stuck when I got the character 'x'."
    -->     , "  in Point at line 1:1"
    -->     , ""
    -->     , "1|(2.71, x)"
    -->     , "  ~~~~~~~^"
    -->     ]


## Usage details documentation

  - [`Parser`](Parser): creating and chaining parsers

  - [`Char.Parser`](Char-Parser): parsing characters

  - [`Text.Parser`](Text-Parser): parsing text

  - [`Parser.Expression`](Parser-Expression):
    expressions with operator precedence

  - [`Parser.Error`](Parser-Error):
    error reporting and formatting

@docs Parser, StateRunning, Error, Expected


## basic

@docs narrowWith, parse
@docs into


## create

@docs succeed, expecting, atomAny, atom
@docs beginning, end


## recursion

@docs lazy


## chain

@docs take, drop
@docs expected, oneOf, andThen, except, map


## sequences

@docs atLeast, between, exactly, foldWhile, maybe, sequence, until


### lookahead

@docs followedBy, notFollowedBy

-}

import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)



-- types


{-| A function that takes an [input state](#StateRunning)
and returns either an [`Error`](#Error),
or a value with the next state.

    {-| [`Parser`](#Parser) on input characters
    -}
    type alias TextParser narrow =
        Parser Char narrow

-}
type alias Parser atom narrow =
    RecordWithoutConstructorFunction
        { parse :
            StateRunning atom
            ->
                Result
                    (Error atom)
                    { narrow : narrow, state : StateRunning atom }
        }


{-| Contains the description of an error.
This includes an error message, the position, and the context stack.
-}
type alias Error atom =
    RecordWithoutConstructorFunction
        { expected : Expected atom
        , stuckOn : Maybe atom
        , -- sometimes called "offset"
          location : Int
        , context : List Context
        }


{-| What went wrong.
-}
type Expected atom
    = ExpectedSpecifically atom
    | ExpectedEnd
    | ExpectedCustom String


{-| Parsing state storing a `List` of remaining atomic inputs
-}
type alias StateRunning atom =
    RecordWithoutConstructorFunction
        { remaining : List atom
        , lastInput : Maybe atom
        , -- sometimes called "offset"
          location : Int
        , context : List Context
        }


{-| Details: What are we trying to parse here?
-}
type alias Context =
    RecordWithoutConstructorFunction
        { name : String
        , location : Int
        }



-- basic


{-| Parse an input text, and get either an [`Error`](#Error)
or the parsed value as a result.

    import Char.Parser as Char
    import Text.Parser exposing (number)
    import Parser.Error

    -- Consumes a single letter, then "bc" are still remaining.
    parse "abc" Char.letter --> Ok 'a'

    -- We can also parse text into other data types like numbers.
    parse "3.14" number --> Ok 3.14

    -- We get an error message if the parser doesn't match.
    Char.letter
        |> parse "123"
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:1: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '1'."

[`narrowWith`](#narrowWith) is the more general version.

-}
parse :
    List atom
    -> Parser atom narrow
    -> Result (Error atom) narrow
parse input =
    \parser ->
        input |> narrowWith parser


{-| Parse an input, and get either an [`Error`](#Error)
or a narrow value as a `Result`.

[`parse`](#parse) is a version that specifically parses `String`s.

-}
narrowWith :
    Parser atom narrow
    -> List atom
    -> Result (Error atom) narrow
narrowWith parser =
    \input ->
        parser.parse
            { remaining = input
            , lastInput = Nothing
            , location = 1
            , context = []
            }
            |> Result.map .narrow


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

    parse "(12,34)" point --> Ok { x = 12, y = 34 }

    -- We can get the error context stack as well as where they started matching.
    parse "(a,b)" point
        |> Result.mapError (\error -> error.context |> List.map .name)
    --> Err [ "Point" ]

-}
into :
    String
    -> Parser atom narrow
    -> Parser atom narrow
into context parser =
    { parse =
        \beforeState ->
            parser.parse
                { beforeState
                    | context =
                        beforeState.context
                            |> (::)
                                { name = context
                                , location = beforeState.location
                                }
                }
                |> Result.andThen
                    (\after ->
                        let
                            afterState =
                                after.state
                        in
                        (succeed after.narrow).parse
                            { afterState | context = beforeState.context }
                    )
    }


{-| Takes a parsed value and feeds it to the return value of the parser.
-}
take :
    Parser atom next
    -> Parser atom (next -> applied)
    -> Parser atom applied
take next =
    andThen (\f -> next |> map f)


{-| Ignores a parsed value, but it still must match to continue.

    import Text.Parser exposing (text)
    import Parser exposing (atom, succeed, atLeast, take, drop)

    -- parse a simple email, but we're only interested in the username
    succeed (\userName -> userName)
        |> take (atLeast 1 letter)
        |> drop (atom '@')
        |> drop (atLeast 1 letter)
        |> drop (text ".com")
        |> parse "user@example.com"
    --> Ok "user"

-}
drop :
    Parser atom next
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
        oneOf [ lazyListEnd, lazyListNext ]

    lazyListEnd : Parser LazyList
    lazyListEnd =
        succeed End
            |> drop (text "[]")

    lazyListNext : Parser LazyList
    lazyListNext =
        succeed Next
            |> take singleAny
            |> drop (token (text "::"))
            |> take (lazy (\() -> lazyList))

    parse "[]" lazyList --> Ok End
    parse "a :: []" lazyList --> Ok (Next 'a' End)
    parse "a :: b :: []" lazyList --> Ok (Next 'a' (Next 'b' End))

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
    { parse = \state -> (makeParser ()).parse state
    }


{-| Matches any character `except` the parser provided.

> ℹ️ It's a good idea to use [`Parser.expecting`](Parser#expecting) alongside this function
> to improve the error messages.

    import Parser exposing (parse)
    import Parser.Error

    -- Anything except a letter is okay.
    parse "123" (except letter) --> Ok '1'
    parse "-123" (except letter) --> Ok '-'

    -- But a letter is not.
    except letter
        |> parse "abc"
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:1: I was expecting a different character. I got stuck when I got the character 'a'."

-}
except :
    Parser atom atom
    -> Parser atom atom
except parser =
    { parse =
        \state ->
            case parser.parse state of
                Ok exception ->
                    (expected "a different character").parse exception.state

                Err _ ->
                    atomAny.parse state
    }


{-| Matches any single character.

> ℹ️ Equivalent regular expression: `.`

    import Parser exposing (parse, atomAny)
    import Parser.Error

    -- can match any character
    parse "abc" singleAny --> Ok 'a'
    parse "#hashtag" singleAny --> Ok '#'

    -- only fails if we run out of inputs
    singleAny
        |> parse ""
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:0: I was expecting a character. I reached the end of the input."

-}
atomAny : Parser atom atom
atomAny =
    { parse =
        \state ->
            case state.remaining of
                [] ->
                    (expected "at least some input").parse state

                next :: tail ->
                    (succeed next).parse
                        { state
                            | remaining = tail
                            , lastInput = next |> Just
                            , location = state.location + 1
                        }
    }


{-| Matches a specific single input.

    import Parser exposing (parse, atom)
    import Parser.Error

    -- Match a specific character, case sensitive
    parse "abc" (atom 'a') --> Ok 'a'

    -- It fails if it's not _exactly_ the same
    atom 'a'
        |> parse "A"
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
            (ExpectedSpecifically expectedSingle)



-- chain


{-| A parser that always succeeds with the given value.

    import Parser exposing (succeed, atom)
    import Text.Parser exposing (int)

    -- Always succeed with "abc" no matter the input text
    parse "" (succeed "abc") --> Ok "abc"

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

    parse "12,34" pair --> Ok { x = 12, y = 34 }

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
succeed : narrow -> Parser atom narrow
succeed narrow =
    { parse =
        \state -> { narrow = narrow, state = state } |> Ok
    }


{-| Always fail with a given error message.

    import Parser.Error
    import Text.Parser

    ""
        |> Text.Parser.narrowWith
            (expected "nothing, this always fails")
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:0: I was expecting nothing, this always fails. I reached the end of the input."

-}
expected : String -> Parser atom narrow
expected description =
    expectedInternal (ExpectedCustom description)


{-| **should not be exposed**
-}
expectedInternal : Expected atom -> Parser atom narrow
expectedInternal expectation =
    { parse =
        \state ->
            Err
                { expected = expectation
                , stuckOn = state.lastInput
                , location = state.location - 1
                , context = state.context
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

    -- Try the first parser
    oneOf [ atom '_', Char.letter ]
        |> parse "_abc"
    --> Ok '_'

    -- If it doesn't work, try the next
    oneOf [ atom '_', Char.letter ]
        |> parse "abc"
    --> Ok 'a'

    -- If none of them work, we get the error from the last parser
    oneOf [ atom '_', Char.letter ]
        |> parse "123"
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:1: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '1'."


### example: fallback parser if the previous parser fails

    import Char.Parser as Char
    import Parser.Error

    -- Try letters, or else give me some digits
    oneOf
        [ atLeast 1 Char.letter
        , atLeast 1 Char.digit
        ]
        |> map String.fromList
        |> parse "abc"
    --> Ok "abc"

    -- We didn't get letters, but we still got digits
    oneOf
        [ atLeast 1 Char.letter
        , atLeast 1 Char.digit
        ]
        |> map String.fromList
        |> parse "123"
    --> Ok "123"

    -- But if we still fail, give the error message of the fallback parser
    oneOf
        [ atLeast 1 Char.letter
        , atLeast 1 Char.digit
        ]
        |> parse "_"
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:1: I was expecting at least 1 digit [0-9]. I got stuck when I got the character '_'."

-}
oneOf :
    List (Parser atom narrow)
    -> Parser atom narrow
oneOf optionParsers =
    optionParsers
        |> List.foldl
            (\soFar parser ->
                { parse =
                    \state ->
                        case parser.parse state of
                            Ok result ->
                                result |> Ok

                            Err _ ->
                                soFar.parse state
                }
            )
            (expected "a parser to match")


{-| Parse one value `andThen` do something with that value,
which results in another parser.

This can be used to validate the last thing we parsed,
or transform the last value,
or to use the last value for the next parser like a backreference.

    import Parser exposing (atLeast, atomAny)
    import Parser.Error

    -- Get some characters `andThen` interpret them as an `Int`.
    (atLeast 1 atomAny |> map String.fromList)
        |> andThen
            (\chars ->
                case String.toInt chars of
                    Just n ->
                        n |> succeed
                    Nothing ->
                        expected "an integer, better use Text.Parser.int"
            )
        |> parse "123"
    --> Ok 123

-}
andThen :
    (narrow -> Parser atom result)
    -> Parser atom narrow
    -> Parser atom result
andThen parserFromNarrow parser =
    { parse =
        \state ->
            parser.parse state
                |> Result.andThen
                    (\next ->
                        (parserFromNarrow next.narrow).parse next.state
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

    -- We can redefine an error message if something goes wrong.
    atLeast 1 Char.letter
        |> expecting "a name consisting of letters"
        |> parse "123"
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:1: I was expecting a name consisting of letters. I got stuck when I got the character '1'."

-}
expecting : String -> Parser atom narrow -> Parser atom narrow
expecting description =
    expectingInternal (ExpectedCustom description)


{-| **should not be exposed**
-}
expectingInternal :
    Expected atom
    -> Parser atom narrow
    -> Parser atom narrow
expectingInternal expectation =
    \parser ->
        { parse =
            \state ->
                parser.parse state
                    |> Result.mapError
                        (\error ->
                            { error
                                | expected = expectation
                            }
                        )
        }



-- transform


{-| Transform the narrow result value.

    import Parser exposing (map, parse, atLeast)
    import Char.Parser as Char

    -- Get some letters, and make them lowercase.
    atLeast 1 Char.letter
        |> map String.fromList
        |> map String.toLower
        |> parse "ABC"
    --> Ok "abc"

-}
map :
    (narrow -> mappedNarrow)
    -> Parser atom narrow
    -> Parser atom mappedNarrow
map narrowMap =
    andThen (\narrow -> narrow |> narrowMap |> succeed)



-- check


{-| Succeeds only if it's the beginning of the input text.
This does not consume any inputs.

    import Parser exposing (parse)
    import Char.Parser as Char
    import Parser.Error

    -- Succeed if we are at the beginning of file
    beginning
        |> andThen (\_ -> atLeast 1 Char.letter |> map String.fromList)
        |> parse "abc"
    --> Ok "abc"

    -- fail otherwise
    atLeast 1 Char.letter
        |> followedBy beginning
        |> parse "abc123"
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:3: I was expecting the beginning of the input text. I got stuck when I got the character 'c'."

TODO: check if should be removed!

-}
beginning : Parser atom ()
beginning =
    { parse =
        \state ->
            case state.lastInput of
                Nothing ->
                    (succeed ()).parse state

                Just _ ->
                    (expected "the beginning of the input text").parse state
    }


{-| Succeeds only if there is no more remaining input.
Does not consume any inputs.

    import Parser exposing (parse, map, end, atLeast)
    import Char.Parser as Char
    import Parser.Error

    atLeast 1 Char.letter
        |> map String.fromList
        |> followedBy end
        |> parse "abc"
    --> Ok "abc"

    atLeast 1 Char.letter
        |> followedBy end
        |> parse "abc123"
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:4: I was expecting the end of the input, but 3 characters are still remaining. I got stuck when I got the character '1'."

-}
end : Parser atom ()
end =
    { parse =
        \state ->
            case atomAny.parse state of
                Err _ ->
                    (succeed ()).parse state

                Ok next ->
                    (expectedInternal ExpectedEnd).parse
                        next.state
    }



-- sequence


{-| Matches a sequence of parsers in order, and gets the result as a `List`.

    import Parser exposing (parse, map, atom)
    import Char.Parser as Char

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
            -> { narrow : List narrow, state : StateRunning atom }
            -> Result (Error atom) { narrow : List narrow, state : StateRunning atom }
        step stepParser =
            \soFar ->
                stepParser.parse soFar.state
                    |> Result.map
                        (\stepParsed ->
                            { narrow =
                                soFar.narrow |> (::) stepParsed.narrow
                            , state = stepParsed.state
                            }
                        )

        parseSequence :
            StateRunning atom
            -> Result (Error atom) { narrow : List narrow, state : StateRunning atom }
        parseSequence =
            \initialState ->
                parsers
                    |> List.foldr
                        (\stepParser ->
                            Result.andThen (step stepParser)
                        )
                        ({ narrow = [], state = initialState } |> Ok)

        sequenced : Parser atom (List narrow)
        sequenced =
            { parse = parseSequence
            }
    in
    sequenced


{-| Matches an optional value and returns it as a `Maybe`.

> ℹ️ Equivalent regular expression: `?`

    import Parser exposing (parse)
    import Char.Parser exposing (letter)

    -- Maybe we get `Just` a letter
    maybe letter |> parse "abc" --> Ok (Just 'a')

    -- Or maybe we get `Nothing`
    maybe letter |> parse "123abc" --> Ok Nothing

-}
maybe :
    Parser atom narrow
    -> Parser atom (Maybe narrow)
maybe parser =
    oneOf
        [ map Just parser
        , succeed Nothing
        ]


{-| Matches a value `exactly` a number of times and returns them as a `List`.

> ℹ️ Equivalent regular expression: `{n}`

    import Parser exposing (parse)
    import Char.Parser as Char
    import Parser.Error

    -- We want `exactly` three letters.
    parse "abcdef" (exactly 3 Char.letter) --> Ok [ 'a', 'b', 'c' ]

    -- Not two or four, we want three.
    exactly 3 Char.letter
        |> parse "ab_def"
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

    import Parser exposing (parse)
    import Char.Parser as Char
    import Parser.Error

    -- We want at least three letters, we are okay with more than three.
    parse "abcdef" (atLeast 3 Char.letter) --> Ok [ 'a', 'b', 'c', 'd', 'e', 'f' ]

    -- But not two, that's sacrilegious.
    atLeast 3 Char.letter
        |> parse "ab_def"
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:3: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '_'."


## `arLeast 0`

> ℹ️ Equivalent regular expression: `*`

    import Parser exposing (parse)
    import Char.Parser as Char

    -- We want as many letters as there are.
    parse "abc" (atLeast 0 Char.letter) --> Ok [ 'a', 'b', 'c' ]
    parse "abc123" (atLeast 0 Char.letter) --> Ok [ 'a', 'b', 'c' ]

    -- Even zero letters is okay.
    parse "123abc" (atLeast 0 Char.letter) --> Ok []


### `atLeast 1`

> ℹ️ Equivalent regular expression: `+`

    import Parser exposing (parse)
    import Char.Parser as Char
    import Parser.Error

    -- We want as many letters as there are.
    parse "abc" (atLeast 1 Char.letter)
    --> Ok [ 'a', 'b', 'c' ]

    parse "abc123" (atLeast 1 Char.letter)
    --> Ok [ 'a', 'b', 'c' ]

    -- But we want at least one.
    atLeast 1 Char.letter
        |> parse "123abc"
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

    import Parser exposing (parse)
    import Char.Parser as Char
    import Parser.Error

    -- We want between two and four letters.
    parse "abcdef" (between 2 4 Char.letter)
    --> Ok [ 'a', 'b', 'c', 'd' ]

    parse "abc_ef" (between 2 4 Char.letter)
    --> Ok [ 'a', 'b', 'c' ]

    parse "ab_def" (between 2 4 Char.letter)
    --> Ok [ 'a', 'b' ]


    -- But less than that is not cool.
    between 2 3 letter
        |> parse "a_cdef"
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:2: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '_'."


### example: `between 0 1`

ALternative to [`maybe`](#maybe) which instead returns a `List`.

> ℹ️ Equivalent regular expression: `?`

    import Parser exposing (parse)
    import Char.Parser exposing (letter)

    -- We want one letter, optionally
    between 0 1 letter |> parse "abc" --> Ok [ 'a' ]

    -- If we don't get any, that's still okay
    between 0 1 letter |> parse "123abc" --> Ok []


### example: at most

> ℹ️ Equivalent regular expression: `{0,max}`

    import Parser exposing (parse, atom)
    import Char.Parser exposing (letter)

    -- We want a maximum of three letters
    parse "abcdef" (between 0 3 letter) --> Ok [ 'a', 'b', 'c' ]

    -- Less than that is also okay
    parse "ab_def" (between 0 3 letter) --> Ok [ 'a', 'b' ]

    -- Even zero letters are fine
    parse "_bcdef" (between 0 3 letter) --> Ok []

    -- Make sure we don't consume more than three letters.
    succeed (\letters -> letters)
        |> take (between 0 3 letter)
        |> drop (atom 'd')
        |> parse "abcdef"
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
    foldWhile
        (\xs x ->
            if List.length xs < maximum then
                Just (xs ++ [ x ])

            else
                Nothing
        )
        []


{-| Matches a values repeatedly until a delimiter parser matches.
The delimiter marks the end of the sequence, and it is _not_ consumed.

    import Parser exposing (drop, map, parse, succeed, take, atom, end)
    import Char.Parser exposing (letter)
    import Parser.Error

    until (atom 'd') letter
        |> parse "abcdef"
    --> Ok { before = [ 'a', 'b', 'c' ], delimiter = 'd' }

    -- the delimiter _must_ be present
    until (atom 'd') letter
        |> parse "abc123"
        |> Result.toMaybe
    --> Nothing

    -- The delimiter is consumed
    succeed (\tag -> tag)
        |> drop (atom '<')
        |> take
            (until (atom '>') letter
                |> map .before
                |> map String.fromList
            )
        |> drop end
        |> parse "<abc>"
    --> Ok "abc"

-}
until :
    Parser atom delimiter
    -> Parser atom before
    -> Parser atom { before : List before, delimiter : delimiter }
until delimiterParser beforeParser =
    succeed ()
        |> notFollowedBy delimiterParser
        |> andThen (\_ -> beforeParser)
        |> atLeast 0
        |> andThen
            (\before ->
                map (\delimiterNarrow -> { before = before, delimiter = delimiterNarrow })
                    delimiterParser
            )


{-| Reduces _matches_ of a parser while a test passes.

    import Parser exposing (Parser, parse, atom, succeed, atLeast, take, drop, foldWhile)
    import Char.Parser as Char
    import Text.Parser exposing (number)

    sumWhileLessThan : Float -> Parser Char Float
    sumWhileLessThan max =
        foldWhile
            (\total n ->
                if total + n <= max then
                    (total + n) |> Just
                else
                    Nothing
            )
            0
            (succeed (\n -> n)
                |> take number
                |> drop (atLeast 0 Char.blank)
            )

    -- The fold stops before we reach a maximum of 6 in the sum
    parse "2 3 4" (sumWhileLessThan 6) --> Ok 5

    -- Make sure we didn't consume too many numbers
    succeed (\sum -> sum)
        |> take (sumWhileLessThan 6)
        |> drop (atom '4')
        |> parse "2 3 4"
    --> Ok 5

-}
foldWhile :
    (result -> narrow -> Maybe result)
    -> result
    -> Parser atom narrow
    -> Parser atom result
foldWhile reduce initial parser =
    oneOf
        [ parser
            |> andThen
                (\x ->
                    case reduce initial x of
                        Just next ->
                            foldWhile reduce next parser

                        Nothing ->
                            expected ""
                )
        , succeed initial
        ]



-- lookahead


{-| Succeeds only if the input text is followed by a _lookahead_ parser.
This does not consume any inputs.

If you want to consume the inputs or use the matched value in any way,
consider using [`Parser.andThen`](Parser#andThen).

> ℹ️ Equivalent regular expression: `(?=...)` _(positive lookahead)_

    import Parser exposing (parse, succeed)
    import Char.Parser exposing (digit, letter)
    import Parser.Error

    -- Succeed only if it's `followedBy` a digit
    succeed ":)"
        |> followedBy digit
        |> parse "123"
    --> Ok ":)"

    -- Match letters only if it's `followedBy` a digit
    (atLeast 1 letter |> map String.fromList)
        |> followedBy digit
        |> parse "abc123"
    --> Ok "abc"

    -- Even if we match the letters, fail if the next character is not a digit
    (atLeast 1 letter |> map String.fromList)
        |> followedBy digit
        |> parse "abc@def"
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:4: I was expecting a digit [0-9]. I got stuck when I got the character '@'."

-}
followedBy :
    Parser atom lookahead_
    -> Parser atom narrow
    -> Parser atom narrow
followedBy lookahead =
    \parser ->
        { parse =
            \state ->
                parser.parse state
                    |> Result.andThen
                        (\parsed ->
                            lookahead.parse parsed.state
                                |> Result.map (\_ -> parsed)
                        )
        }


{-| Succeeds only if the input text is _not_ followed by a _lookahead_ parser.
This does not consume any inputs.

> ℹ️ It's a good idea to use [`Parser.expecting`](Parser#expecting) alongside this function
> to improve the error messages.

> ℹ️ Equivalent regular expression: `(?!...)` _(negative lookahead)_

    import Parser exposing (expecting, parse, succeed)
    import Char.Parser exposing (digit, letter)
    import Parser.Error

    -- Succeed only if it's `notFollowedBy` a digit.
    succeed ":)"
        |> notFollowedBy digit
        |> parse "abc"
    --> Ok ":)"

    -- Match letters only if it's `notFollowedBy` a digit.
    (atLeast 1 letter |> map String.fromList)
        |> notFollowedBy digit
        |> expecting "letters not followed by a number"
        |> parse "abc@def"
    --> Ok "abc"

    -- Even if we match the letters, fail if the next character is a digit.
    -- This is the default error message, but you can use `expecting` to improve it.
    (atLeast 1 letter |> map String.fromList)
        |> notFollowedBy digit
        |> parse "abc123"
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:4: I was expecting to not match a pattern, but I did. I got stuck when I got the character '1'."

-}
notFollowedBy :
    Parser atom lookahead_
    -> Parser atom narrow
    -> Parser atom narrow
notFollowedBy lookahead =
    \parser ->
        { parse =
            \state ->
                parser.parse state
                    |> Result.andThen
                        (\parsed ->
                            case parsed.state |> lookahead.parse of
                                Ok lookaheadParsed ->
                                    (expected "to not match a pattern, but I did").parse
                                        lookaheadParsed.state

                                Err _ ->
                                    parsed |> Ok
                        )
        }



-- zombie
{- Succeeds only if the last character matches the parser provided.
   This does not consume any inputs.

       import Parser exposing (parse, atom, atomAny, atLeast, precededBy)
       import Char.Parser as Char exposing (letter)
       import Parser.Error

       -- Make sure some letters were preceded by a '_'
       singleAny
           |> andThen
               (\_ ->
                   precededBy (atom '_') (atLeast 1 letter)
                       |> map String.fromList
               )
           |> parse "_abc"
       --> Ok "abc"

       -- If it was something different, it fails
       singleAny
           |> andThen (\_ -> precededBy (atom '_') (atLeast 1 letter))
           |> parse "@abc"
           |> Result.mapError Parser.Error.textMessage
       --> Err "1:1: I was expecting the character '_'. I got stuck when I got the character '@'."


   ### example: `precededBy (except ...)`

       import Parser exposing (parse, atom, atLeast, atomAny, except, andThen)
       import Char.Parser as Char exposing (letter)
       import Parser.Error

       -- Make sure some letters were not preceded by a '_'.
       singleAny
           |> andThen
               (\_ ->
                   precededBy (except (atom '_')) (atLeast 1 letter)
                       |> map String.fromList
               )
           |> parse "@abc"
       --> Ok "abc"

       -- If it was preceded by '_', it fails.
       singleAny
           |> andThen (\_ -> precededBy (except (atom '_')) (atLeast 1 letter))
           |> parse "_abc"
           |> Result.mapError Parser.Error.textMessage
       --> Err "1:1: I was expecting a different character. I got stuck when I got the character '_'."



       precededBy :
           Parser atom atom
           -> Parser atom narrow
           -> Parser atom narrow
       precededBy preceding parser =
           { parse =
               \state ->
                   parser.parse state
                       |> Result.andThen
                           (\parsed ->
                               preceding.parse
                                   { state
                                       | remaining =
                                           state.lastInput
                                               |> Maybe.map List.singleton
                                               |> Maybe.withDefault []
                                       , location = state.location - 1
                                   }
                                   |> Result.map (\_ -> parsed)
                           )
           }

-}
