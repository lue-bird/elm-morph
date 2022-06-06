module Text.Parser exposing
    ( narrowWith
    , text, caseAny
    , int, number
    , line, lineEnd, lineBeginning
    )

{-| Parsing text

@docs narrowWith


## match

@docs text, caseAny


## numbers

@docs int, number


## lines

@docs line, lineEnd, lineBeginning

Feeling motivated? implement & PR

  - date, time, datetime
  - email
  - unixPath, windowsPath
  - uri
  - IPv4, IPv6
  - int2 (bin), int8 (oct), int6 (hex)

-}

import Char.Parser as Char exposing (digit)
import Parser exposing (Parser, andThen, atLeast, atom, atomAny, between, exactly, expected, expecting, map, oneOf, sequence, succeed, take, until)


{-| Parse an input text, and get either an [error](Parser#Expected)
or the parsed narrow value as a `Result`.

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
narrowWith :
    Parser Char narrow
    -> String
    -> Result (Parser.Error Char) narrow
narrowWith parser =
    \input ->
        input |> String.toList |> Parser.narrowWith parser


{-| Matches a specific text string.
This is case sensitive.

    import Parser exposing (parse)

    -- Match an exact text, case sensitive.
    parse "abcdef" (text "abc") --> Ok "abc"

    -- But anything else makes it fail.
    import Parser.Error

    text "abc"
        |> parse "abCDEF"
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:3: I was expecting the text 'abc'. I got stuck when I got the character 'C'."

-}
text : String -> Parser Char String
text expectedText =
    sequence (List.map atom (expectedText |> String.toList))
        |> map String.fromList
        |> expecting
            ([ "the text \"", expectedText, "\"" ]
                |> String.concat
            )


{-| Matches a specific text string.
This is case insensitive.

    import Parser exposing (parse)
    import Parser.Error

    parse "aBcdef" (Text.caseAny "abC") --> Ok "aBc"

    Text.caseAny "abc"
        |> parse "ab@"
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:3: I was expecting the text \"abc\" (case insensitive). I got stuck when I got the character '@'."

-}
caseAny : String -> Parser Char String
caseAny expectedText =
    sequence (List.map Char.caseAny (expectedText |> String.toList))
        |> map String.fromList
        |> expecting
            ([ "the text \"", expectedText, "\" (case insensitive)" ]
                |> String.concat
            )


{-| Matches a line from the input text, delimited by `'\\n'`.

    import Parser exposing (parse, atLeast)
    import Parser.Error

    -- A line could be delimited by the newline character '\n'.
    parse "abc\ndef" line --> Ok "abc"

    -- Or this could also be the last line.
    parse "abc" line --> Ok "abc"

    -- An empty line still counts.
    parse "\n" line --> Ok ""

    -- But not an empty file.
    line
        |> parse ""
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:0: I was expecting a line. I reached the end of the input."

    -- So we can parse multiple lines.
    atLeast 0 line
        |> parse "abc\ndef\nghi"
    --> Ok [ "abc", "def", "ghi"]

-}
line : Parser Char String
line =
    oneOf
        [ until lineEnd atomAny
            |> map .before
            |> map String.fromList
        , map (\_ -> "") (expected "a line")
        ]


{-| Matches an integer value as an `Int`.

    import Parser exposing (parse)
    import Parser.Error

    -- You can parse integers as `Int` instead of `String`.
    parse "123" int --> Ok 123

    -- It also works with negative numbers.
    parse "-123" int --> Ok -123

    -- A decimal number is _not_ an integer :)
    parse "3.14" int
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:2: I was expecting an integer value. I got stuck when I got the character '.'."

    -- But not with invalid numbers.
    parse "abc" int
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:1: I was expecting an integer value. I got stuck when I got the character 'a'."

-}
int : Parser Char Int
int =
    sequence
        [ between 0 1 (atom '-')
        , atLeast 1 digit
        ]
        |> map List.concat
        |> map String.fromList
        |> andThen
            (\intString ->
                case intString |> String.toInt of
                    Just intNarrow ->
                        intNarrow |> succeed

                    Nothing ->
                        expected ("an integer" ++ intString)
            )
        |> expecting "an integer value"


{-| Matches a decimal value as a `Float`.

    import Parser exposing (parse)
    import Parser.Error

    number |> parse "12" --> Ok 12.0
    number |> parse "12.34" --> Ok 12.34
    number |> parse "12." --> Ok 12.0
    number |> parse ".12" --> Ok 0.12
    number |> parse "-12.34" --> Ok -12.34
    number |> parse "-.12" --> Ok -0.12

    parse "." number
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:1: I was expecting a digit [0-9]. I got stuck when I got the character '.'."

    parse "abc" number
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:1: I was expecting a digit [0-9]. I got stuck when I got the character 'a'."

-}
number : Parser Char Float
number =
    sequence
        [ between 0 1 (oneOf [ atom '-', atom '+' ])
        , oneOf
            [ sequence
                [ exactly 1 (atom '.')
                , atLeast 1 digit
                ]
                |> map List.concat
            , sequence
                [ atLeast 1 digit
                , between 0 1 (atom '.')
                , atLeast 0 digit
                ]
                |> map List.concat
            ]
        ]
        |> map List.concat
        |> map String.fromList
        |> andThen
            (\floatString ->
                case floatString |> String.toFloat of
                    Just float ->
                        succeed float

                    -- not expected
                    Nothing ->
                        expected ("Failed to parse number from \"" ++ floatString ++ "\"")
            )


{-| Succeeds only the parser is at the end of the current line or there are
no more remaining characters in the input text.
This does not consume any inputs.

> ℹ️ Equivalent regular expression: `$`

    import Parser exposing (parse, map, followedBy, atLeast)
    import Char.Parser as Char
    import Text.Parser exposing (line)
    import Parser.Error

    atLeast 1 Char.letter
        |> map String.fromList
        |> followedBy lineEnd
        |> parse "abc\n123"
    --> Ok "abc"

    -- carriage return also counts
    atLeast 1 Char.letter
        |> map String.fromList
        |> followedBy lineEnd
        |> parse "abc\r123"
    --> Ok "abc"

    -- end of file also counts
    atLeast 1 Char.letter
        |> map String.fromList
        |> followedBy lineEnd
        |> parse "abc"
    --> Ok "abc"

    -- fail otherwise
    atLeast 1 Char.letter
        |> map String.fromList
        |> followedBy lineEnd
        |> parse "abc123"
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:4: I was expecting the end of the current line. I got stuck when I got the character '1'."

-}
lineEnd : Parser Char ()
lineEnd =
    { parse =
        \state ->
            case atomAny.parse state of
                Err _ ->
                    (succeed ()).parse state

                Ok parsed ->
                    case parsed.narrow of
                        '\n' ->
                            (succeed ()).parse parsed.state

                        -- Carriage return '\r'
                        '\u{000D}' ->
                            (succeed ()).parse parsed.state

                        _ ->
                            (expected "the end of the current line").parse parsed.state
    }


{-| Succeeds only the parser is at the beginning of a new line or
at the beginning of the input text.
This does not consume any inputs.

> ℹ️ Equivalent regular expression: `^`

    import Parser exposing (parse, andThen, followedBy, atomAny)
    import Char.Parser as Char
    import Text.Parser exposing (line)
    import Parser.Error

    -- Succeed at the beginning of the file.
    lineBeginning
        |> andThen (\_ -> line)
        |> parse "abc\n123"
    --> Ok "abc"

    -- The end of file also counts.
    line
        |> followedBy lineBeginning
        |> andThen (\_ -> line)
        |> parse "abc\n123"
    --> Ok "123"

    -- But fail otherwise
    singleAny
        |> followedBy lineBeginning
        |> parse "abc"
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:2: I was expecting the beginning of a line. I got stuck when I got the character 'b'."

TODO: not make lookbehind

-}
lineBeginning : Parser Char ()
lineBeginning =
    { parse =
        \state ->
            case state.lastInput of
                Nothing ->
                    (succeed ()).parse state

                Just '\n' ->
                    (succeed ()).parse state

                -- Carriage return '\r'
                Just '\u{000D}' ->
                    (succeed ()).parse state

                Just _ ->
                    (atomAny
                        |> map (\_ -> ())
                        |> (\_ -> expected "the beginning of a line")
                    ).parse
                        state
    }
