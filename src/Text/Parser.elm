module Text.Parser exposing
    ( narrowWith
    , text, caseAny
    , int, number
    , line, lineEnd
    )

{-| Parsing text

@docs narrowWith


## match

@docs text, caseAny


## numbers

@docs int, number


## lines

@docs line, lineEnd

Feeling motivated? implement & PR

  - date, time, datetime
  - email
  - unixPath, windowsPath
  - uri
  - IPv4, IPv6
  - int2 (bin), int8 (oct), int6 (hex)

-}

import Char.Parser as Char exposing (digit)
import Parser exposing (Parser, andThen, atLeast, atom, atomAny, between, exactly, expected, expecting, map, onFailDown, sequence, succeed, take, until)


{-| Parse an input text, and get either an [error](Parser#Expected)
or the parsed narrow value as a `Result`.

    import Char.Parser as Char
    import Text.Parser as Text
    import Parser.Error

    -- consumes a single letter, then "bc" are still remaining
    "abc" |> Text.narrowWith Char.letter --> Ok 'a'

    -- we can also parse text into other data types like numbers
    "3.14" |> Text.narrowWith number --> Ok 3.14

    -- we get an error message if the parser doesn't match
    "123"
        |> Text.narrowWith Char.letter
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:1: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '1'."

[`Parser.narrowWith`](Parser#narrowWith) accepts any input `List`.

-}
narrowWith :
    Parser Char narrow
    -> String
    -> Result (Parser.Error Char narrow) narrow
narrowWith parser =
    \input ->
        input |> String.toList |> Parser.narrowWith parser


{-| Matches a specific text string.
This is case sensitive.

    import Parser.Error

    -- match an exact text, case sensitive
    "abcdef" |> Text.narrowWith (text "abc") --> Ok "abc"

    -- but anything else makes it fail
    "abCDEF"
        |> Text.narrowWith (text "abc")
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:3: I was expecting the text 'abc'. I got stuck when I got the character 'C'."

TODO: rename to `caseSensitive`/`specific`/?

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

    import Parser.Error

    "aBcdef" |> Text.narrowWith (Text.caseAny "abC") --> Ok "aBc"

    parse "ab@"
        |> Text.narrowWith (Text.caseAny "abc")
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


{-| Consumes the [end of the current line](#lineEnd) or succeeds if there are
no more remaining characters in the input text.

> ℹ️ Equivalent regular expression: `$`

    import Parser exposing (map, followedBy, atLeast)
    import Char.Parser as Char
    import Text.Parser exposing (line)
    import Parser.Error

    "abc\n123"
        |> Text.narrowWith
            (succeed (\letters -> letters)
                |> take
                    (atLeast 1 Char.letter
                        |> map String.fromList
                    )
                |> drop lineEnd
            )
    --> Ok "abc"

    -- carriage return also counts
    "abc\r123"
        |> Text.narrowWith
            (succeed (\letters -> letters)
                |> take
                    (atLeast 1 Char.letter
                        |> map String.fromList
                    )
                |> drop lineEnd
            )
    --> Ok "abc"

    -- end of file also counts
    "abc"
        |> Text.narrowWith
            (succeed (\letters -> letters)
                |> take
                    (atLeast 1 Char.letter
                        |> map String.fromList
                    )
                |> drop lineEnd
            )
    --> Ok "abc"

    -- fail otherwise
    "abc123"
        |> Text.narrowWith
            (succeed (\letters -> letters)
                |> take
                    (atLeast 1 Char.letter
                        |> map String.fromList
                    )
                |> drop lineEnd
            )
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:4: I was expecting the end of the current line. I got stuck when I got the character '1'."

-}
lineEnd : Parser Char (Maybe Char)
lineEnd =
    onFailDown
        [ atom '\n' |> map Just
        , -- Carriage return '\r'
          atom '\u{000D}' |> map Just
        , -- end
          { narrow =
                \input ->
                    case input of
                        [] ->
                            (succeed Nothing).narrow input

                        _ :: _ ->
                            (expected "the end of the line").narrow input
          }
        ]


{-| Matches a line from the input text, delimited by `'\\n'`.

    import Parser exposing (atLeast)
    import Parser.Error

    -- A line could be delimited by the newline character '\n'.
    "abc\ndef" |> Text.narrowWith line --> Ok "abc"

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
line : Parser Char String -> Parser Char String
line onLineParser =
    onFailDown
        [ until lineEnd atomAny
            |> map .before
        , map (\_ -> []) (expected "a line")
        ]
        |> andThen
            (\lineInput ->
                { narrow =
                    \input ->
                        onLineParser.narrow lineInput
                }
            )


{-| Matches an integer value as an `Int`.

    import Parser.Error

    -- you can parse integers as `Int` instead of `String`
    "123" |> Text.narrowWith int --> Ok 123

    -- It also works with negative numbers.
    "-123" |> Text.narrowWith int --> Ok -123

    -- a decimal number is _not_ an integer
    "3.14"
        |> Text.narrowWith int
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:2: I was expecting an integer value. I got stuck when I got the character '.'."

    -- but not with invalid numbers
    "abc"
        |> Text.narrowWith int
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

    import Parser.Error

    "12" |> Text.narrowWith number     --> Ok 12.0
    "12.34" |> Text.narrowWith number  --> Ok 12.34
    "12." |> Text.narrowWith number    --> Ok 12.0
    ".12" |> Text.narrowWith number    --> Ok 0.12
    "-12.34" |> Text.narrowWith number --> Ok -12.34
    "-.12" |> Text.narrowWith number   --> Ok -0.12

    "."
        |> Text.narrowWith number
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:1: I was expecting a digit [0-9]. I got stuck when I got the character '.'."

    "abc" |> Text.narrowWith number
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:1: I was expecting a digit [0-9]. I got stuck when I got the character 'a'."

-}
number : Parser Char Float
number =
    sequence
        [ between 0 1 (onFailDown [ atom '-', atom '+' ])
        , onFailDown
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
