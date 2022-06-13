module Text.ConversionStep exposing
    ( narrowWith
    , text, caseAny
    , integer, floatingPoint
    , line, lineEnd
    )

{-| Parsing text

@docs narrowWith


## match

@docs text, caseAny


## number

@docs integer, floatingPoint


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

import Char.Conversion exposing (n0To9ToInt)
import Char.ConversionStep as Char
import Conversion exposing (broaden, stringToList, transfer)
import ConversionStep exposing (ConversionStep, atLeast, atom, drop, expect, fail, map, maybe, possibility, sequence, succeed, take, until)


{-| Parse an input text, and get either an [error](ConversionStep#Expected)
or the parsed narrow value as a `Result`.

    import Char.ConversionStep as Char
    import Text.ConversionStep as Text
    import ConversionStep.Error

    -- consumes a single letter, then "bc" are still remaining
    "abc" |> Text.narrowWith Char.letter --> Ok 'a'

    -- we can also parse text into other data types like numbers
    "3.14" |> Text.narrowWith number --> Ok 3.14

    -- we get an error message if the parser doesn't match
    "123"
        |> Text.narrowWith Char.letter
        |> Result.mapError ConversionStep.Error.textMessage
    --> Err "1:1: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '1'."

[`ConversionStep.narrowWith`](ConversionStep#narrowWith) accepts any input `List`.

-}
narrowWith :
    ConversionStep Char narrow
    -> String
    -> Result (ConversionStep.Error Char narrow) narrow
narrowWith parser =
    \input ->
        input |> String.toList |> ConversionStep.narrowWith parser


{-| Matches a specific text string.
This is case sensitive.

    import ConversionStep.Error

    -- match an exact text, case sensitive
    "abcdef" |> Text.narrowWith (text "abc") --> Ok "abc"

    -- but anything else makes it fail
    "abCDEF"
        |> Text.narrowWith (text "abc")
        |> Result.mapError ConversionStep.Error.textMessage
    --> Err "1:3: I was expecting the text 'abc'. I got stuck when I got the character 'C'."

TODO: rename to `caseSensitive`/`specific`/?

-}
text : String -> ConversionStep Char ()
text expectedText =
    sequence (List.map atom (expectedText |> String.toList))
        |> map
            (transfer
                (\_ -> ())
                (\() -> List.repeat (expectedText |> String.length) ())
            )
        |> expect
            ([ "the text \"", expectedText, "\"" ]
                |> String.concat
            )


{-| Matches a specific text string.
This is case insensitive.

    import ConversionStep.Error

    "aBcdef" |> Text.narrowWith (Text.caseAny "abC") --> Ok "aBc"

    parse "ab@"
        |> Text.narrowWith (Text.caseAny "abc")
        |> Result.mapError ConversionStep.Error.textMessage
    --> Err "1:3: I was expecting the text \"abc\" (case insensitive). I got stuck when I got the character '@'."

-}
caseAny : String -> ConversionStep Char ()
caseAny expectedText =
    expect
        ([ "the text \"", expectedText, "\", case insensitive" ]
            |> String.concat
        )
        (sequence (List.map Char.caseAny (expectedText |> String.toList))
            |> map
                (transfer
                    (\_ -> ())
                    (\() -> List.repeat (expectedText |> String.length) ())
                )
        )


type LineEnd
    = InputEnd
    | Return Char.Return


{-| Consumes the [end of the current line](#lineEnd) or succeeds if there are
no more remaining characters in the input text.

> ℹ️ Equivalent regular expression: `$`

    import ConversionStep exposing (map, followedBy, atLeast)
    import Char.ConversionStep as Char
    import Text.ConversionStep exposing (line)
    import ConversionStep.Error

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
        |> Result.mapError ConversionStep.Error.textMessage
    --> Err "1:4: I was expecting the end of the current line. I got stuck when I got the character '1'."

-}
lineEnd : ConversionStep Char LineEnd
lineEnd =
    expect "the end of the line"
        (ConversionStep.choice
            (\returnVariant inputEndVariant maybe_ ->
                case maybe_ of
                    Return return ->
                        returnVariant return

                    InputEnd ->
                        inputEndVariant ()
            )
            (possibility Return Char.return
                >> possibility (\() -> InputEnd) inputEnd
            )
        )


inputEnd : ConversionStep atom_ ()
inputEnd =
    expect "the end of the input"
        { narrow =
            \input ->
                (case input of
                    [] ->
                        succeed ()

                    _ :: _ ->
                        fail
                ).narrow
                    input
        , broaden =
            \() -> []
        }


{-| Matches a line from the input text, delimited by `'\\n'`.

    import ConversionStep exposing (atLeast)
    import ConversionStep.Error

    -- A line could be delimited by the newline character '\n'.
    "abc\ndef" |> Text.narrowWith line --> Ok "abc"

    -- Or this could also be the last line.
    parse "abc" line --> Ok "abc"

    -- An empty line still counts.
    parse "\n" line --> Ok ""

    -- But not an empty file.
    line
        |> parse ""
        |> Result.mapError ConversionStep.Error.textMessage
    --> Err "1:0: I was expecting a line. I reached the end of the input."

    -- So we can parse multiple lines.
    atLeast 0 line
        |> parse "abc\ndef\nghi"
    --> Ok [ "abc", "def", "ghi"]

-}
line : ConversionStep Char lineNarrow -> ConversionStep Char lineNarrow
line onLineConversionStep =
    until lineEnd onLineConversionStep
        |> map
            (transfer
                .before
                (\before ->
                    { before = before
                    , end = Return Char.NewLine
                    }
                )
            )


digitsToInteger : List Int -> Int
digitsToInteger =
    \integerDigits ->
        integerDigits
            |> List.reverse
            |> List.indexedMap (\decimal digit -> digit * 10 ^ decimal)
            |> List.sum


{-| Matches an integer value as an `Int`.

    import ConversionStep.Error

    -- you can parse integers as `Int` instead of `String`
    "123" |> Text.narrowWith integer --> Ok 123

    -- It also works with negative numbers.
    "-123" |> Text.narrowWith integer --> Ok -123

    -- a decimal number is _not_ an integer
    "3.14"
        |> Text.narrowWith integer
        |> Result.mapError ConversionStep.Error.textMessage
    --> Err "1:2: I was expecting an integer value. I got stuck when I got the character '.'."

    -- but not with invalid numbers
    "abc"
        |> Text.narrowWith integer
        |> Result.mapError ConversionStep.Error.textMessage
    --> Err "1:1: I was expecting an integer value. I got stuck when I got the character 'a'."

-}
integer : ConversionStep Char Int
integer =
    map
        (transfer
            (\integerParsed ->
                let
                    signNumber =
                        case integerParsed.sign of
                            Nothing ->
                                identity

                            Just () ->
                                negate
                in
                signNumber (integerParsed.digits |> digitsToInteger)
            )
            (\int ->
                { sign =
                    if int < 0 then
                        Just ()

                    else
                        Nothing
                , digits =
                    let
                        intAbsolute =
                            int |> abs
                    in
                    List.range 0 ((intAbsolute |> String.fromInt |> String.length) - 1)
                        |> List.reverse
                        |> List.map
                            (\decimal ->
                                intAbsolute // (10 ^ (decimal |> broaden n0To9ToInt))
                            )
                }
            )
        )
        (succeed
            (\sign_ digits_ ->
                { sign = sign_, digits = digits_ }
            )
            |> take .sign (maybe (atom '-'))
            |> take .digits (atLeast 1 Char.n0To9)
        )
        |> expect "an integer value"


{-| Matches a decimal value as a `Float`.

    import ConversionStep.Error

    "12" |> Text.narrowWith number     --> Ok 12.0
    "12.34" |> Text.narrowWith number  --> Ok 12.34
    "12." |> Text.narrowWith number    --> Ok 12.0
    ".12" |> Text.narrowWith number    --> Ok 0.12
    "-12.34" |> Text.narrowWith number --> Ok -12.34
    "-.12" |> Text.narrowWith number   --> Ok -0.12

    "."
        |> Text.narrowWith number
        |> Result.mapError ConversionStep.Error.textMessage
    --> Err "1:1: I was expecting a digit [0-9]. I got stuck when I got the character '.'."

    "abc" |> Text.narrowWith number
        |> Result.mapError ConversionStep.Error.textMessage
    --> Err "1:1: I was expecting a digit [0-9]. I got stuck when I got the character 'a'."

-}
floatingPoint : ConversionStep Char Float
floatingPoint =
    map
        (transfer
            (\floatParsed ->
                let
                    signFloat =
                        case floatParsed.sign of
                            Nothing ->
                                identity

                            Just Char.Plus ->
                                identity

                            Just Char.Minus ->
                                negate

                    absoluteFloor =
                        floatParsed.wholeDigits |> digitsToInteger

                    toFraction fractionDigits =
                        fractionDigits
                            |> List.reverse
                            |> List.indexedMap
                                (\decimal digit ->
                                    (digit |> toFloat) * 10 ^ -(1 + (decimal |> toFloat))
                                )
                            |> List.sum
                in
                signFloat
                    ((absoluteFloor |> toFloat)
                        + (case floatParsed.fractionDigits of
                            Nothing ->
                                0

                            Just fractionDigits ->
                                fractionDigits |> toFraction
                          )
                    )
            )
            (\floatValue ->
                let
                    whole =
                        floatValue |> abs |> floor
                in
                { sign =
                    if floatValue < 0 then
                        Just Char.Minus

                    else
                        Nothing
                , wholeDigits =
                    whole
                        |> String.fromInt
                        |> String.toList
                        |> List.map (Debug.todo "digit")
                , fractionDigits =
                    (floatValue - (whole |> toFloat))
                        |> String.fromFloat
                        |> String.dropLeft 2
                        |> String.toList
                        |> List.map (Debug.todo "digit")
                        |> Just
                }
            )
        )
        (succeed
            (\sign_ wholeDigits fractionDigits ->
                { sign = sign_
                , wholeDigits = wholeDigits
                , fractionDigits = fractionDigits
                }
            )
            |> take .sign (maybe Char.sign)
            |> take .wholeDigits (atLeast 0 Char.n0To9)
            |> take .fractionDigits
                (maybe
                    (succeed (\fractionDigits -> fractionDigits)
                        |> drop (atom '.')
                        |> take (\fractionDigits -> fractionDigits) (atLeast 1 Char.n0To9)
                    )
                )
        )
        |> expect "a floating point number"
