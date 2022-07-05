module Morph.Text exposing
    ( toList, fromList
    , elementEach
    , specific, caseAny
    , LineEnd, lineEnd
    )

{-| [`Morph`](Morph#Morph)s and [`MorphRow`](MorphRow#MorphRow)s from and to a `String`.


## `List`

@docs toList, fromList


## transform

Also available: [`toggle`](Morph#toggle) `String.reverse`

@docs elementEach

Use [`Morph.narrow`](Morph#narrow), [`Morph.broaden`](Morph#broaden)
with [`Text.fromList`](#fromList) `|> over ...`

    import Morph exposing (narrow)
    import Morph.Char as Char
    import Morph.TextRow as Text
    import MorphRow.Error

    -- consumes a single letter, then "bc" are still remaining
    "abc" |> narrow (Char.letter |> map listToString)
    --> Ok 'a'

    -- we can also parse text into other data types like numbers
    "3.14" |> narrow (number |> map listToString)
    --> Ok 3.14

    -- no match
    "123"
        |> narrow (Char.letter |> map listToString)
        |> Result.mapError MorphRow.Error.textMessage
    --> Err "1:1: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '1'."


## exact match

@docs specific, caseAny


## line end

@docs LineEnd, lineEnd

Feeling motivated? implement & PR

  - date, time, datetime
  - email
  - unixPath, windowsPath
  - uri
  - IPv4, IPv6
  - int2 (bin), int8 (oct), int6 (hex)

-}

import Morph exposing (Morph, Translate, broadenFrom, translate)
import Morph.Char
import MorphRow exposing (MorphRow, one)


{-| [`Translate`](Morph#Translate) from a `String` to a `List Char`.

    "0123" |> (Morph.stringToList |> Morph.broaden)
    --> [ '0', '1', '2', '3' ]

-}
toList : Morph String (List Char) error_
toList =
    translate String.toList String.fromList


{-| [`Translate`](Morph#Translate) from `List Char` to a `String`.

    "0123" |> (Morph.stringToList |> Morph.broaden)
    --> [ '0', '1', '2', '3' ]

-}
fromList : Morph (List Char) String error_
fromList =
    toList |> Morph.reverse


{-| Match a specific text string and nothing else.
This is case sensitive.

    import MorphRow.Error

    -- match an exact text, case sensitive
    "abcdef" |> Text.narrowWith (text "abc") --> Ok "abc"

    -- but anything else makes it fail
    "abCDEF"
        |> Text.narrowWith (text "abc")
        |> Result.mapError MorphRow.Error.textMessage
    --> Err "1:3: I was expecting the text 'abc'. I got stuck when I got the character 'C'."

    Morph.Text.specific "hello"
    --> MorphRow.specific ("hello" |> String.toList)

-}
specific : String -> MorphRow Char () expectedCustom_
specific expectedText =
    MorphRow.specific (expectedText |> String.toList)


{-| Match a specific text string.
This is case insensitive.

    import MorphRow.Error

    "aBcdef" |> Text.narrowWith (Text.caseAny "abC") --> Ok "aBc"

    parse "ab@"
        |> Text.narrowWith (Text.caseAny "abc")
        |> Result.mapError MorphRow.Error.textMessage
    --> Err "1:3: I was expecting the text \"abc\" (case insensitive). I got stuck when I got the character '@'."

-}
caseAny : String -> MorphRow Char () (Morph.Error Char variantExpectation_)
caseAny expectedText =
    broadenFrom
        (List.repeat (expectedText |> String.length) ())
        |> MorphRow.over
            (MorphRow.sequence
                (List.map
                    (\expectedChar -> Morph.Char.caseAny expectedChar |> one)
                    (expectedText |> Morph.map toList)
                )
            )


{-| The end of a text line:
either a [return character](Morph-Char#Return) or the end of the whole text.
-}
type LineEnd
    = InputEnd
    | Return Morph.Char.Return


{-| Consume the end of the current line or succeed if there are
no more remaining characters in the input text.

> ℹ️ Equivalent regular expression: `$`

    import MorphRow exposing (map, followedBy, atLeast)
    import Morph.Char as Char
    import Morph.TextRow exposing (line)
    import MorphRow.Error

    "abc\n123"
        |> Text.narrowWith
            (succeed (\letters -> letters)
                |> grab
                    (atLeast 1 Char.letter
                        |> map String.fromList
                    )
                |> skip (lineEnd |> broadenFrom (Return Digit.Morph.NewLine))
            )
    --> Ok "abc"

    -- carriage return also counts
    "abc\r123"
        |> Text.narrowWith
            (succeed (\letters -> letters)
                |> grab
                    (atLeast 1 Char.letter
                        |> map String.fromList
                    )
                |> skip (lineEnd |> broadenFrom (Return Digit.Morph.NewLine))
            )
    --> Ok "abc"

    -- end of file also counts
    "abc"
        |> Text.narrowWith
            (succeed (\letters -> letters)
                |> grab
                    (atLeast 1 Char.letter
                        |> map String.fromList
                    )
                |> skip (lineEnd |> broadenFrom (Return Digit.Morph.NewLine))
            )
    --> Ok "abc"

    -- fail otherwise
    "abc123"
        |> Text.narrowWith
            (succeed (\letters -> letters)
                |> grab
                    (atLeast 1 Char.letter
                        |> map String.fromList
                    )
                |> skip (lineEnd |> broadenFrom (Return Digit.Morph.NewLine))
            )
        |> Result.mapError MorphRow.Error.textMessage
    --> Err "1:4: I was expecting the end of the current line. I got stuck when I got the character '1'."


    line : MorphRow Char { line : String }
    line =
        succeed (\line_ -> { line = line_ })
            |> grab .line
                onLineMorphRow
            |> skip (lineEnd |> broadenFrom (Return Digit.Morph.NewLine))

    -- a line could end with '\n'
    "abc\ndef" |> Text.narrowWith line --> Ok { line = "abc" }

    -- could also be the last line
    "abc" |> Text.narrowWith line --> Ok { line = "abc" }

    -- an empty line counts
    "\n" |> Text.narrowWith line --> Ok { line = "" }

    -- an empty file doesn't count
    ""
        |> Text.narrowWith line
        |> Result.mapError MorphRow.Error.textMessage
    --> Err "1:0: I was expecting a line. I reached the end of the input."

    -- multiple lines
    "hi\nho\nhu"
        |> Text.narrowWith (atLeast 0 line)
    --> Ok [ "hi", "ho", "hu" ]

-}
lineEnd : MorphRow Char LineEnd expectedCustom_
lineEnd =
    Morph.choice
        (\returnVariant inputEndVariant maybe_ ->
            case maybe_ of
                Return return ->
                    returnVariant return

                InputEnd ->
                    inputEndVariant ()
        )
        |> MorphRow.possibility Return
            (Morph.Char.return |> one)
        |> MorphRow.possibility (\() -> InputEnd)
            MorphRow.end
        |> MorphRow.choiceFinish



--


{-| [`Translate`](Morph#Translate) each element in a `List`.
-}
elementEach :
    Translate Char Char
    -> Morph String String error_
elementEach elementTranslate =
    translate
        (String.map (Morph.map elementTranslate))
        (String.map (Morph.unmap elementTranslate))
