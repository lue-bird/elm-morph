module Char.Parser exposing
    ( caseAny
    , digit, blank, punctuation
    , letter, caseLower, caseUpper
    )

{-| Parsing characters.

@docs caseAny
@docs digit, blank, punctuation


## letters

@docs letter, caseLower, caseUpper


### example: `onFailDown [ letter, digit ]`

> ℹ️ Equivalent regular expression: `[a-zA-Z0-9]`

    import Parser exposing (onFailDown)
    import Parser.Error
    import Text.Parser as Text

    -- Match a letter or number.
    "abc" |> Text.narrowWith (onFailDown [ letter, digit ])
    --> Ok 'a'
    "ABC" |> Text.narrowWith (onFailDown [ letter, digit ])
    --> Ok 'A'
    "123" |> Text.narrowWith (onFailDown [ letter, digit ])
    --> Ok '1'

    -- but anything else makes it fail
    "_abc"
        |> Text.narrowWith (onFailDown [ letter, digit ])
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:1: I was expecting a letter or a digit [a-zA-Z0-9]. I got stuck when I got the character '_'."

-}

import Parser exposing (Parser, andThen, atom, atomAny, expected, expecting, onFailDown, succeed)


{-| Matches a specific single character.
This is case insensitive.

    import Parser.Error
    import Char.Parser as Char
    import Text.Parser as Text

    -- match a specific character, case insensitive
    "abc" |> Text.narrowWith (Char.caseAny 'a') --> Ok 'a'
    "ABC" |> Text.narrowWith (Char.caseAny 'a') --> Ok 'A'

    "123"
        |> Text.narrowWith (Char.caseAny 'a')
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:1: I was expecting the character 'a' (case insensitive). I got stuck when I got the character '1'."

-}
caseAny : Char -> Parser Char Char
caseAny expectedCharacter =
    andThen
        (\actualCharacter ->
            if Char.toLower actualCharacter == (expectedCharacter |> Char.toLower) then
                succeed actualCharacter

            else
                expected ""
        )
        atomAny
        |> expecting
            ([ "the character '"
             , expectedCharacter |> String.fromChar
             , "' (case insensitive)"
             ]
                |> String.concat
            )


{-| Matches exactly one digit character.

> ℹ️ Equivalent regular expression: `[0-9]` or `\d`

    import Parser.Error
    import Text.Parser as Text

    -- match a digit
    "123" |> Text.narrowWith digit --> Ok '1'
    "3.14" |> Text.narrowWith digit --> Ok '3'

    -- but anything else makes it fail
    "abc"
        |> Text.narrowWith digit
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:1: I was expecting a digit [0-9]. I got stuck when I got the character 'a'."


### example: `atLeast 1 digit`

> ℹ️ Equivalent regular expression: `[0-9]+` or `\d+`

    import Parser exposing (map)
    import Parser.Error
    import Char.Parser exposing (digit)
    import Text.Parser as Text

    "123abc" |> Text.narrowWith (atLeast 1 digit) --> Ok "123"

    "abc123"
        |> Text.narrowWith
            (atLeast 1 digit
                |> map String.fromList
            )
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:1: I was expecting at least 1 digit [0-9]. I got stuck when I got the character 'a'."

-}
digit : Parser Char Char
digit =
    andThen
        (\character ->
            if character |> Char.isDigit then
                succeed character

            else
                expected ""
        )
        atomAny
        |> expecting "a digit [0-9]"


{-| Matches exactly one letter character.
This is case insensitive.

> ℹ️ Equivalent regular expression: `[a-zA-Z]`

    import Char.Parser exposing (letter)
    import Text.Parser as Text

    -- match any letter, case insensitive
    "abc" |> Text.narrowWith letter --> Ok 'a'
    "ABC" |> Text.narrowWith letter --> Ok 'A'

    -- But anything else makes it fail.
    import Parser.Error

    "123"
        |> Text.narrowWith letter
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:1: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '1'."


### example `atLeast 1 letter`

> ℹ️ Equivalent regular expression: `[a-zA-Z]+`

    import Parser exposing (atLeast)
    import Parser.Error
    import Text.Parser as Text

    -- match many letters, case insensitive
    "aBc123"
        |> Text.narrowWith
            (atLeast 1 letter
                |> map String.fromList
            )
    --> Ok "aBc"

    "123abc"
        |> Text.narrowWith
            (atLeast 1 letter
                |> map String.fromList
            )
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:1: I was expecting at least 1 letters [a-zA-Z]+. I got stuck when I got the character '1'."

-}
letter : Parser Char Char
letter =
    andThen
        (\c ->
            if Char.isAlpha c then
                succeed c

            else
                expected ""
        )
        atomAny
        |> expecting "a letter [a-zA-Z]"


{-| Matches exactly one lowercase letter character.
This is case sensitive.

> ℹ️ Equivalent regular expression: `[a-z]`

    import Parser.Error
    import Text.Parser as Text

    -- match a lowercase letter
    "abc" |> Text.narrowWith lowercase --> Ok 'a'

    -- but anything else makes it fail
    "ABC"
        |> Text.narrowWith lowercase
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:1: I was expecting a lowercase letter [a-z]. I got stuck when I got the character 'A'."

-}
caseLower : Parser Char Char
caseLower =
    andThen
        (\char ->
            if char |> Char.isLower then
                succeed char

            else
                expected ""
        )
        atomAny
        |> expecting "a letter lowercase [a-z]"


{-| Matches exactly one uppercase letter character.
This is case sensitive.

> ℹ️ Equivalent regular expression: `[A-Z]`

    import Parser
    import Parser.Error
    import Text.Parser as Text

    -- match an uppercase letter
    caseUpper |> Text.narrowWith "ABC" --> Ok 'A'

    -- but anything else makes it fail
    "abc"
        |> Text.narrowWith caseUpper
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:1: I was expecting a letter uppercase [A-Z]. I got stuck when I got the character 'a'."

-}
caseUpper : Parser Char Char
caseUpper =
    andThen
        (\character ->
            if character |> Char.isUpper then
                succeed character

            else
                expected ""
        )
        atomAny
        |> expecting "a letter uppercase [A-Z]"


{-| Matches a Unicode blank character, including new lines: One of

  - space `' '`
  - tab `'\t'`
  - new line `'\n'`
  - carriage return `'\r'`
  - form feed `'\f'`

> ℹ️ Equivalent regular expression: `[ \t\n\r\f]` or `\s`

    import Parser.Error
    import Text.Parser as Text

    -- match a blank
    "    abc" |> Text.narrowWith blank --> Ok ' '
    "\n\t abc" |> Text.narrowWith blank --> Ok '\n'

    -- But anything else makes it fail.

    "abc"
        |> Text.narrowWith blank
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:1: I was expecting a blank space or new line. I got stuck when I got the character 'a'."


### example: token surrounded by spaces

    import Parser exposing (succeed, atLeast, drop, take, atom)
    import Text.Parser as Text
    import Char.Parser as Char

    "1  +  2"
        |> Text.narrowWith
            (succeed (\x y -> x + y)
                |> take Text.int
                |> drop (atLeast 0 Char.blank)
                |> drop (atom '+')
                |> drop (atLeast 0 Char.blank)
                |> take Text.int
            )
    --> Ok 3

-}
blank : Parser Char Char
blank =
    onFailDown
        (List.map atom
            [ ' ' -- space
            , '\t' -- tab
            , '\n' -- new line
            , '\u{000D}' -- \r -- carriage return
            , '\u{000C}' -- \f -- form feed
            ]
        )
        |> expecting "a blank space or new line"


{-| Matches an ASCII punctuation character.
A punctuation character can be any of
`!`, `"`, `#`, `$`, `%`, `&`, `'`, `(`, `)`, `*`, `+`, `,`, `-`, `.`,
`/`, `:`, `;`, `<`, `=`, `>`, `?`, `@`, `[`, `]`, `^`, `_`, ``` \``, ```{`,`}`, or`~\`.

> ℹ️ Equivalent regular expression: `[!"#$%&'()*+,-./:;<=>?@[\]^_\\{}~]`

    import Parser.Error
    import Text.Parser as Text

    "#hashtag" |> Text.narrowWith punctuation --> Ok '#'
    "=123" |> Text.narrowWith punctuation --> Ok '='

    "abc"
        |> Text.narrowWith punctuation
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:1: I was expecting a punctuation character. I got stuck when I got the character 'a'."

-}
punctuation : Parser Char Char
punctuation =
    onFailDown
        (List.map atom
            [ '!', '"', '#', '$', '%', '&', '\'', '(', ')', '*', '+', ',', '-', '.', '/', ':', ';', '<', '=', '>', '?', '@', '[', ']', '^', '_', '\\', '{', '}', '~' ]
        )
        |> expecting "a punctuation character"
