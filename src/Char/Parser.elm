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


### example: `oneOf [ letter, digit ]`

> ℹ️ Equivalent regular expression: `[a-zA-Z0-9]`

    import Parser exposing (parse, oneOf)

    -- Match a letter or number.
    oneOf [ letter, digit ] |> parse "abc" --> Ok 'a'
    oneOf [ letter, digit ] |> parse "ABC" --> Ok 'A'
    oneOf [ letter, digit ] |> parse "123" --> Ok '1'

    -- But anything else makes it fail.
    import Parser.Error

    oneOf [ letter, digit ]
        |> parse "_abc"
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:1: I was expecting a letter or a digit [a-zA-Z0-9]. I got stuck when I got the character '_'."

-}

import Parser exposing (Parser, andThen, atom, atomAny, expected, expecting, oneOf, succeed)


{-| Matches a specific single character.
This is case insensitive.

    import Parser exposing (parse)
    import Char.Parser as Char
    import Parser.Error

    -- Match a specific character, case insensitive
    parse "abc" (Char.caseAny 'a') --> Ok 'a'
    parse "ABC" (Char.caseAny 'a') --> Ok 'A'

    Char.caseAny 'a'
        |> parse "123"
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

    import Parser exposing (parse)

    -- Match a digit.
    parse "123" digit --> Ok '1'
    parse "3.14" digit --> Ok '3'

    -- But anything else makes it fail.
    import Parser.Error

    digit
        |> parse "abc"
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:1: I was expecting a digit [0-9]. I got stuck when I got the character 'a'."


### example: `atLeast 1 digit`

> ℹ️ Equivalent regular expression: `[0-9]+` or `\d+`

    import Parser exposing (parse, map)
    import Char.Parser exposing (digit)
    import Parser.Error

    parse "123abc" (atLeast 1 digit) --> Ok "123"

    atLeast 1 digit
        |> map String.fromList
        |> parse "abc123"
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

    import Parser exposing (parse)

    -- Match any letter, case insensitive.
    parse "abc" letter --> Ok 'a'
    parse "ABC" letter --> Ok 'A'

    -- But anything else makes it fail.
    import Parser.Error

    letter
        |> parse "123"
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:1: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '1'."


### example `atLeast 1 letter`

> ℹ️ Equivalent regular expression: `[a-zA-Z]+`

    import Parser exposing (atLeast, parse)
    import Parser.Error

    -- Match many letters, case insensitive
    atLeast 1 letter
        |> map String.fromList
        |> parse "aBc123"
    --> Ok "aBc"

    atLeast 1 letter
        |> map String.fromList
        |> parse "123abc"
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

    import Parser exposing (parse)

    -- Match a lowercase letter.
    parse "abc" lowercase --> Ok 'a'

    -- But anything else makes it fail.
    import Parser.Error

    lowercase
        |> parse "ABC"
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:1: I was expecting a lowercase letter [a-z]. I got stuck when I got the character 'A'."

-}
caseLower : Parser Char Char
caseLower =
    andThen
        (\c ->
            if Char.isLower c then
                succeed c

            else
                expected ""
        )
        atomAny
        |> expecting "a letter lowercase [a-z]"


{-| Matches exactly one uppercase letter character.
This is case sensitive.

> ℹ️ Equivalent regular expression: `[A-Z]`

    import Parser
    import Text.Parser as Text

    -- Match an uppercase letter.
    caseUpper |> Text.parse "ABC" --> Ok 'A'

    -- But anything else makes it fail.
    import Parser.Error

    caseUpper
        |> Text.parse "abc"
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

    import Parser exposing (parse)

    -- Match a blank
    parse "    abc" blank --> Ok ' '
    parse "\n\t abc" blank --> Ok '\n'

    -- But anything else makes it fail.
    import Parser.Error

    parse "abc" blank
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:1: I was expecting a blank space or new line. I got stuck when I got the character 'a'."


### example: token surrounded by spaces

    import Parser exposing (succeed, atLeast, drop, take, parse, atom)
    import Text.Parser as Text
    import Char.Parser as Char

    succeed (\x y -> x + y)
        |> take Text.int
        |> drop (atLeast 0 Char.blank)
        |> drop (atom '+')
        |> drop (atLeast 0 Char.blank)
        |> take Text.int
        |> parse "1  +  2" --> Ok 3

-}
blank : Parser Char Char
blank =
    oneOf
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

    import Parser exposing (parse)
    import Parser.Error

    parse "#hashtag" punctuation --> Ok '#'
    parse "=123" punctuation --> Ok '='

    punctuation
        |> parse "abc"
        |> Result.mapError Parser.Error.textMessage
    --> Err "1:1: I was expecting a punctuation character. I got stuck when I got the character 'a'."

-}
punctuation : Parser Char Char
punctuation =
    oneOf
        (List.map atom
            [ '!', '"', '#', '$', '%', '&', '\'', '(', ')', '*', '+', ',', '-', '.', '/', ':', ';', '<', '=', '>', '?', '@', '[', ']', '^', '_', '\\', '{', '}', '~' ]
        )
        |> expecting "a punctuation character"
