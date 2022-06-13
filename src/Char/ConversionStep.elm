module Char.ConversionStep exposing
    ( caseAny
    , Blank(..), blank
    , Return(..), return
    , Sign(..), sign
    , AToZ(..), Case(..), aToZ
    , aToZLower, aToZUpper, n0To9
    )

{-| Parsing characters.

@docs caseAny
@docs digit
@docs Blank, blank
@docs Return, return
@docs Sign, sign


## letters

@docs AToZ, Case, aToZ, lowerAToZ, upperAToZ


### example: `onFailDown [ letter, digit ]`

> ℹ️ Equivalent regular expression: `[a-zA-Z0-9]`

    import ConversionStep exposing (onFailDown)
    import ConversionStep.Error
    import Text.ConversionStep as Text

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
        |> Result.mapError ConversionStep.Error.textMessage
    --> Err "1:1: I was expecting a letter or a digit [a-zA-Z0-9]. I got stuck when I got the character '_'."

-}

import Char.Conversion exposing (N0To9(..))
import Conversion exposing (transfer)
import ConversionStep exposing (ConversionStep, atom, atomAny, choice, expect, fail, next, possibility, succeed)


{-| Matches a specific single character.
This is case insensitive.

    import ConversionStep.Error
    import Char.ConversionStep as Char
    import Text.ConversionStep as Text

    -- match a specific character, case insensitive
    "abc" |> Text.narrowWith (Char.caseAny 'a') --> Ok 'a'
    "ABC" |> Text.narrowWith (Char.caseAny 'a') --> Ok 'A'

    "123"
        |> Text.narrowWith (Char.caseAny 'a')
        |> Result.mapError ConversionStep.Error.textMessage
    --> Err "1:1: I was expecting the character 'a' (case insensitive). I got stuck when I got the character '1'."

-}
caseAny : Char -> ConversionStep Char ()
caseAny expectedChar =
    choice
        (\lower upper casing ->
            case casing of
                CaseLower ->
                    lower ()

                CaseUpper ->
                    upper ()
        )
        (possibility (\() -> CaseLower)
            (atom (expectedChar |> Char.toLower))
            >> possibility (\() -> CaseUpper)
                (atom (expectedChar |> Char.toUpper))
        )
        |> ConversionStep.map
            (transfer
                (\_ -> ())
                (\() ->
                    if expectedChar |> Char.isLower then
                        CaseLower

                    else
                        CaseUpper
                )
            )


type Case
    = CaseLower
    | CaseUpper


{-| Matches exactly one digit character.

> ℹ️ Equivalent regular expression: `[0-9]` or `\d`

    import ConversionStep.Error
    import Text.ConversionStep as Text

    -- match a digit
    "123" |> Text.narrowWith digit --> Ok '1'
    "3.14" |> Text.narrowWith digit --> Ok '3'

    -- but anything else makes it fail
    "abc"
        |> Text.narrowWith digit
        |> Result.mapError ConversionStep.Error.textMessage
    --> Err "1:1: I was expecting a digit [0-9]. I got stuck when I got the character 'a'."


### example: `atLeast 1 digit`

> ℹ️ Equivalent regular expression: `[0-9]+` or `\d+`

    import ConversionStep exposing (map)
    import ConversionStep.Error
    import Char.ConversionStep as Char
    import Text.ConversionStep as Text

    "123abc" |> Text.narrowWith (atLeast 1 Char.n0To9) --> Ok "123"

    "abc123"
        |> Text.narrowWith
            (atLeast 1 Char.n0To9
                |> map String.fromList
            )
        |> Result.mapError ConversionStep.Error.textMessage
    --> Err "1:1: I was expecting at least 1 digit [0-9]. I got stuck when I got the character 'a'."

-}
n0To9 : ConversionStep Char N0To9
n0To9 =
    expect "0|...|9"
        (choice
            (\n0 n1 n2 n3 n4 n5 n6 n7 n8 n9 digit_ ->
                case digit_ of
                    N0 ->
                        n0 ()

                    N1 ->
                        n1 ()

                    N2 ->
                        n2 ()

                    N3 ->
                        n3 ()

                    N4 ->
                        n4 ()

                    N5 ->
                        n5 ()

                    N6 ->
                        n6 ()

                    N7 ->
                        n7 ()

                    N8 ->
                        n8 ()

                    N9 ->
                        n9 ()
            )
            (possibility (\() -> N0) (atom '0')
                >> possibility (\() -> N1) (atom '1')
                >> possibility (\() -> N2) (atom '2')
                >> possibility (\() -> N3) (atom '3')
                >> possibility (\() -> N4) (atom '4')
                >> possibility (\() -> N5) (atom '5')
                >> possibility (\() -> N6) (atom '6')
                >> possibility (\() -> N7) (atom '7')
                >> possibility (\() -> N8) (atom '8')
                >> possibility (\() -> N9) (atom '9')
            )
        )


{-| Parses exactly 1 ASCII lower or upper case letter character.
This is case insensitive.

> ℹ️ Equivalent regular expression: `[a-zA-Z]`

    import Char.ConversionStep exposing (letter)
    import Text.ConversionStep as Text

    -- match any letter, case insensitive
    "abc" |> Text.narrowWith letter --> Ok 'a'
    "ABC" |> Text.narrowWith letter --> Ok 'A'

    -- But anything else makes it fail.
    import ConversionStep.Error

    "123"
        |> Text.narrowWith letter
        |> Result.mapError ConversionStep.Error.textMessage
    --> Err "1:1: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '1'."


### example `atLeast 1 letter`

> ℹ️ Equivalent regular expression: `[a-zA-Z]+`

    import ConversionStep exposing (atLeast)
    import ConversionStep.Error
    import Text.ConversionStep as Text

    -- match many letters, case insensitive
    "aBcEY"
        |> Text.narrowWith
            (atLeast 1 aToZ
                |> map String.fromList
            )
    --> Ok "aBcEY"

    "π123abc"
        |> Text.narrowWith
            (atLeast 1 aToZ
                |> map String.fromList
            )
        |> Result.mapError ConversionStep.Error.textMessage
    --> Err "1:1: I was expecting at least 1 letters [a-zA-Z]+. I got stuck when I got 'π'."

    "abc-efg"
        |> Text.narrowWith
            (atLeast 1 aToZ
                |> map String.fromList
            )
        |> Result.mapError ConversionStep.Error.textMessage
    --> Err "1:1: I was expecting at least 1 letters [a-zA-Z]+. I got stuck when I got '-'."

-}
aToZ : ConversionStep Char { case_ : Case, letter : AToZ }
aToZ =
    cased { lower = aToZLower, upper = aToZUpper }


cased :
    { lower : ConversionStep atom possibilityNarrow
    , upper : ConversionStep atom possibilityNarrow
    }
    -> ConversionStep atom { case_ : Case, letter : possibilityNarrow }
cased casedLetters =
    choice
        (\lower upper caseValue ->
            case caseValue.case_ of
                CaseLower ->
                    lower caseValue.letter

                CaseUpper ->
                    upper caseValue.letter
        )
        (possibility
            (\letter -> { case_ = CaseLower, letter = letter })
            casedLetters.lower
            >> possibility
                (\letter -> { case_ = CaseUpper, letter = letter })
                casedLetters.upper
        )


type AToZ
    = A
    | B
    | C
    | D
    | E
    | F
    | G
    | H
    | I
    | J
    | K
    | L
    | M
    | N
    | O
    | P
    | Q
    | R
    | S
    | T
    | U
    | V
    | W
    | X
    | Y
    | Z


{-| Matches exactly one lowercase letter character.
This is case sensitive.

> ℹ️ Equivalent regular expression: `[a-z]`

    import ConversionStep.Error
    import Text.ConversionStep as Text

    -- match a lowercase letter
    "abc" |> Text.narrowWith aToZLower --> Ok 'a'

    -- but anything else makes it fail
    "ABC"
        |> Text.narrowWith aToZLower
        |> Result.mapError ConversionStep.Error.textMessage
    --> Err "1:1: I was expecting a lowercase letter [a-z]. I got stuck when I got the character 'A'."

-}
aToZLower : ConversionStep Char AToZ
aToZLower =
    casedAToZ Char.toLower


{-| Matches exactly one uppercase letter character.
This is case sensitive.

> ℹ️ Equivalent regular expression: `[A-Z]`

    import ConversionStep
    import ConversionStep.Error
    import Text.ConversionStep as Text

    -- match an uppercase letter
    aToZUpper |> Text.narrowWith "ABC" --> Ok 'A'

    -- but anything else makes it fail
    "abc"
        |> Text.narrowWith aToZUpper
        |> Result.mapError ConversionStep.Error.textMessage
    --> Err "1:1: I was expecting a letter uppercase [A-Z]. I got stuck when I got the character 'a'."

-}
aToZUpper : ConversionStep Char AToZ
aToZUpper =
    casedAToZ Char.toUpper


casedAToZ : (Char -> Char) -> ConversionStep Char AToZ
casedAToZ aToZTransformation =
    expect
        ([ 'a' |> aToZTransformation |> String.fromChar
         , "|...|"
         , 'z' |> aToZTransformation |> String.fromChar
         ]
            |> String.concat
        )
        (choice
            (\a b c d e f g h i j k l m n o p q r s t u v w x y z aToZ_ ->
                case aToZ_ of
                    A ->
                        a ()

                    B ->
                        b ()

                    C ->
                        c ()

                    D ->
                        d ()

                    E ->
                        e ()

                    F ->
                        f ()

                    G ->
                        g ()

                    H ->
                        h ()

                    I ->
                        i ()

                    J ->
                        j ()

                    K ->
                        k ()

                    L ->
                        l ()

                    M ->
                        m ()

                    N ->
                        n ()

                    O ->
                        o ()

                    P ->
                        p ()

                    Q ->
                        q ()

                    R ->
                        r ()

                    S ->
                        s ()

                    T ->
                        t ()

                    U ->
                        u ()

                    V ->
                        v ()

                    W ->
                        w ()

                    X ->
                        x ()

                    Y ->
                        y ()

                    Z ->
                        z ()
            )
            (possibility (\() -> A) (atom ('a' |> aToZTransformation))
                >> possibility (\() -> B) (atom ('b' |> aToZTransformation))
                >> possibility (\() -> C) (atom ('c' |> aToZTransformation))
                >> possibility (\() -> D) (atom ('d' |> aToZTransformation))
                >> possibility (\() -> E) (atom ('e' |> aToZTransformation))
                >> possibility (\() -> F) (atom ('f' |> aToZTransformation))
                >> possibility (\() -> G) (atom ('g' |> aToZTransformation))
                >> possibility (\() -> H) (atom ('h' |> aToZTransformation))
                >> possibility (\() -> I) (atom ('i' |> aToZTransformation))
                >> possibility (\() -> J) (atom ('j' |> aToZTransformation))
                >> possibility (\() -> K) (atom ('k' |> aToZTransformation))
                >> possibility (\() -> L) (atom ('l' |> aToZTransformation))
                >> possibility (\() -> M) (atom ('m' |> aToZTransformation))
                >> possibility (\() -> N) (atom ('n' |> aToZTransformation))
                >> possibility (\() -> O) (atom ('o' |> aToZTransformation))
                >> possibility (\() -> P) (atom ('p' |> aToZTransformation))
                >> possibility (\() -> Q) (atom ('q' |> aToZTransformation))
                >> possibility (\() -> R) (atom ('r' |> aToZTransformation))
                >> possibility (\() -> S) (atom ('s' |> aToZTransformation))
                >> possibility (\() -> T) (atom ('t' |> aToZTransformation))
                >> possibility (\() -> U) (atom ('u' |> aToZTransformation))
                >> possibility (\() -> V) (atom ('v' |> aToZTransformation))
                >> possibility (\() -> W) (atom ('w' |> aToZTransformation))
                >> possibility (\() -> X) (atom ('x' |> aToZTransformation))
                >> possibility (\() -> Y) (atom ('y' |> aToZTransformation))
                >> possibility (\() -> Z) (atom ('z' |> aToZTransformation))
            )
        )


type Return
    = NewLine
    | CarriageReturn


return : ConversionStep Char Return
return =
    choice
        (\newLine carriageReturn return_ ->
            case return_ of
                NewLine ->
                    newLine ()

                CarriageReturn ->
                    carriageReturn ()
        )
        (possibility (\() -> NewLine) (atom '\n')
            >> -- \r
               possibility (\() -> CarriageReturn) (atom '\u{000D}')
        )


type Blank
    = Space
    | Tab
    | Return Return
    | FormFeed


{-| Matches a Unicode blank character, including new lines: One of

  - space `' '`
  - tab `'\t'`
  - new line `'\n'`
  - carriage return `'\r'`
  - form feed `'\f'`

> ℹ️ Equivalent regular expression: `[ \t\n\r\f]` or `\s`

    import ConversionStep.Error
    import Text.ConversionStep as Text

    -- match a blank
    "    abc" |> Text.narrowWith blank --> Ok ' '
    "\n\t abc" |> Text.narrowWith blank --> Ok '\n'

    -- But anything else makes it fail.

    "abc"
        |> Text.narrowWith blank
        |> Result.mapError ConversionStep.Error.textMessage
    --> Err "1:1: I was expecting a blank space or new line. I got stuck when I got the character 'a'."


### example: token surrounded by spaces

    import ConversionStep exposing (succeed, atLeast, drop, take, atom)
    import Text.ConversionStep as Text
    import Char.ConversionStep as Char

    "1  +  2"
        |> Text.narrowWith
            (succeed (\x y -> x + y)
                |> take Text.int
                |> drop (atLeast 0 Char.blank |> buildFrom [ () ])
                |> drop (atom '+')
                |> drop (atLeast 0 Char.blank |> buildFrom [ () ])
                |> take Text.int
            )
    --> Ok 3

-}
blank : ConversionStep Char Blank
blank =
    choice
        (\space tab returnVariant formFeed blankCharacter ->
            case blankCharacter of
                Space ->
                    space ()

                Tab ->
                    tab ()

                Return return_ ->
                    returnVariant return_

                FormFeed ->
                    formFeed ()
        )
        (possibility (\() -> Space) (atom ' ')
            >> possibility (\() -> Tab) (atom '\t')
            >> possibility Return return
            >> -- \f
               possibility (\() -> FormFeed) (atom '\u{000C}')
        )
        |> expect "a blank space or new line"


type Sign
    = Minus
    | Plus


sign : ConversionStep Char Sign
sign =
    ConversionStep.choice
        (\plus minus signNarrow ->
            case signNarrow of
                Plus ->
                    plus ()

                Minus ->
                    minus ()
        )
        (possibility (\() -> Minus) (atom '-')
            >> possibility (\() -> Plus) (atom '+')
        )
        |> expect "a sign -|+"
