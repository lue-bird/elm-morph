module Morph.Char exposing
    ( caseAny
    , Blank(..), blank
    , Return(..), return
    , AToZ(..), Case(..), aToZ
    , aToZLower, aToZUpper
    )

{-| [`Morph`](Morph#Morph)ing from and to a character.

@docs caseAny
@docs Blank, blank
@docs Return, return


## a|...|z

@docs AToZ, Case, aToZ
@docs aToZLower, aToZUpper

-}

import Morph exposing (Morph, broadenFrom, choice)


{-| Match a specific single character.
This is case insensitive.

    import MorphRow.Error
    import Morph.CharRow as Char
    import Morph.TextRow as Text

    -- match a specific character, case insensitive
    "a" |> Text.narrowWith (Morph.Char.caseAny 'a' |> atom) --> Ok 'a'
    "A" |> Text.narrowWith (Morph.Char.caseAny 'a' |> atom) --> Ok 'A'

    "123"
        |> Text.narrowWith (Char.caseAny 'a')
        |> Result.mapError MorphRow.Error.textMessage
    --> Err "1:1: I was expecting the character 'a' (case insensitive). I got stuck when I got the character '1'."

-}
caseAny :
    Char
    ->
        Morph
            ()
            Char
            (Morph.Error Char variantExpectation_)
caseAny expectedChar =
    let
        expectedCase =
            if expectedChar |> Char.isUpper then
                CaseUpper

            else
                CaseLower
    in
    broadenFrom expectedCase
        |> Morph.over
            (choice
                (\caseLowerVariant caseUpperVariant caseNarrow ->
                    case caseNarrow of
                        CaseLower ->
                            caseLowerVariant ()

                        CaseUpper ->
                            caseUpperVariant ()
                )
                |> Morph.possibility (\() -> CaseLower)
                    (Morph.specific (expectedChar |> Char.toLower))
                |> Morph.possibility (\() -> CaseUpper)
                    (Morph.specific (expectedChar |> Char.toUpper))
                |> Morph.choiceFinish
            )


{-| Parses exactly 1 ASCII lower or upper case letter character.
This is case insensitive.

> ℹ️ Equivalent regular expression: `[a-zA-Z]`

    import Morph.CharRow exposing (letter)
    import Morph.TextRow as Text

    -- match any letter, case insensitive
    "abc" |> Text.narrowWith letter --> Ok 'a'
    "ABC" |> Text.narrowWith letter --> Ok 'A'

    -- But anything else makes it fail.
    import MorphRow.Error

    "123"
        |> Text.narrowWith letter
        |> Result.mapError MorphRow.Error.textMessage
    --> Err "1:1: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '1'."


### example `atLeast 1 letter`

> ℹ️ Equivalent regular expression: `[a-zA-Z]+`

    import MorphRow exposing (atLeast)
    import MorphRow.Error
    import Morph.TextRow as Text

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
        |> Result.mapError MorphRow.Error.textMessage
    --> Err "1:1: I was expecting at least 1 letters [a-zA-Z]+. I got stuck when I got 'π'."

    "abc-efg"
        |> Text.narrowWith
            (atLeast 1 aToZ
                |> map String.fromList
            )
        |> Result.mapError MorphRow.Error.textMessage
    --> Err "1:1: I was expecting at least 1 letters [a-zA-Z]+. I got stuck when I got '-'."

-}
aToZ :
    Morph
        { case_ : Case, letter : AToZ }
        Char
        (Morph.Error Char variantExpectation_)
aToZ =
    cased { lower = aToZLower, upper = aToZUpper }


cased :
    { lower :
        Morph
            possibilityNarrow
            broad
            (Morph.Error atom variantExpectation)
    , upper :
        Morph
            possibilityNarrow
            broad
            (Morph.Error atom variantExpectation)
    }
    ->
        Morph
            { case_ : Case, letter : possibilityNarrow }
            broad
            (Morph.Error atom variantExpectation)
cased casedLetters =
    choice
        (\lower upper caseValue ->
            case caseValue.case_ of
                CaseLower ->
                    lower caseValue.letter

                CaseUpper ->
                    upper caseValue.letter
        )
        |> Morph.possibility
            (\letter -> { case_ = CaseLower, letter = letter })
            casedLetters.lower
        |> Morph.possibility
            (\letter -> { case_ = CaseUpper, letter = letter })
            casedLetters.upper
        |> Morph.choiceFinish


aToZInCase : (Char -> Char) -> Morph AToZ Char (Morph.Error Char variantExpectation_)
aToZInCase aToZTransformation =
    choice
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
        |> Morph.possibility (\() -> A) (Morph.specific ('a' |> aToZTransformation))
        |> Morph.possibility (\() -> B) (Morph.specific ('b' |> aToZTransformation))
        |> Morph.possibility (\() -> C) (Morph.specific ('c' |> aToZTransformation))
        |> Morph.possibility (\() -> D) (Morph.specific ('d' |> aToZTransformation))
        |> Morph.possibility (\() -> E) (Morph.specific ('e' |> aToZTransformation))
        |> Morph.possibility (\() -> F) (Morph.specific ('f' |> aToZTransformation))
        |> Morph.possibility (\() -> G) (Morph.specific ('g' |> aToZTransformation))
        |> Morph.possibility (\() -> H) (Morph.specific ('h' |> aToZTransformation))
        |> Morph.possibility (\() -> I) (Morph.specific ('i' |> aToZTransformation))
        |> Morph.possibility (\() -> J) (Morph.specific ('j' |> aToZTransformation))
        |> Morph.possibility (\() -> K) (Morph.specific ('k' |> aToZTransformation))
        |> Morph.possibility (\() -> L) (Morph.specific ('l' |> aToZTransformation))
        |> Morph.possibility (\() -> M) (Morph.specific ('m' |> aToZTransformation))
        |> Morph.possibility (\() -> N) (Morph.specific ('n' |> aToZTransformation))
        |> Morph.possibility (\() -> O) (Morph.specific ('o' |> aToZTransformation))
        |> Morph.possibility (\() -> P) (Morph.specific ('p' |> aToZTransformation))
        |> Morph.possibility (\() -> Q) (Morph.specific ('q' |> aToZTransformation))
        |> Morph.possibility (\() -> R) (Morph.specific ('r' |> aToZTransformation))
        |> Morph.possibility (\() -> S) (Morph.specific ('s' |> aToZTransformation))
        |> Morph.possibility (\() -> T) (Morph.specific ('t' |> aToZTransformation))
        |> Morph.possibility (\() -> U) (Morph.specific ('u' |> aToZTransformation))
        |> Morph.possibility (\() -> V) (Morph.specific ('v' |> aToZTransformation))
        |> Morph.possibility (\() -> W) (Morph.specific ('w' |> aToZTransformation))
        |> Morph.possibility (\() -> X) (Morph.specific ('x' |> aToZTransformation))
        |> Morph.possibility (\() -> Y) (Morph.specific ('y' |> aToZTransformation))
        |> Morph.possibility (\() -> Z) (Morph.specific ('z' |> aToZTransformation))
        |> Morph.choiceFinish


{-| Match exactly one lowercase letter character.
This is case sensitive.

> ℹ️ Equivalent regular expression: `[a-z]`

    import MorphRow.Error
    import Morph.TextRow as Text

    -- match a lowercase letter
    "abc" |> Text.narrowWith aToZLower --> Ok 'a'

    -- but anything else makes it fail
    "ABC"
        |> Text.narrowWith aToZLower
        |> Result.mapError MorphRow.Error.textMessage
    --> Err "1:1: I was expecting a lowercase letter [a-z]. I got stuck when I got the character 'A'."

-}
aToZLower : Morph AToZ Char (Morph.Error Char variantExpectation_)
aToZLower =
    aToZInCase Char.toLower


{-| Match exactly one uppercase letter character.
This is case sensitive.

> ℹ️ Equivalent regular expression: `[A-Z]`

    import MorphRow
    import MorphRow.Error
    import Morph.TextRow as Text

    -- match an uppercase letter
    aToZUpper |> Text.narrowWith "ABC" --> Ok 'A'

    -- but anything else makes it fail
    "abc"
        |> Text.narrowWith aToZUpper
        |> Result.mapError MorphRow.Error.textMessage
    --> Err "1:1: I was expecting a letter uppercase [A-Z]. I got stuck when I got the character 'a'."

-}
aToZUpper : Morph AToZ Char (Morph.Error Char variantExpectation_)
aToZUpper =
    aToZInCase Char.toUpper


{-| Match a line break character: Either

  - new line `'\n'`
  - carriage return `'\r'`

> ℹ️ Equivalent regular expression: `[\n\r]`

    import MorphRow.Error
    import Morph.TextRow as Text

    -- match a blank
    "\n\t abc" |> Text.narrowWith blank --> Ok '\n'

    -- anything else makes it fail
    "abc"
        |> Text.narrowWith blank
        |> Result.mapError MorphRow.Error.textMessage
    --> Err "1:1: I was expecting a blank space or new line. I got stuck when I got 'a'."

-}
return : Morph Return Char (Morph.Error Char variantExpectation_)
return =
    choice
        (\newLineVariant carriageReturnVariant returnNarrow ->
            case returnNarrow of
                NewLine ->
                    newLineVariant ()

                CarriageReturn ->
                    carriageReturnVariant ()
        )
        |> Morph.possibility (\() -> NewLine)
            (Morph.specific '\n')
        |> Morph.possibility (\() -> CarriageReturn)
            -- \r
            (Morph.specific '\u{000D}')
        |> Morph.choiceFinish


{-| Match a unicode blank character, including new line: One of

  - space `' '`
  - tab `'\t'`
  - new line `'\n'`
  - carriage return `'\r'`
  - form feed `'\f'`

> ℹ️ Equivalent regular expression: `[ \t\n\r\f]` or `\s`

    import MorphRow.Error
    import Morph.TextRow as Text

    -- match a blank
    "    abc" |> Text.narrowWith blank --> Ok ' '
    "\n\t abc" |> Text.narrowWith blank --> Ok '\n'

    -- But anything else makes it fail.

    "abc"
        |> Text.narrowWith blank
        |> Result.mapError MorphRow.Error.textMessage
    --> Err "1:1: I was expecting a blank space or new line. I got stuck when I got the character 'a'."


### example: token surrounded by spaces

    import MorphRow exposing (succeed, atLeast, drop, take, atom)
    import Morph.TextRow as Text
    import Morph.CharRow as Char

    "1  +  2"
        |> Text.narrowWith
            (succeed (\x y -> x + y)
                |> grab Text.int
                |> skip (atLeast 0 Char.blank |> broadenFrom [ () ])
                |> skip (atom '+')
                |> skip (atLeast 0 Char.blank |> broadenFrom [ () ])
                |> grab Text.int
            )
    --> Ok 3

-}
blank : Morph Blank Char (Morph.Error Char String)
blank =
    Morph.expect "a blank character"
        (choice
            (\spaceVariant tabVariant returnVariant formFeedVariant blankNarrow ->
                case blankNarrow of
                    Space ->
                        spaceVariant ()

                    Tab ->
                        tabVariant ()

                    Return return_ ->
                        returnVariant return_

                    FormFeed ->
                        formFeedVariant ()
            )
            |> Morph.possibility (\() -> Space) (Morph.specific ' ')
            |> Morph.possibility (\() -> Tab) (Morph.specific '\t')
            |> Morph.possibility Return return
            |> Morph.possibility (\() -> FormFeed)
                -- \f
                (Morph.specific '\u{000C}')
            |> Morph.choiceFinish
        )


{-| Letter casing. Either lower or upper
-}
type Case
    = CaseLower
    | CaseUpper


{-| Latin letter without case information
-}
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


{-| Invisible spacing character.
-}
type Blank
    = Space
    | Tab
    | Return Return
    | FormFeed


{-| Line break character
-}
type Return
    = NewLine
    | CarriageReturn
