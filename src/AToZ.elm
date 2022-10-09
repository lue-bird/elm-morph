module AToZ exposing
    ( AToZ(..), lowerChar, upperChar
    , Case(..), char
    )

{-| Basic latin letter a|...|z

@docs AToZ, lowerChar, upperChar
@docs Case, char

-}

import Char.Morph
import Choice
import Morph exposing (Morph)


{-| Basic latin letter without [case](#Case) information
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


{-| Either lower or upper case
-}
type Case
    = CaseLower
    | CaseUpper


{-| Parses exactly 1 ASCII lower or upper case letter character.
This is case insensitive.

> ℹ️ Equivalent regular expression: `[a-zA-Z]`

    import Char.Morph exposing (letter)
    import String.Morph as Text

    -- match any letter, case insensitive
    "abc" |> Text.narrowWith letter --> Ok 'a'
    "ABC" |> Text.narrowWith letter --> Ok 'A'

    -- But anything else makes it fail.
    import Morph.Error

    "123"
        |> Text.narrowWith letter
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:1: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '1'."


### example `atLeast n1 letter`

> ℹ️ Equivalent regular expression: `[a-zA-Z]+`

    import Morph exposing (atLeast)
    import Morph.Error
    import String.Morph as Text

    -- match many letters, case insensitive
    "aBcEY"
        |> Text.narrowWith
            (atLeast n1 aToZ
                |> map String.fromList
            )
    --> Ok "aBcEY"

    "π123abc"
        |> Text.narrowWith
            (atLeast n1 aToZ
                |> map String.fromList
            )
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:1: I was expecting at least 1 letters [a-zA-Z]+. I got stuck when I got 'π'."

    "abc-efg"
        |> Text.narrowWith
            (atLeast n1 aToZ
                |> map String.fromList
            )
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:1: I was expecting at least 1 letters [a-zA-Z]+. I got stuck when I got '-'."

-}
char : Morph { case_ : Case, letter : AToZ } Char
char =
    cased { lower = lowerChar, upper = upperChar }


cased :
    { lower :
        Morph possibilityNarrow broad
    , upper :
        Morph possibilityNarrow broad
    }
    ->
        Morph
            { case_ : Case, letter : possibilityNarrow }
            broad
cased casedLetters =
    Choice.between
        (\lowerVariant upperVariant caseValue ->
            case caseValue.case_ of
                CaseLower ->
                    lowerVariant caseValue.letter

                CaseUpper ->
                    upperVariant caseValue.letter
        )
        |> Choice.try
            (\letter -> { case_ = CaseLower, letter = letter })
            casedLetters.lower
        |> Choice.try
            (\letter -> { case_ = CaseUpper, letter = letter })
            casedLetters.upper
        |> Choice.finish


{-| Match exactly one lowercase letter character.
This is case sensitive.

> ℹ️ Equivalent regular expression: `[a-z]`

    import Morph.Error
    import String.Morph as Text

    -- match a lowercase letter
    "abc" |> Text.narrowWith aToZLower --> Ok 'a'

    -- but anything else makes it fail
    "ABC"
        |> Text.narrowWith aToZLower
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:1: I was expecting a lowercase letter [a-z]. I got stuck when I got the character 'A'."

-}
lowerChar : Morph AToZ Char
lowerChar =
    Choice.between
        (\a b c d e f g h i j k l m n o p q r s t u v w x y z aToZNarrow ->
            case aToZNarrow of
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
        |> Choice.try (\() -> A) (Char.Morph.only 'a')
        |> Choice.try (\() -> B) (Char.Morph.only 'b')
        |> Choice.try (\() -> C) (Char.Morph.only 'c')
        |> Choice.try (\() -> D) (Char.Morph.only 'd')
        |> Choice.try (\() -> E) (Char.Morph.only 'e')
        |> Choice.try (\() -> F) (Char.Morph.only 'f')
        |> Choice.try (\() -> G) (Char.Morph.only 'g')
        |> Choice.try (\() -> H) (Char.Morph.only 'h')
        |> Choice.try (\() -> I) (Char.Morph.only 'i')
        |> Choice.try (\() -> J) (Char.Morph.only 'j')
        |> Choice.try (\() -> K) (Char.Morph.only 'k')
        |> Choice.try (\() -> L) (Char.Morph.only 'l')
        |> Choice.try (\() -> M) (Char.Morph.only 'm')
        |> Choice.try (\() -> N) (Char.Morph.only 'n')
        |> Choice.try (\() -> O) (Char.Morph.only 'o')
        |> Choice.try (\() -> P) (Char.Morph.only 'p')
        |> Choice.try (\() -> Q) (Char.Morph.only 'q')
        |> Choice.try (\() -> R) (Char.Morph.only 'r')
        |> Choice.try (\() -> S) (Char.Morph.only 's')
        |> Choice.try (\() -> T) (Char.Morph.only 't')
        |> Choice.try (\() -> U) (Char.Morph.only 'u')
        |> Choice.try (\() -> V) (Char.Morph.only 'v')
        |> Choice.try (\() -> W) (Char.Morph.only 'w')
        |> Choice.try (\() -> X) (Char.Morph.only 'x')
        |> Choice.try (\() -> Y) (Char.Morph.only 'y')
        |> Choice.try (\() -> Z) (Char.Morph.only 'z')
        |> Choice.finish


{-| Match exactly one uppercase letter character.
This is case sensitive.

> ℹ️ Equivalent regular expression: `[A-Z]`

    import Morph
    import Morph.Error
    import String.Morph as Text

    -- match an uppercase letter
    aToZUpper |> Text.narrowWith "ABC" --> Ok 'A'

    -- but anything else makes it fail
    "abc"
        |> Text.narrowWith aToZUpper
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:1: I was expecting a letter uppercase [A-Z]. I got stuck when I got the character 'a'."

-}
upperChar : Morph AToZ Char
upperChar =
    Choice.between
        (\a b c d e f g h i j k l m n o p q r s t u v w x y z aToZNarrow ->
            case aToZNarrow of
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
        |> Choice.try (\() -> A) (Char.Morph.only 'A')
        |> Choice.try (\() -> B) (Char.Morph.only 'B')
        |> Choice.try (\() -> C) (Char.Morph.only 'C')
        |> Choice.try (\() -> D) (Char.Morph.only 'D')
        |> Choice.try (\() -> E) (Char.Morph.only 'E')
        |> Choice.try (\() -> F) (Char.Morph.only 'F')
        |> Choice.try (\() -> G) (Char.Morph.only 'G')
        |> Choice.try (\() -> H) (Char.Morph.only 'H')
        |> Choice.try (\() -> I) (Char.Morph.only 'I')
        |> Choice.try (\() -> J) (Char.Morph.only 'J')
        |> Choice.try (\() -> K) (Char.Morph.only 'K')
        |> Choice.try (\() -> L) (Char.Morph.only 'L')
        |> Choice.try (\() -> M) (Char.Morph.only 'M')
        |> Choice.try (\() -> N) (Char.Morph.only 'N')
        |> Choice.try (\() -> O) (Char.Morph.only 'O')
        |> Choice.try (\() -> P) (Char.Morph.only 'P')
        |> Choice.try (\() -> Q) (Char.Morph.only 'Q')
        |> Choice.try (\() -> R) (Char.Morph.only 'R')
        |> Choice.try (\() -> S) (Char.Morph.only 'S')
        |> Choice.try (\() -> T) (Char.Morph.only 'T')
        |> Choice.try (\() -> U) (Char.Morph.only 'U')
        |> Choice.try (\() -> V) (Char.Morph.only 'V')
        |> Choice.try (\() -> W) (Char.Morph.only 'W')
        |> Choice.try (\() -> X) (Char.Morph.only 'X')
        |> Choice.try (\() -> Y) (Char.Morph.only 'Y')
        |> Choice.try (\() -> Z) (Char.Morph.only 'Z')
        |> Choice.finish
