module AToZ exposing
    ( AToZ(..), Case(..)
    , only
    , lowerChar, upperChar, char
    )

{-| Basic latin letter a|...|z

@docs AToZ, Case


## [`Morph`](Morph#Morph)

@docs only
@docs lowerChar, upperChar, char

-}

import Char.Morph
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


{-| Parses exactly 1 lower or upper case [`AToZ`](#AToZ).

> ℹ️ Equivalent regular expression: `[a-zA-Z]`

    import AToZ
    import String.Morph

    -- match any letter, case insensitive
    "abc" |> Text.narrowTo letter --> Ok 'a'
    "ABC" |> Text.narrowTo letter --> Ok 'A'

    -- But anything else makes it fail.
    import Morph.Error

    "123"
        |> Text.narrowTo letter
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:1: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '1'."


### example `ArraySized.Morph.atLeast n1 (AToZ.char |> Morph.one)`

> ℹ️ Equivalent regular expression: `[a-zA-Z]+`

    import Morph exposing (atLeast)
    import Morph.Error
    import String.Morph as Text

    -- match many letters, case insensitive
    "aBcEY"
        |> Text.narrowTo
            (atLeast n1 aToZ
                |> map String.fromList
            )
    --> Ok "aBcEY"

    "π123abc"
        |> Text.narrowTo
            (atLeast n1 aToZ
                |> map String.fromList
            )
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:1: I was expecting at least 1 letters [a-zA-Z]+. I got stuck when I got 'π'."

    "abc-efg"
        |> Text.narrowTo
            (atLeast n1 aToZ
                |> map String.fromList
            )
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:1: I was expecting at least 1 letters [a-zA-Z]+. I got stuck when I got '-'."

-}
char : Morph { case_ : Case, letter : AToZ } Char
char =
    Morph.choice
        (\lowerVariant upperVariant caseValue ->
            case caseValue.case_ of
                CaseLower ->
                    lowerVariant caseValue.letter

                CaseUpper ->
                    upperVariant caseValue.letter
        )
        |> Morph.try
            (\letter -> { case_ = CaseLower, letter = letter })
            lowerChar
        |> Morph.try
            (\letter -> { case_ = CaseUpper, letter = letter })
            upperChar
        |> Morph.choiceFinish


{-| Match exactly one lowercase letter character.
This is case sensitive.

> ℹ️ Equivalent regular expression: `[a-z]`

    import Morph.Error
    import String.Morph as Text

    -- match a lowercase letter
    "abc" |> Text.narrowTo aToZLower --> Ok 'a'

    -- but anything else makes it fail
    "ABC"
        |> Text.narrowTo aToZLower
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:1: I was expecting a lowercase letter [a-z]. I got stuck when I got the character 'A'."

-}
lowerChar : Morph AToZ Char
lowerChar =
    Morph.choice
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
        |> Morph.try (\() -> A) (Char.Morph.only 'a')
        |> Morph.try (\() -> B) (Char.Morph.only 'b')
        |> Morph.try (\() -> C) (Char.Morph.only 'c')
        |> Morph.try (\() -> D) (Char.Morph.only 'd')
        |> Morph.try (\() -> E) (Char.Morph.only 'e')
        |> Morph.try (\() -> F) (Char.Morph.only 'f')
        |> Morph.try (\() -> G) (Char.Morph.only 'g')
        |> Morph.try (\() -> H) (Char.Morph.only 'h')
        |> Morph.try (\() -> I) (Char.Morph.only 'i')
        |> Morph.try (\() -> J) (Char.Morph.only 'j')
        |> Morph.try (\() -> K) (Char.Morph.only 'k')
        |> Morph.try (\() -> L) (Char.Morph.only 'l')
        |> Morph.try (\() -> M) (Char.Morph.only 'm')
        |> Morph.try (\() -> N) (Char.Morph.only 'n')
        |> Morph.try (\() -> O) (Char.Morph.only 'o')
        |> Morph.try (\() -> P) (Char.Morph.only 'p')
        |> Morph.try (\() -> Q) (Char.Morph.only 'q')
        |> Morph.try (\() -> R) (Char.Morph.only 'r')
        |> Morph.try (\() -> S) (Char.Morph.only 's')
        |> Morph.try (\() -> T) (Char.Morph.only 't')
        |> Morph.try (\() -> U) (Char.Morph.only 'u')
        |> Morph.try (\() -> V) (Char.Morph.only 'v')
        |> Morph.try (\() -> W) (Char.Morph.only 'w')
        |> Morph.try (\() -> X) (Char.Morph.only 'x')
        |> Morph.try (\() -> Y) (Char.Morph.only 'y')
        |> Morph.try (\() -> Z) (Char.Morph.only 'z')
        |> Morph.choiceFinish


{-| Match exactly one uppercase letter character.
This is case sensitive.

> ℹ️ Equivalent regular expression: `[A-Z]`

    import Morph
    import Morph.Error
    import String.Morph as Text

    -- match an uppercase letter
    aToZUpper |> Text.narrowTo "ABC" --> Ok 'A'

    -- but anything else makes it fail
    "abc"
        |> Text.narrowTo aToZUpper
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:1: I was expecting a letter uppercase [A-Z]. I got stuck when I got the character 'a'."

-}
upperChar : Morph AToZ Char
upperChar =
    Morph.choice
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
        |> Morph.try (\() -> A) (Char.Morph.only 'A')
        |> Morph.try (\() -> B) (Char.Morph.only 'B')
        |> Morph.try (\() -> C) (Char.Morph.only 'C')
        |> Morph.try (\() -> D) (Char.Morph.only 'D')
        |> Morph.try (\() -> E) (Char.Morph.only 'E')
        |> Morph.try (\() -> F) (Char.Morph.only 'F')
        |> Morph.try (\() -> G) (Char.Morph.only 'G')
        |> Morph.try (\() -> H) (Char.Morph.only 'H')
        |> Morph.try (\() -> I) (Char.Morph.only 'I')
        |> Morph.try (\() -> J) (Char.Morph.only 'J')
        |> Morph.try (\() -> K) (Char.Morph.only 'K')
        |> Morph.try (\() -> L) (Char.Morph.only 'L')
        |> Morph.try (\() -> M) (Char.Morph.only 'M')
        |> Morph.try (\() -> N) (Char.Morph.only 'N')
        |> Morph.try (\() -> O) (Char.Morph.only 'O')
        |> Morph.try (\() -> P) (Char.Morph.only 'P')
        |> Morph.try (\() -> Q) (Char.Morph.only 'Q')
        |> Morph.try (\() -> R) (Char.Morph.only 'R')
        |> Morph.try (\() -> S) (Char.Morph.only 'S')
        |> Morph.try (\() -> T) (Char.Morph.only 'T')
        |> Morph.try (\() -> U) (Char.Morph.only 'U')
        |> Morph.try (\() -> V) (Char.Morph.only 'V')
        |> Morph.try (\() -> W) (Char.Morph.only 'W')
        |> Morph.try (\() -> X) (Char.Morph.only 'X')
        |> Morph.try (\() -> Y) (Char.Morph.only 'Y')
        |> Morph.try (\() -> Z) (Char.Morph.only 'Z')
        |> Morph.choiceFinish


{-| Match only the specific given broad input. See [`Morph.only`](Morph#only)
-}
only : AToZ -> Morph () AToZ
only =
    Morph.only
        (\aToZ ->
            aToZ
                |> Morph.broadenFrom upperChar
                |> String.fromChar
        )
