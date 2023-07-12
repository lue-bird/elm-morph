module AToZ.Morph exposing
    ( only, broadCase
    , lowerChar, upperChar, char
    )

{-| Morphs for [basic latin letters a|...|z](AToZ#AToZ)


## [`Morph`](Morph#Morph)

@docs only, broadCase
@docs lowerChar, upperChar, char

-}

import AToZ exposing (AToZ(..), Case(..))
import Char.Morph
import Morph exposing (Morph)


{-| [`Morph`](Morph#Morph) that when calling [`toBroad`](Morph#toBroad) always returns a letter with a given [`Case`](AToZ#Case).

For any more complex [`toBroad`](Morph#toBroad) process, use [`oneToOne`](Morph#oneToOne)

For the general idea, check out [`Morph.broad`](Morph#broad)

Example: We want to format it as lowercase but also accept uppercase:

    AToZ.broadCase AToZ.CaseLower
        |> Morph.over AToZ.Morph.char

which would be equivalent to

    Morph.oneToOne .letter (\letter -> { letter = letter, case_ = AToZ.CaseLower })
        |> Morph.over AToZ.Morph.char

-}
broadCase : Case -> Morph AToZ { case_ : Case, letter : AToZ }
broadCase caseSeed =
    Morph.oneToOne .letter (\letter -> { letter = letter, case_ = caseSeed })


{-| Morph a letter [`AToZ`](AToZ#AToZ) and its [`Case`](AToZ#Case)
from a `Char`

> ℹ️ Equivalent regular expression: `[a-zA-Z]`

    import AToZ

    -- match any letter, remembering the case
    'a' |> Morph.toNarrow AToZ.Morph.char
    --> Ok { case_ = AToZ.CaseLower, letter = AToZ.A }

    'A' |> Morph.toNarrow AToZ.Morph.char
    --> Ok { case_ = AToZ.CaseUpper, letter = AToZ.A }

    -- digits, symbols etc makes it fail
    '1'
        |> Morph.toNarrow AToZ.Morph.char
        |> Result.toMaybe
    --> Nothing

Combine with [`broadCase`](#broadCase) to provide a default casing on [`toBroad`](Morph#toBroad)

Combine with [`Morph.one`](Morph#one) to use it in a [`MorphRow`](Morph#MorphRow)


### example `Morph.whilePossible (AToZ.Morph.char |> Morph.one)`

> ℹ️ Equivalent regular expression: `[a-zA-Z]*`

    import Morph
    import List.Morph

    -- multiple letters, remembering their cases
    "aBc"
        |> Morph.toNarrow
            (Morph.whilePossible (AToZ.Morph.char |> Morph.one)
                |> Morph.rowFinish
                |> Morph.over List.Morph.string
            )
    --> Ok
    -->     [ { case_ = AToZ.CaseLower, letter = AToZ.A }
    -->     , { case_ = AToZ.CaseUpper, letter = AToZ.B }
    -->     , { case_ = AToZ.CaseLower, letter = AToZ.C }
    -->     ]

    -- greek letters, space, -, ', . etc are not [a-Z]
    "abπc"
        |> Morph.toNarrow
            (Morph.broad []
                |> Morph.overRow
                    (Morph.whilePossible (AToZ.Morph.char |> Morph.one))
                |> Morph.rowFinish
                |> Morph.over List.Morph.string
            )
        |> Result.toMaybe
    --> Nothing

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
        |> Morph.named "a|..|Z"


choiceAToZ : (() -> a) -> (() -> a) -> (() -> a) -> (() -> a) -> (() -> a) -> (() -> a) -> (() -> a) -> (() -> a) -> (() -> a) -> (() -> a) -> (() -> a) -> (() -> a) -> (() -> a) -> (() -> a) -> (() -> a) -> (() -> a) -> (() -> a) -> (() -> a) -> (() -> a) -> (() -> a) -> (() -> a) -> (() -> a) -> (() -> a) -> (() -> a) -> (() -> a) -> (() -> a) -> AToZ -> a
choiceAToZ =
    \a b c d e f g h i j k l m n o p q r s t u v w x y z aToZNarrow ->
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


{-| Match a lowercase letter `Char`.

> ℹ️ Equivalent regular expression: `[a-z]`

    import Morph
    import List.Morph
    import AToZ exposing (AToZ(..))

    -- match a lowercase letter
    'a' |> Morph.toNarrow AToZ.Morph.lowerChar
    --> Ok A

    -- but anything else makes it fail
    'A'
        |> Morph.toNarrow AToZ.Morph.lowerChar
        |> Result.toMaybe
    --> Nothing

Combine with [`Morph.one`](Morph#one) to use it in a [`MorphRow`](Morph#MorphRow)

This is different from

    AToZ.Morph.broadCase AToZ.CaseLower
        |> Morph.over AToZ.Morph.char

which parses any case but only prints lowercase

-}
lowerChar : Morph AToZ Char
lowerChar =
    Morph.choice choiceAToZ
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
        |> Morph.named "a|..|z"


{-| Match an uppercase letter `Char`.

> ℹ️ Equivalent regular expression: `[A-Z]`

    import Morph
    import List.Morph
    import AToZ exposing (AToZ(..))

    -- match an uppercase a-z letter
    'A' |> Morph.toNarrow AToZ.Morph.upperChar
    --> Ok A

    -- but anything else makes it fail
    'a'
        |> Morph.toNarrow AToZ.Morph.upperChar
        |> Result.toMaybe
    --> Nothing

Combine with [`Morph.one`](Morph#one) to use it in a [`MorphRow`](Morph#MorphRow)

This is different from

    AToZ.Morph.broadCase AToZ.CaseUpper
        |> Morph.over AToZ.Morph.char

which parses any case but only prints uppercase

-}
upperChar : Morph AToZ Char
upperChar =
    Morph.choice choiceAToZ
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
        |> Morph.named "A|..|Z"


{-| Match only the specific given broad input. See [`Morph.only`](Morph#only)
-}
only : AToZ -> Morph () AToZ
only =
    Morph.only
        (\aToZ ->
            aToZ
                |> Morph.toBroad upperChar
                |> String.fromChar
        )
