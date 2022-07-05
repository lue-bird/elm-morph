module Digit.Morph exposing
    ( N0To9(..), n0To9, n0To9ToInt
    , N1To9(..), n1To9, n1To9ToInt
    )

{-| [`Morph`](Morph#Morph)ing to and from a digit.

@docs N0To9, n0To9, n0To9ToInt
@docs N1To9, n1To9, n1To9ToInt

-}

import Morph exposing (Morph)


{-| Any digit
-}
type N0To9
    = N0
    | N1To9 N1To9


{-| A non-0 digit
-}
type N1To9
    = N1
    | N2
    | N3
    | N4
    | N5
    | N6
    | N7
    | N8
    | N9


{-| [`Morph`](Morph#Morph) [`N0To9`](#N0To9) to `Int`
-}
n0To9ToInt : Morph N0To9 Int (Morph.Error Int variantExpectation_)
n0To9ToInt =
    Morph.choice
        (\n0Variant n1To9Variant n0To9Narrow ->
            case n0To9Narrow of
                N0 ->
                    n0Variant ()

                N1To9 n1To9Value ->
                    n1To9Variant n1To9Value
        )
        |> Morph.possibility (\() -> N0) (Morph.specific 0)
        |> Morph.possibility N1To9 n1To9ToInt
        |> Morph.choiceFinish


{-| [`Morph`](Morph#Morph) [`N1To9`](#N1To9) to `Int`
-}
n1To9ToInt : Morph N1To9 Int (Morph.Error Int variantExpectation_)
n1To9ToInt =
    Morph.choice
        (\n1 n2 n3 n4 n5 n6 n7 n8 n9 n1To9Narrow ->
            case n1To9Narrow of
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
        |> Morph.possibility (\() -> N1) (Morph.specific 1)
        |> Morph.possibility (\() -> N2) (Morph.specific 2)
        |> Morph.possibility (\() -> N3) (Morph.specific 3)
        |> Morph.possibility (\() -> N4) (Morph.specific 4)
        |> Morph.possibility (\() -> N5) (Morph.specific 5)
        |> Morph.possibility (\() -> N6) (Morph.specific 6)
        |> Morph.possibility (\() -> N7) (Morph.specific 7)
        |> Morph.possibility (\() -> N8) (Morph.specific 8)
        |> Morph.possibility (\() -> N9) (Morph.specific 9)
        |> Morph.choiceFinish


{-| Match a [0|...|9](#N0To9) digit `Char`.

> ℹ️ Equivalent regular expression: `[0-9]` or `\d`

    import MorphRow.Error
    import Morph.TextRow as Text

    -- match a digit
    "123" |> Text.narrowWith digit --> Ok '1'
    "3.14" |> Text.narrowWith digit --> Ok '3'

    -- but anything else makes it fail
    "abc"
        |> Text.narrowWith digit
        |> Result.mapError MorphRow.Error.textMessage
    --> Err "1:1: I was expecting a digit [0-9]. I got stuck when I got the character 'a'."


### example: `atLeast 1 Digit.Morph.n0To9`

> ℹ️ Equivalent regular expression: `[0-9]+` or `\d+`

    import MorphRow exposing (map)
    import MorphRow.Error
    import Morph.Char as Char
    import Morph.TextRow as Text

    "123abc" |> Text.narrowWith (atLeast 1 Digit.Morph.n0To9) --> Ok "123"

    "abc123"
        |> Text.narrowWith
            (atLeast 1 Digit.Morph.n0To9
                |> map String.fromList
            )
        |> Result.mapError MorphRow.Error.textMessage
    --> Err "1:1: I was expecting at least 1 digit [0-9]. I got stuck when I got the character 'a'."

-}
n0To9 : Morph N0To9 Char (Morph.Error Char variantExpectation_)
n0To9 =
    Morph.choice
        (\n0Variant n1To9Variant n0To9Narrow ->
            case n0To9Narrow of
                N0 ->
                    n0Variant ()

                N1To9 n1To9Value ->
                    n1To9Variant n1To9Value
        )
        |> Morph.possibility (\() -> N0) (Morph.specific '0')
        |> Morph.possibility N1To9 n1To9
        |> Morph.choiceFinish


{-| Match a [1|...|9](#N1To9) digit `Char`.
-}
n1To9 : Morph N1To9 Char (Morph.Error Char variantExpectation_)
n1To9 =
    Morph.choice
        (\n1 n2 n3 n4 n5 n6 n7 n8 n9 n1To9Narrow ->
            case n1To9Narrow of
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
        |> Morph.possibility (\() -> N1) (Morph.specific '1')
        |> Morph.possibility (\() -> N2) (Morph.specific '2')
        |> Morph.possibility (\() -> N3) (Morph.specific '3')
        |> Morph.possibility (\() -> N4) (Morph.specific '4')
        |> Morph.possibility (\() -> N5) (Morph.specific '5')
        |> Morph.possibility (\() -> N6) (Morph.specific '6')
        |> Morph.possibility (\() -> N7) (Morph.specific '7')
        |> Morph.possibility (\() -> N8) (Morph.specific '8')
        |> Morph.possibility (\() -> N9) (Morph.specific '9')
        |> Morph.choiceFinish
