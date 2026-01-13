module Int.Morph exposing (chars, positiveChars)

{-| [`Morph`](Morph#Morph) for an [`Int`](https://dark.elm.dmy.fr/packages/elm/core/latest/Basics#Int)

@docs chars, positiveChars

-}

import Char.Morph
import Morph exposing (MorphRow)
import N0To9
import N1To9 exposing (N1To9)
import String.Morph


chars : MorphRow Int Char
chars =
    Morph.choice
        (\negative n0 positive int ->
            case compare int 0 of
                LT ->
                    negative (Basics.abs int)

                EQ ->
                    n0 ()

                GT ->
                    positive int
        )
        |> Morph.rowTry Basics.negate
            (Morph.narrow identity
                |> Morph.match (String.Morph.only "-")
                |> Morph.grab identity positiveChars
            )
        |> Morph.rowTry (\() -> 0) (String.Morph.only "0")
        |> Morph.rowTry identity positiveChars
        |> Morph.choiceFinish


{-| Only use for ints known to be positive
-}
positiveChars : MorphRow Int Char
positiveChars =
    Morph.narrow
        (\digit0 digit1Up ->
            ((digit0 |> N1To9.toInt) * (10 ^ (digit1Up |> List.length)))
                + (digit1Up |> digitsToInt)
        )
        |> Morph.grab
            (\int ->
                (int // (10 ^ Basics.ceiling (Basics.logBase 10 (Basics.toFloat int) - 1)))
                    |> N1To9.fromInt
                    |> Maybe.withDefault N1To9.N1
            )
            (Morph.one N1To9.morphChar)
        |> Morph.grab
            (\int ->
                positiveIntToDigits [] int |> Debug.log ("positiveIntToDigits, given " ++ String.fromInt int) |> List.drop 1
            )
            (Morph.whilePossible (Morph.one N0To9.morphChar))


positiveIntToDigits : List N0To9.N0To9 -> Int -> List N0To9.N0To9
positiveIntToDigits soFar int =
    if int <= 0 then
        soFar

    else
        positiveIntToDigits
            ((int |> Basics.remainderBy 10 |> N0To9.fromInt |> Maybe.withDefault N0To9.N0)
                :: soFar
            )
            (int // 10)


digitsToInt : List N0To9.N0To9 -> Int
digitsToInt digits =
    digits |> List.foldl (\digit soFar -> (digit |> N0To9.toInt) + soFar * 10) 0
