module Whole exposing (fromIntPositive, rowChar, toIntPositive)

{-| **sShould not be exposed**
-}

import ArraySized.Morph
import Decimal.Internal exposing (Whole)
import Emptiable exposing (Emptiable)
import Morph exposing (MorphRow, grab, one)
import N exposing (In, N, N0, N9, Up0, Up1, Up9, n0, n1, n10, n9)
import N.Morph
import Stack exposing (Stacked)
import Stack.Morph


fromIntPositive : Int -> Whole
fromIntPositive =
    \int ->
        let
            highest10Exponent : Int
            highest10Exponent =
                logBase 10 (int |> Basics.toFloat) |> floor
        in
        { first = int |> digitFor10Exponent highest10Exponent |> N.toIn ( n1, n9 ) |> N.inToNumber
        , afterFirst =
            List.range 0 (highest10Exponent - 1)
                |> List.map
                    (\n10Exponent ->
                        int |> digitFor10Exponent n10Exponent |> N.inToNumber
                    )
                |> Stack.fromList
        }


digitFor10Exponent : Int -> (Int -> N (In (Up0 minX_) (Up9 maxX_)))
digitFor10Exponent n10Exponent =
    \int ->
        (int // (10 ^ n10Exponent))
            |> N.intModBy n10


toIntPositive : Whole -> Int
toIntPositive =
    \whole ->
        Stack.onTopLay
            (whole.first |> N.inToOn |> N.minTo n0 |> N.inToNumber)
            whole.afterFirst
            |> stackToIntPositive


stackToIntPositive : Emptiable (Stacked (N range_)) possiblyOrNever_ -> Int
stackToIntPositive =
    \digits ->
        let
            lastIndex =
                (digits |> Stack.length) - 1
        in
        digits
            |> Stack.map
                (\{ index } digit ->
                    (digit |> N.toInt) * (10 ^ (lastIndex - index))
                )
            |> Stack.sum


rowChar : MorphRow Whole Char
rowChar =
    Morph.to "whole"
        (Morph.succeed (\first afterFirst -> { first = first, afterFirst = afterFirst })
            |> grab .first
                (N.Morph.inOn
                    |> Morph.over (N.Morph.in_ ( n1, n9 ))
                    |> Morph.over N.Morph.char
                    |> one
                )
            |> grab .afterFirst
                (Stack.Morph.list
                    |> Morph.over ArraySized.Morph.toList
                    |> Morph.overRow
                        (ArraySized.Morph.atLeast
                            (N.Morph.inOn
                                |> Morph.over (N.Morph.in_ ( n0, n9 ))
                                |> Morph.over N.Morph.char
                                |> one
                            )
                            n0
                        )
                )
        )
