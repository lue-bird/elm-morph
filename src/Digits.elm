module Digits exposing (fromIntPositive, toIntPositive)

{-| **sShould not be exposed**
-}

import Emptiable exposing (Emptiable)
import N exposing (In, InFixed, N, N0, N9, Up0, Up1, Up9, n0, n1, n9)
import Stack exposing (StackTopBelow)


fromIntPositive :
    Int
    ->
        Emptiable
            (StackTopBelow
                (N (In (Up1 digit0MinX_) (Up9 digit0MaxX_)))
                (N (In (Up0 digit1UpMinX_) (Up9 digit1UpMaxX_)))
            )
            never_
fromIntPositive =
    \int ->
        let
            highest10Exponent : Int
            highest10Exponent =
                logBase 10 (int |> Basics.toFloat) |> floor
        in
        Stack.topDown
            (int |> digitFor10Exponent highest10Exponent |> N.atLeast n1 |> N.maxTo n9)
            (List.range 0 (highest10Exponent - 1)
                |> List.map
                    (\n10Exponent ->
                        int |> digitFor10Exponent n10Exponent
                    )
            )


digitFor10Exponent : Int -> (Int -> N (In (Up0 minX_) (Up9 maxX_)))
digitFor10Exponent n10Exponent =
    \int ->
        (int // (10 ^ n10Exponent))
            |> remainderBy 10
            |> N.intIn ( n0, n9 )


toIntPositive :
    Emptiable
        (StackTopBelow
            (N (InFixed topMin_ N9))
            (N (InFixed N0 N9))
        )
        Never
    -> Int
toIntPositive =
    \digits ->
        let
            lastIndex =
                (digits |> Stack.length) - 1
        in
        digits
            |> Stack.topMap (N.minTo n0)
            |> Stack.map
                (\{ index } digit ->
                    (digit |> N.toInt) * (10 ^ (lastIndex - index))
                )
            |> Stack.sum
