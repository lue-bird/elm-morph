module Char.Conversion exposing (N0To9(..), n0To9ToInt)

import Conversion exposing (Conversion, Expected(..))


{-| A digit
-}
type N0To9
    = N0
    | N1
    | N2
    | N3
    | N4
    | N5
    | N6
    | N7
    | N8
    | N9


n0To9ToInt : Conversion N0To9 Int (Expected { n0To9 : () })
n0To9ToInt =
    { broaden =
        \n0To9 ->
            case n0To9 of
                N0 ->
                    0

                N1 ->
                    1

                N2 ->
                    2

                N3 ->
                    3

                N4 ->
                    4

                N5 ->
                    5

                N6 ->
                    6

                N7 ->
                    7

                N8 ->
                    8

                N9 ->
                    9
    , narrow =
        \int ->
            case int of
                0 ->
                    N0 |> Ok

                1 ->
                    N1 |> Ok

                2 ->
                    N2 |> Ok

                3 ->
                    N3 |> Ok

                4 ->
                    N4 |> Ok

                5 ->
                    N5 |> Ok

                6 ->
                    N6 |> Ok

                7 ->
                    N7 |> Ok

                8 ->
                    N8 |> Ok

                9 ->
                    N9 |> Ok

                _ ->
                    Expected { n0To9 = () } |> Err
    }
