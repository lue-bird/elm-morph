module N1To9 exposing (N1To9(..), fromChar, fromInt, morphChar, morphInt, toChar, toInt)

import Char.Morph
import Morph exposing (Morph)
import Util


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


toInt : N1To9 -> Int
toInt n1To9 =
    case n1To9 of
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


fromInt : Int -> Maybe N1To9
fromInt int =
    case int of
        1 ->
            Just N1

        2 ->
            Just N2

        3 ->
            Just N3

        4 ->
            Just N4

        5 ->
            Just N5

        6 ->
            Just N6

        7 ->
            Just N7

        8 ->
            Just N8

        9 ->
            Just N9

        _ ->
            Nothing


morphInt : Morph N1To9 Int
morphInt =
    Morph.custom "1-9"
        { toNarrow =
            \int ->
                fromInt int
                    |> Util.resultFromMaybeLazy (\() -> String.fromInt int)
        , toBroad = toInt
        }


toChar : N1To9 -> Char
toChar n1To9 =
    case n1To9 of
        N1 ->
            '1'

        N2 ->
            '2'

        N3 ->
            '3'

        N4 ->
            '4'

        N5 ->
            '5'

        N6 ->
            '6'

        N7 ->
            '7'

        N8 ->
            '8'

        N9 ->
            '9'


fromChar : Char -> Maybe N1To9
fromChar char =
    case char of
        '1' ->
            Just N1

        '2' ->
            Just N2

        '3' ->
            Just N3

        '4' ->
            Just N4

        '5' ->
            Just N5

        '6' ->
            Just N6

        '7' ->
            Just N7

        '8' ->
            Just N8

        '9' ->
            Just N9

        _ ->
            Nothing


morphChar : Morph N1To9 Char
morphChar =
    Morph.custom "1-9"
        { toNarrow =
            \char ->
                fromChar char
                    |> Util.resultFromMaybeLazy (\() -> String.fromChar char)
        , toBroad = toChar
        }
