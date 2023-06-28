module Point exposing (Point, chars)

import Decimal exposing (Decimal)
import Decimal.Morph
import Morph exposing (MorphRow, broad, grab, match)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import String.Morph


chars : MorphRow Point Char
chars =
    Morph.succeed (\x y -> { x = x, y = y })
        |> match (String.Morph.only "(")
        |> match
            (broad [ () ]
                |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
            )
        |> grab .x Decimal.Morph.chars
        |> match
            (broad []
                |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
            )
        |> match (String.Morph.only ",")
        |> match
            (broad [ () ]
                |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
            )
        |> grab .y Decimal.Morph.chars
        |> match
            (broad [ () ]
                |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
            )
        |> match (String.Morph.only ")")


type alias Point =
    RecordWithoutConstructorFunction
        { x : Decimal, y : Decimal }
