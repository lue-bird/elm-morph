module Point exposing (Point, chars)

import ArraySized
import ArraySized.Morph exposing (atLeast)
import Decimal exposing (Decimal)
import Decimal.Morph
import Morph exposing (MorphRow, broad, grab, match)
import N exposing (n0)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import String.Morph


chars : MorphRow Point Char
chars =
    Morph.succeed (\x y -> { x = x, y = y })
        |> match (String.Morph.only "(")
        |> match
            (broad (ArraySized.one ())
                |> Morph.overRow (atLeast n0 (String.Morph.only " "))
            )
        |> grab .x Decimal.Morph.chars
        |> match
            (broad ArraySized.empty
                |> Morph.overRow (atLeast n0 (String.Morph.only " "))
            )
        |> match (String.Morph.only ",")
        |> match
            (broad (ArraySized.one ())
                |> Morph.overRow (atLeast n0 (String.Morph.only " "))
            )
        |> grab .y Decimal.Morph.chars
        |> match
            (broad (ArraySized.one ())
                |> Morph.overRow (atLeast n0 (String.Morph.only " "))
            )
        |> match (String.Morph.only ")")


type alias Point =
    RecordWithoutConstructorFunction
        { x : Decimal, y : Decimal }
