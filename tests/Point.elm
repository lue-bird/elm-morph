module Point exposing (Point, chars)

import Int.Morph
import Morph exposing (MorphRow, broad, grab, match)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import String.Morph


chars : MorphRow Point Char
chars =
    Morph.narrow (\x y -> { x = x, y = y })
        |> match (String.Morph.only "(")
        |> match (broad [ () ] |> Morph.overRow spaces)
        |> grab .x Int.Morph.chars
        |> match (broad [] |> Morph.overRow spaces)
        |> match (String.Morph.only ",")
        |> match (broad [ () ] |> Morph.overRow spaces)
        |> grab .y Int.Morph.chars
        |> match (broad [ () ] |> Morph.overRow spaces)
        |> match (String.Morph.only ")")


spaces : MorphRow (List ()) Char
spaces =
    Morph.named "spaces"
        (Morph.whilePossible (String.Morph.only " "))


type alias Point =
    RecordWithoutConstructorFunction
        { x : Int, y : Int }
