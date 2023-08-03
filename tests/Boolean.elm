module Boolean exposing (Boolean(..), chars)

import Morph exposing (MorphRow, broad, grab, match)
import String.Morph


type Boolean
    = BooleanTrue
    | BooleanFalse
    | BooleanOr { left : Boolean, right : Boolean }


chars : MorphRow Boolean Char
chars =
    Morph.recursive "boolean"
        (\step ->
            Morph.choice
                (\variantTrue variantFalse variantOr booleanChoice ->
                    case booleanChoice of
                        BooleanTrue ->
                            variantTrue ()

                        BooleanFalse ->
                            variantFalse ()

                        BooleanOr arguments ->
                            variantOr arguments
                )
                |> Morph.rowTry (\() -> BooleanTrue)
                    (String.Morph.only "true")
                |> Morph.rowTry (\() -> BooleanFalse)
                    (String.Morph.only "false")
                |> Morph.rowTry BooleanOr (orChars step)
                |> Morph.choiceFinish
        )


orChars : MorphRow Boolean Char -> MorphRow { left : Boolean, right : Boolean } Char
orChars step =
    let
        spaces : MorphRow (List ()) Char
        spaces =
            Morph.named "spaces"
                (Morph.whilePossible (String.Morph.only " "))
    in
    Morph.narrow
        (\left right -> { left = left, right = right })
        |> match (String.Morph.only "(")
        |> match (broad [] |> Morph.overRow spaces)
        |> grab .left step
        |> match (broad [ () ] |> Morph.overRow spaces)
        |> match (String.Morph.only "||")
        |> match (broad [ () ] |> Morph.overRow spaces)
        |> grab .right step
        |> match (broad [] |> Morph.overRow spaces)
        |> match (String.Morph.only ")")
