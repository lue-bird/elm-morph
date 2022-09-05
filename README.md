## [elm morph](https://dark.elm.dmy.fr/packages/lue-bird/elm-morph/latest/)

> build conversions between narrow ⇄ broad types at once

Simple, easy to use, general-purpose parser-builder with good error messages.

There's a lot of shiny applications of ["morph"](Morph)s that go both ways.

  - 📻 related: ["codecs" elm-radio episode](https://elm-radio.com/episode/codecs/)
  - 🎧 while reading the docs: ["Morphing", microtonal electronic music by Sevish](https://youtu.be/J-JZhCWsk3M?t=1702)

↓ are some appetizers; click headers for more examples and documentation

## [`Value`](Value)

Serialize from and to elm values the easy way.
Independent of output format.

Here's a 1:1 port of [an example from `elm/json`](https://dark.elm.dmy.fr/packages/elm/json/latest/)

```elm
module Cause exposing (Cause, value)

import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Morph
import Value

type alias Cause =
    RecordWithoutConstructorFunction
        { name : String
        , percent : Float
        , per100k : Float
        }


value : Morph Cause Value (Value.Error expectationCustom_)
value =
    Value.group
        (\name percent per100k ->
            { name = name, percent = percent, per100k = per100k }
        )
        |> Value.part ( .name, "name" ) Value.string
        |> Value.part ( .percent, "percent" ) Value.float
        |> Value.part ( .per100k, "per100k" ) Value.float
        |> Value.groupFinish
```
That was surprisingly easy!

This is powerful! How about [`Morph`](Morph#Morph)ing

  - form `Model`-stored data ⇄ user-editable data?
  - ...

## [`MorphRow`](Morph#MorphRow)

Know `Parser`s? [`MorphRow`](Morph#MorphRow) simply always create a builder alongside. Think

  - `Email/Id/Time.fromString` ⇄ `Email/Id/Time.toString`
  - concrete syntax tree parser ⇄ pretty formatter
  - language tree ⇄ simplified, partially evaluated language tree
  - ...

As with all [`Morph`](Morph#Morph)s, [`MorphRow`](Morph#MorphRow) makes the process simpler and more reliable.

Here a 1:1 port of [an example from `elm/parser`](https://dark.elm.dmy.fr/packages/elm/parser/latest/Parser#lazy):
```elm
import Morph exposing (MorphRow, broad, atLeast, grab, skip, one)
import Char.Morph
import N exposing (n1)
import ArraySized

type Boolean
    = BooleanTrue
    | BooleanFalse
    | BooleanOr { left : Boolean, right : Boolean }

"((true || false) || false)"
    |> narrow
        (boolean
            |> Morph.rowFinish
            |> Morph.over String.Morph.fromList
        )
--> Ok (BooleanOr ( BooleanOr ( BooleanTrue, BooleanFalse ), BooleanFalse ))

boolean : MorphRow Char Boolean expectationCustom_
boolean =
    Morph.choice
        (\true false or booleanNarrow ->
            case booleanNarrow of
                BooleanTrue ->
                    true ()

                BooleanFalse ->
                    false ()

                BooleanOr arguments ->
                    or arguments
        )
        |> Morph.rowTry BooleanTrue
            (String.Morph.only "true")
        |> Morph.rowTry BooleanFalse
            (String.Morph.only "false")
        |> Morph.rowTry BooleanOr
            (Morph.succeed
                (\left right -> { left = left, right = right })
                |> skip (String.Morph.only "(")
                |> skip
                    (broad ArraySized.empty
                        |> Morph.rowOver (atLeast n0 (Char.Morph.only ' ' |> one))
                    )
                |> grab .left (Morph.lazy (\() -> boolean))
                |> skip
                    (broad (ArraySized.l1 Blank.Space)
                        |> Morph.rowOver (atLeast n0 (Char.Morph.only ' ' |> one))
                    )
                |> skip (String.Morph.only "||")
                |> skip
                    (broad (ArraySized.l1 Blank.Space)
                        |> Morph.rowOver (atLeast n0 (Char.Morph.only ' ' |> one))
                    )
                |> grab .right (Morph.lazy (\() -> boolean))
                |> skip
                    (broad (ArraySized.l1 Blank.Space)
                        |> Morph.rowOver (atLeast n0 (Char.Morph.only ' ' |> one))
                    )
                |> skip (String.Morph.only ")")
            )
```
What's different from writing a parser?

  - `broad ...` provides defaults for generated broad values
  - `choice (\... -> case ... of ...)` exhaustively matches possibilities with according broad values
  - `grab ... ...` also shows how to access the morphed positional part

Confused? Hyped? Hit @lue up on anything on slack!
