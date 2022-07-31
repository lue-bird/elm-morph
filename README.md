## [elm morph](https://dark.elm.dmy.fr/packages/lue-bird/elm-morph/latest/)

> build conversions between narrow ⇄ broad types at once

There's a lot of shiny applications of "morph"s that go both ways.

↓ are some appetizers; click headers for more examples and documentation

related: [📻 elm-radio episode "Codecs"](https://elm-radio.com/episode/codecs/)

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
        |> Value.part ( .name, "name" ) Value.string
        |> Value.part ( .percent, "percent" ) Value.float
        |> Value.part ( .per100k, "per100k" ) Value.float
        |> Value.groupFinish
```
That was surprisingly easy!

## [`MorphRow`](MorphRow)

Know `Parser`s? `MorphRow` simply always creates a builder alongside. Think

  - `Email/Id/Time.fromString` ⇄ `Email/Id/Time.toString`
  - concrete syntax tree parser ⇄ pretty formatter
  - language tree ⇄ simplified, partially evaluated language tree
  - ...

As with all [`Morph`](Morph#Morph)s, [`MorphRow`](MorphRow) makes the process simpler and more reliable.

Here a 1:1 port of [an example from `elm/parser`](https://dark.elm.dmy.fr/packages/elm/parser/latest/Parser#lazy):
```elm
import Morph exposing (Morph, broadenFrom)
import MorphRow (succeed, atLeast, grab, skip, one)
import Morph.Char

type Boolean
    = BooleanTrue
    | BooleanFalse
    | BooleanOr ( Boolean, Boolean )

"((true || false) || false)"
    |> narrow
        (boolean
            |> Morph.over Morph.Text.fromList
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
        |> MorphRow.possibility MyTrue (Morph.Text.specific "true")
        |> MorphRow.possibility MyFalse (Morph.Text.specific "false")
        |> MorphRow.possibility MyOr
            (succeed Tuple.pair
                |> skip (Morph.Text.specific "(")
                |> skip
                    (broadenFrom []
                        |> MorphRow.over
                            (atLeast 0 (Morph.Char.blank |> one))
                    )
                |> grab (Morph.lazy (\_ -> boolean))
                |> skip
                    (broadenFrom [ Morph.Char.Space ]
                        |> MorphRow.over
                            (atLeast 0 (Morph.Char.blank |> one))
                    )
                |> grab (Morph.Text.specific "||")
                |> skip
                    (broadenFrom [ Morph.Char.Space ]
                        |> MorphRow.over
                            (atLeast 0 (Morph.Char.blank |> one))
                    )
                |> grab (Morph.lazy (\_ -> boolean))
                |> skip
                    (broadenFrom []
                        |> MorphRow.over
                            (atLeast 0 (Morph.Char.blank |> one))
                    )
                |> skip (Morph.Text.specific ")")
            )
```
What's different from writing a parser?

  - `broadenFrom ...` provides defaults for generated broad values
  - `choice (\... -> case ... of ...)` exhaustively matches possibilities with according broad values

Confused? Hyped? Hit @lue up on anything on slack!
