## [elm morph](https://dark.elm.dmy.fr/packages/lue-bird/elm-morph/latest/)

Simple, easy to use, general-purpose parser-builder with great error messages

> build one to transform narrow ⇄ broad types

There's a lot of shiny applications of these ["morph"](Morph)s

  - 📻 related: ["codecs" elm-radio episode](https://elm-radio.com/episode/codecs/)
  - 🎧 while reading: ["Morphing", microtonal electronic music by Sevish](https://youtu.be/J-JZhCWsk3M?t=1702)

↓ some appetizers. Click headers for documentation

## [`Value`](Value)

Serialize from and to elm values the easy way.
Independent of output format

1:1 port of [an `elm/json` example](https://dark.elm.dmy.fr/packages/elm/json/latest/) ↓

```elm
module Cause exposing (Cause, value)

import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Value exposing (MorphValue)
import Float.Morph
import String.Morph

type alias Cause =
    RecordWithoutConstructorFunction
        { name : String
        , percent : Float
        , per100k : Float
        }


value : MorphValue Cause
value =
    Value.record
        (\name percent per100k ->
            { name = name, percent = percent, per100k = per100k }
        )
        |> Value.field ( .name, "name" ) String.Morph.value
        |> Value.field ( .percent, "percent" ) Float.Morph.value
        |> Value.field ( .per100k, "per100k" ) Float.Morph.value
        |> Value.recordFinish
```
surprisingly easy!

Another example with a `type` adapted from [elm guide on custom types](https://guide.elm-lang.org/types/custom_types.html)
```elm
module User exposing (value)

import Unit
import Value exposing (MorphValue)
import Morph
import String.Morph

type User
    = Anonymous
    | SignedIn SignedIn

type alias SignedIn =
    RecordWithoutConstructorFunction
        { name : String, status : String }

value : MorphValue User
value =
    Morph.choice
        (\variantAnonymous variantSignedIn user ->
            case user of
                Anonymous ->
                    variantAnonymous ()
                
                SignedIn signedIn ->
                    variantSignedIn signedIn
        )
        |> Value.variant ( \() -> Anonymous, "Anonymous" ) Unit.value
        |> Value.variant ( SignedIn, "SignedIn" ) signedInValue
        |> Value.choiceFinish

signedInValue : MorphValue SignedIn
signedInValue =
    Value.record
        (\name status ->
            { name = name, status = status }
        )
        |> Value.field ( .name, "name" ) String.Morph.value
        |> Value.field ( .statue, "status" ) String.Morph.value
        |> Value.recordFinish
```
clean

## [`MorphRow`](Morph#MorphRow)

Know `Parser`s? [`MorphRow`](Morph#MorphRow) simply always create a builder alongside. Think

  - `Email/Id/Time.fromString` ⇄ `Email/Id/Time.toString`
  - concrete syntax tree parser ⇄ pretty formatter
  - language tree ⇄ simplified, partially evaluated language tree
  - ...

Like [`Morph`](Morph#Morph), [`MorphRow`](Morph#MorphRow) makes the process simpler and more reliable

Here a 1:1 port of [an example from `elm/parser`](https://dark.elm.dmy.fr/packages/elm/parser/latest/Parser#lazy):
```elm
import Morph exposing (MorphRow, broad, atLeast, grab, skip, one, narrow)
import Char.Morph
import String.Morph
import N exposing (n0)
import ArraySized

type Boolean
    = True
    | False
    | Or { left : Boolean, right : Boolean }

"((true || false) || false)"
    |> narrow
        (boolean
            |> Morph.rowFinish
            |> Morph.over String.Morph.list
        )
--> Ok (BooleanOr { left = BooleanOr { left = BooleanTrue, right = BooleanFalse }, right = BooleanFalse })

boolean : MorphRow Char Boolean
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
        |> Morph.rowTry (\() -> True) (String.Morph.only "true")
        |> Morph.rowTry (\() -> False) (String.Morph.only "false")
        |> Morph.rowTry Or or

or : MorphRow Char { left : Boolean, right : Boolean }
or =
    let 
        spaces : MorphRow Char ()
        spaces =
            broad ArraySized.empty
                |> Morph.rowOver (atLeast n0 (String.Morph.only " "))
    in
    Morph.succeed
        (\left right -> { left = left, right = right })
        |> skip (String.Morph.only "(")
        |> skip spaces
        |> grab .left boolean
        |> skip spaces
        |> skip (String.Morph.only "||")
        |> skip spaces
        |> grab .right boolean
        |> skip spaces
        |> skip (String.Morph.only ")")
```
What's different from writing a parser?

  - `broad ...` provides defaults for generated broad values
  - `choice (\... -> case ... of ...)` exhaustively matches possibilities with according broad values
  - `grab ... ...` also shows how to access the morphed positional part

Confused? Hyped? Hit @lue up on anything on slack!
