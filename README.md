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
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Choice
import Value
import FloatExplicit
import String.Morph

type alias Cause =
    RecordWithoutConstructorFunction
        { name : String
        , percent : Float
        , per100k : Float
        }


value : Value.Morph Cause
value =
    Value.group
        (\name percent per100k ->
            { name = name, percent = percent, per100k = per100k }
        )
        |> Value.field ( .name, "name" ) String.Morph.value
        |> Value.field ( .percent, "percent" ) FloatExplicit.value
        |> Value.field ( .per100k, "per100k" ) FloatExplicit.value
        |> Value.groupFinish
```
surprisingly easy!

Another example with a `type` adapted from [elm guide on custom types](https://guide.elm-lang.org/types/custom_types.html)
```elm
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Unit
import Choice
import Value
import String.Morph

type User
    = Anonymous
    | SignedIn SignedIn

type alias SignedIn =
    RecordWithoutConstructorFunction
        { name : String, status : String }

value : Value.Morph User
value =
    Choice.between
        (\variantAnonymous variantSignedIn user ->
            case user of
                Anonymous ->
                    variantAnonymous ()
                
                SignedIn signedIn ->
                    variantSignedIn signedIn
        )
        |> Choice.variantValue ( \() -> Anonymous, "Anonymous" ) Value.unit
        |> Choice.variantValue ( SignedIn, "SignedIn" ) signedInValue
        |> Choice.finishValue

signedInValue : Value.Morph SignedIn
signedInValue =
    Value.group
        (\name status ->
            { name = name, status = status }
        )
        |> Value.field ( .name, "name" ) String.Morph.value
        |> Value.field ( .statue, "status" ) String.Morph.value
        |> Value.groupFinish
```
clean

## [`MorphRow`](Morph#MorphRow)

Know `Parser`s? [`MorphRow`](Morph#MorphRow) simply always creates a builder alongside. Think

  - `Email/Id/Time/Url.fromString` ⇄ `Email/Id/Time/Url.toString`
  - concrete syntax tree parser ⇄ pretty formatter
  - decompiler ⇄ compiler
  - ...

Like [`Morph`](Morph#Morph), [`MorphRow`](Morph#MorphRow) makes the process simpler and more reliable

Here a 1:1 port of [an example from `elm/parser`](https://dark.elm.dmy.fr/packages/elm/parser/latest/Parser#lazy):
```elm
import Choice
import Morph exposing (MorphRow, broad, narrowTo, one, skip, grab)
import Char.Morph
import String.Morph
import N exposing (n0)
import ArraySized
import ArraySized.Morph

type Boolean
    = True
    | False
    | Or { left : Boolean, right : Boolean }

"((true || false) || false)"
    |> narrowTo
        (boolean
            |> Morph.rowFinish
            |> Morph.over Stack.Morph.string
        )
--> Ok (BooleanOr { left = BooleanOr { left = BooleanTrue, right = BooleanFalse }, right = BooleanFalse })

boolean : MorphRow Boolean Char
boolean =
    Choice.between
        (\variantTrue variantFalse variantOr booleanChoice ->
            case booleanChoice of
                BooleanTrue ->
                    variantTrue ()

                BooleanFalse ->
                    variantFalse ()

                BooleanOr arguments ->
                    variantOr arguments
        )
        |> Choice.tryRow (\() -> True) (String.Morph.only "true")
        |> Choice.tryRow (\() -> False) (String.Morph.only "false")
        |> Choice.tryRow Or or
        |> Choice.finishRow

or : MorphRow { left : Boolean, right : Boolean } Char
or =
    let 
        spaces =
            ArraySized.Morph.atLeast (String.Morph.only " ") n0
    in
    Morph.succeed
        (\left right -> { left = left, right = right })
        |> skip (String.Morph.only "(")
        |> skip (broad ArraySized.empty |> Morph.rowOver spaces)
        |> grab .left boolean
        |> skip (broad (ArraySized.one ()) |> Morph.rowOver spaces)
        |> skip (String.Morph.only "||")
        |> skip (broad (ArraySized.one ()) |> Morph.rowOver spaces)
        |> grab .right boolean
        |> skip (broad ArraySized.empty |> Morph.rowOver spaces)
        |> skip (String.Morph.only ")")
```

What's different from writing a parser?

  - `broad ...` provides defaults for generated broad values
  - `Choice.between (\... -> case ... of ...)` exhaustively matches possibilities with according broad values
  - `grab ... ...` also shows how to access the morphed positional part

Confused? Hyped? Hit @lue up on anything on slack!
