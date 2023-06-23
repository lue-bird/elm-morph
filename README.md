## [elm morph](https://dark.elm.dmy.fr/packages/lue-bird/elm-morph/latest/)

a parser-builder: developer-friendly, general-purpose, great errors

> build one to convert between narrow ⇄ broad types

  - 📻 related: ["codecs" elm-radio episode](https://elm-radio.com/episode/codecs/)
  - 🎧 while reading: ["Morphable", microtonal electronic music by Sevish](https://www.youtube.com/watch?v=J-JZhCWsk3M&t=330s)

There's a lot of shiny applications of these ["morph"](Morph)s!
↓ some appetizers. Click headers for documentation

## [`Value`](Value)

Serialize from and to elm values the easy way.
Independent of output format

### example translated from [`elm/json`](https://dark.elm.dmy.fr/packages/elm/json/latest/)

```elm
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Value
import Decimal exposing (Decimal)
import Decimal.Morph
import String.Morph

type alias Cause =
    RecordWithoutConstructorFunction
        { name : String
        , percent : Decimal
        , per100k : Decimal
        }


value : Value.Morph Cause
value =
    Value.group
        (\name percent per100k ->
            { name = name, percent = percent, per100k = per100k }
        )
        |> Value.part ( .name, "name" ) String.Morph.value
        |> Value.part ( .percent, "percent" ) Decimal.Morph.value
        |> Value.part ( .per100k, "per100k" ) Decimal.Morph.value
        |> Value.groupFinish
```
surprisingly easy!

### Another example adapted from [elm guide on custom types](https://guide.elm-lang.org/types/custom_types.html)
```elm
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Value
import Morph
import String.Morph

type User
    = Anonymous
    | SignedIn SignedIn

type alias SignedIn =
    RecordWithoutConstructorFunction
        { name : String, status : String }

value : Value.Morph User
value =
    Morph.choice
        (\variantAnonymous variantSignedIn user ->
            case user of
                Anonymous ->
                    variantAnonymous ()
                
                SignedIn signedIn ->
                    variantSignedIn signedIn
        )
        |> Value.variant ( \() -> Anonymous, "Anonymous" ) Value.unit
        |> Value.variant ( SignedIn, "SignedIn" ) signedInValue
        |> Value.choiceFinish

signedInValue : Value.Morph SignedIn
signedInValue =
    Value.group
        (\name status ->
            { name = name, status = status }
        )
        |> Value.part ( .name, "name" ) String.Morph.value
        |> Value.part ( .statue, "status" ) String.Morph.value
        |> Value.groupFinish
```
clean

## [`MorphRow`](Morph#MorphRow)

Know `Parser`s? [`MorphRow`](Morph#MorphRow) simply always creates a builder alongside. Think

  - `Email/Id/Time/Path/Url.fromString` ⇄ `Email/Id/Time/Path/Url.toString`
  - concrete syntax tree parser ⇄ pretty formatter
  - decompiler ⇄ code generation
  - ...

Like [`Morph`](Morph#Morph), [`MorphRow`](Morph#MorphRow) makes the process simpler and more reliable

Here a 1:1 port of [an example from `elm/parser`](https://dark.elm.dmy.fr/packages/elm/parser/latest/Parser#lazy):
```elm
import Morph exposing (MorphRow, broad, match, grab)
import List.Morph
import String.Morph

type Boolean
    = BooleanTrue
    | BooleanFalse
    | BooleanOr { left : Boolean, right : Boolean }

"((true || false) || false)"
    |> Morph.toNarrow
        (boolean
            |> Morph.rowFinish
            |> Morph.over List.Morph.string
        )
--> Ok (BooleanOr { left = BooleanOr { left = BooleanTrue, right = BooleanFalse }, right = BooleanFalse })

boolean : MorphRow Boolean Char
boolean =
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
        |> Morph.tryRow (\() -> BooleanTrue) (String.Morph.only "true")
        |> Morph.tryRow (\() -> BooleanFalse) (String.Morph.only "false")
        |> Morph.tryRow BooleanOr or
        |> Morph.choiceFinish

or : MorphRow { left : Boolean, right : Boolean } Char
or =
    let
        spaces : MorphRow (List ()) Char
        spaces =
            Morph.whilePossible (String.Morph.only " ")
    in
    Morph.succeed
        (\left right -> { left = left, right = right })
        |> match (String.Morph.only "(")
        |> match (broad [] |> Morph.overRow spaces)
        |> grab .left boolean
        |> match (broad [ () ] |> Morph.overRow spaces)
        |> match (String.Morph.only "||")
        |> match (broad [ () ] |> Morph.overRow spaces)
        |> grab .right boolean
        |> match (broad [] |> Morph.overRow spaces)
        |> match (String.Morph.only ")")
```

What's different from writing a parser?

  - [`broad ...`](Morph#broad) provides a "default value" for the builder
  - [`Morph.choice (\... -> case ... of ...)`](Morph#choice) exhaustively matches narrow possibilities
  - [`grab ... ...`](Morph#grab) also shows how to access the morphed positional part
  - no `loop`! Instead we have atLeast, between, exactly, optional, while possible, until, ... See [section sequence in the `Morph` module documentation](Morph#sequence)

Confused? Hyped? Hit @lue up on anything on slack!
