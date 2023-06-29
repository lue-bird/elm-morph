## [elm morph](https://dark.elm.dmy.fr/packages/lue-bird/elm-morph/latest/)

a parser-builder: developer-friendly, general-purpose, great errors

  - 📻 related: ["codecs" elm-radio episode](https://elm-radio.com/episode/codecs/)
  - 🎧 while reading: ["Morphable", microtonal electronic music by Sevish](https://www.youtube.com/watch?v=J-JZhCWsk3M&t=330s)

One ["morph"](Morph) can convert between narrow ⇄ broad types which is surprisingly useful!
Below some appetizers

## [`Value`](Value)

Easily serialize from and to elm values independent of output format.

An example adapted from [elm guide on custom types](https://guide.elm-lang.org/types/custom_types.html):
```elm
import Value
import Value.Morph exposing (MorphValue)
import Morph
import String.Morph
-- from lue-bird/elm-no-record-type-alias-constructor-function
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)

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
        |> Value.Morph.variant ( \() -> Anonymous, "Anonymous" ) Value.Morph.unit
        |> Value.Morph.variant ( SignedIn, "SignedIn" ) signedInValue
        |> Value.Morph.choiceFinish

signedInValue : MorphValue SignedIn
signedInValue =
    Value.Morph.group
        (\name status ->
            { name = name, status = status }
        )
        |> Value.Morph.part ( .name, "name" ) String.Morph.value
        |> Value.Morph.part ( .statue, "status" ) String.Morph.value
        |> Value.Morph.groupFinish
```
surprisingly easy and clean!

## [`MorphRow`](Morph#MorphRow)

Know `Parser`s? [`MorphRow`](Morph#MorphRow) simply always creates a builder alongside. Think

  - `Email/Id/Time/Path/Url.fromString` ⇄ `Email/Id/Time/Path/Url.toString`
  - `Midi.fromBitList` ⇄ `Midi.toBitList`
  - (concrete syntax tree parser ⇄ syntax token builder) over (tokenization ⇄ pretty formatter) (or without intermediate tokens)
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

"((true || false) || false)"
    |> Morph.toNarrow
        (boolean
            |> Morph.rowFinish
            |> Morph.over List.Morph.string
        )
--> Ok (BooleanOr { left = BooleanOr { left = BooleanTrue, right = BooleanFalse }, right = BooleanFalse })
```

What's different from writing a parser?

  - [`broad ...`](Morph#broad) provides a "default value" for the builder
  - [`Morph.choice (\... -> case ... of ...)`](Morph#choice) exhaustively matches narrow possibilities
  - [`grab ... ...`](Morph#grab) also shows how to access the morphed positional part
  - no `loop`! Instead we have atLeast, between, exactly, optional, while possible, until, ... See [section sequence in the `Morph` module documentation](Morph#sequence)

Confused? Hyped? Hit @lue up on anything on slack!
