## [elm morph](https://dark.elm.dmy.fr/packages/lue-bird/elm-morph/latest/)

a parser-printer: developer-friendly, general-purpose, great errors

  - 📻 related: ["codecs" elm-radio episode](https://elm-radio.com/episode/codecs/)
  - 🎧 while reading: ["Morphable", microtonal electronic music by Sevish](https://youtu.be/J-JZhCWsk3M?t=733)

One ["morph"](Morph) can convert between narrow ⇄ broad types which is surprisingly useful!
Below some appetizers


## [`MorphRow`](Morph#MorphRow)

Know `Parser`s? [`MorphRow`](Morph#MorphRow) simply always creates a printer alongside. Think

  - `Email/Id/Time/Path/Url.fromString` ⇄ `Email/Id/Time/Path/Url.toString`
  - `Midi.fromBitList` ⇄ `Midi.toBitList`
  - concrete syntax tree parser ⇄ syntax token builder
  - ...

Like [`Morph`](Morph#Morph), [`MorphRow`](Morph#MorphRow) makes the process simpler and more reliable

A 1:1 port of [an example from `elm/parser`](https://dark.elm.dmy.fr/packages/elm/parser/latest/Parser#lazy):
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
                |> Morph.tryRow (\() -> BooleanTrue) (String.Morph.only "true")
                |> Morph.tryRow (\() -> BooleanFalse) (String.Morph.only "false")
                |> Morph.tryRow BooleanOr (or (step ()))
                |> Morph.choiceFinish
        )

or : MorphRow Boolean Char -> MorphRow { left : Boolean, right : Boolean } Char
or step =
    let
        spaces : MorphRow (List ()) Char
        spaces =
            Morph.whilePossible (String.Morph.only " ")
    in
    Morph.succeed
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

"((true || false) || false)"
    |> Morph.toNarrow
        (boolean
            |> Morph.rowFinish
            |> Morph.over List.Morph.string
        )
--> Ok (BooleanOr { left = BooleanOr { left = BooleanTrue, right = BooleanFalse }, right = BooleanFalse })
```

What's different from writing a parser?

  - [`Morph.choice (\... -> case ... of ...)`](Morph#choice) matches possibilities exhaustively
  - [`grab ... ...`](Morph#grab) also shows how to access the morphed positional part
  - [`broad ...`](Morph#broad) provides a "default value" for the printer
  - no `loop` and no classic `andThen`! Instead we have atLeast, between, exactly, optional, while possible, until, ... See [section sequence in the `Morph` module documentation](Morph#sequence)

## [`MorphValue`](Value-Morph)

Easily serialize from and to elm values independent of output format.

An example adapted from [elm guide on custom types](https://guide.elm-lang.org/types/custom_types.html):
```elm
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

## [`Morph.OneToOne`](Morph#OneToOne)

The simplest of them all: convert between any two types where nothing can fail. Think

  - [`List Bit`](https://dark.elm.dmy.fr/packages/lue-bird/elm-bits/latest/Bit) ⇄ [`Bytes`](https://dark.elm.dmy.fr/packages/elm/bytes/latest/), see [`List.Morph.bytes`](List-Morph#bytes)
  - case-able [`Value`](Value) ⇄ [`Json`](Json) – both just elm union `type`s, see [`Json.Morph.value`](Json-Morph#value)
  - package-facing type ⇄ package-internal type
  - decompiled AST ⇄ generated code

## [`Morph`](Morph#Morph)

The parent of `MorphRow`, `MorphValue`, `Morph.OneToOne` etc.: convert between any two types. Think

  - accepting numbers only in a specific range
  - [`Decimal`](Decimal) (just digits) ⇄ `Float` with NaN and infinity
  - [`AToZ`](AToZ) ⇄ `Char`, see [`AToZ.Morph.char`](AToZ-Morph#char)

-------

Confused? Hyped? Hit @lue up on anything on slack!
