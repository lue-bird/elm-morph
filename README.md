## [elm morph](https://dark.elm.dmy.fr/packages/lue-bird/elm-morph/latest/)

a parser-printer: dev-friendly, general-purpose, great errors

  - 📻 related: ["codecs" elm-radio episode](https://elm-radio.com/episode/codecs/)
  - 🎧 while reading: ["Morphable", microtonal electronic music by Sevish](https://youtu.be/J-JZhCWsk3M?t=733)

One ["morph"](Morph) can convert between narrow ⇄ broad types which is surprisingly useful!
Below some appetizers


## [`MorphRow`](Morph#MorphRow)

Know parsers? [`MorphRow`](Morph#MorphRow) simply always creates a printer alongside. Think

  - `Email/Id/Time/Path/Url.fromString` ⇄ `Email/Id/Time/Path/Url.toString`
  - `Midi.fromBitList` ⇄ `Midi.toBitList`
  - parse a syntax tree from tokens ⇄ build tokens from a syntax tree

Building both in one is simpler and more reliable.

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
                |> Morph.rowTry (\() -> BooleanTrue)
                    (String.Morph.only "true")
                |> Morph.rowTry (\() -> BooleanFalse)
                    (String.Morph.only "false")
                |> Morph.rowTry BooleanOr (or step)
                |> Morph.choiceFinish
        )

or : MorphRow Boolean Char -> MorphRow { left : Boolean, right : Boolean } Char
or step =
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
  - [`grab ... ...`](Morph#grab) also shows how to access the part
  - [`broad ...`](Morph#broad) provides a "default value" for the printer

Morph also doesn't have `loop` or a classic `andThen`! Instead we have [atLeast, between, exactly, optional, while possible, until next, until last, ...](Morph#sequence)

This allows the quality of errors to be different to what you're used to. Here's a section of the [example app](https://github.com/lue-bird/elm-morph/blob/master/example):
![screenshot of a combined error and description tree view, partially expanded](https://github.com/lue-bird/elm-morph/blob/master/social/point-example-error-description-tree.png?raw=true)

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
  - type exposed from package ⇄ package-internal type
  - decompiled AST ⇄ generated code

## [`Morph`](Morph#Morph)

The parent of `MorphRow`, `MorphValue`, `Morph.OneToOne` etc.: convert between any two types. Think

  - accepting numbers only in a specific range
  - [`Decimal`](Decimal) (just digits) ⇄ `Float` with NaN and infinity
  - [`AToZ`](AToZ) ⇄ `Char`, see [`AToZ.Morph.char`](AToZ-Morph#char)

-------

Confused? Hyped? Hit @lue up on anything on slack!

## thanks 🌸

  - [`miniBill/elm-rope`](https://dark.elm.dmy.fr/packages/miniBill/elm-rope/latest/)
    allows our nested printer to still be `O(n)`
  - Many ideas in [`lambda-phi/parser`](https://dark.elm.dmy.fr/packages/lambda-phi/parser/latest/) inspired [`MorphRow`](#MorphRow)'s initial design
  - [`zwilias/elm-bytes-parser`](https://dark.elm.dmy.fr/packages/zwilias/elm-bytes-parser/latest/Bytes-Parser)
    showed me how to convert a list of bits from and to `Bytes` and gave me the courage to make `MorphRow ... Bit`s
  - all the elm tools, especially [`elm-verify-examples`](https://github.com/stoeffel/elm-verify-examples) and [`elm-review-documentation`](https://dark.elm.dmy.fr/packages/jfmengels/elm-review-documentation/latest/)
