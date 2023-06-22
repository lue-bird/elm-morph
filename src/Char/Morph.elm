module Char.Morph exposing
    ( only
    , code, string, value
    , bits
    )

{-|

@docs only
@docs code, string, value
@docs bits

-}

import ArraySized
import Bit exposing (Bit)
import BitArray
import Bitwise
import Char.Morph.Internal
import Linear exposing (Direction(..))
import Morph exposing (Morph, MorphOrError, MorphRow)
import N exposing (n0, n11, n16, n3, n4, n5, n6, n7)
import N.Local exposing (n21)
import String.Morph.Internal
import Utf8CodePoint exposing (Utf8CodePoint)
import Value


{-| Character by unicode [code point][https://en.wikipedia.org/wiki/Code_point]

    Morph.toNarrow Char.code 65 --> Ok 'A'

    Morph.toNarrow Char.code 66 --> Ok 'B'

    Morph.toNarrow Char.code 0x6728 --> Ok '木'

    Morph.toNarrow Char.code 0x0001D306 --> Ok '𝌆'

    Morph.toNarrow Char.code 0x0001F603 --> Ok '😃'

    Morph.toNarrow Char.code -1
    --> Err (Morph.DeadEnd "unicode code point outside of range 0 to 0x10FFFF")

The full range of unicode is from `0` to `0x10FFFF`. With numbers outside that
range, you'll get an error (unlike `Char.fromCode`).

-}
code : Morph Char Int
code =
    Morph.custom "unicode character"
        { toBroad = Char.toCode
        , toNarrow =
            \codePoint ->
                if codePoint >= 0 && codePoint <= 0x0010FFFF then
                    codePoint |> Char.fromCode |> Ok

                else
                    "unicode code point outside of range 0 to 0x10FFFF" |> Err
        }


{-| `Char` [`Value.Morph`](Value#Morph)

Be aware, that [special-cased characters as the result of `Char.toUpper`](https://github.com/elm/core/issues/1001)
are [encoded](Morph#toBroad) as 2 `Char`s in a `String`
and therefore can't be [decoded](Morph#toNarrow) again

-}
value : Value.Morph Char
value =
    string |> Morph.over String.Morph.Internal.value


{-| [`Morph`](Morph#Morph) a `String` of length 1 to a `Char`

Be aware, that [special-cased characters as the result of `Char.toUpper`](https://github.com/elm/core/issues/1001)
are [encoded](Morph#toBroad) as 2 `Char`s in a `String`
and therefore can't be [decoded](Morph#toNarrow) again

-}
string : Morph Char String
string =
    Morph.custom "Char"
        { toBroad = String.fromChar
        , toNarrow =
            \stringBroad ->
                case stringBroad |> String.uncons of
                    Just ( charValue, "" ) ->
                        charValue |> Ok

                    Just ( _, stringFrom1 ) ->
                        [ stringFrom1 |> String.length |> String.fromInt
                        , " too many characters"
                        ]
                            |> String.concat
                            |> Err

                    Nothing ->
                        "empty" |> Err
        }


{-| Match only the specific given broad input. See [`Morph.only`](Morph#only)
-}
only : Char -> Morph () Char
only broadConstant =
    Char.Morph.Internal.only broadConstant


{-| [`MorphRow`](Morph#MorphRow) for a `Char`.

Note that a `Char` isn't equivalent to a visual unit like 🇨🇿 or 🦸🏽‍♂️ or ả̴̫̼̫̀̅
(→ see [grapheme](https://dark.elm.dmy.fr/packages/BrianHicks/elm-string-graphemes/latest/)).
Instead, it's just one UTF-8 code point.

-}
bits : MorphRow Char Bit
bits =
    Morph.named "UTF-8 code point"
        (code
            |> Morph.over codeUtf8
            |> Morph.overRow Utf8CodePoint.bits
        )


codeUtf8 : MorphOrError Int Utf8CodePoint error_
codeUtf8 =
    -- Granted this is far from elegant
    Morph.oneToOne
        (\utf8CodePointChoice ->
            case utf8CodePointChoice of
                Utf8CodePoint.OneByte oneByte ->
                    oneByte |> BitArray.toN |> N.toInt

                Utf8CodePoint.TwoBytes bytes ->
                    (bytes.first |> BitArray.toN |> N.toInt |> Bitwise.shiftLeftBy 6)
                        + (bytes.second |> BitArray.toN |> N.toInt)

                Utf8CodePoint.ThreeBytes bytes ->
                    (bytes.first |> BitArray.toN |> N.toInt |> Bitwise.shiftLeftBy 12)
                        + (bytes.second |> BitArray.toN |> N.toInt |> Bitwise.shiftLeftBy 6)
                        + (bytes.third |> BitArray.toN |> N.toInt)

                Utf8CodePoint.FourBytes bytes ->
                    (bytes.first |> BitArray.toN |> N.toInt |> Bitwise.shiftLeftBy 18)
                        + (bytes.second |> BitArray.toN |> N.toInt |> Bitwise.shiftLeftBy 12)
                        + (bytes.third |> BitArray.toN |> N.toInt |> Bitwise.shiftLeftBy 6)
                        + (bytes.fourth |> BitArray.toN |> N.toInt)
        )
        (\codeChoice ->
            if codeChoice <= 0x7F then
                Utf8CodePoint.OneByte (codeChoice |> N.intToAtLeast n0 |> BitArray.fromN n7)

            else if codeChoice <= 0x07FF then
                let
                    bytes =
                        codeChoice - 0x80 |> N.intToAtLeast n0 |> BitArray.fromN n11
                in
                Utf8CodePoint.TwoBytes
                    { first = bytes |> ArraySized.toSize Up n5 (\_ -> Bit.O)
                    , second = bytes |> ArraySized.toSize Down n6 (\_ -> Bit.O)
                    }

            else if codeChoice <= 0xFFFF then
                let
                    bytes =
                        codeChoice - 0x0800 |> N.intToAtLeast n0 |> BitArray.fromN n16
                in
                Utf8CodePoint.ThreeBytes
                    { first = bytes |> ArraySized.toSize Up n4 (\_ -> Bit.O)
                    , second = bytes |> ArraySized.drop Up n4 |> ArraySized.toSize Down n6 (\_ -> Bit.O)
                    , third = bytes |> ArraySized.toSize Down n6 (\_ -> Bit.O)
                    }

            else
                -- codeChoice should be <= 0x10FFFF
                let
                    bytes =
                        codeChoice - 0x00010000 |> N.intToAtLeast n0 |> BitArray.fromN n21
                in
                Utf8CodePoint.FourBytes
                    { first = bytes |> ArraySized.toSize Up n3 (\_ -> Bit.O)
                    , second = bytes |> ArraySized.drop Up n3 |> ArraySized.toSize Down n6 (\_ -> Bit.O)
                    , third = bytes |> ArraySized.drop Up (n3 |> N.add n6) |> ArraySized.toSize Down n6 (\_ -> Bit.O)
                    , fourth = bytes |> ArraySized.toSize Down n6 (\_ -> Bit.O)
                    }
        )
