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

import Bit exposing (Bit)
import Char.Morph.Internal
import Morph exposing (Morph, MorphRow)
import String.Morph.Internal
import Utf8CodePoint
import Value.Morph.Internal exposing (MorphValue)


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


{-| `Char` [`MorphValue`](Value-Morph#MorphValue)

Be aware, that [special-cased characters as the result of `Char.toUpper`](https://github.com/elm/core/issues/1001)
are [broadened](Morph#toBroad) as 2 `Char`s in a [`Value.String`](Value#Value)
and therefore [narrowing](Morph#toNarrow) to a `Char` will fail.

-}
value : MorphValue Char
value =
    string |> Morph.over String.Morph.Internal.value


{-| [`Morph`](Morph#Morph) a `String` of length 1 to a `Char`

Be aware, that [special-cased characters as the result of `Char.toUpper`](https://github.com/elm/core/issues/1001)
are [broadened](Morph#toBroad) as 2 `Char`s in a `String`
and therefore [narrowing](Morph#toNarrow) to a `Char` will fail.

-}
string : Morph Char String
string =
    Morph.custom "character"
        { toBroad = String.fromChar
        , toNarrow =
            \stringBroad ->
                case stringBroad |> String.uncons of
                    Just ( charValue, "" ) ->
                        charValue |> Ok

                    Just ( _, stringFrom1 ) ->
                        [ stringFrom1 |> String.length |> String.fromInt
                        , " characters too many"
                        ]
                            |> String.concat
                            |> Err

                    Nothing ->
                        "no characters" |> Err
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
    Utf8CodePoint.charBits
