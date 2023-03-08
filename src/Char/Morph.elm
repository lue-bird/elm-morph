module Char.Morph exposing
    ( only
    , code, string, value
    )

{-|

@docs only
@docs code, string, value

-}

import Morph exposing (Morph)
import String.Morph.Internal
import Value


{-| Character by unicode [code point][cp]

    Morph.narrowTo Char.code 65 --> Ok 'A'

    Morph.narrowTo Char.code 66 --> Ok 'B'

    Morph.narrowTo Char.code 0x6728 --> Ok '木'

    Morph.narrowTo Char.code 0x0001D306 --> Ok '𝌆'

    Morph.narrowTo Char.code 0x0001F603 --> Ok '😃'

    Morph.narrowTo Char.code -1
    --> Err "unicode code point outside of range 0 to 0x10FFFF"

The full range of unicode is from `0` to `0x10FFFF`. With numbers outside that
range, you'll get an error.

[cp]: https://en.wikipedia.org/wiki/Code_point

-}
code : Morph Char Int
code =
    Morph.value "unicode character"
        { broaden = Char.toCode
        , narrow =
            \codePoint ->
                if codePoint >= 0 && codePoint <= 0x0010FFFF then
                    codePoint |> Char.fromCode |> Ok

                else
                    "unicode code point outside of range 0 to 0x10FFFF" |> Err
        }


{-| `Char` [`Value.Morph`](Value#Morph)

Be aware, that [special-cased characters as the result of `Char.toUpper`](https://github.com/elm/core/issues/1001)
are [encoded](Morph#broadenFrom) as 2 `Char`s in a `String`
and therefore can't be [decoded](Morph#narrowTo) again

-}
value : Value.Morph Char
value =
    string |> Morph.over String.Morph.Internal.value


{-| [`Morph`](Morph#Morph) a `String` of length 1 to a `Char`

Be aware, that [special-cased characters as the result of `Char.toUpper`](https://github.com/elm/core/issues/1001)
are [encoded](Morph#broadenFrom) as 2 `Char`s in a `String`
and therefore can't be [decoded](Morph#narrowTo) again

-}
string : Morph Char String
string =
    Morph.value "Char"
        { broaden = String.fromChar
        , narrow =
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
    Morph.only String.fromChar broadConstant
