module Integer.Morph exposing (Integer, text, toInt, fromInt)

{-| [`Morph`](Morph#Morph) to and from whole decimal numbers

@docs Integer, text, toInt, fromInt

-}

import ArraySized.Morph
import Digit.Morph
import Emptiable exposing (Emptiable, fill)
import Integer.Morph.Internal exposing (intAbsoluteTo0To9s, n0To9sToInt)
import Morph exposing (Morph, choice)
import Morph.Text
import MorphRow exposing (MorphRow, atLeast, grab, one, succeed)
import N exposing (n0)
import Sign
import Sign.Morph exposing (Signable(..))
import Stack exposing (StackTopBelow)


{-| A whole decimal number
-}
type alias Integer =
    Signable
        { absolute :
            Emptiable
                (StackTopBelow Digit.Morph.N1To9 Digit.Morph.N0To9)
                Never
        }


{-| [`Translate`](Morph#Translate) between an `Int` and a [decimal representation](#Integer).

Keep in mind that `Integer -> Int` can overflow
since `Int` is fixed in bit size while [`Integer`](#Integer) is not.

-}
fromInt : Morph Int Integer error_
fromInt =
    toInt |> Morph.reverse


{-| [`Translate`](Morph#Translate) between an `Int` and a [decimal representation](#Integer).

Keep in mind that `Integer -> Int` can overflow
since `Int` is fixed in bit size while [`Integer`](#Integer) is not.

-}
toInt : Morph Integer Int error_
toInt =
    Morph.translate
        (\int ->
            case Sign.ofNumber int of
                Nothing ->
                    N0

                Just signed ->
                    Signed
                        { sign = signed.sign
                        , absolute =
                            case signed.absolute |> abs |> intAbsoluteTo0To9s |> fill of
                                -- TODO: make starting with 0 impossible
                                Stack.TopDown Digit.Morph.N0 _ ->
                                    Stack.only Digit.Morph.N1

                                Stack.TopDown (Digit.Morph.N1To9 top) down ->
                                    Stack.topDown top down
                        }
        )
        (\integerNarrow ->
            case integerNarrow of
                N0 ->
                    0

                Signed integerSigned ->
                    Sign.number integerSigned.sign
                        (integerSigned.absolute
                            |> Stack.topMap Digit.Morph.N1To9
                            |> n0To9sToInt
                        )
        )



--


{-| Match an integer value as an `Int`.

    import MorphRow.Error

    -- you can parse integers as `Int` instead of `String`
    "123" |> Text.narrowWith integer --> Ok 123

    -- It also works with negative numbers.
    "-123" |> Text.narrowWith integer --> Ok -123

    -- a decimal number is _not_ an integer
    "3.14"
        |> Text.narrowWith integer
        |> Result.mapError MorphRow.Error.textMessage
    --> Err "1:2: I was expecting an integer value. I got stuck when I got the character '.'."

    -- but not with invalid numbers
    "abc"
        |> Text.narrowWith integer
        |> Result.mapError MorphRow.Error.textMessage
    --> Err "1:1: I was expecting an integer value. I got stuck when I got the character 'a'."

-}
text : MorphRow Char Integer
text =
    MorphRow.expect "integer"
        (choice
            (\integer0Variant integerSignedVariant integerNarrow ->
                case integerNarrow of
                    N0 ->
                        integer0Variant ()

                    Signed integer0Not ->
                        integerSignedVariant integer0Not
            )
            |> MorphRow.possibility (\() -> N0)
                (Morph.Text.specific "0")
            |> MorphRow.possibility Signed
                (succeed
                    (\signPart absolutePart ->
                        { sign = signPart, absolute = absolutePart }
                    )
                    |> grab .sign Sign.Morph.maybeMinus
                    |> grab .absolute
                        (succeed Stack.topDown
                            |> grab Stack.top (Digit.Morph.n1To9 |> one)
                            |> grab (Stack.topRemove >> Stack.toList)
                                (ArraySized.Morph.fromList
                                    |> MorphRow.over
                                        (atLeast n0 (Digit.Morph.n0To9 |> one))
                                )
                        )
                )
            |> MorphRow.choiceFinish
        )
