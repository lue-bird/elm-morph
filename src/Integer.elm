module Integer exposing
    ( Integer(..)
    , fromInt, fromNatural
    , toInt
    , decimalRowChar
    )

{-| Typed-precision `Int`

@docs Integer


## create

@docs fromInt, fromNatural


## transform

@docs toInt

-}

import ArraySized exposing (ArraySized)
import ArraySized.Morph
import Bit exposing (Bit)
import Bits
import Choice
import Group
import Linear exposing (Direction(..))
import Morph exposing (MorphRow)
import N exposing (Add1, Fixed, In, Infinity, Min, N, N0, To, Up, Value, n0, n1, n2, n9)
import N.Local exposing (N31, Up31, n32)
import N.Morph
import Natural exposing (Natural)
import Possibly exposing (Possibly(..))
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Sign exposing (Sign)
import Stack
import String.Morph


type Integer afterIBitCountMax
    = N0
    | Signed
        { sign : Sign
        , absoluteAfterI : ArraySized (In (Value N0) afterIBitCountMax) Bit
        }


{-| TODO

How about

    import Natural

    |> Natural.fromN |> Integer.fromNatural

-}
fromNatural : Natural afterIBitCountMax -> Integer afterIBitCountMax
fromNatural =
    \natural ->
        case natural of
            Natural.N0 ->
                N0

            Natural.Positive { afterI } ->
                Signed { sign = Sign.Positive, absoluteAfterI = afterI }


{-| [`Translate`](Morph#Translate) between an `Int` and a [decimal representation](#Integer).

Keep in mind that `Integer -> Int` can overflow
since `Int` is fixed in bit size while [`Integer`](#Integer) is not.

-}
fromInt : Int -> Integer (Up31 x_)
fromInt =
    internalFromInt


internalFromInt : Int -> Integer (Up31 x_)
internalFromInt =
    \intBroad ->
        case
            intBroad
                |> abs
                |> N.intAtLeast n0
                |> Bits.fromN
                |> Bits.unpad
                |> ArraySized.maxTo n32
                |> ArraySized.hasAtLeast n1
        of
            Ok absoluteAtLeast1 ->
                Signed
                    { sign =
                        if intBroad >= 1 then
                            Sign.Positive

                        else
                            Sign.Negative
                    , absoluteAfterI =
                        absoluteAtLeast1
                            |> ArraySized.elementRemove ( Up, n0 )
                            |> ArraySized.minToValue
                    }

            Err _ ->
                N0


{-| [`Translate`](Morph#Translate) between an `Int` and a [decimal representation](#Integer).

Keep in mind that `Integer -> Int` can overflow
since `Int` is fixed in bit size while [`Integer`](#Integer) is not.

-}
toInt : Integer afterIBitCountMax_ -> Int
toInt =
    \integerNarrow ->
        case integerNarrow of
            N0 ->
                0

            Signed signed ->
                (case signed.sign of
                    Sign.Negative ->
                        negate

                    Sign.Positive ->
                        identity
                )
                    (signed.absoluteAfterI
                        |> Debug.todo ""
                    )


{-| Match an integer value as an `Int`.

    import Morph.Error

    -- you can parse integers as `Int` instead of `String`
    "123" |> Text.narrowTo integer --> Ok 123

    -- It also works with negative numbers.
    "-123" |> Text.narrowTo integer --> Ok -123

    -- a decimal number is _not_ an integer
    "3.14"
        |> Text.narrowTo integer
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:2: I was expecting an integer value. I got stuck when I got the character '.'."

    -- but not with invalid numbers
    "abc"
        |> Text.narrowTo integer
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:1: I was expecting an integer value. I got stuck when I got the character 'a'."

-}
decimalRowChar : MorphRow Char (Integer (Min N0))
decimalRowChar =
    Morph.to "whole"
        (Choice.between
            (\n0Variant signedVariant integerNarrow ->
                case integerNarrow of
                    N0 ->
                        n0Variant ()

                    Signed signed ->
                        signedVariant signed
            )
            |> Choice.tryRow (\() -> N0)
                (String.Morph.only "0")
            |> Choice.tryRow Signed
                (Morph.succeed
                    (\signPart absoluteAfterI ->
                        { sign = signPart
                        , absoluteAfterI = absoluteAfterI
                        }
                    )
                    |> Group.grab .sign Sign.maybeMinusChar
                    |> Group.grab .absolute
                        (Morph.translate digitsToBitsAfterI bitsToDigits
                            |> Morph.over
                                (Morph.succeed
                                    |> Group.grab Stack.top
                                        (N.Morph.in_ ( n1, n9 )
                                            |> Morph.over N.Morph.char
                                        )
                                    |> Group.grab Stack.topRemove
                                        (ArraySized.toStackEmptiable
                                            >> Morph.over
                                                (ArraySized.Morph.atLeast n0
                                                    (N.Morph.in_ ( n1, n9 )
                                                        |> Morph.over N.Morph.char
                                                    )
                                                )
                                        )
                                )
                        )
                )
            |> Choice.finishRow
        )


bitsToDigits =
    Debug.todo ""


digitsToBitsAfterI =
    Debug.todo ""
