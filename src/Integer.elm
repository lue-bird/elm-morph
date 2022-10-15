module Integer exposing
    ( Integer(..)
    , int, toInt
    , decimal
    , value
    , rowChar
    )

{-| Typed-precision `Int`

@docs Integer


## [`Morph`](Morph#Morph)

@docs int, toInt
@docs decimal
@docs value
@docs rowChar

-}

import ArraySized exposing (ArraySized)
import ArraySized.Morph
import Bit exposing (Bit)
import Bits
import Choice
import Decimal exposing (Decimal)
import Group
import Linear exposing (Direction(..))
import Morph exposing (Morph, MorphRow, Translate)
import N exposing (Add1, Fixed, In, Infinity, InfinityValue, Min, N, N0, To, Up, Value, n0, n1, n2, n9)
import N.Local exposing (N31, Up31, n32)
import N.Morph
import Possibly exposing (Possibly(..))
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Sign exposing (Sign)
import Stack
import String.Morph
import Value exposing (MorphValue)


type Integer
    = N0
    | Signed
        { sign : Sign
        , absoluteAfterI : ArraySized (In (Value N0) InfinityValue) Bit
        }


decimal : Morph Integer Decimal
decimal =
    Debug.todo ""


{-| [`MorphValue`](Value#MorphValue) from an [`Integer`](#Integer)
-}
value : MorphValue Integer
value =
    decimal
        |> Morph.over Decimal.value


{-| [`Translate`](Morph#Translate) between an `Int` and a [decimal representation](#Integer).

Keep in mind that `Integer -> Int` can overflow
since `Int` is fixed in bit size while [`Integer`](#Integer) is not.

-}
int : Translate Integer Int
int =
    Morph.translate internalFromInt internalToInt


internalFromInt : Int -> Integer
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
                        if intBroad >= 0 then
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
toInt : Translate Int Integer
toInt =
    Morph.reverse int


internalToInt : Integer -> Int
internalToInt =
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
rowChar : MorphRow Char Integer
rowChar =
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
