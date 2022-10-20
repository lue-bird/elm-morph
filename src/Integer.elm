module Integer exposing
    ( Integer(..), Signed
    , int, toInt
    , decimal
    , value
    , rowChar
    )

{-| Typed-precision `Int`

@docs Integer, Signed


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
import Emptiable exposing (Emptiable)
import Group
import Linear exposing (Direction(..))
import Morph exposing (Morph, MorphRow, Translate)
import N exposing (Add1, Fixed, In, InFixed, Infinity, InfinityValue, Min, N, N0, N1, N9, To, Up, Up0, Up1, Up9, Value, n0, n1, n2, n9)
import N.Local exposing (N31, Up31, n32)
import N.Morph
import Possibly exposing (Possibly(..))
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Sign exposing (Sign)
import Stack exposing (StackTopBelow)
import String.Morph
import Value exposing (MorphValue)


type Integer
    = N0
    | Signed Signed


type alias Signed =
    RecordWithoutConstructorFunction
        { sign : Sign
        , absoluteAfterI : ArraySized (In (Value N0) InfinityValue) Bit
        }


decimal : Morph Integer Decimal
decimal =
    Choice.toFrom
        ( \n0 signed decimalChoice ->
            case decimalChoice of
                Decimal.N0 ->
                    n0 ()

                Decimal.Signed signedValue ->
                    signed signedValue
        , \n0 signed integerChoice ->
            case integerChoice of
                N0 ->
                    n0 ()

                Signed signedValue ->
                    signed signedValue
        )
        |> Choice.variant
            ( \() -> N0, \() -> Decimal.N0 )
            (Morph.broad ())
        |> Choice.variant
            ( Signed, Decimal.Signed )
            decimalSigned
        |> Choice.finishToFrom


decimalSigned : Morph Signed Decimal.Signed
decimalSigned =
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
                            |> ArraySized.elementRemoveMin ( Up, n0 )
                            |> ArraySized.maxToInfinity
                            |> ArraySized.toValue
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
                    |> Group.grab .absoluteAfterI
                        (ArraySized.Morph.toValue
                            |> Morph.over
                                (Morph.translate digitsToBitsAfterI bitsAfterIToDigits)
                            |> Morph.overRow
                                (Morph.succeed Stack.onTopLay
                                    |> Group.grab Stack.top
                                        (N.Morph.in_ ( n1, n9 )
                                            |> Morph.over N.Morph.char
                                            |> Morph.one
                                        )
                                    |> Group.grab Stack.topRemove
                                        (ArraySized.Morph.toStackEmptiable
                                            |> Morph.overRow
                                                (ArraySized.Morph.atLeast n0
                                                    (N.Morph.in_ ( n0, n9 )
                                                        |> Morph.over N.Morph.char
                                                        |> Morph.one
                                                    )
                                                )
                                        )
                                )
                        )
                )
            |> Choice.finishRow
        )


bitsAfterIToDigits :
    ArraySized range_ Bit
    ->
        Emptiable
            (StackTopBelow
                (N (In (Up1 topMinX_) (Up9 topMaxX_)))
                (N (In (Up0 belowTopMinX_) (Up9 belowTopMaxX_)))
            )
            never_
bitsAfterIToDigits =
    \bitsAfterI ->
        bitsAfterI
            |> Debug.todo ""
            |> intToDigits


intToDigits :
    Int
    ->
        Emptiable
            (StackTopBelow
                (N (In (Up1 topMinX_) (Up9 topMaxX_)))
                (N (In (Up0 belowTopMinX_) (Up9 belowTopMaxX_)))
            )
            never_
intToDigits =
    Debug.todo ""


digitsToBitsAfterI :
    Emptiable
        (StackTopBelow
            (N (InFixed N1 N9))
            (N (InFixed N0 N9))
        )
        Never
    -> ArraySized (Min (Up0 minX_)) Bit
digitsToBitsAfterI =
    \digits ->
        digits
            |> digitsToInt
            |> Debug.todo ""


digitsToInt :
    Emptiable
        (StackTopBelow
            (N (InFixed N1 N9))
            (N (InFixed N0 N9))
        )
        Never
    -> Int
digitsToInt =
    Debug.todo ""
