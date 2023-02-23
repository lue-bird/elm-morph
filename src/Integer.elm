module Integer exposing
    ( Integer(..), Signed
    , int, toInt
    , decimal
    , value
    , rowChar
    )

{-| Arbitrary-precision `Int`

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
import Decimal exposing (Decimal)
import Decimal.Internal exposing (Whole)
import Emptiable exposing (Emptiable)
import Linear exposing (Direction(..))
import Morph exposing (Morph, MorphRow, Translate)
import N exposing (Add1, In, Infinity, Min, N, N0, N1, N9, On, To, Up, Up0, Up1, Up9, n0, n1, n2, n9)
import N.Local exposing (N31, Up31, n32)
import N.Morph
import Possibly exposing (Possibly(..))
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Sign exposing (Sign)
import Stack exposing (Stacked)
import String.Morph
import Value
import Whole


{-| Arbitrary-precision `Int`, constructable from bits
-}
type Integer
    = N0
    | Signed Signed


{-| Arbitrary-precision signed [`Integer`](#Integer), constructable from bits
-}
type alias Signed =
    RecordWithoutConstructorFunction
        { sign : Sign
        , absoluteAfterI : ArraySized Bit (Min N0)
        }


{-| [`Morph`](Morph#Morph) an [`Integer`](#Integer)
from a [`Decimal`](Decimal#Decimal)
without digits after the decimal point
-}
decimal : Morph Integer Decimal
decimal =
    Morph.choiceToFrom
        ( \n0 variantSigned decimalChoice ->
            case decimalChoice of
                Decimal.N0 ->
                    n0 ()

                Decimal.Signed signedValue ->
                    variantSigned signedValue
        , \variantN0 variantSigned integerChoice ->
            case integerChoice of
                N0 ->
                    variantN0 ()

                Signed signedValue ->
                    variantSigned signedValue
        )
        |> Morph.variant
            ( \() -> N0, \() -> Decimal.N0 )
            (Morph.broad ())
        |> Morph.variant
            ( Signed, Decimal.Signed )
            decimalSigned
        |> Morph.choiceToFromFinish


decimalSigned : Morph Signed Decimal.Signed
decimalSigned =
    Debug.todo ""


{-| [`Value.Morph`](Value#Morph) from an [`Integer`](#Integer)
-}
value : Value.Morph Integer
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
                |> N.intToAtLeast n0
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
                            |> ArraySized.removeMin ( Up, n0 )
                            |> ArraySized.minToNumber
                            |> ArraySized.maxToInfinity
                    }

            Err _ ->
                N0


{-| [`Translate`](Morph#Translate) between an `Int` and a [decimal representation](#Integer).

Keep in mind that `Integer -> Int` can overflow
since `Int` is fixed in bit size while [`Integer`](#Integer) is not.

-}
toInt : Translate Int Integer
toInt =
    Morph.invert int


internalToInt : Integer -> Int
internalToInt =
    \integerNarrow ->
        case integerNarrow of
            N0 ->
                0

            Signed signedValue ->
                let
                    intSign =
                        case signedValue.sign of
                            Sign.Negative ->
                                negate

                            Sign.Positive ->
                                identity
                in
                signedValue.absoluteAfterI
                    |> ArraySized.minToOn
                    |> ArraySized.insertMin ( Up, n0 ) Bit.I
                    |> Bits.takeAtMost n32
                    |> Bits.toN
                    |> N.toInt
                    |> intSign


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
rowChar : MorphRow Integer Char
rowChar =
    Morph.to "integer"
        (Morph.choice
            (\n0Variant signedVariant integerNarrow ->
                case integerNarrow of
                    N0 ->
                        n0Variant ()

                    Signed signedValue ->
                        signedVariant signedValue
            )
            |> Morph.tryRow (\() -> N0) (String.Morph.only "0")
            |> Morph.tryRow Signed signed
            |> Morph.choiceRowFinish
        )


signed : MorphRow Signed Char
signed =
    Morph.succeed
        (\signPart absoluteAfterIPart ->
            { sign = signPart
            , absoluteAfterI = absoluteAfterIPart
            }
        )
        |> Morph.grab .sign Sign.maybeMinusChar
        |> Morph.grab .absoluteAfterI absoluteAfterI


absoluteAfterI :
    Morph.MorphRowIndependently
        (ArraySized Bit (In broadMin broadMax))
        (ArraySized Bit (Min N0))
        Char
absoluteAfterI =
    Morph.translate digitsToBitsAfterI bitsAfterIToDigits
        |> Morph.overRow Whole.rowChar


bitsAfterIToDigits : ArraySized Bit range_ -> Whole
bitsAfterIToDigits =
    \bitsAfterI ->
        bitsAfterI
            |> Debug.todo ""


digitsToBitsAfterI : Whole -> ArraySized Bit (Min N0)
digitsToBitsAfterI =
    \digits ->
        digits
            |> Debug.todo ""
