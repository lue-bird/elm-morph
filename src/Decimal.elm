module Decimal exposing
    ( Decimal(..), Signed, SignedAbsolute(..), Fraction, AtLeast1
    , Exception(..)
    , fromFloat
    , ceiling, floor, truncate
    , toFloat, orExceptionToFloat, exceptionToFloat, exceptionToString
    )

{-| safe and explicit floating point number
without the possibility of [exceptions](Decimal#Exception)

@docs Decimal, Signed, SignedAbsolute, Fraction, AtLeast1
@docs Exception


## create

@docs fromFloat


## alter

@docs ceiling, floor, truncate


## transform

@docs toFloat, orExceptionToFloat, exceptionToFloat, exceptionToString

More in [`Decimal.Morph`](Decimal-Morph)

-}

import Emptiable exposing (Emptiable)
import Integer exposing (Integer)
import N exposing (In, N, N0, N1, N9, n0, n1, n9)
import Natural
import NaturalAtLeast1
import NaturalAtLeast1Base10
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Sign exposing (Sign(..))
import Stack exposing (Stacked)


{-| A decimal number that can have a floating point

Don't shy away from spinning your own version of this if needed, like

    type FieldException
        = DivisionByZeroResult
        | Infinity Sign

    Result FieldException Decimal

See also [`Decimal.Exception`](Decimal#Exception)

-}
type Decimal
    = N0
    | Signed Signed


{-| _Some_ digits after the decimal point. Can't be none.

Note that "fraction" here doesn't mean "rational number with a numerator and denominator"
or "number in range [0;1]"
It means: the absolute value of a written decimal number without a period, like 0.345 or 0.001

You think this name can be improved? → issue

-}
type alias Fraction =
    RecordWithoutConstructorFunction
        { beforeLast : List (N (In N0 N9))
        , last : N (In N1 N9)
        }


{-| Any [`Decimal`](Decimal#Decimal) except `0` is represented this way
-}
type alias Signed =
    RecordWithoutConstructorFunction
        { sign : Sign
        , absolute : SignedAbsolute
        }


{-| What comes after its [`Sign`](Sign#Sign)
-}
type SignedAbsolute
    = Fraction Fraction
    | AtLeast1 AtLeast1


{-| Absolute value of a signed [`Decimal`](#Decimal) ≥ 1
-}
type alias AtLeast1 =
    RecordWithoutConstructorFunction
        { whole : Natural.AtLeast1
        , fraction : Maybe Fraction
        }


{-| Non-number calculation result, see also [IEEE 754 number exception states](#Exception)

`Result Exception Decimal` for example is like an [`elm/core` `Float`](https://dark.elm.dmy.fr/packages/elm/core/latest/Basics#Float)
except

  - [`Exception`](#Exception)s are an explicit case so you can easily extract a [`Decimal`](Decimal#Decimal)
  - it can have arbitrary decimal points, see [`Decimal`](Decimal#Decimal)

-}
type Exception
    = NaN
    | Infinity Sign



--


{-| Remove the [`Fraction`](#Fraction) part after the decimal point `.`
to create an [`Integer`](Integer#Integer)
-}
truncate : Decimal -> Integer
truncate =
    \decimal ->
        case decimal of
            N0 ->
                Integer.N0

            Signed signed ->
                signed |> signedTruncate


signedTruncate : Signed -> Integer
signedTruncate =
    \signed ->
        case signed.absolute of
            Fraction _ ->
                Integer.N0

            AtLeast1 atLeast1 ->
                Integer.Signed
                    { sign = signed.sign
                    , absolute = atLeast1.whole
                    }


{-| Its nearest lower [`Integer`](Integer#Integer) number
-}
floor : Decimal -> Integer
floor =
    \decimal ->
        case decimal of
            N0 ->
                Integer.N0

            Signed signed ->
                signed |> signedFloor


signedFloor : Signed -> Integer
signedFloor =
    \signed ->
        case signed.absolute of
            Fraction _ ->
                Integer.N0

            AtLeast1 atLeast1 ->
                Integer.Signed
                    { sign = signed.sign
                    , absolute =
                        case signed.sign of
                            Positive ->
                                atLeast1.whole

                            Negative ->
                                atLeast1.whole |> NaturalAtLeast1.add NaturalAtLeast1.n1
                    }


{-| Its nearest greater [`Integer`](Integer#Integer) number
-}
ceiling : Decimal -> Integer
ceiling =
    \decimal ->
        case decimal of
            N0 ->
                Integer.N0

            Signed signed ->
                signed |> signedCeiling


signedCeiling : Signed -> Integer
signedCeiling =
    \signed ->
        case signed.absolute of
            Fraction _ ->
                Integer.N0

            AtLeast1 atLeast1 ->
                Integer.Signed
                    { sign = signed.sign
                    , absolute =
                        case signed.sign of
                            Positive ->
                                atLeast1.whole |> NaturalAtLeast1.add NaturalAtLeast1.n1

                            Negative ->
                                atLeast1.whole
                    }


{-| Print an [`Exception`](#Exception)
-}
exceptionToString : Exception -> String
exceptionToString =
    \exception ->
        case exception of
            NaN ->
                "NaN"

            Infinity Sign.Negative ->
                "negative infinity"

            Infinity Sign.Positive ->
                "positive infinity"


{-| Convert from an [`elm/core` `Float`](https://dark.elm.dmy.fr/packages/elm/core/latest/Basics#Float).
Don't be surprised to find more precise representations in [`Decimal`](#Decimal)-land

    -9999.124 |> Decimal.fromFloat
    --→ Ok with the Decimal representation of
    --→ 999.1239999999997962731868028640747070312

-}
fromFloat : Float -> Result Exception Decimal
fromFloat =
    \float_ ->
        if float_ |> Basics.isNaN then
            Err NaN

        else if float_ |> Basics.isInfinite then
            Err
                (Infinity
                    (if float_ < 0 then
                        Negative

                     else
                        Positive
                    )
                )

        else
            Ok (float_ |> fromNonExceptionFloat)


{-| **Should not be exposed**
-}
fromNonExceptionFloat : Float -> Decimal
fromNonExceptionFloat =
    \float_ ->
        if float_ == 0 then
            N0

        else
            -- /= 0
            let
                floatAbsolute =
                    float_ |> Basics.abs

                wholeAbsolute =
                    floatAbsolute |> Basics.truncate
            in
            { sign =
                if float_ < 0 then
                    Negative

                else
                    Positive
            , absolute =
                case wholeAbsolute of
                    0 ->
                        floatAbsolute |> floatToFraction |> Fraction

                    wholeAbsoluteExcept0 ->
                        { whole =
                            wholeAbsoluteExcept0 |> NaturalAtLeast1Base10.fromIntPositive |> NaturalAtLeast1Base10.toBase2
                        , fraction =
                            let
                                floatFraction =
                                    floatAbsolute - (wholeAbsoluteExcept0 |> Basics.toFloat)
                            in
                            if floatFraction == 0 then
                                Nothing

                            else
                                -- floatFraction /= 0
                                floatFraction
                                    |> Basics.abs
                                    |> floatToFraction
                                    |> Just
                        }
                            |> AtLeast1
            }
                |> Signed


floatToFraction : Float -> Fraction
floatToFraction =
    \float_ ->
        case float_ |> floatFractionToBase10 |> unpadLeading0Digits of
            [] ->
                { beforeLast = [], last = n1 |> N.maxTo n9 |> N.inToNumber }

            first :: afterFirst ->
                let
                    digitsReverse : Emptiable (Stacked (N (In N0 N9))) never_
                    digitsReverse =
                        Stack.topBelow first afterFirst |> Stack.reverse
                in
                { beforeLast = digitsReverse |> Stack.removeTop |> Stack.toList |> List.reverse
                , last = digitsReverse |> Stack.top |> N.inToOn |> N.toIn ( n1, n9 ) |> N.inToNumber
                }


floatFractionToBase10 : Float -> List (N (In N0 N9))
floatFractionToBase10 =
    \float_ ->
        if float_ == 0 then
            []

        else
            let
                floatShifted1Digit : Float
                floatShifted1Digit =
                    float_ * 10

                digit : Int
                digit =
                    floatShifted1Digit |> Basics.floor
            in
            (digit |> N.intToIn ( n0, n9 ) |> N.inToNumber)
                :: ((floatShifted1Digit - (digit |> Basics.toFloat))
                        |> floatFractionToBase10
                   )


unpadLeading0Digits : List (N (In N0 N9)) -> List (N (In N0 N9))
unpadLeading0Digits =
    \digits ->
        case digits of
            [] ->
                []

            digit0 :: digits1Up ->
                case N.toInt digit0 of
                    0 ->
                        unpadLeading0Digits digits1Up

                    _ ->
                        digit0 :: digits1Up


{-| NaN, infinity or [`Decimal.toFloat`](#toFloat)

Keep in mind that `Decimal -> Float` can be lossy
since `Float` is fixed in bit size while [`Decimal`](Decimal#Decimal) is not

-}
orExceptionToFloat : Result Exception Decimal -> Float
orExceptionToFloat =
    \decimalOrException ->
        case decimalOrException of
            Err exception ->
                exception |> exceptionToFloat

            Ok decimal ->
                decimal |> toFloat


{-| infinity/NaN represented as a `Float`
-}
exceptionToFloat : Exception -> Float
exceptionToFloat =
    \exception ->
        case exception of
            NaN ->
                floatNaN

            Infinity Sign.Positive ->
                floatInfinity

            Infinity Sign.Negative ->
                -floatInfinity


floatNaN : Float
floatNaN =
    0.0 / 0.0


floatInfinity : Float
floatInfinity =
    1.0 / 0.0


{-| TODO

Keep in mind that `DecimalOrException -> Float` can be lossy
since `Float` is fixed in bit size while [`Decimal`](Decimal#Decimal) is not

-}
toFloat : Decimal -> Float
toFloat =
    \decimal ->
        case decimal of
            N0 ->
                0

            Signed numberSigned ->
                let
                    toSigned =
                        case numberSigned.sign of
                            Negative ->
                                Basics.negate

                            Positive ->
                                Basics.abs
                in
                numberSigned.absolute |> signedAbsoluteToFloat |> toSigned


signedAbsoluteToFloat : SignedAbsolute -> Float
signedAbsoluteToFloat =
    \absolute ->
        case absolute of
            Fraction fraction ->
                fraction |> fractionToFloat

            AtLeast1 atLeast1 ->
                let
                    wholeFloat : Float
                    wholeFloat =
                        atLeast1.whole |> Natural.AtLeast1 |> Natural.toN |> N.toFloat
                in
                case atLeast1.fraction of
                    Nothing ->
                        wholeFloat

                    Just fraction_ ->
                        wholeFloat + (fraction_ |> fractionToFloat)


fractionToFloat : Fraction -> Float
fractionToFloat =
    \fraction_ ->
        fraction_.beforeLast
            ++ [ fraction_.last |> N.inToOn |> N.minTo n0 |> N.inToNumber ]
            |> List.indexedMap
                (\decimal digit ->
                    (digit |> N.toFloat)
                        * (10 ^ -(1 + (decimal |> Basics.toFloat)))
                )
            |> List.sum
