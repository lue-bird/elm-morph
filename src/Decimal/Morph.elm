module Decimal.Morph exposing
    ( orException, value
    , chars
    , orExceptionValue, exceptionValue
    , orExceptionFloat, orExceptionToFloat
    )

{-| [`Decimal`](Decimal#Decimal) [`Morph`](Morph#Morph)

@docs orException, value


## row

@docs chars


## [`Decimal`](Decimal#Decimal) [`OrException`](Decimal#OrException)

[`Morph`](Morph#Morph) a [`Decimal`](Decimal#Decimal) where infinities and NaN are possible states

@docs orExceptionValue, exceptionValue


## `Float`

@docs orExceptionFloat, orExceptionToFloat

-}

import Decimal exposing (Decimal(..), Exception(..), Fraction, OrException(..), SignedAbsolute(..))
import Emptiable exposing (Emptiable)
import Maybe.Morph
import Morph exposing (Morph, MorphOrError, MorphRow, grab, match, one, oneToOne)
import N exposing (In, N, N0, N9, n0, n1, n9)
import N.Morph
import NaturalAtLeast1.Internal
import NaturalAtLeast1Base10
import Sign exposing (Sign(..))
import Sign.Morph
import Stack exposing (Stacked)
import String.Morph
import Value
import Value.Morph exposing (MorphValue)


{-| [`Morph`](Morph#Morph)
a [`Decimal`](Decimal#Decimal)
to an [`OrException Decimal`](Decimal#OrException)
-}
orException : Morph Decimal (OrException Decimal)
orException =
    Morph.custom "Decimal"
        { toNarrow =
            \floatExplicit_ ->
                case floatExplicit_ of
                    Number number ->
                        number |> Ok

                    Exception _ ->
                        "Exception" |> Err
        , toBroad = Number
        }


{-| Match a decimal number

    import Morph.Error


    -- trailing 0s aren't represented in the final type

    "12.0340000" |> Text.toNarrow number  --> Ok 12.034
    "-12.000" |> Text.toNarrow number --> Ok -12.0

    -- leading floating point is allowed

    ".012" |> Text.toNarrow number    --> Ok 0.012
    "-.12" |> Text.toNarrow number   --> Ok -0.12

    -- fails for integers without a floating point

    "12"
        |> Text.toNarrow number
        |> Result.mapError Morph.Error.textMessage
    --> Err ...

    -- but succeeds for integers with a trailing floating point

    "12." |> Text.toNarrow number    --> Ok 12.0

    -- fails for everything else

    "."
        |> Text.toNarrow number
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:1: I was expecting a digit [0-9]. I got stuck when I got the character '.'."

    "abc"
        |> Text.toNarrow number
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:1: I was expecting a digit [0-9]. I got stuck when I got the character 'a'."

To allow integers to parse as decimals as well,
build a [`Morph.choice`](Morph#choice)
[`Decimal.Morph.chars`](#chars)
and [`Integer.Morph.chars`](Integer-Morph#chars)

For different parsing behavior, spin your own
using [`Decimal.Morph.chars`](#chars) implementation as a reference

-}
chars : MorphRow Decimal Char
chars =
    Morph.named "decimal"
        (Morph.choice
            (\signedVariant n0Variant numberNarrow ->
                case numberNarrow of
                    N0 ->
                        n0Variant ()

                    Signed signedValue ->
                        signedVariant signedValue
            )
            |> Morph.tryRow Signed signedChars
            |> Morph.tryRow (\() -> N0) (String.Morph.only "0.")
            |> Morph.choiceFinish
            |> match
                (Morph.broad []
                    |> Morph.overRow
                        (Morph.whilePossible
                            (String.Morph.only "0")
                        )
                )
        )


signedChars : MorphRow Decimal.Signed Char
signedChars =
    Morph.named "signed"
        (Morph.succeed
            (\signPart absolutePart ->
                { sign = signPart
                , absolute = absolutePart
                }
            )
            |> grab .sign Sign.Morph.maybeMinusChar
            |> grab .absolute signedAbsoluteChars
        )


signedAbsoluteChars : MorphRow Decimal.SignedAbsolute Char
signedAbsoluteChars =
    Morph.named "absolute"
        (Morph.choice
            (\fractionVariant atLeast1Variant absoluteUnion ->
                case absoluteUnion of
                    Fraction fractionValue ->
                        fractionVariant fractionValue

                    AtLeast1 atLeast1Value ->
                        atLeast1Variant atLeast1Value
            )
            |> Morph.tryRow Fraction
                (Morph.succeed (\fraction_ -> fraction_)
                    |> match
                        (Morph.broad (Just ())
                            |> Morph.overRow
                                (Maybe.Morph.row (String.Morph.only "0"))
                        )
                    |> match (String.Morph.only ".")
                    |> grab (\fraction_ -> fraction_) fractionChars
                )
            |> Morph.tryRow AtLeast1
                (Morph.succeed
                    (\wholePart fractionPart ->
                        { whole = wholePart
                        , fraction = fractionPart
                        }
                    )
                    |> grab .whole NaturalAtLeast1.Internal.chars
                    |> match (String.Morph.only ".")
                    |> grab .fraction (Maybe.Morph.row fractionChars)
                )
            |> Morph.choiceFinish
        )


fractionChars : MorphRow Fraction Char
fractionChars =
    Morph.named "fraction"
        (Morph.succeed
            (\beforeLast last ->
                { beforeLast = beforeLast, last = last }
            )
            |> Morph.grab .beforeLast
                (Morph.whilePossible
                    (oneToOne N.inToNumber N.inToOn
                        |> Morph.over N.Morph.char
                        |> one
                    )
                )
            |> Morph.grab .last
                (oneToOne N.inToNumber N.inToOn
                    |> Morph.over (N.Morph.in_ ( n1, n9 ))
                    |> Morph.over N.Morph.char
                    |> one
                )
        )


{-| [`MorphValue`](Value-Morph#MorphValue) from a [`Decimal`](Decimal#Decimal)

To get a [`MorphValue`](Value-Morph#MorphValue) from a `Float`,
use [`Decimal.Morph.orExceptionToFloat`](#orExceptionToFloat)
over [`Decimal.Morph.orExceptionValue`](#orExceptionValue)

-}
value : MorphValue Decimal
value =
    Morph.custom "Decimal"
        { toNarrow =
            \atom ->
                case atom of
                    Value.Number decimal ->
                        decimal |> Ok

                    atomExceptDecimal ->
                        atomExceptDecimal |> Value.atomKindToString |> Err
        , toBroad = Value.Number
        }
        |> Morph.over Value.Morph.atom



-- OrException Decimal


{-| [`MorphValue`](Value-Morph#MorphValue) from an [`Exception`](Decimal#Exception)
-}
exceptionValue : MorphValue Exception
exceptionValue =
    Morph.choice
        (\variantNaN variantInfinity choiceException ->
            case choiceException of
                NaN ->
                    variantNaN ()

                Infinity sign ->
                    variantInfinity sign
        )
        |> Value.Morph.variant ( \() -> NaN, "NaN" ) Value.Morph.unit
        |> Value.Morph.variant ( Infinity, "NaN" ) signValue
        |> Value.Morph.choiceFinish


signValue : MorphValue Sign
signValue =
    Morph.choice
        (\negative positive sign ->
            case sign of
                Negative ->
                    negative ()

                Positive ->
                    positive ()
        )
        |> Value.Morph.variant ( \() -> Negative, "Negative" ) Value.Morph.unit
        |> Value.Morph.variant ( \() -> Positive, "Positive" ) Value.Morph.unit
        |> Value.Morph.choiceFinish


{-| [`Morph`](Morph#Morph)
an [`elm/core` `Float`](https://dark.elm.dmy.fr/packages/elm/core/latest/Basics#Float)
to a [`OrException Decimal`](Decimal#OrException)

Keep in mind that `DecimalOrException -> Float` can be lossy
since `Float` is fixed in bit size while [`OrException Decimal`](Decimal#OrException) is not

    -9999.124
        |> broaden
            (Decimal.Morph.chars
                |> Morph.overRow Decimal.orException
                |> Morph.overRow DecimalOrException.toFloat
                |> Morph.rowFinish
            )
    --> "-999.1239999999997962731868028640747070312"

-}
orExceptionFloat : MorphOrError (OrException Decimal) Float error_
orExceptionFloat =
    Morph.variants
        ( \variantDecimal variantNaN variantInfinity choiceFloat ->
            if choiceFloat |> Basics.isNaN then
                variantNaN ()

            else if choiceFloat |> Basics.isInfinite then
                variantInfinity
                    (if choiceFloat < 0 then
                        Negative

                     else
                        Positive
                    )

            else
                variantDecimal choiceFloat
        , \variantDecimal variantNaN variantInfinity choiceExplicit ->
            case choiceExplicit of
                Number decimal ->
                    variantDecimal decimal

                Exception NaN ->
                    variantNaN ()

                Exception (Infinity sign) ->
                    variantInfinity sign
        )
        |> Morph.variant "Number"
            ( Number, identity )
            (Morph.oneToOne
                (\float_ ->
                    if float_ == 0 then
                        Decimal.N0

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
                                    floatAbsolute |> floatToFraction |> Decimal.Fraction

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
                                        |> Decimal.AtLeast1
                        }
                            |> Decimal.Signed
                )
                (\floatNarrow ->
                    case floatNarrow of
                        Decimal.N0 ->
                            0

                        Decimal.Signed numberSigned ->
                            let
                                toSigned =
                                    case numberSigned.sign of
                                        Negative ->
                                            Basics.negate

                                        Positive ->
                                            Basics.abs
                            in
                            numberSigned.absolute |> signedAbsoluteToFloat |> toSigned
                )
            )
        |> Morph.variant "NaN" ( \() -> Exception NaN, identity ) (Morph.oneToOne identity (\() -> floatNaN))
        |> Morph.variant "Infinity"
            ( \sign -> Exception (Infinity sign), identity )
            (Morph.oneToOne identity
                (\sign ->
                    let
                        toSigned =
                            case sign of
                                Negative ->
                                    Basics.negate

                                Positive ->
                                    Basics.abs
                    in
                    floatInfinity |> toSigned
                )
            )
        |> Morph.variantsFinish
        |> Morph.narrowErrorMap Morph.deadEndNever


floatNaN : Float
floatNaN =
    0.0 / 0.0


floatInfinity : Float
floatInfinity =
    1.0 / 0.0


signedAbsoluteToFloat : Decimal.SignedAbsolute -> Float
signedAbsoluteToFloat =
    \absolute ->
        case absolute of
            Decimal.Fraction fraction ->
                fraction |> fractionToFloat

            Decimal.AtLeast1 atLeast1 ->
                let
                    wholeFloat : Float
                    wholeFloat =
                        atLeast1.whole |> NaturalAtLeast1.Internal.toN |> N.toFloat
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
                    floatShifted1Digit |> floor
            in
            (digit |> N.intToIn ( n0, n9 ) |> N.inToNumber)
                :: ((floatShifted1Digit - (digit |> Basics.toFloat))
                        |> floatFractionToBase10
                   )


{-| [`Morph`](Morph#Morph)
a [`OrException Decimal`](Decimal#OrException)
to an [`elm/core` `Float`](https://dark.elm.dmy.fr/packages/elm/core/latest/Basics#Float)

Keep in mind that `DecimalOrException -> Float` can be lossy
since `Float` is fixed in bit size while [`OrException Decimal`](Decimal#OrException) is not

    -9999.124
        |> broaden
            (Decimal.Morph.chars
                |> Morph.overRow Decimal.orException
                |> Morph.overRow DecimalOrException.toFloat
                |> Morph.rowFinish
            )
    --> "-999.1239999999997962731868028640747070312"

-}
orExceptionToFloat : MorphOrError Float (OrException Decimal) error_
orExceptionToFloat =
    Morph.invert orExceptionFloat


{-| `Float` [`MorphValue`](Value-Morph#MorphValue)
-}
orExceptionValue : MorphValue (OrException Decimal)
orExceptionValue =
    Morph.choice
        (\variantDecimal variantException choiceExplicit ->
            case choiceExplicit of
                Number decimal ->
                    variantDecimal decimal

                Exception exception ->
                    variantException exception
        )
        |> Value.Morph.variant ( Number, "Decimal" ) decimalInternalValue
        |> Value.Morph.variant ( Exception, "Exception" ) exceptionValue
        |> Value.Morph.choiceFinish


decimalInternalValue : MorphValue Decimal
decimalInternalValue =
    Morph.custom "Decimal"
        { toNarrow =
            \atom ->
                case atom of
                    Value.Number decimal ->
                        decimal |> Ok

                    atomExceptDecimal ->
                        atomExceptDecimal
                            |> Value.atomKindToString
                            |> Err
        , toBroad = Value.Number
        }
        |> Morph.over Value.Morph.atom
