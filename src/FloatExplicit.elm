module FloatExplicit exposing
    ( FloatExplicit(..), Exception(..)
    , value, exceptionValue
    , float, toFloat
    )

{-| [`Morph`](Morph#Morph) an IEEE 754 floating point number


## [`Decimal`](Decimal#Decimal) or [`Exception`](#Exception)

@docs FloatExplicit, Exception
@docs value, exceptionValue


## `Float`

@docs float, toFloat

-}

import Choice
import Decimal.Internal
import Emptiable exposing (Emptiable)
import Linear exposing (Direction(..))
import Morph exposing (MorphOrError)
import N exposing (Down, In, N, N0, N1, N9, Up0, Up1, Up9, n0, n1, n9)
import Possibly exposing (Possibly)
import Sign exposing (Sign)
import Sign.Internal
import Stack exposing (Stacked)
import Value exposing (MorphValue)
import Value.PackageInternal
import Whole


{-| IEEE 754 floating point number

It's like an [`elm/core` `Float`](https://dark.elm.dmy.fr/packages/elm/core/latest/Basics#Float)
but with an explicit case for an [`Exception`](#Exception)

-}
type FloatExplicit
    = Decimal Decimal.Internal.Decimal
    | Exception Exception


{-| Non-number float
-}
type Exception
    = NaN
    | Infinity Sign


{-| [`Morph`](Morph#Morph)
an [`elm/core` `Float`](https://dark.elm.dmy.fr/packages/elm/core/latest/Basics#Float)
to a [`FloatExplicit`](#FloatExplicit)

Keep in mind that `FloatExplicit -> Float` can be lossy
since `Float` is fixed in bit size while [`FloatExplicit`](#FloatExplicit) is not

    -9999.124
        |> broaden
            (Decimal.rowChar
                |> Morph.overRow Decimal.floatExplicit
                |> Morph.overRow FloatExplicit.toFloat
                |> Morph.rowFinish
            )
    --> "-999.1239999999997962731868028640747070312"

-}
float : MorphOrError FloatExplicit Float error_
float =
    Choice.toFrom
        ( \variantDecimal variantNaN variantInfinity choiceFloat ->
            if choiceFloat |> Basics.isNaN then
                variantNaN ()

            else if choiceFloat |> Basics.isInfinite then
                variantInfinity
                    (if choiceFloat < 0 then
                        Sign.Negative

                     else
                        Sign.Positive
                    )

            else
                variantDecimal choiceFloat
        , \variantDecimal variantNaN variantInfinity choiceExplicit ->
            case choiceExplicit of
                Decimal decimal ->
                    variantDecimal decimal

                Exception NaN ->
                    variantNaN ()

                Exception (Infinity sign) ->
                    variantInfinity sign
        )
        |> Choice.variant ( Decimal, identity )
            (Morph.translate
                (\float_ ->
                    if float_ == 0 then
                        Decimal.Internal.N0

                    else
                        -- /= 0
                        let
                            floatAbsolute =
                                float_ |> Basics.abs

                            wholeAbsolute =
                                floatAbsolute |> truncate
                        in
                        { sign =
                            if float_ < 0 then
                                Sign.Internal.Negative

                            else
                                Sign.Internal.Positive
                        , absolute =
                            case wholeAbsolute of
                                0 ->
                                    floatAbsolute |> floatToFraction |> Decimal.Internal.Fraction

                                wholeAbsoluteExcept0 ->
                                    { whole =
                                        wholeAbsoluteExcept0 |> Whole.fromIntPositive
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
                                        |> Decimal.Internal.AtLeast1
                        }
                            |> Decimal.Internal.Signed
                )
                (\floatNarrow ->
                    case floatNarrow of
                        Decimal.Internal.N0 ->
                            0

                        Decimal.Internal.Signed numberSigned ->
                            let
                                toSigned =
                                    case numberSigned.sign of
                                        Sign.Internal.Negative ->
                                            Basics.negate

                                        Sign.Internal.Positive ->
                                            Basics.abs
                            in
                            numberSigned.absolute |> absoluteToFloat |> toSigned
                )
            )
        |> Choice.variant ( \() -> Exception NaN, identity ) (Morph.broaden (\() -> floatNaN))
        |> Choice.variant ( \sign -> Exception (Infinity sign), identity )
            (Morph.broaden
                (\sign ->
                    let
                        toSigned =
                            case sign of
                                Sign.Negative ->
                                    Basics.negate

                                Sign.Positive ->
                                    Basics.abs
                    in
                    floatInfinity |> toSigned
                )
            )
        |> Choice.finishToFrom


{-| [`Morph`](Morph#Morph)
a [`FloatExplicit`](#FloatExplicit)
to an [`elm/core` `Float`](https://dark.elm.dmy.fr/packages/elm/core/latest/Basics#Float)

Keep in mind that `FloatExplicit -> Float` can be lossy
since `Float` is fixed in bit size while [`FloatExplicit`](#FloatExplicit) is not

    -9999.124
        |> broaden
            (Decimal.rowChar
                |> Morph.overRow Decimal.floatExplicit
                |> Morph.overRow FloatExplicit.toFloat
                |> Morph.rowFinish
            )
    --> "-999.1239999999997962731868028640747070312"

-}
toFloat : MorphOrError Float FloatExplicit error_
toFloat =
    Morph.invert float


floatNaN : Float
floatNaN =
    0.0 / 0.0


floatInfinity : Float
floatInfinity =
    1.0 / 0.0


absoluteToFloat : Decimal.Internal.Absolute -> Float
absoluteToFloat =
    \absolute ->
        case absolute of
            Decimal.Internal.Fraction fraction ->
                fraction |> fractionToFloat

            Decimal.Internal.AtLeast1 atLeast1 ->
                (atLeast1.whole |> Whole.toIntPositive |> Basics.toFloat)
                    + (case atLeast1.fraction of
                        Nothing ->
                            0

                        Just fraction_ ->
                            fraction_ |> fractionToFloat
                      )


fractionToFloat : Decimal.Internal.Fraction -> Float
fractionToFloat =
    \fraction_ ->
        Stack.one (fraction_.last |> N.inToOn |> N.minTo n0 |> N.inToNumber)
            |> Stack.attach Down fraction_.beforeLast
            |> Stack.map
                (\decimal digit ->
                    (digit |> N.toFloat)
                        * (10 ^ -(1 + (decimal.index |> Basics.toFloat)))
                )
            |> Stack.sum


floatToFraction :
    Float
    -> Decimal.Internal.Fraction
floatToFraction =
    \float_ ->
        case float_ |> floatFractionToDigits of
            Emptiable.Empty _ ->
                { beforeLast = Emptiable.empty, last = n1 |> N.maxTo n9 |> N.inToNumber }

            Emptiable.Filled stacked ->
                let
                    digitsReverse =
                        stacked |> Emptiable.filled |> Stack.reverse
                in
                { beforeLast = digitsReverse |> Stack.removeTop |> Stack.reverse
                , last = digitsReverse |> Stack.top |> N.inToOn |> N.toIn ( n1, n9 ) |> N.inToNumber
                }


floatFractionToDigits : Float -> Emptiable (Stacked (N (In N0 N9))) Possibly
floatFractionToDigits =
    \float_ ->
        let
            floatShifted1Decimal =
                float_ * 10

            decimalInt =
                floatShifted1Decimal |> floor
        in
        (case decimalInt |> N.intIsIn ( n0, n9 ) of
            Err _ ->
                identity

            Ok decimal ->
                Stack.onTopLay (decimal |> N.inToNumber)
        )
            ((floatShifted1Decimal - (decimalInt |> Basics.toFloat))
                |> floatFractionToDigits
            )


{-| `Float` [`MorphValue`](Value#MorphValue)
-}
value : MorphValue FloatExplicit
value =
    Choice.between
        (\variantDecimal variantException choiceExplicit ->
            case choiceExplicit of
                Decimal decimal ->
                    variantDecimal decimal

                Exception exception ->
                    variantException exception
        )
        |> Choice.variantValue ( Decimal, "Decimal" ) decimalInternalValue
        |> Choice.variantValue ( Exception, "Exception" ) exceptionValue
        |> Choice.finishValue


decimalInternalValue : MorphValue Decimal.Internal.Decimal
decimalInternalValue =
    Morph.value "Decimal"
        { narrow =
            \atom ->
                case atom of
                    Value.Number decimal ->
                        decimal |> Ok

                    atomExceptDecimal ->
                        atomExceptDecimal
                            |> Value.PackageInternal.atomKindToString
                            |> Err
        , broaden = Value.Number
        }
        |> Morph.over Value.atom


{-| [`MorphValue`](Value#MorphValue) from an [`Exception`](#Exception)
-}
exceptionValue : MorphValue Exception
exceptionValue =
    Choice.between
        (\variantNaN variantInfinity choiceException ->
            case choiceException of
                NaN ->
                    variantNaN ()

                Infinity sign ->
                    variantInfinity sign
        )
        |> Choice.variantValue ( \() -> NaN, "NaN" ) Value.unit
        |> Choice.variantValue ( Infinity, "NaN" ) signValue
        |> Choice.finishValue


signValue : MorphValue Sign
signValue =
    Choice.between
        (\negative positive sign ->
            case sign of
                Sign.Negative ->
                    negative ()

                Sign.Positive ->
                    positive ()
        )
        |> Choice.variantValue ( \() -> Sign.Negative, "Negative" ) Value.unit
        |> Choice.variantValue ( \() -> Sign.Positive, "Positive" ) Value.unit
        |> Choice.finishValue
