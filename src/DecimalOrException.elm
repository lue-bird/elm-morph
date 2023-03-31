module DecimalOrException exposing
    ( OrException(..), Exception(..)
    , value, exceptionValue
    , float, toFloat
    )

{-| [`Morph`](Morph#Morph) a [`Decimal`](Decimal#Decimal) where infinities and NaN are possible states


## [`Decimal`](Decimal#Decimal) or [`Exception`](#Exception)

@docs OrException, Exception
@docs value, exceptionValue


## `Float`

@docs float, toFloat

-}

import Decimal exposing (Decimal, Fraction)
import Emptiable exposing (Emptiable)
import Linear exposing (Direction(..))
import Morph exposing (MorphOrError)
import N exposing (In, N, N0, N9, n0, n1, n9)
import NaturalAtLeast1.Internal
import NaturalAtLeast1Base10
import Possibly exposing (Possibly)
import Sign exposing (Sign(..))
import Stack exposing (Stacked)
import Value


{-| Number where [IEEE 754 number exception states](#Exception) are possible

`OrException Decimal` for example is like an [`elm/core` `Float`](https://dark.elm.dmy.fr/packages/elm/core/latest/Basics#Float)
except

  - [`Exception`](#Exception)s are an explicit case so you can easily extract a [`Decimal`](Decimal#Decimal)
  - it can have arbitrary decimal points, see [`Decimal`](Decimal#Decimal)

-}
type OrException aNumber
    = Number aNumber
    | Exception Exception


{-| Non-number calculation result
-}
type Exception
    = NaN
    | Infinity Sign


{-| [`Value.Morph`](Value#Morph) from an [`Exception`](#Exception)
-}
exceptionValue : Value.Morph Exception
exceptionValue =
    Morph.choice
        (\variantNaN variantInfinity choiceException ->
            case choiceException of
                NaN ->
                    variantNaN ()

                Infinity sign ->
                    variantInfinity sign
        )
        |> Value.variant ( \() -> NaN, "NaN" ) Value.unit
        |> Value.variant ( Infinity, "NaN" ) signValue
        |> Value.choiceFinish


signValue : Value.Morph Sign
signValue =
    Morph.choice
        (\negative positive sign ->
            case sign of
                Negative ->
                    negative ()

                Positive ->
                    positive ()
        )
        |> Value.variant ( \() -> Negative, "Negative" ) Value.unit
        |> Value.variant ( \() -> Positive, "Positive" ) Value.unit
        |> Value.choiceFinish


{-| [`Morph`](Morph#Morph)
an [`elm/core` `Float`](https://dark.elm.dmy.fr/packages/elm/core/latest/Basics#Float)
to a [`OrException Decimal`](#OrException)

Keep in mind that `DecimalOrException -> Float` can be lossy
since `Float` is fixed in bit size while [`OrException Decimal`](#OrException) is not

    -9999.124
        |> broaden
            (Decimal.Morph.chars
                |> Morph.overRow Decimal.orException
                |> Morph.overRow DecimalOrException.toFloat
                |> Morph.rowFinish
            )
    --> "-999.1239999999997962731868028640747070312"

-}
float : MorphOrError (OrException Decimal) Float error_
float =
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
            (Morph.translate
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
        |> Morph.variant "NaN" ( \() -> Exception NaN, identity ) (Morph.broaden (\() -> floatNaN))
        |> Morph.variant "Infinity"
            ( \sign -> Exception (Infinity sign), identity )
            (Morph.broaden
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
        Stack.one (fraction_.last |> N.inToOn |> N.minTo n0 |> N.inToNumber)
            |> Stack.attach Down fraction_.beforeLast
            |> Stack.map
                (\decimal digit ->
                    (digit |> N.toFloat)
                        * (10 ^ -(1 + (decimal.index |> Basics.toFloat)))
                )
            |> Stack.sum


floatToFraction : Float -> Fraction
floatToFraction =
    \float_ ->
        case float_ |> floatFractionToBase10 of
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


floatFractionToBase10 : Float -> Emptiable (Stacked (N (In N0 N9))) Possibly
floatFractionToBase10 =
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
                |> floatFractionToBase10
            )


{-| [`Morph`](Morph#Morph)
a [`OrException Decimal`](#OrException)
to an [`elm/core` `Float`](https://dark.elm.dmy.fr/packages/elm/core/latest/Basics#Float)

Keep in mind that `DecimalOrException -> Float` can be lossy
since `Float` is fixed in bit size while [`OrException Decimal`](#OrException) is not

    -9999.124
        |> broaden
            (Decimal.Morph.chars
                |> Morph.overRow Decimal.orException
                |> Morph.overRow DecimalOrException.toFloat
                |> Morph.rowFinish
            )
    --> "-999.1239999999997962731868028640747070312"

-}
toFloat : MorphOrError Float (OrException Decimal) error_
toFloat =
    Morph.invert float


{-| `Float` [`Value.Morph`](Value#Morph)
-}
value : Value.Morph (OrException Decimal)
value =
    Morph.choice
        (\variantDecimal variantException choiceExplicit ->
            case choiceExplicit of
                Number decimal ->
                    variantDecimal decimal

                Exception exception ->
                    variantException exception
        )
        |> Value.variant ( Number, "Decimal" ) decimalInternalValue
        |> Value.variant ( Exception, "Exception" ) exceptionValue
        |> Value.choiceFinish


decimalInternalValue : Value.Morph Decimal
decimalInternalValue =
    Morph.value "Decimal"
        { narrow =
            \atom ->
                case atom of
                    Value.Number decimal ->
                        decimal |> Ok

                    atomExceptDecimal ->
                        atomExceptDecimal
                            |> Value.atomKindToString
                            |> Err
        , broaden = Value.Number
        }
        |> Morph.over Value.atom
