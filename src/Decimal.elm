module Decimal exposing
    ( Decimal(..), Absolute(..), Fraction
    , rowChar
    , toFloat, float
    , Signed
    )

{-| number TODO MorphIndependently

@docs Decimal, Absolute, Fraction

@docs rowChar
@docs toFloat, float

-}

import ArraySized exposing (ArraySized)
import ArraySized.Morph
import Bit exposing (Bit)
import Char.Morph
import Emptiable exposing (Emptiable, fill, fillMap, fillMapFlat, filled)
import Linear exposing (Direction(..))
import Maybe.Morph
import Morph exposing (Morph, MorphIndependently, MorphOrError, MorphRow, broadenWith, narrowWith, one, translate)
import N exposing (Add1, In, InFixed, Min, N, N0, N1, N9, To, Up, Up0, Up1, Up9, n0, n1, n9)
import N.Morph
import Natural exposing (Natural)
import Possibly exposing (Possibly)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Sign
import Stack exposing (StackTopBelow, Stacked)
import String.Morph



-- number


{-| A decimal number that can have a floating point

Don't shy away from spinning your own version of this if needed, like

    type FieldNumber
        = DivisionByZeroResult
        | Infinity Sign
        | Decimal Decimal

-}
type Decimal
    = N0
    | Signed Signed


{-| Any [`Decimal`](#Decimal) except `0` is represented this way
-}
type alias Signed =
    RecordWithoutConstructorFunction
        { sign : Sign
        , absolute : Absolute
        }


type Absolute
    = Fraction Fraction
    | AtLeast1
        { whole :
            Emptiable
                (StackTopBelow (N (InFixed N1 N9)) (N (InFixed N0 N9)))
                Never
        , fraction : Emptiable Fraction Possibly
        }


type alias Fraction =
    RecordWithoutConstructorFunction
        { beforeLast : Emptiable (Stacked (N (InFixed N0 N9))) Possibly
        , last : N (InFixed N1 N9)
        }


{-| [`Translate`](Morph#Translate) between a `Float` and [decimal representation](#Number).

Keep in mind that `Number -> Float` can be lossy
since `Float` is fixed in bit size while [`Number`](#Number) is not.

    -9999.124
        |> broaden
            (Number.Morph.toFloat
                |> Morph.rowOver Number.Morph.text
            )
    --> "-999.1239999999997962731868028640747070312"

-}
float :
    MorphIndependently
        (Float -> Result error_ Decimal)
        (Decimal -> Float)
float =
    toFloat |> Morph.reverse


{-| [`Translate`](Morph#Translate) between a `Float` and a [decimal representation](#Number).

Keep in mind that `Number -> Float` can be lossy
since `Float` is fixed in bit size while [`Number`](#Number) is not.

-}
toFloat : MorphOrError Float Decimal error_
toFloat =
    Morph.translate
        (\floatValue ->
            case floatValue |> numberSign of
                Nothing ->
                    N0

                Just signed_ ->
                    let
                        wholeAbsolute =
                            signed_.absolute |> truncate
                    in
                    { sign = signed_.sign
                    , absolute =
                        if signed_.absolute < 1 then
                            signed_.absolute |> floatToFraction |> Fraction

                        else
                            { whole =
                                wholeAbsolute |> intPositiveToDigits
                            , fraction =
                                (signed_.absolute - (wholeAbsolute |> Basics.toFloat))
                                    |> abs
                                    |> floatToFraction
                            }
                                |> AtLeast1
                    }
                        |> Signed
        )
        (\floatNarrow ->
            case floatNarrow of
                N0 ->
                    0

                Signed numberSigned ->
                    numberSignWith numberSigned.sign
                        (numberSigned.absolute |> absoluteToFloat)
        )


absoluteToFloat : Absolute -> Float
absoluteToFloat =
    \absolute_ ->
        case absolute_ of
            Fraction fraction_ ->
                fraction_ |> fractionToFloat

            AtLeast1 atLeast1 ->
                (atLeast1.whole
                    |> Stack.topMap (N.minTo n0)
                    |> digitsToInt
                    |> Basics.toFloat
                )
                    + (case atLeast1.fraction of
                        Emptiable.Empty _ ->
                            0

                        Emptiable.Filled fraction_ ->
                            fraction_ |> filled |> fractionToFloat
                      )


intPositiveToDigits :
    Int
    ->
        Emptiable
            (StackTopBelow
                (N (In (Up1 digit0MinX_) (Up9 digit0MaxX_)))
                (N (In (Up0 digit1UpMinX_) (Up9 digit1UpMaxX_)))
            )
            never_
intPositiveToDigits =
    \int ->
        let
            highest10Exponent : Int
            highest10Exponent =
                logBase 10 (int |> toFloat) |> floor
        in
        Stack.topDown
            (int |> digitFor10Exponent highest10Exponent |> N.atLeast n1 |> N.maxTo n9)
            (List.range 0 (highest10Exponent - 1)
                |> List.map
                    (\n10Exponent ->
                        int |> digitFor10Exponent n10Exponent
                    )
            )


digitFor10Exponent : Int -> (Int -> N (In (Up0 minX_) (Up9 maxX_)))
digitFor10Exponent n10Exponent =
    \int ->
        (int // (10 ^ n10Exponent))
            |> remainderBy 10
            |> N.intIn ( n0, n9 )


digitsToInt : Emptiable (Stacked (N range_)) possiblyOrNever_ -> Int
digitsToInt =
    \digits ->
        let
            lastIndex =
                (digits |> Stack.length) - 1
        in
        digits
            |> Stack.map
                (\{ index } digit ->
                    (digit |> N.toInt) * (10 ^ (lastIndex - index))
                )
            |> Stack.sum


fractionToFloat : Fraction -> Float
fractionToFloat =
    \fraction_ ->
        Stack.only fraction.last
            |> Stack.onTopStack fraction_.beforeLast
            |> Stack.map
                (\decimal digit ->
                    (digit |> N.toFloat)
                        * (10 ^ -(1 + (decimal.index |> Basics.toFloat)))
                )
            |> Stack.sum


floatToFraction :
    Float
    ->
        -- TODO Fraction
        Emptiable (Stacked (N (InFixed N0 N9))) Possibly
floatToFraction =
    \float_ ->
        if float_ == 0 then
            Emptiable.empty

        else
            let
                floatShifted1Decimal =
                    float_ * 10

                decimalInt =
                    floatShifted1Decimal |> floor
            in
            (case decimalInt |> narrowWith (N.Morph.intIn ( n0, n9 )) of
                Err _ ->
                    identity

                Ok decimal ->
                    Stack.onTopLay decimal
            )
                ((floatShifted1Decimal - (decimalInt |> Basics.toFloat))
                    |> floatToFraction
                )



--


{-| Match a decimal number

    import Morph.Error

    "12.34" |> Text.narrowWith number  --> Ok 12.34
    "12." |> Text.narrowWith number    --> Ok 12.0
    ".12" |> Text.narrowWith number    --> Ok 0.12
    "-12.34" |> Text.narrowWith number --> Ok -12.34
    "-.12" |> Text.narrowWith number   --> Ok -0.12


    "12"
        |> Text.narrowWith number
        |> Result.mapError Morph.Error.textMessage
    --> Err ...

    "."
        |> Text.narrowWith number
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:1: I was expecting a digit [0-9]. I got stuck when I got the character '.'."

    "abc"
        |> Text.narrowWith number
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:1: I was expecting a digit [0-9]. I got stuck when I got the character 'a'."

-}
rowChar : MorphRow Char Decimal
rowChar =
    -- TODO: make decimal point obligatory
    Morph.to "rational"
        (Choice.between
            (\signedVariant n0Variant numberNarrow ->
                case numberNarrow of
                    N0 ->
                        n0Variant ()

                    Signed signedValue ->
                        signedVariant signedValue
            )
            |> Choice.tryRow Signed signed
            |> Choice.tryRow (\() -> N0) (String.Morph.only "0.")
            |> Choice.finishRow
        )


signed : MorphRow Char Signed
signed =
    Morph.to "signed"
        (Morph.succeed
            (\signPart absolutePart ->
                { sign = signPart
                , absolute = absolutePart
                }
            )
            |> grab .sign Sign.maybeMinusChar
            |> grab .absolute
                absolute
        )


absolute : MorphRow Char Absolute
absolute =
    Morph.to "absolute"
        (Choice.between
            (\fractionVariant atLeast1Variant absoluteUnion ->
                case absoluteUnion of
                    Fraction fractionValue ->
                        fractionVariant fractionValue

                    AtLeast1 atLeast1Value ->
                        atLeast1Variant atLeast1Value
            )
            |> Choice.tryRow Fraction
                (Morph.succeed (\fraction_ -> fraction)
                    |> skip
                        (Morph.broad (Just ())
                            |> Morph.overRow
                                (Maybe.Morph.row (String.Morph.only "0"))
                        )
                    |> skip (String.Morph.only ".")
                    |> grab (\fraction_ -> fraction_) fraction
                )
            |> Choice.tryRow AtLeast1
                (let
                    whole :
                        MorphRow
                            Char
                            (Emptiable
                                (StackTopBelow (N (InFixed N1 N9)) (N (InFixed N0 N9)))
                                Never
                            )
                    whole =
                        Morph.to "whole"
                            (Morph.succeed Stack.onTopLay
                                |> grab Stack.top (N.Morph.charIn ( n1, n9 ) |> one)
                                |> grab Stack.topRemove
                                    (ArraySized.Morph.toStackEmptiable
                                        (ArraySized.Morph.atLeast n0
                                            (N.Morph.charIn ( n0, n9 ) |> one)
                                        )
                                    )
                            )
                 in
                 Morph.succeed
                    (\wholePart fractionPart ->
                        { whole = wholePart
                        , fraction = fractionPart.whole
                        }
                    )
                    |> grab .whole whole
                    |> skip (String.Morph.only ".")
                    |> grab .fraction (Maybe.Morph.row fraction)
                )
            |> Choice.finishRow
        )


fraction : Morph.MorphRow Char Fraction
fraction =
    Morph.to "fraction"
        (translate
            (fillMap ArraySized.toStackFilled)
            (fillMap
                (\stack ->
                    ArraySized.l1 (stack |> Stack.top)
                        |> ArraySized.glueMin Up
                            (stack |> Stack.topRemove |> ArraySized.fromStackEmptiable)
                )
            )
            |> Morph.overRow
                (Maybe.Morph.row
                    (ArraySized.Morph.atLeast n1
                        (N.Morph.charIn ( n0, n9 ) |> one)
                    )
                )
        )



-- sign


{-| A number's signum. Either positive or negative
-}
type Sign
    = Positive
    | Negative


numberSignWith : Sign -> (number -> number)
numberSignWith signToAdapt =
    case signToAdapt of
        Positive ->
            abs

        Negative ->
            abs >> negate


numberSign : number -> Maybe { sign : Sign, absolute : number }
numberSign =
    \numberSigned ->
        case compare numberSigned 0 of
            EQ ->
                Nothing

            LT ->
                { sign = Negative, absolute = numberSigned |> abs }
                    |> Just

            GT ->
                { sign = Positive, absolute = numberSigned }
                    |> Just
