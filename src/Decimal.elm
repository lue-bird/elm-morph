module Decimal exposing
    ( Decimal(..), Absolute(..), Fraction
    , rowChar
    , toFloat, float
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
import Morph exposing (Morph, MorphIndependently, MorphOrError, MorphRow, atLeast, broadenWith, choice, emptiable, grab, narrowWith, one, skip, succeed, translate)
import N exposing (Add1, In, InFixed, Min, N, N0, N1, N9, To, Up, n0, n1, n9)
import N.Morph
import Natural exposing (Natural)
import Possibly exposing (Possibly)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Sign
import Stack exposing (StackTopBelow, Stacked)
import String.Morph



-- number


{-| A decimal number that can have a floating point.

Don't shy away from spinning your own version of this if needed, like

    type FieldNumber
        = DivisionByZeroResult
        | Infinity Sign
        | Decimal Number

-}
type Decimal
    = N0
    | Signed
        { sign : Sign
        , absolute : Absolute
        }


type Absolute
    = Fraction Fraction
    | AtLeast1
        { whole : StackTopBelow (N (InFixed N1 N9)) (N (InFixed N0 N9))
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

                Just signed ->
                    let
                        wholeAbsolute =
                            signed.absolute |> truncate
                    in
                    { sign = signed.sign
                    , absolute =
                        if signed.absolute < 1 then
                            signed.absolute |> floatToFraction |> Fraction

                        else
                            { whole =
                                wholeAbsolute |> intAbsoluteTo0To9s
                            , fraction =
                                (signed.absolute - (wholeAbsolute |> Basics.toFloat))
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
    \absolute ->
        case absolute of
            Fraction fraction ->
                fraction |> fractionToFloat

            AtLeast1 atLeast1 ->
                (atLeast1.whole |> n0To9sToInt |> Basics.toFloat)
                    + (case atLeast1.fraction of
                        Emptiable.Empty _ ->
                            0

                        Emptiable.Filled fraction ->
                            fraction |> filled |> fractionToFloat
                      )


fractionToFloat : Fraction -> Float
fractionToFloat =
    \fraction ->
        Stack.only fraction.last
            |> Stack.onTopStack fraction.beforeLast
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


{-| Match a decimal value as a `Float`.

    import Morph.Error

    "12" |> Text.narrowWith number     --> Ok 12.0
    "12.34" |> Text.narrowWith number  --> Ok 12.34
    "12." |> Text.narrowWith number    --> Ok 12.0
    ".12" |> Text.narrowWith number    --> Ok 0.12
    "-12.34" |> Text.narrowWith number --> Ok -12.34
    "-.12" |> Text.narrowWith number   --> Ok -0.12

    "."
        |> Text.narrowWith number
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:1: I was expecting a digit [0-9]. I got stuck when I got the character '.'."

    "abc" |> Text.narrowWith number
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:1: I was expecting a digit [0-9]. I got stuck when I got the character 'a'."

-}
rowChar : MorphRow Char Decimal
rowChar =
    -- TODO:
    --   rethink: split off rationalPointSeparated
    --   and redefine below as try whole |> try rationalPointSeparated
    Morph.to "rational"
        (Morph.choice
            (\n0Variant signedVariant numberNarrow ->
                case numberNarrow of
                    N0 ->
                        n0Variant ()

                    Signed signedValue ->
                        signedVariant signedValue
            )
            |> Morph.rowTry (\() -> N0)
                (String.Morph.only "0")
            |> Morph.rowTry Signed
                (succeed
                    (\signPart wholePart fractionPart ->
                        { sign = signPart
                        , whole = wholePart
                        , fraction = fractionPart
                        }
                    )
                    |> grab .sign Sign.emptiableMinusChar
                    |> grab .whole
                        (Morph.to "whole"
                            (ArraySized.Morph.toStackFilled
                                |> Morph.overRow
                                    (atLeast n1 (N.Morph.charIn ( n0, n9 ) |> one))
                            )
                        )
                    |> grab .fraction
                        (translate
                            (fillMapFlat ArraySized.toStackFilled)
                            (fillMap
                                (\(Stack.TopDown top down) ->
                                    ArraySized.l1 top
                                        |> ArraySized.glueMin Up (down |> ArraySized.fromList)
                                )
                            )
                            |> Morph.overRow
                                (emptiable
                                    (succeed (\fractionDigits -> fractionDigits)
                                        |> skip (String.Morph.only ".")
                                        |> grab (\fractionDigits -> fractionDigits)
                                            (atLeast n1 (N.Morph.charIn ( n0, n9 ) |> one))
                                    )
                                )
                        )
                )
            |> Morph.rowChoiceFinish
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
