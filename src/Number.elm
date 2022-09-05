module Number exposing
    ( N0Or(..)
    , Rational
    , rationalRowChar
    , toFloat, fromFloat
    , Whole, WholePositive
    , fromInt
    , Sign(..)
    , plusOrMinusChar
    , emptiableMinusChar, toN, wholeRowChar
    )

{-| number TODO MorphIndependently

@docs N0Or


## rational

@docs Rational
@docs rationalRowChar
@docs toFloat, fromFloat


## whole

@docs Whole, WholePositive
@docs integerRowChar
@docs toInt, fromInt


## signs

@docs Sign
@docs plusOrMinusChar, maybeMinusChar

Feeling motivated? implement & PR

  - int2 (bin), int8 (oct), int6 (hex)

-}

import ArraySized exposing (ArraySized)
import ArraySized.Morph
import Bit exposing (Bit)
import Char.Morph
import Emptiable exposing (Emptiable, fill, fillMap, fillMapFlat, filled)
import Linear exposing (DirectionLinear(..))
import Morph exposing (Morph, MorphIndependently, MorphOrError, MorphRow, atLeast, broadenWith, choice, emptiable, grab, narrowWith, one, skip, succeed, translate)
import N exposing (Add1, In, InFixed, Min, N, N0, N1, N9, To, Up, n0, n1, n9)
import N.Morph
import Possibly exposing (Possibly)
import Stack exposing (StackTopBelow, Stacked)
import String.Morph


{-| `0` or not
-}
type N0Or n0Not
    = N0
    | N0Not n0Not



-- integer


{-| A whole decimal number
-}
type alias Whole signedAbsoluteBitCount =
    N0Or
        { sign : Sign
        , absolute : ArraySized signedAbsoluteBitCount Bit
        }


type alias WholePositive bitCount =
    { positiveInBits1Then : ArraySized bitCount Bit
    }


{-| Convert an `ArraySized` of up to 53 `Bit`s to a `N`.

    Bit.listToN [ I, O, O, O, I, O, I, O ]
    --> n138
    --: TODO

-}
toN : WholePositive (In min_ max_) -> N (Min (Up x To (Add1 x)))
toN =
    \bits ->
        bits
            |> ArraySized.reverse
            |> ArraySized.foldFrom
                { total = n0 |> N.maxNo, exponent = n0 |> N.maxNo }
                Up
                (\bit soFar ->
                    { exponent = soFar.exponent |> N.minAdd n1
                    , total =
                        soFar.total
                            |> N.minAdd
                                (bit
                                    |> toN
                                    |> (case soFar.exponent |> N.isAtLeast n1 of
                                            Ok exponentAtLeast1 ->
                                                N.mul (n2 |> N.toPower exponentAtLeast1)

                                            Err _ ->
                                                N.maxNo
                                       )
                                )
                    }
                )
            |> .total


{-| [`Translate`](Morph#Translate) between an `Int` and a [decimal representation](#Integer).

Keep in mind that `Integer -> Int` can overflow
since `Int` is fixed in bit size while [`Integer`](#Integer) is not.

-}
toInt : MorphOrError Int (Whole (Min N0)) error_
toInt =
    fromInt |> Morph.reverse


{-| [`Translate`](Morph#Translate) between an `Int` and a [decimal representation](#Integer).

Keep in mind that `Integer -> Int` can overflow
since `Int` is fixed in bit size while [`Integer`](#Integer) is not.

-}
fromInt : MorphOrError (Whole (Min N0)) Int error_
fromInt =
    Morph.translate
        (\intBroad ->
            case intBroad |> numberSign of
                Nothing ->
                    N0

                Just signed ->
                    N0Not
                        { sign = signed.sign
                        , absolute =
                            Debug.todo ""
                        }
        )
        (\integerNarrow ->
            case integerNarrow of
                N0 ->
                    0

                N0Not integerSigned ->
                    numberSignWith integerSigned.sign
                        (integerSigned.absolute
                            |> Debug.todo ""
                        )
        )



--


{-| Match an integer value as an `Int`.

    import Morph.Error

    -- you can parse integers as `Int` instead of `String`
    "123" |> Text.narrowWith integer --> Ok 123

    -- It also works with negative numbers.
    "-123" |> Text.narrowWith integer --> Ok -123

    -- a decimal number is _not_ an integer
    "3.14"
        |> Text.narrowWith integer
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:2: I was expecting an integer value. I got stuck when I got the character '.'."

    -- but not with invalid numbers
    "abc"
        |> Text.narrowWith integer
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:1: I was expecting an integer value. I got stuck when I got the character 'a'."

-}
wholeRowChar : MorphRow Char (Whole (Min N0))
wholeRowChar =
    Morph.to "whole"
        (choice
            (\integer0Variant integerSignedVariant integerNarrow ->
                case integerNarrow of
                    N0 ->
                        integer0Variant ()

                    N0Not integer0Not ->
                        integerSignedVariant integer0Not
            )
            |> Morph.rowTry (\() -> N0)
                (String.Morph.only "0")
            |> Morph.rowTry N0Not
                (succeed
                    (\signPart absolutePart ->
                        { sign = signPart, absolute = absolutePart }
                    )
                    |> grab .sign emptiableMinusChar
                    |> grab .absolute
                        (Debug.todo "")
                )
            |> Morph.rowChoiceFinish
        )



-- number


{-| A decimal number that can have a floating point.

Don't shy away from spinning your own version of this if needed, like

    type FieldNumber
        = DivisionByZeroResult
        | Infinity Sign
        | Decimal Number

-}
type alias Rational =
    N0Or
        { whole : N0Or (WholePositive (Min N0))
        , fraction : Emptiable (Stacked (N (InFixed N0 N9))) Possibly
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
fromFloat :
    MorphIndependently
        (Float -> Result error_ Rational)
        (Rational -> Float)
fromFloat =
    toFloat |> Morph.reverse


{-| [`Translate`](Morph#Translate) between a `Float` and a [decimal representation](#Number).

Keep in mind that `Number -> Float` can be lossy
since `Float` is fixed in bit size while [`Number`](#Number) is not.

-}
toFloat :
    MorphIndependently
        (Rational -> Result error_ Float)
        (Float -> Rational)
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
                    , whole =
                        wholeAbsolute |> intAbsoluteTo0To9s
                    , fraction =
                        (signed.absolute - (wholeAbsolute |> Basics.toFloat))
                            |> abs
                            |> floatFraction
                    }
                        |> N0Not
        )
        (\floatNarrow ->
            case floatNarrow of
                N0 ->
                    0

                N0Not numberSigned ->
                    let
                        toFraction fractionDigits =
                            fractionDigits
                                |> Stack.map
                                    (\decimal digit ->
                                        (digit
                                            |> N.toInt
                                            |> Basics.toFloat
                                        )
                                            * (10 ^ -(1 + (decimal.index |> Basics.toFloat)))
                                    )
                                |> Stack.sum
                    in
                    numberSignWith numberSigned.sign
                        ((numberSigned.whole |> n0To9sToInt |> Basics.toFloat)
                            + (case numberSigned.fraction of
                                Emptiable.Empty _ ->
                                    0

                                Emptiable.Filled fraction ->
                                    fraction |> filled |> toFraction
                              )
                        )
        )


floatFraction : Float -> Emptiable (Stacked (N (InFixed N0 N9))) Possibly
floatFraction =
    \float ->
        if float == 0 then
            Emptiable.empty

        else
            let
                floatShifted1Decimal =
                    float * 10

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
                    |> floatFraction
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
rationalRowChar : MorphRow Char Rational
rationalRowChar =
    -- TODO:
    --   rethink: split off rationalPointSeparated
    --   and redefine below as try whole |> try rationalPointSeparated
    Morph.to "rational"
        (Morph.choice
            (\n0Variant signedVariant numberNarrow ->
                case numberNarrow of
                    N0 ->
                        n0Variant ()

                    N0Not signedValue ->
                        signedVariant signedValue
            )
            |> Morph.rowTry (\() -> N0)
                (String.Morph.only "0")
            |> Morph.rowTry N0Not
                (succeed
                    (\signPart wholePart fractionPart ->
                        { sign = signPart
                        , whole = wholePart
                        , fraction = fractionPart
                        }
                    )
                    |> grab .sign emptiableMinusChar
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
                                        |> ArraySized.minGlue Up (down |> ArraySized.fromList)
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


{-| [`Sign`](#Sign) `'+'` or `'-'`
-}
plusOrMinusChar : Morph Sign Char
plusOrMinusChar =
    Morph.to "sign"
        (Morph.choice
            (\plus minus signNarrow ->
                case signNarrow of
                    Positive ->
                        plus ()

                    Negative ->
                        minus ()
            )
            |> Morph.try (\() -> Positive) (Char.Morph.only '+')
            |> Morph.try (\() -> Negative) (Char.Morph.only '-')
            |> Morph.choiceFinish
        )


{-| A possible `'-'` sign → [`Negative`](#Sign),
else [narrows to](Morph#narrowWith) [`Positive`](#Sign)
-}
emptiableMinusChar : MorphRow Char Sign
emptiableMinusChar =
    Morph.to "negation"
        (translate
            (\minusSymbol ->
                case minusSymbol of
                    Emptiable.Empty _ ->
                        Positive

                    Emptiable.Filled () ->
                        Negative
            )
            (\signNarrow ->
                case signNarrow of
                    Positive ->
                        Emptiable.empty

                    Negative ->
                        () |> filled
            )
            |> Morph.overRow
                (emptiable (String.Morph.only "-"))
        )
