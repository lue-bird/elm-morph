module Decimal.Morph exposing
    ( orException, value
    , chars, bitsVariableCount
    , orExceptionValue, exceptionValue
    , orExceptionFloat, orExceptionToFloat
    )

{-| [`Decimal`](Decimal#Decimal) [`Morph`](Morph#Morph)

@docs orException, value


## row

@docs chars, bitsVariableCount


## [`Decimal`](Decimal#Decimal) [`OrException`](Decimal#OrException)

[`Morph`](Morph#Morph) a [`Decimal`](Decimal#Decimal) where infinities and NaN are possible states

@docs orExceptionValue, exceptionValue


## `Float`

@docs orExceptionFloat, orExceptionToFloat

-}

import Bit exposing (Bit)
import Bit.Morph
import Decimal exposing (Decimal(..), Exception(..), Fraction, OrException(..), SignedAbsolute(..))
import Emptiable exposing (Emptiable)
import List.Morph
import Maybe.Morph
import Morph exposing (Morph, MorphOrError, MorphRow, grab, match, one, oneToOne)
import N exposing (Add1, In, N, N0, N1, N9, To, Up, n0, n1, n2, n3, n4, n5, n6, n7, n8, n9)
import N.Morph
import Natural
import Natural.Morph
import NaturalAtLeast1
import NaturalAtLeast1Base10
import Sign exposing (Sign(..))
import Sign.Morph
import Stack exposing (Stacked)
import String.Morph
import Value
import Value.Morph.Internal exposing (MorphValue)


{-| [`Morph`](Morph#Morph)
a [`Decimal`](Decimal#Decimal)
to an [`OrException Decimal`](Decimal#OrException)
-}
orException : Morph Decimal (OrException Decimal)
orException =
    Morph.custom "decimal"
        { toNarrow =
            \floatExplicit_ ->
                case floatExplicit_ of
                    Number number ->
                        number |> Ok

                    Exception _ ->
                        "Exception" |> Err
        , toBroad = Number
        }


{-| [`MorphRow`](Morph#MorphRow) from chars to a [`Decimal`](Decimal#Decimal) number.

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
            |> Morph.match
                (Morph.broad []
                    |> Morph.overRow
                        (Morph.whilePossible
                            (String.Morph.only "0")
                        )
                )
        )


{-| [`MorphRow`](Morph#MorphRow) from chars to a [`Decimal.Signed`](Decimal#Signed) number.
-}
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
                    |> grab .whole NaturalAtLeast1.chars
                    |> match (String.Morph.only ".")
                    |> grab .fraction (Maybe.Morph.row fractionChars)
                )
            |> Morph.choiceFinish
        )


{-| [`MorphRow`](Morph#MorphRow) from chars to a [`Decimal.Fraction`](Decimal#Fraction).
-}
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
    Morph.custom "decimal"
        { toNarrow =
            \atom ->
                case atom of
                    Value.Number decimal ->
                        decimal |> Ok

                    atomExceptDecimal ->
                        atomExceptDecimal |> Value.atomKindToString |> Err
        , toBroad = Value.Number
        }
        |> Morph.over Value.Morph.Internal.atom



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
        |> Value.Morph.Internal.variant ( \() -> NaN, "NaN" ) Value.Morph.Internal.unit
        |> Value.Morph.Internal.variant ( Infinity, "Infinity" ) signValue
        |> Value.Morph.Internal.choiceFinish


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
        |> Value.Morph.Internal.variant ( \() -> Negative, "Negative" ) Value.Morph.Internal.unit
        |> Value.Morph.Internal.variant ( \() -> Positive, "Positive" ) Value.Morph.Internal.unit
        |> Value.Morph.Internal.choiceFinish


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
                        atLeast1.whole |> Natural.AtLeast1 |> Morph.mapTo N.Morph.natural |> N.toFloat
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
        |> Value.Morph.Internal.variant ( Number, "Decimal" ) decimalInternalValue
        |> Value.Morph.Internal.variant ( Exception, "Exception" ) exceptionValue
        |> Value.Morph.Internal.choiceFinish


decimalInternalValue : MorphValue Decimal
decimalInternalValue =
    Morph.custom "decimal"
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
        |> Morph.over Value.Morph.Internal.atom


{-| `MorphRow` from from `Bit`s to a [`Decimal`](Decimal#Decimal)
-}
bitsVariableCount : MorphRow Decimal Bit
bitsVariableCount =
    Morph.named "decimal"
        (Morph.choice
            (\n0 signed decimal ->
                case decimal of
                    Decimal.N0 ->
                        n0 ()

                    Decimal.Signed signedValue ->
                        signed signedValue
            )
            |> Morph.tryRow (\() -> Decimal.N0)
                (Morph.named "0" (Bit.Morph.only Bit.O |> Morph.one))
            |> Morph.tryRow Decimal.Signed
                (Morph.succeed (\signed -> signed)
                    |> Morph.match (Bit.Morph.only Bit.I |> Morph.one)
                    |> Morph.grab (\signed -> signed) signedBits
                )
            |> Morph.choiceFinish
        )


{-| [`MorphRow`](Morph#MorphRow) from `Bit`s to a [`Decimal.Signed`](Decimal#Signed)
-}
signedBits : MorphRow Decimal.Signed Bit
signedBits =
    Morph.named "signed"
        (Morph.succeed (\sign absolute -> { sign = sign, absolute = absolute })
            |> Morph.grab .sign (Sign.Morph.bit |> Morph.one)
            |> Morph.grab .absolute signedAbsoluteBits
        )


{-| [`MorphRow`](Morph#MorphRow) from `Bit`s to a [`Decimal.Fraction`](Decimal#Fraction)
-}
fractionBits : MorphRow Decimal.Fraction Bit
fractionBits =
    Morph.named "fraction"
        (Morph.oneToOne
            (\initial0CountAndAfter ->
                { beforeLast =
                    List.repeat
                        (initial0CountAndAfter.initial0Count |> N.toInt)
                        (n0 |> N.maxTo n9 |> N.inToNumber)
                        ++ (case initial0CountAndAfter.after0s of
                                Natural.N0 ->
                                    []

                                Natural.AtLeast1 after0sAtLeast1 ->
                                    let
                                        after0sAtLeast1Base2 =
                                            after0sAtLeast1 |> NaturalAtLeast1Base10.fromBase2
                                    in
                                    (after0sAtLeast1Base2.first |> N.minTo0 |> N.minToNumber)
                                        :: after0sAtLeast1Base2.afterFirst
                           )
                , last = initial0CountAndAfter.last
                }
            )
            (\fraction ->
                let
                    beforeLast =
                        fraction.beforeLast
                            |> List.foldl
                                (\el soFar ->
                                    case soFar.after0sBase10 of
                                        Just after0sBase10AtLeast1 ->
                                            { soFar
                                                | after0sBase10 =
                                                    { after0sBase10AtLeast1
                                                        | afterFirstReverse =
                                                            after0sBase10AtLeast1.afterFirstReverse |> (::) el
                                                    }
                                                        |> Just
                                            }

                                        Nothing ->
                                            case el |> N.inToOn |> N.isAtLeast n1 of
                                                Err _ ->
                                                    { soFar | initial0Count = soFar.initial0Count + 1 }

                                                Ok elAtLeast1 ->
                                                    { soFar
                                                        | after0sBase10 =
                                                            { first = elAtLeast1 |> N.inToNumber
                                                            , afterFirstReverse = []
                                                            }
                                                                |> Just
                                                    }
                                )
                                { initial0Count = 0, after0sBase10 = Nothing }
                in
                { initial0Count = beforeLast.initial0Count |> N.intToAtLeast n0
                , last = fraction.last
                , after0s =
                    case beforeLast.after0sBase10 of
                        Nothing ->
                            Natural.N0

                        Just after0sBase10AtLeast1 ->
                            { first = after0sBase10AtLeast1.first
                            , afterFirst =
                                after0sBase10AtLeast1.afterFirstReverse
                                    |> List.reverse
                            }
                                |> NaturalAtLeast1Base10.toBase2
                                |> Natural.AtLeast1
                }
            )
            |> Morph.overRow
                (Morph.succeed
                    (\initial0Count after0s last ->
                        { initial0Count = initial0Count, after0s = after0s, last = last }
                    )
                    |> Morph.grab .initial0Count
                        (N.Morph.natural
                            |> Morph.overRow Natural.Morph.bitsVariableCount
                        )
                    |> Morph.grab .after0s Natural.Morph.bitsVariableCount
                    |> Morph.grab .last fractionLastDigitBits
                )
        )


fractionLastDigitBits : MorphRow (N (In N1 N9)) Bit
fractionLastDigitBits =
    let
        withDigitRange :
            N (In (N.On (Add1 minX_)) (Up max_ To N9))
            -> N (In N1 N9)
        withDigitRange digit =
            digit |> N.minTo n1 |> N.maxTo n9 |> N.inToNumber
    in
    Morph.named "1|..|9"
        (Morph.choice
            (\v1 v2 v3 v4 v5 v6 v7 v8 v9 n ->
                case n |> N.toInt of
                    1 ->
                        v1 ()

                    2 ->
                        v2 ()

                    3 ->
                        v3 ()

                    4 ->
                        v4 ()

                    5 ->
                        v5 ()

                    6 ->
                        v6 ()

                    7 ->
                        v7 ()

                    8 ->
                        v8 ()

                    -- 9
                    _ ->
                        v9 ()
            )
            |> Morph.tryRow (\() -> n1 |> withDigitRange)
                (List.Morph.broadSequenceMap
                    (Bit.Morph.only >> Morph.one)
                    [ Bit.O, Bit.O, Bit.O, Bit.O ]
                )
            |> Morph.tryRow (\() -> n2 |> withDigitRange)
                (List.Morph.broadSequenceMap
                    (Bit.Morph.only >> Morph.one)
                    [ Bit.O, Bit.O, Bit.O, Bit.I ]
                )
            |> Morph.tryRow (\() -> n3 |> withDigitRange)
                (List.Morph.broadSequenceMap
                    (Bit.Morph.only >> Morph.one)
                    [ Bit.O, Bit.O, Bit.I, Bit.O ]
                )
            |> Morph.tryRow (\() -> n4 |> withDigitRange)
                (List.Morph.broadSequenceMap
                    (Bit.Morph.only >> Morph.one)
                    [ Bit.O, Bit.O, Bit.I, Bit.I ]
                )
            |> Morph.tryRow (\() -> n5 |> withDigitRange)
                (List.Morph.broadSequenceMap
                    (Bit.Morph.only >> Morph.one)
                    [ Bit.O, Bit.I, Bit.O, Bit.O ]
                )
            |> Morph.tryRow (\() -> n6 |> withDigitRange)
                (List.Morph.broadSequenceMap
                    (Bit.Morph.only >> Morph.one)
                    [ Bit.O, Bit.I, Bit.O, Bit.I ]
                )
            |> Morph.tryRow (\() -> n7 |> withDigitRange)
                (List.Morph.broadSequenceMap
                    (Bit.Morph.only >> Morph.one)
                    [ Bit.O, Bit.I, Bit.I, Bit.O ]
                )
            |> Morph.tryRow (\() -> n8 |> withDigitRange)
                (List.Morph.broadSequenceMap
                    (Bit.Morph.only >> Morph.one)
                    [ Bit.O, Bit.I, Bit.I, Bit.I ]
                )
            |> Morph.tryRow (\() -> n9 |> withDigitRange)
                (List.Morph.broadSequenceMap
                    (Bit.Morph.only >> Morph.one)
                    [ Bit.I ]
                )
            |> Morph.choiceFinish
        )


signedAbsoluteBits : MorphRow Decimal.SignedAbsolute Bit
signedAbsoluteBits =
    Morph.named "absolute"
        (Morph.choice
            (\fraction atLeast1 absolute ->
                case absolute of
                    Decimal.Fraction fractionValue ->
                        fraction fractionValue

                    Decimal.AtLeast1 atLeast1Value ->
                        atLeast1 atLeast1Value
            )
            |> Morph.tryRow Decimal.Fraction
                (Morph.succeed (\fraction -> fraction)
                    |> Morph.match (Bit.Morph.only Bit.O |> Morph.one)
                    |> Morph.grab (\fraction -> fraction) fractionBits
                )
            |> Morph.tryRow Decimal.AtLeast1
                (Morph.succeed (\atLeast1 -> atLeast1)
                    |> Morph.match (Bit.Morph.only Bit.I |> Morph.one)
                    |> Morph.grab (\atLeast1 -> atLeast1) atLeast1Bits
                )
            |> Morph.choiceFinish
        )


atLeast1Bits : MorphRow Decimal.AtLeast1 Bit
atLeast1Bits =
    Morph.named "≥ 1"
        (Morph.succeed (\whole fraction -> { whole = whole, fraction = fraction })
            |> Morph.grab .whole NaturalAtLeast1.bits
            |> Morph.grab .fraction (maybeBits fractionBits)
        )


maybeBits : MorphRow content Bit -> MorphRow (Maybe content) Bit
maybeBits contentMorphRow =
    Morph.choice
        (\nothing just maybe ->
            case maybe of
                Nothing ->
                    nothing ()

                Just content ->
                    just content
        )
        |> Morph.tryRow (\() -> Nothing)
            (Bit.Morph.only Bit.O |> Morph.one)
        |> Morph.tryRow Just
            (Morph.succeed (\content -> content)
                |> Morph.match (Bit.Morph.only Bit.I |> Morph.one)
                |> Morph.grab (\content -> content) contentMorphRow
            )
        |> Morph.choiceFinish
