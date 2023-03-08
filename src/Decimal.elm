module Decimal exposing
    ( Decimal, Fraction
    , ceiling, floor, truncate
    , rowChar, orException, value
    )

{-| safe and explicit floating point number
without the possibility of [exceptions](DecimalOrException#Exception)

@docs Decimal, Fraction


## alter

@docs ceiling, floor, truncate


## [`Morph`](Morph#Morph)

@docs rowChar, orException, value

-}

import ArraySized
import ArraySized.Morph
import DecimalOrException exposing (OrException)
import Maybe.Morph
import Morph exposing (Morph, MorphRow, grab, one, skip)
import N exposing (n0, n1, n9)
import N.Morph
import NaturalAtLeast1
import NaturalAtLeast1.Internal
import Number exposing (Decimal(..), DecimalSigned, DecimalSignedAbsolute(..), Integer(..), Sign(..))
import Sign
import Stack.Morph
import String.Morph
import Value


{-| A decimal number that can have a floating point

Don't shy away from spinning your own version of this if needed, like

    type FieldNumber
        = DivisionByZeroResult
        | Infinity Sign
        | Decimal Decimal

See also [`OrException Decimal`](DecimalOrException#OrException)

-}
type alias Decimal =
    Number.Decimal


{-| _Some_ digits after the decimal point. Can't be none
-}
type alias Fraction =
    Number.Fraction



--


{-| [`Morph`](Morph#Morph)
a [`Decimal`](#Decimal)
to an [`OrException Decimal`](DecimalOrException#OrException)
-}
orException : Morph Decimal (OrException Decimal)
orException =
    Morph.value "Decimal"
        { narrow =
            \floatExplicit_ ->
                case floatExplicit_ of
                    DecimalOrException.Number number ->
                        number |> Ok

                    DecimalOrException.Exception _ ->
                        "Exception" |> Err
        , broaden = DecimalOrException.Number
        }


{-| Match a decimal number

    import Morph.Error


    -- trailing 0s aren't represented in the final type

    "12.0340000" |> Text.narrowTo number  --> Ok 12.034
    "-12.000" |> Text.narrowTo number --> Ok -12.0

    -- leading floating point is allowed

    ".012" |> Text.narrowTo number    --> Ok 0.012
    "-.12" |> Text.narrowTo number   --> Ok -0.12

    -- fails for integers without a floating point

    "12"
        |> Text.narrowTo number
        |> Result.mapError Morph.Error.textMessage
    --> Err ...

    -- but succeeds for integers with a trailing floating point

    "12." |> Text.narrowTo number    --> Ok 12.0

    -- fails for everything else

    "."
        |> Text.narrowTo number
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:1: I was expecting a digit [0-9]. I got stuck when I got the character '.'."

    "abc"
        |> Text.narrowTo number
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:1: I was expecting a digit [0-9]. I got stuck when I got the character 'a'."

To allow integers to parse as decimals as well,
build a [`Morph.choice`](Morph#choice)
[`Decimal.rowChar`](#rowChar)
and [`Integer.rowChar`](Integer#rowChar)

For different parsing behavior, spin your own
using [`Decimal.rowChar`](#rowChar) implementation as a reference

-}
rowChar : MorphRow Decimal Char
rowChar =
    Morph.to "decimal"
        (Morph.choice
            (\signedVariant n0Variant numberNarrow ->
                case numberNarrow of
                    DecimalN0 ->
                        n0Variant ()

                    DecimalSigned signedValue ->
                        signedVariant signedValue
            )
            |> Morph.tryRow DecimalSigned signedRowChar
            |> Morph.tryRow (\() -> DecimalN0) (String.Morph.only "0.")
            |> Morph.choiceRowFinish
            |> skip
                (Morph.broad (ArraySized.repeat () n0)
                    |> Morph.overRow
                        (ArraySized.Morph.atLeast
                            (String.Morph.only "0")
                            n0
                        )
                )
        )


signedRowChar : MorphRow DecimalSigned Char
signedRowChar =
    Morph.to "signed"
        (Morph.succeed
            (\signPart absolutePart ->
                { sign = signPart
                , absolute = absolutePart
                }
            )
            |> grab .sign Sign.maybeMinusChar
            |> grab .absolute signedAbsoluteRowChar
        )


signedAbsoluteRowChar : MorphRow DecimalSignedAbsolute Char
signedAbsoluteRowChar =
    Morph.to "absolute"
        (Morph.choice
            (\fractionVariant atLeast1Variant absoluteUnion ->
                case absoluteUnion of
                    DecimalFraction fractionValue ->
                        fractionVariant fractionValue

                    DecimalAtLeast1 atLeast1Value ->
                        atLeast1Variant atLeast1Value
            )
            |> Morph.tryRow DecimalFraction
                (Morph.succeed (\fraction_ -> fraction_)
                    |> skip
                        (Morph.broad (Just ())
                            |> Morph.overRow
                                (Maybe.Morph.row (String.Morph.only "0"))
                        )
                    |> skip (String.Morph.only ".")
                    |> grab (\fraction_ -> fraction_) fractionRowChar
                )
            |> Morph.tryRow DecimalAtLeast1
                (Morph.succeed
                    (\wholePart fractionPart ->
                        { whole = wholePart
                        , fraction = fractionPart
                        }
                    )
                    |> grab .whole NaturalAtLeast1.Internal.rowChar
                    |> skip (String.Morph.only ".")
                    |> grab .fraction (Maybe.Morph.row fractionRowChar)
                )
            |> Morph.choiceRowFinish
        )


fractionRowChar : MorphRow Fraction Char
fractionRowChar =
    Morph.to "fraction"
        (Morph.succeed
            (\beforeLast last ->
                { beforeLast = beforeLast, last = last }
            )
            |> Morph.grab .beforeLast
                (Stack.Morph.list
                    |> Morph.over ArraySized.Morph.toList
                    |> Morph.overRow
                        (ArraySized.Morph.atLeast
                            (N.Morph.inOn
                                |> Morph.over (N.Morph.in_ ( n0, n9 ))
                                |> Morph.over N.Morph.char
                                |> one
                            )
                            n0
                        )
                )
            |> Morph.grab .last
                (N.Morph.inOn
                    |> Morph.over (N.Morph.in_ ( n1, n9 ))
                    |> Morph.over N.Morph.char
                    |> one
                )
        )


{-| [`Value.Morph`](Value#Morph) from a [`Decimal`](#Decimal)

To get a [`Value.Morph`](Value#Morph) from a `Float`,
see [`DecimalOrException.value`](DecimalOrException#value)

-}
value : Value.Morph Decimal
value =
    Morph.value "Decimal"
        { narrow =
            \atom ->
                case atom of
                    Value.Number decimal ->
                        decimal |> Ok

                    atomExceptDecimal ->
                        atomExceptDecimal |> Value.atomKindToString |> Err
        , broaden = Value.Number
        }
        |> Morph.over Value.atom


{-| Remove the [`Fraction`](#Fraction) part after the decimal point `.`
to create an [`Integer`](Integer#Integer)
-}
truncate : Decimal -> Integer
truncate =
    \decimal ->
        case decimal of
            DecimalN0 ->
                IntegerN0

            DecimalSigned signed ->
                signed |> signedTruncate


signedTruncate : DecimalSigned -> Integer
signedTruncate =
    \signed ->
        case signed.absolute of
            DecimalFraction _ ->
                IntegerN0

            DecimalAtLeast1 atLeast1 ->
                IntegerSigned
                    { sign = signed.sign
                    , absolute = atLeast1.whole
                    }


{-| Its nearest lower [`Integer`](Integer#Integer) number
-}
floor : Decimal -> Integer
floor =
    \decimal ->
        case decimal of
            DecimalN0 ->
                IntegerN0

            DecimalSigned signed ->
                signed |> signedFloor


signedFloor : DecimalSigned -> Integer
signedFloor =
    \signed ->
        case signed.absolute of
            DecimalFraction _ ->
                IntegerN0

            DecimalAtLeast1 atLeast1 ->
                IntegerSigned
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
            DecimalN0 ->
                IntegerN0

            DecimalSigned signed ->
                signed |> signedCeiling


signedCeiling : DecimalSigned -> Integer
signedCeiling =
    \signed ->
        case signed.absolute of
            DecimalFraction _ ->
                IntegerN0

            DecimalAtLeast1 atLeast1 ->
                IntegerSigned
                    { sign = signed.sign
                    , absolute =
                        case signed.sign of
                            Positive ->
                                atLeast1.whole |> NaturalAtLeast1.add NaturalAtLeast1.n1

                            Negative ->
                                atLeast1.whole
                    }
