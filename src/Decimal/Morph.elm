module Decimal.Morph exposing (chars, orException, value)

{-| [`Decimal`](Decimal#Decimal) [`Morph`](Morph#Morph)

@docs chars, orException, value

-}

import ArraySized
import ArraySized.Morph
import Decimal exposing (Decimal(..), Fraction, SignedAbsolute(..))
import DecimalOrException exposing (OrException)
import Emptiable exposing (Emptiable)
import Integer exposing (Integer)
import Maybe.Morph
import Morph exposing (Morph, MorphRow, grab, match, one)
import N exposing (In, N, N0, N1, N9, n0, n1, n9)
import N.Morph
import Natural
import NaturalAtLeast1
import NaturalAtLeast1.Internal
import Possibly exposing (Possibly)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Sign exposing (Sign(..))
import Sign.Morph
import Stack exposing (Stacked)
import Stack.Morph
import String.Morph
import Value


{-| [`Morph`](Morph#Morph)
a [`Decimal`](#Decimal)
to an [`OrException Decimal`](DecimalOrException#OrException)
-}
orException : Morph Decimal (OrException Decimal)
orException =
    Morph.value "Decimal"
        { toNarrow =
            \floatExplicit_ ->
                case floatExplicit_ of
                    DecimalOrException.Number number ->
                        number |> Ok

                    DecimalOrException.Exception _ ->
                        "Exception" |> Err
        , toBroad = DecimalOrException.Number
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
and [`Integer.chars`](Integer#chars)

For different parsing behavior, spin your own
using [`Decimal.Morph.chars`](#chars) implementation as a reference

-}
chars : MorphRow Decimal Char
chars =
    Morph.to "decimal"
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
            |> Morph.choiceRowFinish
            |> match
                (Morph.broad (ArraySized.repeat () n0)
                    |> Morph.overRow
                        (ArraySized.Morph.atLeast n0
                            (String.Morph.only "0")
                        )
                )
        )


signedChars : MorphRow Decimal.Signed Char
signedChars =
    Morph.to "signed"
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
    Morph.to "absolute"
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
            |> Morph.choiceRowFinish
        )


fractionChars : MorphRow Fraction Char
fractionChars =
    Morph.to "fraction"
        (Morph.succeed
            (\beforeLast last ->
                { beforeLast = beforeLast, last = last }
            )
            |> Morph.grab .beforeLast
                (Stack.Morph.list
                    |> Morph.over ArraySized.Morph.toList
                    |> Morph.overRow
                        (ArraySized.Morph.atLeast n0
                            (N.Morph.inOn
                                |> Morph.over (N.Morph.in_ ( n0, n9 ))
                                |> Morph.over N.Morph.char
                                |> one
                            )
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
        { toNarrow =
            \atom ->
                case atom of
                    Value.Number decimal ->
                        decimal |> Ok

                    atomExceptDecimal ->
                        atomExceptDecimal |> Value.atomKindToString |> Err
        , toBroad = Value.Number
        }
        |> Morph.over Value.atom
