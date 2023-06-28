module Decimal.Morph exposing
    ( orException, value
    , chars
    )

{-| [`Decimal`](Decimal#Decimal) [`Morph`](Morph#Morph)

@docs orException, value


## row

@docs chars

-}

import Decimal exposing (Decimal(..), Fraction, SignedAbsolute(..))
import DecimalOrException exposing (OrException)
import Maybe.Morph
import Morph exposing (Morph, MorphRow, grab, match, one, oneToOne)
import N exposing (n1, n9)
import N.Morph
import NaturalAtLeast1.Internal
import Sign.Morph
import String.Morph
import Value


{-| [`Morph`](Morph#Morph)
a [`Decimal`](Decimal#Decimal)
to an [`OrException Decimal`](DecimalOrException#OrException)
-}
orException : Morph Decimal (OrException Decimal)
orException =
    Morph.custom "Decimal"
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


{-| [`Value.Morph`](Value#Morph) from a [`Decimal`](Decimal#Decimal)

To get a [`Value.Morph`](Value#Morph) from a `Float`,
see [`DecimalOrException.value`](DecimalOrException#value)

-}
value : Value.Morph Decimal
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
        |> Morph.over Value.atom
