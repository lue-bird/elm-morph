module Integer.Morph exposing
    ( int, toInt
    , decimal
    , value
    , chars
    )

{-| [`Integer`](#Integer) [`Morph`](Morph#Morph)

@docs int, toInt
@docs decimal
@docs value
@docs chars

-}

import Decimal exposing (Decimal)
import Decimal.Morph
import Integer exposing (Integer(..))
import Integer.Internal
import Morph exposing (Morph, MorphRow, Translate)
import Natural exposing (AtLeast1, Natural)
import NaturalAtLeast1
import Sign
import Sign.Morph
import String.Morph
import Value


{-| [`Morph`](Morph#Morph) an [`Integer`](#Integer)
from a [`Decimal`](Decimal#Decimal)
without digits after the decimal point

Other possibilities of handling the fraction after the decimal points are

  - [`Decimal.truncate`](Decimal#truncate)
  - [`Decimal.floor`](Decimal#floor)
  - [`Decimal.ceiling`](Decimal#ceiling)

-}
decimal : Morph Integer Decimal
decimal =
    Morph.variants
        ( \n0 variantSigned decimalChoice ->
            case decimalChoice of
                Decimal.N0 ->
                    n0 ()

                Decimal.Signed signedValue ->
                    variantSigned signedValue
        , \variantN0 variantSigned integerChoice ->
            case integerChoice of
                Integer.N0 ->
                    variantN0 ()

                Integer.Signed signedValue ->
                    variantSigned signedValue
        )
        |> Morph.variant "0"
            ( \() -> Integer.N0, \() -> Decimal.N0 )
            (Morph.broad ())
        |> Morph.variant "signed"
            ( Integer.Signed, Decimal.Signed )
            decimalSigned
        |> Morph.variantsFinish


decimalSigned : Morph Integer.Signed Decimal.Signed
decimalSigned =
    Morph.parts
        ( \sign absolute_ -> { sign = sign, absolute = absolute_ }
        , \sign absolute_ -> { sign = sign, absolute = absolute_ }
        )
        |> Morph.part "sign" ( .sign, .sign ) Morph.keep
        |> Morph.part "absolute" ( .absolute, .absolute ) decimalSignedAbsolute
        |> Morph.partsFinish


decimalSignedAbsolute : Morph Natural.AtLeast1 Decimal.SignedAbsolute
decimalSignedAbsolute =
    Morph.value "whole absolute"
        { toBroad = \whole -> Decimal.AtLeast1 { whole = whole, fraction = Nothing }
        , toNarrow =
            \decimalAbsolute ->
                case decimalAbsolute of
                    Decimal.Fraction _ ->
                        Err "decimal is fraction"

                    Decimal.AtLeast1 atLeast1 ->
                        case atLeast1.fraction of
                            Nothing ->
                                Ok atLeast1.whole

                            Just _ ->
                                Err "decimal has fraction part"
        }


{-| [`Value.Morph`](Value#Morph) from an [`Integer`](#Integer)
-}
value : Value.Morph Integer
value =
    decimal |> Morph.over Decimal.Morph.value


{-| [`Translate`](Morph#Translate) between an `Int` and a [decimal representation](#Integer).

Keep in mind that `Integer -> Int` can overflow
since `Int` is fixed in bit size while [`Integer`](#Integer) is not.

-}
int : Translate Integer Int
int =
    Morph.translate Integer.Internal.fromInt Integer.Internal.toInt


{-| [`Translate`](Morph#Translate) between an `Int` and a [decimal representation](#Integer).

Keep in mind that `Integer -> Int` can overflow
since `Int` is fixed in bit size while [`Integer`](#Integer) is not.

-}
toInt : Translate Int Integer
toInt =
    Morph.invert int


{-| [`Integer`](#Integer) [`MorphRow`](Morph#MorphRow)

    import Morph.Error

    "123" |> Text.toNarrow integer --> Ok 123

    -- It also works with negative numbers
    "-123" |> Text.toNarrow integer --> Ok -123

    -- a decimal number is _not_ an integer
    "3.14"
        |> Text.toNarrow integer
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:2: I was expecting an integer value. I got stuck when I got the character '.'."

    -- but not with invalid numbers
    "abc"
        |> Text.toNarrow integer
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:1: I was expecting an integer value. I got stuck when I got the character 'a'."

-}
chars : MorphRow Integer Char
chars =
    Morph.to "integer"
        (Morph.choice
            (\n0Variant signedVariant integerNarrow ->
                case integerNarrow of
                    Integer.N0 ->
                        n0Variant ()

                    Integer.Signed signedValue ->
                        signedVariant signedValue
            )
            |> Morph.tryRow (\() -> Integer.N0) (String.Morph.only "0")
            |> Morph.tryRow Integer.Signed signedChars
            |> Morph.choiceRowFinish
        )


signedChars : MorphRow Integer.Signed Char
signedChars =
    Morph.succeed
        (\signPart absolutePart ->
            { sign = signPart
            , absolute = absolutePart
            }
        )
        |> Morph.grab .sign Sign.Morph.maybeMinusChar
        |> Morph.grab .absolute NaturalAtLeast1.chars
