module Integer exposing
    ( Integer
    , absolute, negate
    , int, toInt
    , decimal
    , value
    , chars
    )

{-| Arbitrary-precision whole number

@docs Integer


## alter

@docs absolute, negate


## [`Morph`](Morph#Morph)

@docs int, toInt
@docs decimal
@docs value
@docs chars

-}

import Decimal exposing (Decimal)
import Integer.Internal
import Morph exposing (Morph, MorphRow, Translate)
import NaturalAtLeast1
import Number exposing (Decimal(..), DecimalSigned, DecimalSignedAbsolute(..), Integer(..), IntegerSigned, Natural(..), NaturalAtLeast1)
import Sign
import String.Morph
import Value


{-| Arbitrary-precision `Int`, constructable from bits
-}
type alias Integer =
    Number.Integer


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
                DecimalN0 ->
                    n0 ()

                DecimalSigned signedValue ->
                    variantSigned signedValue
        , \variantN0 variantSigned integerChoice ->
            case integerChoice of
                IntegerN0 ->
                    variantN0 ()

                IntegerSigned signedValue ->
                    variantSigned signedValue
        )
        |> Morph.variant "0"
            ( \() -> IntegerN0, \() -> DecimalN0 )
            (Morph.broad ())
        |> Morph.variant "signed"
            ( IntegerSigned, DecimalSigned )
            decimalSigned
        |> Morph.variantsFinish


decimalSigned : Morph IntegerSigned DecimalSigned
decimalSigned =
    Morph.parts
        ( \sign absolute_ -> { sign = sign, absolute = absolute_ }
        , \sign absolute_ -> { sign = sign, absolute = absolute_ }
        )
        |> Morph.part "sign" ( .sign, .sign ) Morph.keep
        |> Morph.part "absolute" ( .absolute, .absolute ) decimalSignedAbsolute
        |> Morph.partsFinish


decimalSignedAbsolute : Morph NaturalAtLeast1 DecimalSignedAbsolute
decimalSignedAbsolute =
    Morph.value "whole absolute"
        { broaden = \whole -> DecimalAtLeast1 { whole = whole, fraction = Nothing }
        , narrow =
            \decimalAbsolute ->
                case decimalAbsolute of
                    DecimalFraction _ ->
                        Err "decimal is fraction"

                    DecimalAtLeast1 atLeast1 ->
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
    decimal |> Morph.over Decimal.value


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

    "123" |> Text.narrowTo integer --> Ok 123

    -- It also works with negative numbers
    "-123" |> Text.narrowTo integer --> Ok -123

    -- a decimal number is _not_ an integer
    "3.14"
        |> Text.narrowTo integer
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:2: I was expecting an integer value. I got stuck when I got the character '.'."

    -- but not with invalid numbers
    "abc"
        |> Text.narrowTo integer
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:1: I was expecting an integer value. I got stuck when I got the character 'a'."

-}
chars : MorphRow Integer Char
chars =
    Morph.to "integer"
        (Morph.choice
            (\n0Variant signedVariant integerNarrow ->
                case integerNarrow of
                    IntegerN0 ->
                        n0Variant ()

                    IntegerSigned signedValue ->
                        signedVariant signedValue
            )
            |> Morph.tryRow (\() -> IntegerN0) (String.Morph.only "0")
            |> Morph.tryRow IntegerSigned signedChars
            |> Morph.choiceRowFinish
        )


signedChars : MorphRow IntegerSigned Char
signedChars =
    Morph.succeed
        (\signPart absolutePart ->
            { sign = signPart
            , absolute = absolutePart
            }
        )
        |> Morph.grab .sign Sign.maybeMinusChar
        |> Morph.grab .absolute NaturalAtLeast1.chars


{-| Flip its [`Sign`](Sign#Sign)
-}
negate : Integer -> Integer
negate =
    \integer ->
        case integer of
            IntegerN0 ->
                IntegerN0

            IntegerSigned signed ->
                IntegerSigned { signed | sign = signed.sign |> Sign.opposite }


{-| Remove its [`Sign`](Sign#Sign)
-}
absolute : Integer -> Natural
absolute =
    \integer ->
        case integer of
            IntegerN0 ->
                NaturalN0

            IntegerSigned signed ->
                NaturalAtLeast1 signed.absolute



{-
   add : Integer -> (Integer -> Integer)
   add toAdd =
       \integer ->
           case ( integer, toAdd ) of
               ( IntegerN0, result ) ->
                   result

               ( IntegerSigned integerSigned, IntegerN0 ) ->
                   IntegerSigned integerSigned

               ( IntegerSigned integerSigned, IntegerSigned toAddSigned ) ->
                   integerSigned |> signedAdd toAddSigned


   signedAdd : IntegerSigned -> (IntegerSigned -> Integer)
   signedAdd toAdd =
       \signed ->
           case ( signed.sign, toAdd.sign ) of
               ( Positive, Positive ) ->
                   IntegerSigned { sign = Positive, absolute = signed.absolute |> NaturalAtLeast1.add toAdd.absolute }

               ( Negative, Negative ) ->
                   IntegerSigned { sign = Negative, absolute = signed.absolute |> NaturalAtLeast1.add toAdd.absolute }

               ( Negative, Positive ) ->
                   signed.absolute |> NaturalAtLeast1.subtract toAdd.absolute |> negate

               ( Positive, Negative ) ->
                   signed.absolute |> NaturalAtLeast1.subtract toAdd.absolute
-}
