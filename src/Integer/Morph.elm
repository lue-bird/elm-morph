module Integer.Morph exposing
    ( int, decimal
    , value
    , chars, bits
    )

{-| [`Morph`](Morph#Morph) for an [arbitrary-sized `Integer`](Integer#Integer)

@docs int, decimal
@docs value


## row

@docs chars, bits

-}

import ArraySized exposing (ArraySized)
import ArraySized.Morph
import Bit exposing (Bit)
import BitArray.Extra
import Bytes
import Decimal exposing (Decimal)
import Decimal.Morph
import Integer exposing (Integer)
import Linear exposing (Direction(..))
import Morph exposing (Morph, MorphOrError, MorphRow)
import N exposing (Add1, In, N, On, To, Up, n1)
import Natural
import Natural.Internal
import NaturalAtLeast1
import Sign
import Sign.Morph
import String.Morph
import Value.Morph.Internal exposing (MorphValue)


{-| [`Morph`](Morph#Morph) an [`Integer`](Integer#Integer)
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
    Morph.custom "whole absolute"
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


{-| [`MorphValue`](Value-Morph#MorphValue) from an [`Integer`](Integer#Integer)
-}
value : MorphValue Integer
value =
    decimal |> Morph.over Decimal.Morph.value


{-| [`Morph.OneToOne`](Morph#OneToOne) between an `Int` and a [decimal representation](Integer#Integer).

Keep in mind that `Integer -> Int` can overflow
since `Int` is fixed in bit size while [`Integer`](Integer#Integer) is not.

[Inverse](Morph#invert) of [`Int.Morph.integer`](Int-Morph#integer)

-}
int : MorphOrError Integer Int error_
int =
    Morph.oneToOne Integer.fromInt Integer.toInt


{-| [`Integer`](Integer#Integer) [`MorphRow`](Morph#MorphRow).

You can of course work with the [`Integer`](Decimal#Decimal) type directly,
or convert it to for example an `Int`

    import Morph
    import List.Morph
    import Int.Morph

    -- works with natural numbers
    "123"
        |> Morph.toNarrow
            (Int.Morph.integer
                |> Morph.overRow Integer.Morph.chars
                |> Morph.rowFinish
                |> Morph.over List.Morph.string
            )
    --> Ok 123

    -- works with negative numbers
    "-123"
        |> Morph.toNarrow
            (Int.Morph.integer
                |> Morph.overRow Integer.Morph.chars
                |> Morph.rowFinish
                |> Morph.over List.Morph.string
            )
    --> Ok -123

    -- a decimal number is _not_ an integer
    "3.14"
        |> Morph.toNarrow
            (Int.Morph.integer
                |> Morph.overRow Integer.Morph.chars
                |> Morph.rowFinish
                |> Morph.over List.Morph.string
            )
        |> Result.toMaybe
    --> Nothing

    -- exponential notation, other letters, symbols etc make it fail
    "3e10"
        |> Morph.toNarrow
            (Int.Morph.integer
                |> Morph.overRow Integer.Morph.chars
                |> Morph.rowFinish
                |> Morph.over List.Morph.string
            )
        |> Result.toMaybe
    --> Nothing

-}
chars : MorphRow Integer Char
chars =
    Morph.named "integer"
        (Morph.choice
            (\n0Variant signedVariant integerNarrow ->
                case integerNarrow of
                    Integer.N0 ->
                        n0Variant ()

                    Integer.Signed signedValue ->
                        signedVariant signedValue
            )
            |> Morph.rowTry (\() -> Integer.N0) (String.Morph.only "0")
            |> Morph.rowTry Integer.Signed signedChars
            |> Morph.choiceFinish
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


{-| [`MorphRow`](Morph#MorphRow) from a given count of [`Bit`](https://dark.elm.dmy.fr/packages/lue-bird/elm-bits/latest/Bit)s
in 2's complement,
and with a given [endianness](https://dark.elm.dmy.fr/packages/elm/bytes/latest/Bytes#Endianness).

For [`toBroad`](Morph#toBroad): If the number is greater than the capacity possible with the given bit count,
the greatest possible value will be returned instead.

-}
bits : Bytes.Endianness -> N (In (On (Add1 minFrom1_)) (Up maxX_ To (Add1 maxFrom1PlusX_))) -> MorphRow Integer Bit
bits endianness bitCount =
    bitArrayOfSize bitCount
        |> (case endianness of
                Bytes.BE ->
                    identity

                Bytes.LE ->
                    Morph.over
                        (Morph.oneToOne BitArray.Extra.reverseEndian BitArray.Extra.reverseEndian)
           )
        |> Morph.overRow
            (ArraySized.Morph.exactly bitCount (Morph.keep |> Morph.one))


bitArrayOfSize :
    N (In (On (Add1 minFrom1)) (Up maxX To (Add1 maxFrom1PlusX)))
    ->
        Morph.MorphIndependently
            (ArraySized Bit (In (On (Add1 minFrom1)) (Up maxX To (Add1 maxFrom1PlusX)))
             -> Result error_ Integer
            )
            (Integer
             ->
                ArraySized
                    Bit
                    (In (On (Add1 minFrom1)) (Up maxX To (Add1 maxFrom1PlusX)))
            )
bitArrayOfSize bitCount =
    Morph.oneToOne
        fromBitArray
        (toBitArrayOfSize bitCount)


negative1OfSize : N range -> ArraySized Bit range
negative1OfSize bitCount =
    ArraySized.repeat Bit.I bitCount


fromBitArray :
    ArraySized Bit (In (On (Add1 minFrom1_)) (Up maxX_ To (Add1 maxFrom1PlusX_)))
    -> Integer
fromBitArray =
    \bitArray ->
        case bitArray |> ArraySized.element ( Up, n1 ) of
            Bit.O ->
                bitArray
                    |> ArraySized.remove ( Up, n1 )
                    |> Natural.Internal.fromBitArray
                    |> Morph.toBroad Natural.Internal.integer

            Bit.I ->
                let
                    negativeAbsolute =
                        bitArray
                            |> ArraySized.remove ( Up, n1 )
                            |> ArraySized.map Bit.opposite
                            |> BitArray.Extra.add
                                (negative1OfSize (bitArray |> ArraySized.length |> N.subtract n1))
                in
                { sign = Sign.Negative
                , absolute =
                    { bitsAfterI =
                        negativeAbsolute.overflow :: (negativeAbsolute.inRange |> ArraySized.toList)
                    }
                }
                    |> Integer.Signed


toBitArrayOfSize :
    N (In (On (Add1 minFrom1)) (Up maxX To (Add1 maxFrom1PlusX)))
    ->
        (Integer
         -> ArraySized Bit (In (On (Add1 minFrom1)) (Up maxX To (Add1 maxFrom1PlusX)))
        )
toBitArrayOfSize bitCount =
    \integer ->
        case integer of
            Integer.N0 ->
                ArraySized.repeat Bit.O bitCount

            Integer.Signed signed ->
                case signed.sign of
                    Sign.Negative ->
                        signed.absolute
                            |> NaturalAtLeast1.toBitArrayOfSize
                                (bitCount |> N.subtract n1)
                            |> BitArray.Extra.add
                                (negative1OfSize (bitCount |> N.subtract n1))
                            |> .inRange
                            |> ArraySized.insert ( Linear.Up, n1 ) Bit.O
                            |> ArraySized.map Bit.opposite

                    Sign.Positive ->
                        signed.absolute
                            |> NaturalAtLeast1.toBitArrayOfSize
                                (bitCount |> N.subtract n1)
                            |> ArraySized.insert ( Linear.Up, n1 ) Bit.O
