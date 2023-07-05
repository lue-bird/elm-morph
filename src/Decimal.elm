module Decimal exposing
    ( Decimal(..), Signed, SignedAbsolute(..), Fraction, AtLeast1
    , OrException(..), Exception(..)
    , ceiling, floor, truncate
    )

{-| safe and explicit floating point number
without the possibility of [exceptions](Decimal#Exception)

@docs Decimal, Signed, SignedAbsolute, Fraction, AtLeast1
@docs OrException, Exception


## alter

@docs ceiling, floor, truncate

More in [`Decimal.Morph`](Decimal-Morph)

-}

import Integer exposing (Integer)
import N exposing (In, N, N0, N1, N9)
import Natural
import NaturalAtLeast1
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Sign exposing (Sign(..))


{-| A decimal number that can have a floating point

Don't shy away from spinning your own version of this if needed, like

    type FieldNumber
        = DivisionByZeroResult
        | Infinity Sign
        | Decimal Decimal

See also [`OrException Decimal`](Decimal#OrException)

-}
type Decimal
    = N0
    | Signed Signed


{-| _Some_ digits after the decimal point. Can't be none.

Note that "fraction" here doesn't mean "rational number with a numerator and denominator"
or "number in range [0;1]"
It means: the absolute value of a written decimal number without a period, like 0.345 or 0.001

You think this name can be improved? → issue

-}
type alias Fraction =
    RecordWithoutConstructorFunction
        { beforeLast : List (N (In N0 N9))
        , last : N (In N1 N9)
        }


{-| Any [`Decimal`](Decimal#Decimal) except `0` is represented this way
-}
type alias Signed =
    RecordWithoutConstructorFunction
        { sign : Sign
        , absolute : SignedAbsolute
        }


{-| What comes after its [`Sign`](Sign#Sign)
-}
type SignedAbsolute
    = Fraction Fraction
    | AtLeast1 AtLeast1


{-| Absolute value of a signed [`Decimal`](#Decimal) ≥ 1
-}
type alias AtLeast1 =
    RecordWithoutConstructorFunction
        { whole : Natural.AtLeast1
        , fraction : Maybe Fraction
        }


{-| Number where [IEEE 754 number exception states](#Exception) are possible

`OrException Decimal` for example is like an [`elm/core` `Float`](https://dark.elm.dmy.fr/packages/elm/core/latest/Basics#Float)
except

  - [`Exception`](#Exception)s are an explicit case so you can easily extract a [`Decimal`](Decimal#Decimal)
  - it can have arbitrary decimal points, see [`Decimal`](Decimal#Decimal)

-}
type OrException aNumber
    = Number aNumber
    | Exception Exception


{-| Non-number calculation result
-}
type Exception
    = NaN
    | Infinity Sign



--


{-| Remove the [`Fraction`](#Fraction) part after the decimal point `.`
to create an [`Integer`](Integer#Integer)
-}
truncate : Decimal -> Integer
truncate =
    \decimal ->
        case decimal of
            N0 ->
                Integer.N0

            Signed signed ->
                signed |> signedTruncate


signedTruncate : Signed -> Integer
signedTruncate =
    \signed ->
        case signed.absolute of
            Fraction _ ->
                Integer.N0

            AtLeast1 atLeast1 ->
                Integer.Signed
                    { sign = signed.sign
                    , absolute = atLeast1.whole
                    }


{-| Its nearest lower [`Integer`](Integer#Integer) number
-}
floor : Decimal -> Integer
floor =
    \decimal ->
        case decimal of
            N0 ->
                Integer.N0

            Signed signed ->
                signed |> signedFloor


signedFloor : Signed -> Integer
signedFloor =
    \signed ->
        case signed.absolute of
            Fraction _ ->
                Integer.N0

            AtLeast1 atLeast1 ->
                Integer.Signed
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
            N0 ->
                Integer.N0

            Signed signed ->
                signed |> signedCeiling


signedCeiling : Signed -> Integer
signedCeiling =
    \signed ->
        case signed.absolute of
            Fraction _ ->
                Integer.N0

            AtLeast1 atLeast1 ->
                Integer.Signed
                    { sign = signed.sign
                    , absolute =
                        case signed.sign of
                            Positive ->
                                atLeast1.whole |> NaturalAtLeast1.add NaturalAtLeast1.n1

                            Negative ->
                                atLeast1.whole
                    }
