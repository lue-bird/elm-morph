module Decimal exposing
    ( Decimal(..), Signed, SignedAbsolute(..), Fraction
    , ceiling, floor, truncate
    )

{-| safe and explicit floating point number
without the possibility of [exceptions](DecimalOrException#Exception)

@docs Decimal, Signed, SignedAbsolute, Fraction


## alter

@docs ceiling, floor, truncate

-}

import ArraySized
import Emptiable exposing (Emptiable)
import Integer exposing (Integer)
import N exposing (In, N, N0, N1, N9, n0, n1, n9)
import N.Morph
import Natural
import NaturalAtLeast1
import NaturalAtLeast1.Internal
import Possibly exposing (Possibly)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Sign exposing (Sign(..))
import Stack exposing (Stacked)


{-| A decimal number that can have a floating point

Don't shy away from spinning your own version of this if needed, like

    type FieldNumber
        = DivisionByZeroResult
        | Infinity Sign
        | Decimal Decimal

See also [`OrException Decimal`](DecimalOrException#OrException)

-}
type Decimal
    = N0
    | Signed Signed


{-| _Some_ digits after the decimal point. Can't be none
-}
type alias Fraction =
    RecordWithoutConstructorFunction
        { beforeLast : Emptiable (Stacked (N (In N0 N9))) Possibly
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
    | AtLeast1
        { whole : Natural.AtLeast1
        , fraction : Maybe Fraction
        }



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
