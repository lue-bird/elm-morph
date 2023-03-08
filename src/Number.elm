module Number exposing
    ( Sign(..)
    , Decimal(..), DecimalSignedAbsolute(..), Fraction, DecimalSigned
    , Integer(..), IntegerSigned
    , Natural(..), NaturalAtLeast1
    )

{-| Types of `module` [`Decimal`](Decimal), [`Integer`](Integer), [`Natural`](Natural).

This way, no import cycles are created (for example with [`Integer.decimal`](Integer#decimal) and [`Decimal.truncate`](Decimal#truncate)) while safe internals are still exposed.

@docs Sign


## [`Decimal`](Decimal) types

@docs Decimal, DecimalSignedAbsolute, Fraction, DecimalSigned


## [`Integer`](Integer) types

@docs Integer, IntegerSigned


## [`Natural`](Natural) types

@docs Natural, NaturalAtLeast1

-}

import ArraySized exposing (ArraySized)
import Bit exposing (Bit)
import Emptiable exposing (Emptiable)
import N exposing (In, Min, N, N0, N1, N9)
import Possibly exposing (Possibly)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Stack exposing (Stacked)


{-| See [`Decimal.Decimal`](Decimal#Decimal)
-}
type Decimal
    = DecimalN0
    | DecimalSigned DecimalSigned


{-| Any [`Decimal`](Decimal#Decimal) except `0` is represented this way
-}
type alias DecimalSigned =
    RecordWithoutConstructorFunction
        { sign : Sign
        , absolute : DecimalSignedAbsolute
        }


{-| See [`Sign.Sign`](Sign#Sign)
-}
type Sign
    = Negative
    | Positive


{-| What comes after its [`Sign`](Sign#Sign)
-}
type DecimalSignedAbsolute
    = DecimalFraction Fraction
    | DecimalAtLeast1
        { whole : NaturalAtLeast1
        , fraction : Maybe Fraction
        }


{-| Positive natural number, can be arbitrarily large
-}
type alias NaturalAtLeast1 =
    RecordWithoutConstructorFunction
        { bitsAfterI : ArraySized Bit (Min N0) }


{-| See [`Decimal.Fraction`](Decimal#Fraction)
-}
type alias Fraction =
    RecordWithoutConstructorFunction
        { beforeLast : Emptiable (Stacked (N (In N0 N9))) Possibly
        , last : N (In N1 N9)
        }


{-| See [`Integer.Integer`](Integer#Integer)
-}
type Integer
    = IntegerN0
    | IntegerSigned IntegerSigned


{-| Arbitrary-precision signed [`Integer`](#Integer), constructable from bits
-}
type alias IntegerSigned =
    RecordWithoutConstructorFunction
        { sign : Sign
        , absolute : NaturalAtLeast1
        }


{-| See [`Natural.Natural`](Natural#Natural)
-}
type Natural
    = NaturalN0
    | NaturalAtLeast1 NaturalAtLeast1
