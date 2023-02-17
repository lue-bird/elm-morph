module Decimal.Internal exposing (Decimal(..), Absolute(..), Fraction, Signed, Whole)

{-| Used by `module Decimal` and `module Value`

@docs Decimal, Absolute, Fraction, Signed, Whole


## [`Morph`](Morph#Morph)

-}

import Emptiable exposing (Emptiable)
import Morph
import N exposing (In, N, N0, N1, N9)
import Possibly exposing (Possibly)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Sign.Internal exposing (Sign)
import Stack exposing (Stacked)


type Decimal
    = N0
    | Signed Signed


{-| Any [`Decimal`](#Decimal) except `0` is represented this way
-}
type alias Signed =
    RecordWithoutConstructorFunction
        { sign : Sign
        , absolute : Absolute
        }


type Absolute
    = Fraction Fraction
    | AtLeast1
        { whole : Whole
        , fraction : Maybe Fraction
        }


type alias Whole =
    { first : N (In N1 N9)
    , afterFirst :
        Emptiable (Stacked (N (In N0 N9))) Possibly
    }


type alias Fraction =
    RecordWithoutConstructorFunction
        { beforeLast : Emptiable (Stacked (N (In N0 N9))) Possibly
        , last : N (In N1 N9)
        }
