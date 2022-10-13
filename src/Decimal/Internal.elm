module Decimal.Internal exposing (Decimal(..), Absolute(..), Fraction, Signed)

{-| Used by `module Decimal` and `module Value`

@docs Decimal, Absolute, Fraction, Signed


## [`Morph`](Morph#Morph)

-}

import Emptiable exposing (Emptiable)
import Morph
import N exposing (InFixed, N, N0, N1, N9)
import Possibly exposing (Possibly)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Sign.Internal exposing (Sign)
import Stack exposing (StackTopBelow, Stacked)


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
        { whole :
            Emptiable
                (StackTopBelow (N (InFixed N1 N9)) (N (InFixed N0 N9)))
                Never
        , fraction : Maybe Fraction
        }


type alias Fraction =
    RecordWithoutConstructorFunction
        { beforeLast : Emptiable (Stacked (N (InFixed N0 N9))) Possibly
        , last : N (InFixed N1 N9)
        }
