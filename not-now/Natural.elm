module Natural exposing (Natural(..), AtLeast1)

{-| Arbitrary-size, safe natural number ≥ 0. See also [`Natural.Morph`](Natural-Morph)

@docs Natural, AtLeast1


## create

@docs fromN


## transform

@docs toN

The type is rarely useful in its current state,
as the only thing you can do is convert from and to other types.

This is enough for my use-cases
but feel free to PR or open an issue if you'd like to see support
for arbitrary-precision arithmetic like addition, multiplication, ...

-}

import Bit exposing (Bit)
import BitArray
import BitArray.Extra
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)


{-| Whole number (integer) ≥ 0 of arbitrary precision.
Either 0 directly or a positive number represented by the bit `I` followed by some
[`Bit`](https://dark.elm.dmy.fr/packages/lue-bird/elm-bits/latest/Bit)s

Why does the type look like this?

If you need a natural number representation with a specific number of bits, you can try

    ArraySized Bit (Exactly (On bitLength))

For larger numbers, where you want to allow numbers of arbitrary precision,
only our version can enforce that `==` always gives the correct answer,
since the `ArraySized` could be constructed with leading `O`s!

-}
type Natural
    = N0
    | AtLeast1 AtLeast1


{-| Positive natural number, can be arbitrarily large
-}
type alias AtLeast1 =
    RecordWithoutConstructorFunction
        { bitsAfterI : List Bit }
