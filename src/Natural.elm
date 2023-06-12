module Natural exposing (Natural(..), AtLeast1)

{-| Natural number ≥ 0

@docs Natural, AtLeast1

-}

import ArraySized exposing (ArraySized)
import Bit exposing (Bit)
import N exposing (Min, N0)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)


{-| Whole number (integer) >= 0 of arbitrary precision.
Either the bit `O` directly or `I` followed by at most a given count of
[`Bit`](https://dark.elm.dmy.fr/packages/lue-bird/elm-bits/latest/Bit)s

If you need a natural number representation with a specific number of bits, go

    ArraySized Bit (Exactly bitLength)

For larger numbers, where you want to allow numbers of arbitrary precision,
only `O | I ...` can enforce that `==` always gives the correct answer,
since the `ArraySized` could be constructed with leading `O`s!

Feel free to incorporate this into a new `type`
with variants `NaN`, `Infinity`, ... based on your specific use-case

The type is pretty rarely useful in its current state,
as the only thing you can do is convert from and to other types.

This is enough for my use-cases
but feel free to PR or open an issue if you'd like to see support
for arbitrary-precision arithmetic like addition, multiplication, ...

-}
type Natural
    = N0
    | AtLeast1 AtLeast1


{-| Positive natural number, can be arbitrarily large
-}
type alias AtLeast1 =
    RecordWithoutConstructorFunction
        { bitsAfterI : ArraySized Bit (Min N0) }
