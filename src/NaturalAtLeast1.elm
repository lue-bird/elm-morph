module NaturalAtLeast1 exposing (NaturalAtLeast1, rowChar, toWhole, whole)

import ArraySized exposing (ArraySized)
import Bit exposing (Bit)
import Bits
import Decimal.Internal exposing (Whole)
import Linear exposing (Direction(..))
import Morph exposing (Morph, MorphOrError, MorphRow, Translate)
import N exposing (In, Min, N0, Up, n0)
import N.Local exposing (n32)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Whole


type alias NaturalAtLeast1 =
    RecordWithoutConstructorFunction
        { bitsAfterI : ArraySized Bit (Min N0) }


whole : MorphOrError NaturalAtLeast1 Whole never_
whole =
    Morph.translate fromWholeImplementation toWholeImplementation


toWholeImplementation : NaturalAtLeast1 -> Whole
toWholeImplementation =
    \bitsAfterI ->
        bitsAfterI
            |> Debug.todo ""


fromWholeImplementation : Whole -> NaturalAtLeast1
fromWholeImplementation =
    \digits ->
        digits
            |> Debug.todo ""


toWhole : MorphOrError Whole NaturalAtLeast1 never_
toWhole =
    Morph.invert whole


rowChar : MorphRow NaturalAtLeast1 Char
rowChar =
    whole
        |> Morph.overRow Whole.rowChar
