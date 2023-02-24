module NaturalPositive exposing (NaturalPositive, rowChar, toWhole, whole)

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


type alias NaturalPositive =
    RecordWithoutConstructorFunction
        { bitsAfterI : ArraySized Bit (Min N0) }


whole : MorphOrError NaturalPositive Whole never_
whole =
    Morph.translate fromWholeImplementation toWholeImplementation


toWholeImplementation : NaturalPositive -> Whole
toWholeImplementation =
    \bitsAfterI ->
        bitsAfterI
            |> Debug.todo ""


fromWholeImplementation : Whole -> NaturalPositive
fromWholeImplementation =
    \digits ->
        digits
            |> Debug.todo ""


toWhole : MorphOrError Whole NaturalPositive never_
toWhole =
    Morph.invert whole


rowChar : MorphRow NaturalPositive Char
rowChar =
    whole
        |> Morph.overRow Whole.rowChar
