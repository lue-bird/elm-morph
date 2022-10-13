module Decimal exposing
    ( Decimal(..), Signed, Absolute(..), Fraction
    , rowChar, floatExplicit, value
    )

{-| number TODO MorphIndependently

@docs Decimal, Signed, Absolute, Fraction


## [`Morph`](Morph#Morph)

@docs rowChar, floatExplicit, value

-}

import ArraySized exposing (ArraySized)
import ArraySized.Morph
import Bit exposing (Bit)
import Char.Morph
import Choice
import Decimal.Internal
import Emptiable exposing (Emptiable, fill, fillMap, fillMapFlat, filled)
import FloatExplicit exposing (FloatExplicit)
import Group exposing (grab, skip)
import Linear exposing (Direction(..))
import Maybe.Morph
import Morph exposing (Morph, MorphIndependently, MorphOrError, MorphRow, Translate, broadenFrom, narrowTo, one, translate)
import N exposing (Add1, In, InFixed, Min, N, N0, N1, N9, To, Up, Up0, Up1, Up9, n0, n1, n9)
import N.Morph
import Natural exposing (Natural)
import Possibly exposing (Possibly)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Sign exposing (Sign(..))
import Sign.Internal
import Stack exposing (StackTopBelow, Stacked)
import String.Morph
import Value exposing (MorphValue)
import Value.PackageInternal



-- number


{-| A decimal number that can have a floating point

Don't shy away from spinning your own version of this if needed, like

    type FieldNumber
        = DivisionByZeroResult
        | Infinity Sign
        | Decimal Decimal

-}
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


{-| [`Morph`](Morph#Morph)
a [`Decimal`](#Decimal)
to a [`FloatExplicit`](FloatExplicit#FloatExplicit)
-}
floatExplicit : Morph Decimal FloatExplicit
floatExplicit =
    internal
        |> Morph.over
            (Morph.value "Decimal"
                { narrow =
                    \floatExplicit_ ->
                        case floatExplicit_ of
                            FloatExplicit.Decimal decimal ->
                                decimal |> Ok

                            FloatExplicit.Exception _ ->
                                "Exception" |> Err
                , broaden = FloatExplicit.Decimal
                }
            )



--


{-| Match a decimal number

    import Morph.Error


    -- trailing 0s aren't represented in the final type

    "12.0340000" |> Text.narrowTo number  --> Ok 12.034
    "-12.000" |> Text.narrowTo number --> Ok -12.0

    -- leading floating point is allowed

    ".012" |> Text.narrowTo number    --> Ok 0.012
    "-.12" |> Text.narrowTo number   --> Ok -0.12

    -- fails for integers without a floating point

    "12"
        |> Text.narrowTo number
        |> Result.mapError Morph.Error.textMessage
    --> Err ...

    -- but succeeds for integers with a trailing floating point

    "12." |> Text.narrowTo number    --> Ok 12.0

    -- fails for everything else

    "."
        |> Text.narrowTo number
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:1: I was expecting a digit [0-9]. I got stuck when I got the character '.'."

    "abc"
        |> Text.narrowTo number
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:1: I was expecting a digit [0-9]. I got stuck when I got the character 'a'."

To allow integers to parse as decimals as well,
build a [`Choice.between`](Choice#between)
[`Decimal.rowChar`](#rowChar)
and [`Integer.rowChar`](Integer#rowChar)

For different parsing behavior, spin your own
using [`Decimal.rowChar`](#rowChar) implementation as a reference

  - [`fractionRowChar`](#fractionRowChar)
  - [`Sign.rowChar`](Sign#rowChar)
  - TODO include? [`atLeast1WholeRowChar`](#fractionRowChar)
  - TODO include? [`unnecessary0RowChar`](#fractionRowChar)

-}
rowChar : MorphRow Char Decimal
rowChar =
    -- TODO: make decimal point obligatory
    Morph.to "decimal"
        (Choice.between
            (\signedVariant n0Variant numberNarrow ->
                case numberNarrow of
                    N0 ->
                        n0Variant ()

                    Signed signedValue ->
                        signedVariant signedValue
            )
            |> Choice.tryRow Signed signed
            |> Choice.tryRow (\() -> N0) (String.Morph.only "0.")
            |> Choice.finishRow
            |> skip
                (Morph.broad (ArraySized.repeat () n0)
                    |> Morph.overRow
                        (ArraySized.Morph.atLeast n0
                            (String.Morph.only "0")
                        )
                )
        )


signed : MorphRow Char Signed
signed =
    Morph.to "signed"
        (Morph.succeed
            (\signPart absolutePart ->
                { sign = signPart
                , absolute = absolutePart
                }
            )
            |> grab .sign Sign.maybeMinusChar
            |> grab .absolute absolute
        )


absolute : MorphRow Char Absolute
absolute =
    Morph.to "absolute"
        (Choice.between
            (\fractionVariant atLeast1Variant absoluteUnion ->
                case absoluteUnion of
                    Fraction fractionValue ->
                        fractionVariant fractionValue

                    AtLeast1 atLeast1Value ->
                        atLeast1Variant atLeast1Value
            )
            |> Choice.tryRow Fraction
                (Morph.succeed (\fraction_ -> fraction_)
                    |> skip
                        (Morph.broad (Just ())
                            |> Morph.overRow
                                (Maybe.Morph.row (String.Morph.only "0"))
                        )
                    |> skip (String.Morph.only ".")
                    |> grab (\fraction_ -> fraction_) fraction
                )
            |> Choice.tryRow AtLeast1
                (let
                    whole :
                        MorphRow
                            Char
                            (Emptiable
                                (StackTopBelow (N (InFixed N1 N9)) (N (InFixed N0 N9)))
                                Never
                            )
                    whole =
                        Morph.to "whole"
                            (Morph.succeed Stack.onTopLay
                                |> grab Stack.top
                                    (N.Morph.in_ ( n1, n9 )
                                        |> Morph.over N.Morph.char
                                        |> one
                                    )
                                |> grab Stack.topRemove
                                    (ArraySized.Morph.toStackEmptiable
                                        |> Morph.overRow
                                            (ArraySized.Morph.atLeast n0
                                                (N.Morph.in_ ( n0, n9 )
                                                    |> Morph.over N.Morph.char
                                                    |> one
                                                )
                                            )
                                    )
                            )
                 in
                 Morph.succeed
                    (\wholePart fractionPart ->
                        { whole = wholePart
                        , fraction = fractionPart
                        }
                    )
                    |> grab .whole whole
                    |> skip (String.Morph.only ".")
                    |> grab .fraction (Maybe.Morph.row fraction)
                )
            |> Choice.finishRow
        )


fraction : Morph.MorphRow Char Fraction
fraction =
    Morph.to "fraction"
        (Morph.succeed
            (\beforeLast last ->
                { beforeLast = beforeLast, last = last }
            )
            |> Group.grab .beforeLast
                (ArraySized.Morph.toStackEmptiable
                    |> Morph.overRow
                        (ArraySized.Morph.atLeast n0
                            (N.Morph.in_ ( n0, n9 )
                                |> Morph.over N.Morph.char
                                |> one
                            )
                        )
                )
            |> Group.grab .last
                (N.Morph.in_ ( n1, n9 )
                    |> Morph.over N.Morph.char
                    |> one
                )
        )


value : MorphValue Decimal
value =
    internal
        |> Morph.over
            (Morph.value "Decimal"
                { narrow =
                    \literal ->
                        case literal of
                            Value.Decimal decimal ->
                                decimal |> Ok

                            literalExceptDecimal ->
                                literalExceptDecimal |> Value.PackageInternal.literalKindToString |> Err
                , broaden = Value.Decimal
                }
            )
        |> Morph.over Value.literal


internal :
    MorphOrError
        Decimal
        Decimal.Internal.Decimal
        (Morph.ErrorWithDeadEnd deadEnd_)
internal =
    Choice.toFrom
        ( \variantN0 variantSigned decimal ->
            case decimal of
                Decimal.Internal.N0 ->
                    variantN0 ()

                Decimal.Internal.Signed signedValue ->
                    variantSigned signedValue
        , \variantN0 variantSigned decimal ->
            case decimal of
                N0 ->
                    variantN0 ()

                Signed signedValue ->
                    variantSigned signedValue
        )
        |> Choice.variant ( \() -> N0, \() -> Decimal.Internal.N0 ) Morph.keep
        |> Choice.variant ( Signed, Decimal.Internal.Signed ) signedInternal
        |> Choice.finishToFrom


signedInternal :
    MorphOrError
        Signed
        Decimal.Internal.Signed
        (Morph.ErrorWithDeadEnd deadEnd_)
signedInternal =
    Group.toFrom
        ( \sign absolutePart -> { sign = sign, absolute = absolutePart }
        , \sign absolutePart -> { sign = sign, absolute = absolutePart }
        )
        |> Group.part ( .sign, .sign ) signInternal
        |> Group.part ( .absolute, .absolute ) absoluteInternal
        |> Group.finish


absoluteInternal : MorphOrError Absolute Decimal.Internal.Absolute error_
absoluteInternal =
    Choice.toFrom
        ( \variantFraction variantAtLeast1 decimal ->
            case decimal of
                Decimal.Internal.Fraction fractionValue ->
                    variantFraction fractionValue

                Decimal.Internal.AtLeast1 atLeast1Value ->
                    variantAtLeast1 atLeast1Value
        , \variantFraction variantAtLeast1 decimal ->
            case decimal of
                Fraction fractionValue ->
                    variantFraction fractionValue

                AtLeast1 atLeast1Value ->
                    variantAtLeast1 atLeast1Value
        )
        |> Choice.variant ( Fraction, Decimal.Internal.Fraction ) Morph.keep
        |> Choice.variant ( AtLeast1, Decimal.Internal.AtLeast1 ) Morph.keep
        |> Choice.finishToFrom



-- sign


signInternal : MorphOrError Sign Sign.Internal.Sign error_
signInternal =
    Debug.todo ""


numberSignWith : Sign -> (number -> number)
numberSignWith signToAdapt =
    case signToAdapt of
        Positive ->
            abs

        Negative ->
            abs >> negate


numberSign : number -> Maybe { sign : Sign, absolute : number }
numberSign =
    \numberSigned ->
        case compare numberSigned 0 of
            EQ ->
                Nothing

            LT ->
                { sign = Negative, absolute = numberSigned |> abs }
                    |> Just

            GT ->
                { sign = Positive, absolute = numberSigned }
                    |> Just
