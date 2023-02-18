module Decimal exposing
    ( Decimal(..), Signed, Absolute(..), Fraction
    , rowChar, floatExplicit, value
    )

{-| safe and explicit `Float`
without the possibility of [exceptions](FloatExplicit#Exception)

@docs Decimal, Signed, Absolute, Fraction


## [`Morph`](Morph#Morph)

@docs rowChar, floatExplicit, value

-}

import ArraySized exposing (ArraySized)
import ArraySized.Morph
import Bit exposing (Bit)
import Char.Morph
import Decimal.Internal exposing (Whole)
import Emptiable exposing (Emptiable, fill, filled)
import FloatExplicit exposing (FloatExplicit)
import Linear exposing (Direction(..))
import Maybe.Morph
import Morph exposing (Morph, MorphIndependently, MorphOrError, MorphRow, Translate, broadenFrom, grab, narrowTo, one, skip, translate)
import N exposing (Add1, In, Min, N, N0, N1, N9, To, Up, Up0, Up1, Up9, n0, n1, n9)
import N.Morph
import Possibly exposing (Possibly)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Sign exposing (Sign(..))
import Sign.Internal
import Stack exposing (Stacked)
import Stack.Morph
import String.Morph
import Value
import Whole



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


{-| What comes after its [`Sign`](Sign#Sign)
-}
type Absolute
    = Fraction Fraction
    | AtLeast1
        { whole : Whole
        , fraction : Maybe Fraction
        }


{-| _Some_ digits after the decimal point. Can't be none
-}
type alias Fraction =
    RecordWithoutConstructorFunction
        { beforeLast : Emptiable (Stacked (N (In N0 N9))) Possibly
        , last : N (In N1 N9)
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
build a [`Morph.choice`](Choice#between)
[`Decimal.rowChar`](#rowChar)
and [`Integer.rowChar`](Integer#rowChar)

For different parsing behavior, spin your own
using [`Decimal.rowChar`](#rowChar) implementation as a reference

  - [`fractionRowChar`](#fractionRowChar)
  - [`Sign.rowChar`](Sign#rowChar)
  - TODO include? [`atLeast1WholeRowChar`](#fractionRowChar)
  - TODO include? [`unnecessary0RowChar`](#fractionRowChar)

-}
rowChar : MorphRow Decimal Char
rowChar =
    -- TODO: make decimal point obligatory
    Morph.to "decimal"
        (Morph.choice
            (\signedVariant n0Variant numberNarrow ->
                case numberNarrow of
                    N0 ->
                        n0Variant ()

                    Signed signedValue ->
                        signedVariant signedValue
            )
            |> Morph.tryRow Signed signed
            |> Morph.tryRow (\() -> N0) (String.Morph.only "0.")
            |> Morph.choiceRowFinish
            |> skip
                (Morph.broad (ArraySized.repeat () n0)
                    |> Morph.overRow
                        (ArraySized.Morph.atLeast
                            (String.Morph.only "0")
                            n0
                        )
                )
        )


signed : MorphRow Signed Char
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


absolute : MorphRow Absolute Char
absolute =
    Morph.to "absolute"
        (Morph.choice
            (\fractionVariant atLeast1Variant absoluteUnion ->
                case absoluteUnion of
                    Fraction fractionValue ->
                        fractionVariant fractionValue

                    AtLeast1 atLeast1Value ->
                        atLeast1Variant atLeast1Value
            )
            |> Morph.tryRow Fraction
                (Morph.succeed (\fraction_ -> fraction_)
                    |> skip
                        (Morph.broad (Just ())
                            |> Morph.overRow
                                (Maybe.Morph.row (String.Morph.only "0"))
                        )
                    |> skip (String.Morph.only ".")
                    |> grab (\fraction_ -> fraction_) fraction
                )
            |> Morph.tryRow AtLeast1
                (Morph.succeed
                    (\wholePart fractionPart ->
                        { whole = wholePart
                        , fraction = fractionPart
                        }
                    )
                    |> grab .whole Whole.rowChar
                    |> skip (String.Morph.only ".")
                    |> grab .fraction (Maybe.Morph.row fraction)
                )
            |> Morph.choiceRowFinish
        )


fraction : MorphRow Fraction Char
fraction =
    Morph.to "fraction"
        (Morph.succeed
            (\beforeLast last ->
                { beforeLast = beforeLast, last = last }
            )
            |> Morph.grab .beforeLast
                (Stack.Morph.list
                    |> Morph.over ArraySized.Morph.toList
                    |> Morph.overRow
                        (ArraySized.Morph.atLeast
                            (N.Morph.inOn
                                |> Morph.over (N.Morph.in_ ( n0, n9 ))
                                |> Morph.over N.Morph.char
                                |> one
                            )
                            n0
                        )
                )
            |> Morph.grab .last
                (N.Morph.inOn
                    |> Morph.over (N.Morph.in_ ( n1, n9 ))
                    |> Morph.over N.Morph.char
                    |> one
                )
        )


{-| [`Value.Morph`](Value#Morph) from a [`Decimal`](#Decimal)

To get a [`Value.Morph`](Value#Morph) from a `Float`,
see [`FloatExplicit.value`](FloatExplicit#value)

-}
value : Value.Morph Decimal
value =
    internal
        |> Morph.over
            (Morph.value "Decimal"
                { narrow =
                    \atom ->
                        case atom of
                            Value.Number decimal ->
                                decimal |> Ok

                            atomExceptDecimal ->
                                atomExceptDecimal |> Value.atomKindToString |> Err
                , broaden = Value.Number
                }
            )
        |> Morph.over Value.atom


internal :
    MorphOrError
        Decimal
        Decimal.Internal.Decimal
        (Morph.ErrorWithDeadEnd deadEnd_)
internal =
    Morph.choiceToFrom
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
        |> Morph.variant ( \() -> N0, \() -> Decimal.Internal.N0 ) Morph.keep
        |> Morph.variant ( Signed, Decimal.Internal.Signed ) signedInternal
        |> Morph.choiceToFromFinish


signedInternal :
    MorphOrError
        Signed
        Decimal.Internal.Signed
        (Morph.ErrorWithDeadEnd deadEnd_)
signedInternal =
    Morph.groupToFrom
        ( \sign absolutePart -> { sign = sign, absolute = absolutePart }
        , \sign absolutePart -> { sign = sign, absolute = absolutePart }
        )
        |> Morph.part ( .sign, .sign ) signInternal
        |> Morph.part ( .absolute, .absolute ) absoluteInternal
        |> Morph.groupFinish


absoluteInternal : MorphOrError Absolute Decimal.Internal.Absolute error_
absoluteInternal =
    Morph.choiceToFrom
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
        |> Morph.variant ( Fraction, Decimal.Internal.Fraction ) Morph.keep
        |> Morph.variant ( AtLeast1, Decimal.Internal.AtLeast1 ) Morph.keep
        |> Morph.choiceToFromFinish


signInternal : MorphOrError Sign Sign.Internal.Sign error_
signInternal =
    Morph.translate
        (\signInternalBeforeNarrow ->
            case signInternalBeforeNarrow of
                Sign.Internal.Negative ->
                    Sign.Negative

                Sign.Internal.Positive ->
                    Sign.Positive
        )
        (\signBeforeBroaden ->
            case signBeforeBroaden of
                Sign.Negative ->
                    Sign.Internal.Negative

                Sign.Positive ->
                    Sign.Internal.Positive
        )
