module Decimal.Morph exposing
    ( orException, value
    , chars, bitsVariableCount
    , orExceptionValue, exceptionValue
    , orExceptionFloat
    )

{-| [`Decimal`](Decimal#Decimal) [`Morph`](Morph#Morph)

@docs orException, value


## row

@docs chars, bitsVariableCount


## [`Decimal`](Decimal#Decimal) or [`Exception`](Decimal#Exception)

[`Morph`](Morph#Morph) a [`Decimal`](Decimal#Decimal) where infinities and NaN are possible states

@docs orExceptionValue, exceptionValue


## `Float`

@docs orExceptionFloat

-}

import Bit exposing (Bit)
import Bit.Morph
import Decimal exposing (Decimal(..), Exception(..), Fraction, SignedAbsolute(..))
import List.Morph
import Maybe.Morph
import Morph exposing (Morph, MorphOrError, MorphRow, grab, match)
import N exposing (Add1, In, N, N1, N9, To, Up, n0, n1, n2, n3, n4, n5, n6, n7, n8, n9)
import N.Morph
import Natural
import Natural.Morph
import NaturalAtLeast1
import NaturalAtLeast1Base10
import Sign exposing (Sign(..))
import Sign.Morph
import String.Morph
import Value
import Value.Morph.Internal exposing (MorphValue)


{-| [`Morph`](Morph#Morph)
a value
to a [`Result Exception value`](Decimal#Exception)
-}
orException : Morph aValue (Result Exception aValue)
orException =
    Morph.custom "without exception"
        { toNarrow =
            \floatExplicit_ ->
                case floatExplicit_ of
                    Ok number ->
                        number |> Ok

                    Err exception ->
                        ("exception: " ++ (exception |> Decimal.exceptionToString)) |> Err
        , toBroad = Ok
        }


{-| [`MorphRow`](Morph#MorphRow) from chars to a [`Decimal`](Decimal#Decimal) number.

    import Morph
    import List.Morph

    -- trailing 0s are matched but have no effect
    "12.0340000"
        |> Morph.toNarrow
            (Decimal.Morph.chars
                |> Morph.rowFinish
                |> Morph.over List.Morph.string
            )
    --→ Ok with a Decimal representing
    --→ 12.034

    "-12.000"
        |> Morph.toNarrow
            (Decimal.Morph.chars
                |> Morph.rowFinish
                |> Morph.over List.Morph.string
            )
    --→ Ok with a Decimal representing
    --→ -12.0


    -- leading floating point is allowed

    ".012"
        |> Morph.toNarrow
            (Decimal.Morph.chars
                |> Morph.rowFinish
                |> Morph.over List.Morph.string
            )
    --→ Ok with a Decimal representing
    --→ 0.012

    "-.12"
        |> Morph.toNarrow
            (Decimal.Morph.chars
                |> Morph.rowFinish
                |> Morph.over List.Morph.string
            )
    --→ Ok with a Decimal representing
    --→ -0.12

    -- fails for integers without a floating point, see the not below
    "12"
        |> Morph.toNarrow
            (Decimal.Morph.chars
                |> Morph.rowFinish
                |> Morph.over List.Morph.string
            )
        |> Result.toMaybe
    --> Nothing

    -- but succeeds for integers with a trailing floating point
    "12."
        |> Morph.toNarrow
            (Decimal.Morph.chars
                |> Morph.rowFinish
                |> Morph.over List.Morph.string
            )
    --→ Ok with a Decimal representing
    --→ 12.0

    -- exponential notation, other letters, symbols etc make it fail

    "."
        |> Morph.toNarrow
            (Decimal.Morph.chars
                |> Morph.rowFinish
                |> Morph.over List.Morph.string
            )
        |> Result.toMaybe
    --> Nothing

    "3e10"
        |> Morph.toNarrow
            (Decimal.Morph.chars
                |> Morph.rowFinish
                |> Morph.over List.Morph.string
            )
        |> Result.toMaybe
    --> Nothing

To allow integers to parse as decimals as well,
build a [`Morph.choice`](Morph#choice) between
[`Decimal.Morph.chars`](#chars)
and [`Integer.Morph.chars`](Integer-Morph#chars)

The fact that `"12."` parses as 12 might also seem weird to you.
If you don't want to allow that,
you'll need to spin your own version, taking this implementation as a reference.
It's not that scary I swear!

-}
chars : MorphRow Decimal Char
chars =
    Morph.named "decimal"
        (Morph.choice
            (\signedVariant n0Variant numberNarrow ->
                case numberNarrow of
                    N0 ->
                        n0Variant ()

                    Signed signedValue ->
                        signedVariant signedValue
            )
            |> Morph.tryRow Signed signedChars
            |> Morph.tryRow (\() -> N0) (String.Morph.only "0.")
            |> Morph.choiceFinish
            |> Morph.match
                (Morph.broad []
                    |> Morph.overRow
                        (Morph.whilePossible
                            (String.Morph.only "0")
                        )
                )
        )


{-| [`MorphRow`](Morph#MorphRow) from chars to a [`Decimal.Signed`](Decimal#Signed) number.
-}
signedChars : MorphRow Decimal.Signed Char
signedChars =
    Morph.named "signed"
        (Morph.succeed
            (\signPart absolutePart ->
                { sign = signPart
                , absolute = absolutePart
                }
            )
            |> grab .sign Sign.Morph.maybeMinusChar
            |> grab .absolute signedAbsoluteChars
        )


signedAbsoluteChars : MorphRow Decimal.SignedAbsolute Char
signedAbsoluteChars =
    Morph.named "absolute"
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
                    |> match
                        (Morph.broad (Just ())
                            |> Morph.overRow
                                (Maybe.Morph.row (String.Morph.only "0"))
                        )
                    |> match (String.Morph.only ".")
                    |> grab (\fraction_ -> fraction_) fractionChars
                )
            |> Morph.tryRow AtLeast1
                (Morph.succeed
                    (\wholePart fractionPart ->
                        { whole = wholePart
                        , fraction = fractionPart
                        }
                    )
                    |> grab .whole NaturalAtLeast1.chars
                    |> match (String.Morph.only ".")
                    |> grab .fraction (Maybe.Morph.row fractionChars)
                )
            |> Morph.choiceFinish
        )


{-| [`MorphRow`](Morph#MorphRow) from chars to a [`Decimal.Fraction`](Decimal#Fraction).
-}
fractionChars : MorphRow Fraction Char
fractionChars =
    Morph.named "fraction"
        (Morph.succeed
            (\beforeLast last ->
                { beforeLast = beforeLast, last = last }
            )
            |> Morph.grab .beforeLast
                (Morph.whilePossible
                    (Morph.oneToOne N.inToNumber N.inToOn
                        |> Morph.over N.Morph.char
                        |> Morph.one
                    )
                )
            |> Morph.grab .last
                (Morph.oneToOne N.inToNumber N.inToOn
                    |> Morph.over (N.Morph.in_ ( n1, n9 ))
                    |> Morph.over N.Morph.char
                    |> Morph.one
                )
        )


{-| [`MorphValue`](Value-Morph#MorphValue) from a [`Decimal`](Decimal#Decimal)

To get a [`MorphValue`](Value-Morph#MorphValue) from a `Float`,
use [`Float.Morph.decimalOrException`](Float-Morph#decimalOrException)
over [`Decimal.Morph.orExceptionValue`](#orExceptionValue)

-}
value : MorphValue Decimal
value =
    Morph.custom "decimal"
        { toNarrow =
            \atom ->
                case atom of
                    Value.Number decimal ->
                        decimal |> Ok

                    atomExceptDecimal ->
                        atomExceptDecimal |> Value.atomKindToString |> Err
        , toBroad = Value.Number
        }
        |> Morph.over Value.Morph.Internal.atom



-- OrException Decimal


{-| [`MorphValue`](Value-Morph#MorphValue) from an [`Exception`](Decimal#Exception)
-}
exceptionValue : MorphValue Exception
exceptionValue =
    Morph.choice
        (\variantNaN variantInfinity choiceException ->
            case choiceException of
                NaN ->
                    variantNaN ()

                Infinity sign ->
                    variantInfinity sign
        )
        |> Value.Morph.Internal.variant ( \() -> NaN, "NaN" ) Value.Morph.Internal.unit
        |> Value.Morph.Internal.variant ( Infinity, "Infinity" ) signValue
        |> Value.Morph.Internal.choiceFinish


signValue : MorphValue Sign
signValue =
    Morph.named "sign"
        (Morph.choice
            (\negative positive sign ->
                case sign of
                    Negative ->
                        negative ()

                    Positive ->
                        positive ()
            )
            |> Value.Morph.Internal.variant ( \() -> Negative, "negative" ) Value.Morph.Internal.unit
            |> Value.Morph.Internal.variant ( \() -> Positive, "positive" ) Value.Morph.Internal.unit
            |> Value.Morph.Internal.choiceFinish
        )


{-| [`Morph`](Morph#Morph)
an [`elm/core` `Float`](https://dark.elm.dmy.fr/packages/elm/core/latest/Basics#Float)
to a [`Result Exception Decimal`](Decimal#Exception)

Keep in mind that `DecimalOrException -> Float` can be lossy
since `Float` is fixed in bit size while [`Decimal`](Decimal#Decimal) is not

[Inverse] of [`Float.Morph.decimalOrException`](Float-Morph#decimalOrException)

-}
orExceptionFloat : MorphOrError (Result Exception Decimal) Float error_
orExceptionFloat =
    Morph.oneToOne Decimal.fromFloat Decimal.orExceptionToFloat


{-| `Float` [`MorphValue`](Value-Morph#MorphValue)
-}
orExceptionValue : MorphValue (Result Exception Decimal)
orExceptionValue =
    Morph.choice
        (\variantDecimal variantException choiceExplicit ->
            case choiceExplicit of
                Ok decimal ->
                    variantDecimal decimal

                Err exception ->
                    variantException exception
        )
        |> Value.Morph.Internal.variant ( Ok, "decimal" ) decimalInternalValue
        |> Value.Morph.Internal.variant ( Err, "exception" ) exceptionValue
        |> Value.Morph.Internal.choiceFinish


decimalInternalValue : MorphValue Decimal
decimalInternalValue =
    Morph.custom "decimal"
        { toNarrow =
            \atom ->
                case atom of
                    Value.Number decimal ->
                        decimal |> Ok

                    atomExceptDecimal ->
                        atomExceptDecimal
                            |> Value.atomKindToString
                            |> Err
        , toBroad = Value.Number
        }
        |> Morph.over Value.Morph.Internal.atom


{-| `MorphRow` from from `Bit`s to a [`Decimal`](Decimal#Decimal)
-}
bitsVariableCount : MorphRow Decimal Bit
bitsVariableCount =
    Morph.named "decimal"
        (Morph.choice
            (\n0 signed decimal ->
                case decimal of
                    Decimal.N0 ->
                        n0 ()

                    Decimal.Signed signedValue ->
                        signed signedValue
            )
            |> Morph.tryRow (\() -> Decimal.N0)
                (Morph.named "0" (Bit.Morph.only Bit.O |> Morph.one))
            |> Morph.tryRow Decimal.Signed
                (Morph.succeed (\signed -> signed)
                    |> Morph.match (Bit.Morph.only Bit.I |> Morph.one)
                    |> Morph.grab (\signed -> signed) signedBits
                )
            |> Morph.choiceFinish
        )


{-| [`MorphRow`](Morph#MorphRow) from `Bit`s to a [`Decimal.Signed`](Decimal#Signed)
-}
signedBits : MorphRow Decimal.Signed Bit
signedBits =
    Morph.named "signed"
        (Morph.succeed (\sign absolute -> { sign = sign, absolute = absolute })
            |> Morph.grab .sign (Sign.Morph.bit |> Morph.one)
            |> Morph.grab .absolute signedAbsoluteBits
        )


{-| [`MorphRow`](Morph#MorphRow) from `Bit`s to a [`Decimal.Fraction`](Decimal#Fraction)
-}
fractionBits : MorphRow Decimal.Fraction Bit
fractionBits =
    Morph.named "fraction"
        (Morph.oneToOne
            (\initial0CountAndAfter ->
                { beforeLast =
                    List.repeat
                        (initial0CountAndAfter.initial0Count |> N.toInt)
                        (n0 |> N.maxTo n9 |> N.inToNumber)
                        ++ (case initial0CountAndAfter.after0s of
                                Natural.N0 ->
                                    []

                                Natural.AtLeast1 after0sAtLeast1 ->
                                    let
                                        after0sAtLeast1Base2 =
                                            after0sAtLeast1 |> NaturalAtLeast1Base10.fromBase2
                                    in
                                    (after0sAtLeast1Base2.first |> N.minTo0 |> N.minToNumber)
                                        :: after0sAtLeast1Base2.afterFirst
                           )
                , last = initial0CountAndAfter.last
                }
            )
            (\fraction ->
                let
                    beforeLast =
                        fraction.beforeLast
                            |> List.foldl
                                (\el soFar ->
                                    case soFar.after0sBase10 of
                                        Just after0sBase10AtLeast1 ->
                                            { soFar
                                                | after0sBase10 =
                                                    { after0sBase10AtLeast1
                                                        | afterFirstReverse =
                                                            after0sBase10AtLeast1.afterFirstReverse |> (::) el
                                                    }
                                                        |> Just
                                            }

                                        Nothing ->
                                            case el |> N.inToOn |> N.isAtLeast n1 of
                                                Err _ ->
                                                    { soFar | initial0Count = soFar.initial0Count + 1 }

                                                Ok elAtLeast1 ->
                                                    { soFar
                                                        | after0sBase10 =
                                                            { first = elAtLeast1 |> N.inToNumber
                                                            , afterFirstReverse = []
                                                            }
                                                                |> Just
                                                    }
                                )
                                { initial0Count = 0, after0sBase10 = Nothing }
                in
                { initial0Count = beforeLast.initial0Count |> N.intToAtLeast n0
                , last = fraction.last
                , after0s =
                    case beforeLast.after0sBase10 of
                        Nothing ->
                            Natural.N0

                        Just after0sBase10AtLeast1 ->
                            { first = after0sBase10AtLeast1.first
                            , afterFirst =
                                after0sBase10AtLeast1.afterFirstReverse
                                    |> List.reverse
                            }
                                |> NaturalAtLeast1Base10.toBase2
                                |> Natural.AtLeast1
                }
            )
            |> Morph.overRow
                (Morph.succeed
                    (\initial0Count after0s last ->
                        { initial0Count = initial0Count, after0s = after0s, last = last }
                    )
                    |> Morph.grab .initial0Count
                        (N.Morph.natural
                            |> Morph.overRow Natural.Morph.bitsVariableCount
                        )
                    |> Morph.grab .after0s Natural.Morph.bitsVariableCount
                    |> Morph.grab .last fractionLastDigitBits
                )
        )


fractionLastDigitBits : MorphRow (N (In N1 N9)) Bit
fractionLastDigitBits =
    let
        withDigitRange :
            N (In (N.On (Add1 minX_)) (Up max_ To N9))
            -> N (In N1 N9)
        withDigitRange digit =
            digit |> N.minTo n1 |> N.maxTo n9 |> N.inToNumber
    in
    Morph.named "1|..|9"
        (Morph.choice
            (\v1 v2 v3 v4 v5 v6 v7 v8 v9 n ->
                case n |> N.toInt of
                    1 ->
                        v1 ()

                    2 ->
                        v2 ()

                    3 ->
                        v3 ()

                    4 ->
                        v4 ()

                    5 ->
                        v5 ()

                    6 ->
                        v6 ()

                    7 ->
                        v7 ()

                    8 ->
                        v8 ()

                    -- 9
                    _ ->
                        v9 ()
            )
            |> Morph.tryRow (\() -> n1 |> withDigitRange)
                (List.Morph.broadSequenceMap
                    (Bit.Morph.only >> Morph.one)
                    [ Bit.O, Bit.O, Bit.O, Bit.O ]
                )
            |> Morph.tryRow (\() -> n2 |> withDigitRange)
                (List.Morph.broadSequenceMap
                    (Bit.Morph.only >> Morph.one)
                    [ Bit.O, Bit.O, Bit.O, Bit.I ]
                )
            |> Morph.tryRow (\() -> n3 |> withDigitRange)
                (List.Morph.broadSequenceMap
                    (Bit.Morph.only >> Morph.one)
                    [ Bit.O, Bit.O, Bit.I, Bit.O ]
                )
            |> Morph.tryRow (\() -> n4 |> withDigitRange)
                (List.Morph.broadSequenceMap
                    (Bit.Morph.only >> Morph.one)
                    [ Bit.O, Bit.O, Bit.I, Bit.I ]
                )
            |> Morph.tryRow (\() -> n5 |> withDigitRange)
                (List.Morph.broadSequenceMap
                    (Bit.Morph.only >> Morph.one)
                    [ Bit.O, Bit.I, Bit.O, Bit.O ]
                )
            |> Morph.tryRow (\() -> n6 |> withDigitRange)
                (List.Morph.broadSequenceMap
                    (Bit.Morph.only >> Morph.one)
                    [ Bit.O, Bit.I, Bit.O, Bit.I ]
                )
            |> Morph.tryRow (\() -> n7 |> withDigitRange)
                (List.Morph.broadSequenceMap
                    (Bit.Morph.only >> Morph.one)
                    [ Bit.O, Bit.I, Bit.I, Bit.O ]
                )
            |> Morph.tryRow (\() -> n8 |> withDigitRange)
                (List.Morph.broadSequenceMap
                    (Bit.Morph.only >> Morph.one)
                    [ Bit.O, Bit.I, Bit.I, Bit.I ]
                )
            |> Morph.tryRow (\() -> n9 |> withDigitRange)
                (List.Morph.broadSequenceMap
                    (Bit.Morph.only >> Morph.one)
                    [ Bit.I ]
                )
            |> Morph.choiceFinish
        )


signedAbsoluteBits : MorphRow Decimal.SignedAbsolute Bit
signedAbsoluteBits =
    Morph.named "absolute"
        (Morph.choice
            (\fraction atLeast1 absolute ->
                case absolute of
                    Decimal.Fraction fractionValue ->
                        fraction fractionValue

                    Decimal.AtLeast1 atLeast1Value ->
                        atLeast1 atLeast1Value
            )
            |> Morph.tryRow Decimal.Fraction
                (Morph.succeed (\fraction -> fraction)
                    |> Morph.match (Bit.Morph.only Bit.O |> Morph.one)
                    |> Morph.grab (\fraction -> fraction) fractionBits
                )
            |> Morph.tryRow Decimal.AtLeast1
                (Morph.succeed (\atLeast1 -> atLeast1)
                    |> Morph.match (Bit.Morph.only Bit.I |> Morph.one)
                    |> Morph.grab (\atLeast1 -> atLeast1) atLeast1Bits
                )
            |> Morph.choiceFinish
        )


atLeast1Bits : MorphRow Decimal.AtLeast1 Bit
atLeast1Bits =
    Morph.named "≥ 1"
        (Morph.succeed (\whole fraction -> { whole = whole, fraction = fraction })
            |> Morph.grab .whole NaturalAtLeast1.bits
            |> Morph.grab .fraction (maybeBits fractionBits)
        )


maybeBits : MorphRow content Bit -> MorphRow (Maybe content) Bit
maybeBits contentMorphRow =
    Morph.choice
        (\nothing just maybe ->
            case maybe of
                Nothing ->
                    nothing ()

                Just content ->
                    just content
        )
        |> Morph.tryRow (\() -> Nothing)
            (Bit.Morph.only Bit.O |> Morph.one)
        |> Morph.tryRow Just
            (Morph.succeed (\content -> content)
                |> Morph.match (Bit.Morph.only Bit.I |> Morph.one)
                |> Morph.grab (\content -> content) contentMorphRow
            )
        |> Morph.choiceFinish
