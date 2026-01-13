module Float.Morph exposing (chars)

{-| [`Morph`](Morph#Morph) for a [`Float`](https://dark.elm.dmy.fr/packages/elm/core/latest/Basics#Float)

@docs chars

-}

import Char.Morph
import Int.Morph
import Morph exposing (MorphRow)
import N1To9
import Sign
import Sign.Morph
import String.Morph


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
chars : MorphRow Float Char
chars =
    Morph.named "decimal"
        (Morph.narrow
            (\wholePart fractionPart ->
                wholePart
                    + ((0.1 ^ Basics.logBase 10 fractionPart)
                        * fractionPart
                      )
            )
            |> Morph.grab
                -- not truncate due to precision loss
                (\f ->
                    if f >= 0 then
                        Basics.floor f

                    else
                        Basics.ceiling f
                )
                Int.Morph.chars
            |> Morph.match (String.Morph.only ".")
            |> Morph.grab
                (\f ->
                    let
                        fractionPart =
                            f - (f |> Basics.floor |> Basics.toFloat)
                    in
                    fractionPart
                        * (10 ^ Basics.logBase 10 fractionPart)
                )
                (Morph.choice
                    (\signedVariant n0Variant numberNarrow ->
                        case numberNarrow of
                            0 ->
                                n0Variant ()

                            signedNumber ->
                                signedVariant signedNumber
                    )
                    |> Morph.rowTry identity Int.Morph.positiveChars
                    |> Morph.rowTry (\() -> 0)
                        (String.Morph.only "0"
                            |> Morph.match
                                (Morph.broad []
                                    |> Morph.overRow
                                        (Morph.whilePossible (Char.Morph.only '0'))
                                )
                        )
                    |> Morph.choiceFinish
                )
        )
