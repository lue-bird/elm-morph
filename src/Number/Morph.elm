module Number.Morph exposing
    ( Number, text
    , toFloat, fromFloat
    )

{-| [`Morph`](Morph#Morph) to and from decimal numbers
Morph.Morph

@docs Number, text
@docs toFloat, fromFloat

-}

import ArraySized
import ArraySized.Morph
import Digit.Morph
import Emptiable exposing (Emptiable, filled)
import Integer.Morph.Internal exposing (intAbsoluteTo0To9s, n0To9sToInt)
import Linear exposing (DirectionLinear(..))
import Morph exposing (Morph, translate)
import Morph.Text
import MorphRow exposing (MorphRow, atLeast, grab, maybe, one, skip, succeed)
import N exposing (n0, n1)
import Possibly exposing (Possibly)
import Sign
import Sign.Morph exposing (Signable(..))
import Stack exposing (Stacked)


{-| A decimal number that can have a floating point.

Don't shy away from spinning your own version of this if needed, like

    type FieldNumber
        = DivisionByZeroResult
        | Infinity Sign
        | Decimal Number

-}
type alias Number =
    Signable
        { whole : Emptiable (Stacked Digit.Morph.N0To9) Never
        , fraction : Emptiable (Stacked Digit.Morph.N0To9) Possibly
        }


{-| [`Translate`](Morph#Translate) between a `Float` and [decimal representation](#Number).

Keep in mind that `Number -> Float` can be lossy
since `Float` is fixed in bit size while [`Number`](#Number) is not.

    -9999.124
        |> broaden
            (Number.Morph.toFloat
                |> MorphRow.over Number.Morph.text
            )
    --> "-999.1239999999997962731868028640747070312"

-}
fromFloat : Morph Float Number error_
fromFloat =
    toFloat |> Morph.reverse


{-| [`Translate`](Morph#Translate) between a `Float` and a [decimal representation](#Number).

Keep in mind that `Number -> Float` can be lossy
since `Float` is fixed in bit size while [`Number`](#Number) is not.

-}
toFloat : Morph Number Float error_
toFloat =
    Morph.translate
        (\floatNarrow ->
            case floatNarrow of
                N0 ->
                    0

                Signed numberSigned ->
                    let
                        toFraction fractionDigits =
                            fractionDigits
                                |> Stack.map
                                    (\decimal digit ->
                                        (digit
                                            |> Morph.broaden Digit.Morph.n0To9ToInt
                                            |> Basics.toFloat
                                        )
                                            * (10 ^ -(1 + (decimal.index |> Basics.toFloat)))
                                    )
                                |> Stack.sum
                    in
                    Sign.number numberSigned.sign
                        ((numberSigned.whole |> n0To9sToInt |> Basics.toFloat)
                            + (case numberSigned.fraction of
                                Emptiable.Empty _ ->
                                    0

                                Emptiable.Filled fraction ->
                                    fraction |> filled |> toFraction
                              )
                        )
        )
        (\floatValue ->
            case floatValue |> Sign.ofNumber of
                Nothing ->
                    N0

                Just signed ->
                    let
                        wholeAbsolute =
                            signed.absolute |> truncate
                    in
                    { sign = signed.sign
                    , whole =
                        wholeAbsolute |> intAbsoluteTo0To9s
                    , fraction =
                        (signed.absolute - (wholeAbsolute |> Basics.toFloat))
                            |> abs
                            |> floatFraction
                    }
                        |> Signed
        )


floatFraction : Float -> Emptiable (Stacked Digit.Morph.N0To9) Possibly
floatFraction =
    \float ->
        if float == 0 then
            Emptiable.empty

        else
            let
                floatShifted1Decimal =
                    float * 10

                decimalInt =
                    floatShifted1Decimal |> floor
            in
            (case decimalInt |> Morph.narrow Digit.Morph.n0To9ToInt of
                Err _ ->
                    identity

                Ok decimal ->
                    Stack.onTopLay decimal
            )
                ((floatShifted1Decimal - (decimalInt |> Basics.toFloat))
                    |> floatFraction
                )



--


{-| Match a decimal value as a `Float`.

    import MorphRow.Error

    "12" |> Text.narrowWith number     --> Ok 12.0
    "12.34" |> Text.narrowWith number  --> Ok 12.34
    "12." |> Text.narrowWith number    --> Ok 12.0
    ".12" |> Text.narrowWith number    --> Ok 0.12
    "-12.34" |> Text.narrowWith number --> Ok -12.34
    "-.12" |> Text.narrowWith number   --> Ok -0.12

    "."
        |> Text.narrowWith number
        |> Result.mapError MorphRow.Error.textMessage
    --> Err "1:1: I was expecting a digit [0-9]. I got stuck when I got the character '.'."

    "abc" |> Text.narrowWith number
        |> Result.mapError MorphRow.Error.textMessage
    --> Err "1:1: I was expecting a digit [0-9]. I got stuck when I got the character 'a'."

-}
text : MorphRow Char Number
text =
    MorphRow.expect "number"
        (Morph.choice
            (\n0Variant signedVariant numberNarrow ->
                case numberNarrow of
                    N0 ->
                        n0Variant ()

                    Signed signedValue ->
                        signedVariant signedValue
            )
            |> MorphRow.possibility (\() -> N0)
                (Morph.Text.specific "0")
            |> MorphRow.possibility Signed
                (succeed
                    (\signPart wholePart fractionPart ->
                        { sign = signPart
                        , whole = wholePart
                        , fraction = fractionPart
                        }
                    )
                    |> grab .sign
                        Sign.Morph.maybeMinus
                    |> grab .whole
                        (succeed Stack.topDown
                            |> grab Stack.top
                                (Digit.Morph.n0To9 |> one)
                            |> grab (Stack.topRemove >> Stack.toList)
                                (ArraySized.Morph.fromList
                                    |> MorphRow.over (atLeast n0 (Digit.Morph.n0To9 |> one))
                                )
                        )
                    |> grab .fraction
                        (translate
                            (\maybeFraction ->
                                case maybeFraction of
                                    Nothing ->
                                        Emptiable.empty

                                    Just fractionDigits ->
                                        fractionDigits |> ArraySized.toStackFilled
                            )
                            (\fractionStack ->
                                case fractionStack of
                                    Emptiable.Empty _ ->
                                        Nothing

                                    Emptiable.Filled (Stack.TopDown top down) ->
                                        ArraySized.l1 top
                                            |> ArraySized.minGlue Up (down |> ArraySized.fromList)
                                            |> Just
                            )
                            |> MorphRow.over
                                (maybe
                                    (succeed (\fractionDigits -> fractionDigits)
                                        |> skip (Morph.Text.specific ".")
                                        |> grab (\fractionDigits -> fractionDigits)
                                            (atLeast n1 (Digit.Morph.n0To9 |> one))
                                    )
                                )
                        )
                )
            |> MorphRow.choiceFinish
        )
