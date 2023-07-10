module Sign.Morph exposing
    ( char, bit
    , maybeMinusChar
    )

{-| [`Sign`](Sign#Sign) [`Morph`](Morph#Morph)

@docs char, bit

@docs maybeMinusChar

-}

import Bit exposing (Bit)
import Char.Morph
import Maybe.Morph
import Morph exposing (Morph, MorphOrError, MorphRow)
import Sign exposing (Sign(..))
import String.Morph


{-| [`Sign`](Sign#Sign) `'+'` or `'-'`
-}
char : Morph Sign Char
char =
    Morph.named "sign"
        (Morph.choice
            (\plus minus signNarrow ->
                case signNarrow of
                    Positive ->
                        plus ()

                    Negative ->
                        minus ()
            )
            |> Morph.try (\() -> Positive) (Char.Morph.only '+')
            |> Morph.try (\() -> Negative) (Char.Morph.only '-')
            |> Morph.choiceFinish
        )


{-| `Bit.O` `Positive`, `Bit.I` ⇄ `Negative`
-}
bit : MorphOrError Sign Bit error_
bit =
    Morph.named "sign"
        (Morph.oneToOne
            (\bit_ ->
                case bit_ of
                    Bit.O ->
                        Positive

                    Bit.I ->
                        Negative
            )
            (\bit_ ->
                case bit_ of
                    Positive ->
                        Bit.O

                    Negative ->
                        Bit.I
            )
        )


{-| An optional `'-'` sign → [`Negative`](Sign#Sign),
else [narrows to](Morph#toNarrow) [`Positive`](Sign#Sign)
-}
maybeMinusChar : MorphRow Sign Char
maybeMinusChar =
    Morph.named "negation"
        (Morph.oneToOne
            (\minusSymbol ->
                case minusSymbol of
                    Nothing ->
                        Positive

                    Just () ->
                        Negative
            )
            (\signNarrow ->
                case signNarrow of
                    Positive ->
                        Nothing

                    Negative ->
                        () |> Just
            )
            |> Morph.overRow
                (Maybe.Morph.row (String.Morph.only "-"))
        )
