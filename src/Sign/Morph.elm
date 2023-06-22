module Sign.Morph exposing (char, maybeMinusChar)

{-| [`Sign`](Sign#Sign) [`Morph`](Morph#Morph)

@docs char, maybeMinusChar

-}

import Char.Morph
import Maybe.Morph
import Morph exposing (Morph, MorphRow, oneToOne)
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


{-| An optional `'-'` sign → [`Negative`](Sign#Sign),
else [narrows to](Morph#toNarrow) [`Positive`](Sign#Sign)
-}
maybeMinusChar : MorphRow Sign Char
maybeMinusChar =
    Morph.named "negation"
        (oneToOne
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
