module Sign.Morph exposing (char, maybeMinusChar)

{-| [`Sign`](Sign#Sign) [`Morph`](Morph#Morph)
-}

import Char.Morph
import Maybe.Morph
import Morph exposing (Morph, MorphRow, translate)
import Sign exposing (Sign(..))
import String.Morph


{-| [`Sign`](#Sign) `'+'` or `'-'`
-}
char : Morph Sign Char
char =
    Morph.to "sign"
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


{-| An optional `'-'` sign → [`Negative`](#Sign),
else [narrows to](Morph#toNarrow) [`Positive`](#Sign)
-}
maybeMinusChar : MorphRow Sign Char
maybeMinusChar =
    Morph.to "negation"
        (translate
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
