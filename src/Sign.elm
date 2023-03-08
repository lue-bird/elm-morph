module Sign exposing
    ( Sign, opposite
    , char, maybeMinusChar
    )

{-|

@docs Sign, opposite

@docs char, maybeMinusChar

-}

import Char.Morph
import Maybe.Morph
import Morph exposing (Morph, MorphRow, translate)
import Number exposing (Sign(..))
import String.Morph


{-| A number's sign
-}
type alias Sign =
    Number.Sign


{-| Flip the [`Sign`](#Sign) `Negative` ↔ `Positive`
-}
opposite : Sign -> Sign
opposite =
    \sign ->
        case sign of
            Negative ->
                Positive

            Positive ->
                Negative


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


{-| Am optional `'-'` sign → [`Negative`](#Sign),
else [narrows to](Morph#narrowTo) [`Positive`](#Sign)
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
