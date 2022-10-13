module Sign exposing
    ( Sign(..)
    , char, maybeMinusChar
    )

{-|

@docs Sign

@docs char, maybeMinusChar

-}

import Char.Morph
import Choice
import Emptiable
import Maybe.Morph
import Morph exposing (Morph, MorphRow, translate)
import String.Morph


{-| A number's sign
-}
type Sign
    = Negative
    | Positive


{-| [`Sign`](#Sign) `'+'` or `'-'`
-}
char : Morph Sign Char
char =
    Morph.to "sign"
        (Choice.between
            (\plus minus signNarrow ->
                case signNarrow of
                    Positive ->
                        plus ()

                    Negative ->
                        minus ()
            )
            |> Choice.try (\() -> Positive) (Char.Morph.only '+')
            |> Choice.try (\() -> Negative) (Char.Morph.only '-')
            |> Choice.finish
        )


{-| Am optional `'-'` sign → [`Negative`](#Sign),
else [narrows to](Morph#narrowTo) [`Positive`](#Sign)
-}
maybeMinusChar : MorphRow Char Sign
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
