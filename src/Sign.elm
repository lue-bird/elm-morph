module Sign exposing
    ( Sign(..)
    , char, emptiableMinusChar
    )

{-|

@docs Sign

@docs char, emptiableMinusChar

-}

import Char.Morph
import Emptiable
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


{-| A possible `'-'` sign → [`Negative`](#Sign),
else [narrows to](Morph#narrowWith) [`Positive`](#Sign)
-}
emptiableMinusChar : MorphRow Char Sign
emptiableMinusChar =
    Morph.to "negation"
        (translate
            (\minusSymbol ->
                case minusSymbol of
                    Emptiable.Empty _ ->
                        Positive

                    Emptiable.Filled () ->
                        Negative
            )
            (\signNarrow ->
                case signNarrow of
                    Positive ->
                        Emptiable.empty

                    Negative ->
                        () |> Emptiable.filled
            )
            |> Morph.overRow
                (Morph.emptiable (String.Morph.only "-"))
        )
