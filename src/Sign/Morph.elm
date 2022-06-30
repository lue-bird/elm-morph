module Sign.Morph exposing (Sign(..), plusOrMinus, maybeMinus)

{-|

@docs Sign, plusOrMinus, maybeMinus

-}

import Morph exposing (Morph, translate)
import Morph.Text
import MorphRow exposing (MorphRow, maybe)


{-| A number's signum. Either positive or negative
-}
type Sign
    = Positive
    | Negative


{-| `'+'` or `'-'`.
-}
plusOrMinus : Morph Sign Char (Morph.Error Char description_)
plusOrMinus =
    Morph.choice
        (\plus minus signNarrow ->
            case signNarrow of
                Positive ->
                    plus ()

                Negative ->
                    minus ()
        )
        |> Morph.possibility (\() -> Positive) (Morph.specific '+')
        |> Morph.possibility (\() -> Negative) (Morph.specific '-')
        |> Morph.choiceFinish


{-| A possible negate sign. If none is found, `Positive` is returned.
-}
maybeMinus : MorphRow Char Sign expectedCustom_
maybeMinus =
    translate
        (\signNarrow ->
            case signNarrow of
                Positive ->
                    Nothing

                Negative ->
                    Just ()
        )
        (\minusSymbol ->
            case minusSymbol of
                Nothing ->
                    Positive

                Just () ->
                    Negative
        )
        |> MorphRow.over (maybe (Morph.Text.specific "-"))