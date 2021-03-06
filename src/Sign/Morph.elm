module Sign.Morph exposing
    ( Sign(..), plusOrMinus, maybeMinus
    , Signable(..)
    )

{-|

@docs Sign, plusOrMinus, maybeMinus
@docs Signable

-}

import Emptiable exposing (filled)
import Morph exposing (Morph, translate)
import Morph.Text
import MorphRow exposing (MorphRow, maybe)


{-| A number's signum. Either positive or negative
-}
type Sign
    = Positive
    | Negative


{-| `0` or a signed number.
-}
type Signable signedNumber
    = N0
    | Signed { signedNumber | sign : Sign }


{-| `'+'` or `'-'`.
-}
plusOrMinus : Morph Sign Char (Morph.Error Char)
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
maybeMinus : MorphRow Char Sign
maybeMinus =
    translate
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
                    () |> filled
        )
        |> MorphRow.over
            (maybe (Morph.Text.specific "-"))
