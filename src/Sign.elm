module Sign exposing (Sign(..), opposite)

{-| Number sign. See also [`Sign.Morph`](Sign-Morph)

@docs Sign, opposite

-}


{-| A number's sign

  - minus-sign: `Negative`
  - plus-sign: `Positive`

-}
type Sign
    = Negative
    | Positive


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
