module Int.Morph exposing (integer)

{-| `Int` [`Morph`](Morph#Morph)s

@docs integer

-}

import Integer exposing (Integer)
import Morph exposing (MorphOrError)


integer : MorphOrError Int Integer error_
integer =
    Morph.oneToOne Integer.toInt Integer.fromInt
