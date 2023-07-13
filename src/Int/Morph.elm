module Int.Morph exposing (integer)

{-| `Int` [`Morph`](Morph#Morph)s

@docs integer

-}

import Integer exposing (Integer)
import Morph exposing (MorphOrError)


{-| [`Morph.OneToOne`](Morph#OneToOne) between a [decimal representation](Integer#Integer)
and an `Int`.

Keep in mind that `Integer -> Int` can overflow
since `Int` is fixed in bit size while [`Integer`](Integer#Integer) is not.

[Inverse] of [`Integer.Morph.int`](Integer-Morph#int)

-}
integer : MorphOrError Int Integer error_
integer =
    Morph.oneToOne Integer.toInt Integer.fromInt
