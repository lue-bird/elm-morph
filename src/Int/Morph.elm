module Int.Morph exposing (integer)

{-| [`Morph`](Morph#Morph) for an [`elm/core` `Int`](https://dark.elm.dmy.fr/packages/elm/core/latest/Basics#Int)

@docs integer

-}

import Integer exposing (Integer)
import Morph exposing (MorphOrError)


{-| [`Morph.OneToOne`](Morph#OneToOne) between a [decimal representation](Integer#Integer)
and an `Int`.

Keep in mind that `Integer -> Int` can overflow
since `Int` is fixed in bit size while [`Integer`](Integer#Integer) is not.

[Inverse](Morph#invert) of [`Integer.Morph.int`](Integer-Morph#int)

-}
integer : MorphOrError Int Integer error_
integer =
    Morph.oneToOne Integer.toInt Integer.fromInt
