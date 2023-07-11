module Float.Morph exposing (decimalOrException)

{-| [`Morph`](Morph#Morph) an [`elm/core` `Float`](https://dark.elm.dmy.fr/packages/elm/core/latest/Basics#Float)

@docs decimalOrException

-}

import Decimal exposing (Decimal)
import Morph exposing (MorphOrError)


{-| [`Morph.OneToOne`](Morph#OneToOne) from a [`Decimal`](Decimal#Decimal) or [`Exception`](Decimal#Exception)
to a `Float`

Keep in mind that `Decimal -> Float` can be lossy
since `Float` is fixed in bit size while [`Decimal`](Decimal#Decimal) is not

[Inverse](Morph#invert) of [`Decimal.Morph.orExceptionFloat`](Decimal-Morph#orExceptionFloat)

-}
decimalOrException : MorphOrError Float (Result Decimal.Exception Decimal) error_
decimalOrException =
    Morph.oneToOne Decimal.orExceptionToFloat Decimal.fromFloat
