module List.Morph exposing (elementEach)

{-| [`Morph`](Morph#Morph) to and from a `List`


## transform

Also available: [`toggle`](Morph#toggle) `List.reverse`

@docs elementEach

-}

import Morph exposing (Morph, Translate, translate, translateOn)


{-| [`Translate`](Morph#Translate) each element in a `List`.
-}
elementEach :
    Translate unmapped mapped
    -> Morph (List unmapped) (List mapped) error_
elementEach elementTranslate =
    ( List.map, List.map )
        |> translateOn elementTranslate
