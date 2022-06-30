module List.Morph exposing (elementEach)

{-| [`Morph`](Morph#Morph) to and from a `List`

@docs elementEach

-}

import Morph exposing (Morph, Translate, translate)


{-| [`Translate`](Morph#Translate) each element in a `List`.
-}
elementEach :
    Translate unmapped mapped
    -> Morph (List unmapped) (List mapped) error_
elementEach elementTranslate =
    translate
        (List.map (Morph.map elementTranslate))
        (List.map (Morph.unmap elementTranslate))
