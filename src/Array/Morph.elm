module Array.Morph exposing
    ( elementTranslate
    , list, toList
    )

{-| [`Morph`](Morph) an `Array`


## alter

@docs elementTranslate

Also try [`toggle`](Morph#toggle) [`Array.Extra.reverse`](https://dark.elm.dmy.fr/packages/elm-community/array-extra/latest/Array-Extra#reverse)


## transform

@docs list, toList

-}

import Array exposing (Array)
import Morph exposing (ErrorWithDeadEnd, Morph, MorphIndependently, Translate, translate, translateOn)


{-| [`Translate`](Morph#Translate) from `List` to `Array`

    import Array

    [ 0, 1, 2, 3 ]
        |> Morph.mapWith Array.Morph.list
    --> Array.fromList [ 0, 1, 2, 3 ]

-}
list :
    MorphIndependently
        (List narrowElement -> Result error_ (Array narrowElement))
        (Array broadElement -> List broadElement)
list =
    translate Array.fromList Array.toList


{-| [`Translate`](Morph#Translate) from `Array` to `List`

    import Array

    Array.fromList [ 0, 1, 2, 3 ]
        |> Morph.mapWith Array.Morph.list
    --> [ 0, 1, 2, 3 ]

-}
toList :
    MorphIndependently
        (Array narrowElement -> Result error_ (List narrowElement))
        (List element -> Array element)
toList =
    translate Array.toList Array.fromList



--


{-| [`Translate`](Morph#Translate) each element in an `Array`
-}
elementTranslate :
    MorphIndependently
        (beforeMapElement -> Result (ErrorWithDeadEnd Never) mappedElement)
        (beforeUnmapElement -> unmappedElement)
    ->
        MorphIndependently
            (Array beforeMapElement
             -> Result error_ (Array mappedElement)
            )
            (Array beforeUnmapElement -> Array unmappedElement)
elementTranslate elementTranslate_ =
    translateOn ( Array.map, Array.map ) elementTranslate_
