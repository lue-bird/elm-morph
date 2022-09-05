module Set.Morph exposing
    ( elementTranslate
    , list, toList
    )

{-| [`Morph`](Morph) a `Set`


## alter

@docs elementTranslate


## transform

@docs list, toList

-}

import Morph exposing (ErrorWithDeadEnd, Morph, MorphIndependently, Translate, translate, translateOn)
import Set exposing (Set)


{-| [`Translate`](Morph#Translate) from `List` to `Set`.

    import Set

    [ 0, 1, 2, 3 ]
        |> (Set.Morph.fromList |> Morph.map)
    --> Set.fromList [ 0, 1, 2, 3 ]

-}
list :
    MorphIndependently
        (List comparableNarrowElement -> Result error_ (Set comparableNarrowElement))
        (Set broadElement -> List broadElement)
list =
    translate Set.fromList Set.toList


{-| [`Translate`](Morph#Translate) from `Set` to `List`.

    import Set

    Set.fromList [ 0, 1, 2, 3 ]
        |> (Set.Morph.toList |> Morph.map)
    --> [ 0, 1, 2, 3 ]

-}
toList :
    MorphIndependently
        (Set narrowElement -> Result error_ (List narrowElement))
        (List comparableBroadElement -> Set comparableBroadElement)
toList =
    translate Set.toList Set.fromList



--


{-| [`Translate`](Morph#Translate) each element in a `Set`
-}
elementTranslate :
    MorphIndependently
        (comparableBeforeMapElement
         -> Result (ErrorWithDeadEnd Never) comparableMappedElement
        )
        (comparableBeforeUnmapElement -> comparableUnmappedElement)
    ->
        MorphIndependently
            (Set comparableBeforeMapElement
             -> Result error_ (Set comparableMappedElement)
            )
            (Set comparableBeforeUnmapElement
             -> Set comparableUnmappedElement
            )
elementTranslate elementTranslate_ =
    translateOn ( Set.map, Set.map ) elementTranslate_
