module Stack.Morph exposing
    ( belowTopTranslate, topTranslate
    , list, toList
    , string, toString
    )

{-| [`Morph`](Morph#Morph) to and from a [`Stack`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Stack)


## alter

@docs reverse
@docs belowTopTranslate, topTranslate


## transform

@docs list, toList
@docs string, toString

-}

import Emptiable exposing (Emptiable)
import Morph exposing (ErrorWithDeadEnd, Morph, MorphIndependently, MorphOrError, Translate, translate, translateOn)
import Possibly exposing (Possibly)
import Set exposing (Set)
import Stack exposing (StackTopBelow, Stacked)



-- alter


reverse :
    MorphIndependently
        (List narrowElement
         -> Result error_ (List narrowElement)
        )
        (List broadElement -> List broadElement)
reverse =
    translate List.reverse List.reverse


{-| [`Translate`](Morph#Translate) each stacked element except the top one
-}
belowTopTranslate :
    MorphIndependently
        (beforeMapElement -> Result (ErrorWithDeadEnd Never) mappedElement)
        (beforeUnmapElement -> unmappedElement)
    ->
        MorphIndependently
            (Emptiable (StackTopBelow top beforeMapElement) possiblyOrNever
             ->
                Result
                    error_
                    (Emptiable (StackTopBelow top mappedElement) possiblyOrNever)
            )
            (Emptiable (StackTopBelow top beforeUnmapElement) possiblyOrNever
             -> Emptiable (StackTopBelow top unmappedElement) possiblyOrNever
            )
belowTopTranslate elementMorph =
    translateOn ( belowTopElementMap, belowTopElementMap ) elementMorph


belowTopElementMap :
    (belowElement -> belowElementMapped)
    ->
        (Emptiable (StackTopBelow top belowElement) possiblyOrNever
         -> Emptiable (StackTopBelow top belowElementMapped) possiblyOrNever
        )
belowTopElementMap elementMap =
    Stack.belowTopMap (\_ -> elementMap)


{-| [`Translate`](Morph#Translate) the top element
-}
topTranslate :
    MorphIndependently
        (beforeMapElement -> Result (ErrorWithDeadEnd Never) mappedElement)
        (beforeUnmapElement -> unmappedElement)
    ->
        MorphIndependently
            (Emptiable (StackTopBelow beforeMapElement narrowBelowTop) possiblyOrNever
             ->
                Result
                    error_
                    (Emptiable (StackTopBelow mappedElement narrowBelowTop) possiblyOrNever)
            )
            (Emptiable (StackTopBelow beforeUnmapElement broadBelowTop) possiblyOrNever
             -> Emptiable (StackTopBelow unmappedElement broadBelowTop) possiblyOrNever
            )
topTranslate elementMorph =
    translateOn ( Stack.topMap, Stack.topMap ) elementMorph



-- transform


{-| [`Translate`](Morph#Translate) from a stack to a `List`.

    import Stack

    Stack.topDown 0 [ 12, 3 ]
        |> Morph.map Stack.Morph.toList
    --> [ 0, 12, 3 ]

-}
toList :
    MorphIndependently
        (Emptiable (Stacked broadElement) Possibly
         -> Result error_ (List broadElement)
        )
        (List narrowElement
         -> Emptiable (Stacked narrowElement) Possibly
        )
toList =
    translate Stack.toList Stack.fromList


{-| [`Translate`](Morph#Translate) from `List` to a stack.

    import Stack

    [ 0, 12, 3 ]
        |> Morph.mapWith Stack.Morph.list
    --> Stack.topDown 0 [ 12, 3 ]

-}
list :
    MorphIndependently
        (List broadElement
         ->
            Result
                error_
                (Emptiable (Stacked broadElement) Possibly)
        )
        (Emptiable (Stacked narrowElement) Possibly
         -> List narrowElement
        )
list =
    translate Stack.fromList Stack.toList


{-| [`Translate`](Morph#Translate) from a stack of `Char`s to a `String`.

    import Stack

    Stack.topDown '0' [ '1', '2' ]
        |> Morph.map Stack.Morph.toString
    --> "012"

-}
toString :
    MorphOrError
        String
        (Emptiable (Stacked Char) Possibly)
        error_
toString =
    translate Stack.toText Stack.fromText


{-| [`Translate`](Morph#Translate) from `String` to a stack of `Char`s.

    import Stack

    "012" |> Morph.mapWith Stack.Morph.string
    --> Stack.fromList [ '0', '1', '2' ]

-}
string :
    MorphOrError
        (Emptiable (Stacked Char) Possibly)
        String
        error_
string =
    translate Stack.fromText Stack.toText
