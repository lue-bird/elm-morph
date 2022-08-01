module Stack.Morph exposing
    ( fromList, toList
    , toText, fromText
    , belowTopEach
    )

{-| [`Morph`](Morph#Morph) to and from a [`Stack`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Stack)


## `List`

@docs fromList, toList


## text

@docs toText, fromText


## transform

Also available: [`toggle`](Morph#toggle) `Stack.reverse`

@docs belowTopEach

-}

import Emptiable exposing (Emptiable)
import Morph exposing (Morph, Translate, translate, translateOn)
import Possibly exposing (Possibly)
import Stack exposing (StackTopBelow, Stacked)


{-| [`Translate`](Morph#Translate) from a stack to a `List`.

    import Stack

    Stack.topDown 0 [ 12, 3 ]
        |> Morph.map Stack.Morph.toList
    --> [ 0, 12, 3 ]

-}
toList :
    Morph
        (List element)
        (Emptiable (Stacked element) Possibly)
        error_
toList =
    translate Stack.toList Stack.fromList


{-| [`Translate`](Morph#Translate) from `List` to a stack.

    import Stack

    [ 0, 12, 3 ]
        |> Morph.map Stack.Morph.fromList
    --> Stack.topDown 0 [ 12, 3 ]

-}
fromList :
    Morph
        (Emptiable (Stacked element) Possibly)
        (List element)
        error_
fromList =
    translate Stack.fromList Stack.toList


{-| [`Translate`](Morph#Translate) from a stack of `Char`s to a `String`.

    import Stack

    Stack.topDown '0' [ '1', '2' ]
        |> Morph.map Stack.Morph.toString
    --> "012"

-}
toText :
    Morph
        String
        (Emptiable (Stacked Char) Possibly)
        error_
toText =
    translate Stack.toText Stack.fromText


{-| [`Translate`](Morph#Translate) from `String` to a stack of `Char`s.

    import Stack

    "012" |> Morph.map Stack.Morph.fromText
    --> Stack.fromList [ '0', '1', '2' ]

-}
fromText :
    Morph
        (Emptiable (Stacked Char) Possibly)
        String
        error_
fromText =
    translate Stack.fromText Stack.toText



--


{-| [`Translate`](Morph#Translate) each stacked element except the top one.
-}
belowTopEach :
    Translate elementNarrow elementBroad
    ->
        Morph
            (Emptiable (StackTopBelow top elementNarrow) possiblyOrNever)
            (Emptiable (StackTopBelow top elementBroad) possiblyOrNever)
            error_
belowTopEach elementMorph =
    translateOn ( belowTopElementMap, belowTopElementMap ) elementMorph


belowTopElementMap elementMap =
    Stack.belowTopMap (\_ -> elementMap)
