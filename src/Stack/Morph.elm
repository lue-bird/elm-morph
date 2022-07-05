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

import Hand exposing (Empty, Hand)
import Morph exposing (Morph, Translate, translate)
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
        (Hand (Stacked element) Possibly Empty)
        (List element)
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
        (List element)
        (Hand (Stacked element) Possibly Empty)
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
        (Hand (Stacked Char) Possibly Empty)
        String
        error_
toText =
    translate Stack.toString Stack.fromString


{-| [`Translate`](Morph#Translate) from `String` to a stack of `Char`s.

    import Stack

    "012" |> Morph.map Stack.Morph.fromText
    --> Stack.fromList [ '0', '1', '2' ]

-}
fromText :
    Morph
        String
        (Hand (Stacked Char) Possibly Empty)
        error_
fromText =
    translate Stack.fromString Stack.toString



--


{-| [`Translate`](Morph#Translate) each stacked element except the top one.
-}
belowTopEach :
    Translate elementNarrow elementBroad
    ->
        Morph
            (Hand (StackTopBelow top elementNarrow) possiblyOrNever Empty)
            (Hand (StackTopBelow top elementBroad) possiblyOrNever Empty)
            error_
belowTopEach elementMorph =
    translate
        (Stack.belowTopMap (\_ -> Morph.map elementMorph))
        (Stack.belowTopMap (\_ -> Morph.unmap elementMorph))
