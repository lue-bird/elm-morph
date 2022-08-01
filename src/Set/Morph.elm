module Set.Morph exposing
    ( fromList, toList
    , elementEach
    )

{-| [`Morph`](Morph) a `Set`


## `List`

@docs fromList, toList


## transform

Also available: [`toggle`](Morph#toggle) [`Set.Extra.reverse`](https://dark.elm.dmy.fr/packages/elm-community/array-extra/latest/Set-Extra#reverse)

@docs elementEach

-}

import Morph exposing (Morph, Translate, translate, translateOn)
import Set exposing (Set)


{-| [`Translate`](Morph#Translate) from `List` to `Set`.

    import Set

    [ 0, 1, 2, 3 ]
        |> (Set.Morph.fromList |> Morph.map)
    --> Set.fromList [ 0, 1, 2, 3 ]

-}
fromList : Morph (Set comparableElement) (List comparableElement) error_
fromList =
    translate Set.fromList Set.toList


{-| [`Translate`](Morph#Translate) from `Set` to `List`.

    import Set

    Set.fromList [ 0, 1, 2, 3 ]
        |> (Set.Morph.toList |> Morph.map)
    --> [ 0, 1, 2, 3 ]

-}
toList : Morph (List comparableElement) (Set comparableElement) error_
toList =
    translate Set.toList Set.fromList



--


{-| [`Translate`](Morph#Translate) each element in a `Set`.
-}
elementEach :
    Translate comparableMapped comparableUnmapped
    -> Morph (Set comparableMapped) (Set comparableUnmapped) error_
elementEach elementTranslate =
    translateOn ( Set.map, Set.map ) elementTranslate
