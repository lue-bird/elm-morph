module Dict.Morph exposing
    ( fromList, toList
    , valueEach
    )

{-| [`Morph`](Morph#Morph) to and from a `Dict`


## `List`

@docs fromList, toList


## each

@docs valueEach

-}

import Dict exposing (Dict)
import Morph exposing (Morph, Translate, translate, translateOn)


fromListImplementation :
    List { key : comparableKey, value : value }
    -> Dict comparableKey value
fromListImplementation =
    List.foldl
        (\entry -> Dict.insert entry.key entry.value)
        Dict.empty


toListImplementation : Dict key value -> List { key : key, value : value }
toListImplementation =
    Dict.foldr
        (\key value -> (::) { key = key, value = value })
        []



--


{-| [`Translate`](Morph#Translate) from a `List { key : key, value : value }` to a `Dict key value`.

    import Array

    [ 0, 1, 2, 3 ]
        |> (Morph.listToArray |> Morph.map)
    --> Array.fromList [ 0, 1, 2, 3 ]

-}
fromList :
    Morph
        (Dict comparableKey value)
        (List { key : comparableKey, value : value })
        error_
fromList =
    translate fromListImplementation toListImplementation


{-| [`Translate`](Morph#Translate) from a `Dict key value` to a `List { key : key, value : value }`.

    import Array

    Array.fromList [ 0, 1, 2, 3 ]
        |> (Morph.arrayToList |> Morph.map)
    --> [ 0, 1, 2, 3 ]

-}
toList :
    Morph
        (List { key : comparableKey, value : value })
        (Dict comparableKey value)
        error_
toList =
    translate toListImplementation fromListImplementation



--


{-| [`Translate`](Morph#Translate) each key in a `Dict`.
-}
valueEach :
    Translate unmappedValue mappedValue
    -> Morph (Dict key unmappedValue) (Dict key mappedValue) error_
valueEach elementTranslate =
    translateOn ( valuesMap, valuesMap ) elementTranslate


valuesMap :
    (value -> valueMapped)
    -> (Dict key value -> Dict key valueMapped)
valuesMap valueMap =
    Dict.map (\_ -> valueMap)
