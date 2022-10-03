module Dict.Morph exposing
    ( valueTranslate
    , list, toList
    , value
    )

{-| [`Morph`](Morph#Morph) to and from a `Dict`


## alter

@docs valueTranslate


## transform

@docs list, toList
@docs value

-}

import Dict exposing (Dict)
import Morph exposing (ErrorWithDeadEnd, Morph, MorphIndependently, Translate, translate, translateOn)
import Value exposing (MorphValue)


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

    [ { key = "hi", value = "there" }
    , { key = "git", value = "gud" }
    ]
        |> Morph.mapWith Dict.Morph.list
    --> Dict.fromList [ ( "Hi", "there" ), ( "git", "gud" ) ]

-}
list :
    MorphIndependently
        (List { key : comparableNarrowKey, value : narrowValue }
         -> Result error_ (Dict comparableNarrowKey narrowValue)
        )
        (Dict broadKey broadValue
         -> List { key : broadKey, value : broadValue }
        )
list =
    translate fromListImplementation toListImplementation


{-| [`Translate`](Morph#Translate) from a `Dict key value` to a `List { key : key, value : value }`.

    import Array

    Array.fromList [ 0, 1, 2, 3 ]
        |> (Morph.arrayToList |> Morph.map)
    --> [ 0, 1, 2, 3 ]

-}
toList :
    MorphIndependently
        (Dict broadKey broadValue
         -> Result error_ (List { key : broadKey, value : broadValue })
        )
        (List { key : comparableNarrowKey, value : narrowValue }
         -> Dict comparableNarrowKey narrowValue
        )
toList =
    translate toListImplementation fromListImplementation



--


{-| [`Translate`](Morph#Translate) each key in a `Dict`
-}
valueTranslate :
    MorphIndependently
        (beforeMapValue -> Result (ErrorWithDeadEnd Never) mappedValue)
        (beforeUnmapValue -> unmappedValue)
    ->
        MorphIndependently
            (Dict narrowKey beforeMapValue
             -> Result error_ (Dict narrowKey mappedValue)
            )
            (Dict broadKey beforeUnmapValue
             -> Dict broadKey unmappedValue
            )
valueTranslate entryValueTranslate =
    translateOn ( valuesMap, valuesMap ) entryValueTranslate


valuesMap :
    (value -> valueMapped)
    -> (Dict key value -> Dict key valueMapped)
valuesMap valueMap =
    Dict.map (\_ -> valueMap)



--


{-| `Dict` [`MorphValue`](Value#MorphValue)
-}
value :
    { key : MorphValue comparableKey
    , value : MorphValue value
    }
    -> MorphValue (Dict comparableKey value)
value entryMorphs =
    Dict.Morph.list
        |> Morph.over (list (keyValueValue entryMorphs))


keyValueValue :
    { key : MorphValue key
    , value : MorphValue value
    }
    -> MorphValue { key : key, value : value }
keyValueValue entryMorph =
    Value.record
        (\key value -> { key = key, value = value })
        |> Value.field ( .key, "key" ) entryMorph.key
        |> Value.field ( .value, "value" ) entryMorph.value
        |> Value.recordFinish
