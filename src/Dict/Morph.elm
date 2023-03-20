module Dict.Morph exposing
    ( eachValue
    , list, toList
    , value
    )

{-| [`Morph`](Morph#Morph) to and from a `Dict`


## alter

@docs eachValue


## transform

@docs list, toList
@docs value

-}

import Dict exposing (Dict)
import List.Morph
import Morph exposing (ErrorWithDeadEnd, MorphIndependently, translate, translateOn)
import Value


{-| [`Translate`](Morph#Translate) from a `List { key : key, value : value }` to a `Dict key value`.

    import Dict
    import Dict.Morph
    import Morph

    [ { key = "hi", value = "there" }
    , { key = "git", value = "gud" }
    ]
        |> Morph.mapTo Dict.Morph.list
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
    translate
        (\dict ->
            dict
                |> List.foldl
                    (\entry -> Dict.insert entry.key entry.value)
                    Dict.empty
        )
        (\dict ->
            dict
                |> Dict.foldr
                    (\key value_ -> (::) { key = key, value = value_ })
                    []
        )


{-| [`Translate`](Morph#Translate) from a `Dict key value` to a `List { key : key, value : value }`.

    import Dict
    import Dict.Morph
    import Morph

    Dict.fromList [ ( 0, 'a' ), ( 1, 'b' ) ]
        |> Morph.mapTo Dict.Morph.toList
    --> [ { key = 0, value = 'a' }, { key = 1, value = 'b' } ]

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
    Morph.invert list



--


{-| [`Translate`](Morph#Translate) each value in a `Dict`
-}
eachValue :
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
eachValue entryValueTranslate =
    translateOn ( eachValueMap, eachValueMap ) entryValueTranslate


eachValueMap :
    (value -> valueMapped)
    -> (Dict key value -> Dict key valueMapped)
eachValueMap valueMap =
    Dict.map (\_ -> valueMap)



--


{-| `Dict` [`Value.Morph`](Value#Morph)
-}
value :
    { key : Value.Morph comparableKey
    , value : Value.Morph value
    }
    -> Value.Morph (Dict comparableKey value)
value entryMorphs =
    list
        |> Morph.over
            (List.Morph.value (keyValueValue entryMorphs))


keyValueValue :
    { key : Value.Morph key
    , value : Value.Morph value
    }
    -> Value.Morph { key : key, value : value }
keyValueValue entryMorph =
    Value.group
        (\key value_ -> { key = key, value = value_ })
        |> Value.part ( .key, "key" ) entryMorph.key
        |> Value.part ( .value, "value" ) entryMorph.value
        |> Value.groupFinish
