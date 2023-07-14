module Dict.Morph exposing
    ( eachValue
    , list
    , value
    )

{-| [`Morph`](Morph#Morph) to and from a `Dict`


## alter

@docs eachValue


## transform

@docs list
@docs value

-}

import Dict exposing (Dict)
import List.Morph
import Morph exposing (ErrorWithDeadEnd, MorphIndependently)
import Value.Morph.Internal exposing (MorphValue)


{-| [`Morph.OneToOne`](Morph#OneToOne) from a `List { key : key, value : value }` to a `Dict key value`.

    import Dict
    import Morph

    [ { key = "git", value = "gud" }
    , { key = "Hi", value = "there" }
    ]
        |> Morph.mapTo Dict.Morph.list
    --> Dict.fromList [ ( "Hi", "there" ), ( "git", "gud" ) ]

[Inverse](Morph#invert) of [`List.Morph.dict`](List-Morph#dict)

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
    Morph.oneToOne
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



--


{-| [`Morph.OneToOne`](Morph#OneToOne) each value in a `Dict`
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
    Morph.oneToOneOn ( eachValueMap, eachValueMap ) entryValueTranslate


eachValueMap :
    (value -> valueMapped)
    -> (Dict key value -> Dict key valueMapped)
eachValueMap valueMap =
    Dict.map (\_ -> valueMap)



--


{-| `Dict` [`MorphValue`](Value-Morph#MorphValue)
-}
value :
    { key : MorphValue comparableKey
    , value : MorphValue value
    }
    -> MorphValue (Dict comparableKey value)
value entryMorphs =
    list
        |> Morph.over
            (List.Morph.value (keyValueValue entryMorphs))


keyValueValue :
    { key : MorphValue key
    , value : MorphValue value
    }
    -> MorphValue { key : key, value : value }
keyValueValue entryMorph =
    Value.Morph.Internal.group
        (\key value_ -> { key = key, value = value_ })
        |> Value.Morph.Internal.part ( .key, "key" ) entryMorph.key
        |> Value.Morph.Internal.part ( .value, "value" ) entryMorph.value
        |> Value.Morph.Internal.groupFinish
