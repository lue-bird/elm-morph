module Json exposing
    ( Json, Atom(..), Composed(..), Tagged
    , tagMap
    )

{-| JSON. See [`Json.Morph`](Json-Morph) for conversions

@docs Json, Atom, Composed, Tagged
@docs tagMap

-}

import Array
import Decimal exposing (Decimal)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Value exposing (AtomOrComposed(..))


{-| A valid JSON value. `case`able. Elm doesn't crash on `==`.
Can't contain any [spooky impure stuff](Json-Morph#JsValueMagic)
-}
type alias Json tag =
    AtomOrComposed Atom (Composed tag)


{-| json atom. null, bool, number, string
-}
type Atom
    = Null ()
    | Bool Bool
    | Number Decimal
    | String String


{-| json structure. record/object/dict or array
-}
type Composed tag
    = Array (Array.Array (Json tag))
    | Object (List (Tagged tag))


{-| tag-[value](#Json) pair used to represent a field
-}
type alias Tagged tag =
    RecordWithoutConstructorFunction
        { tag : tag, value : Json tag }


{-| Reduce the amount of tag information.
Used to make its representation [`compact`](Value-Morph#compact) or [`descriptive`](Value-Morph#descriptive)
-}
tagMap : (tag -> tagMapped) -> (Json tag -> Json tagMapped)
tagMap tagChange =
    \json ->
        json |> Value.composedMap (composedTagMap tagChange)


composedTagMap :
    (tag -> tagMapped)
    -> (Composed tag -> Composed tagMapped)
composedTagMap tagChange =
    \composed ->
        case composed of
            Array array ->
                array |> Array.map (tagMap tagChange) |> Array

            Object object ->
                object |> List.map (taggedTagMap tagChange) |> Object


taggedTagMap : (tag -> tagMapped) -> (Tagged tag -> Tagged tagMapped)
taggedTagMap tagChange =
    \tagged ->
        { tag = tagged.tag |> tagChange
        , value = tagged.value |> tagMap tagChange
        }
