module Json exposing
    ( Json, Atom(..), Composed(..), Tagged
    , fromValue
    , toValue
    )

{-| JSON. See also [`Json.Morph`](Json-Morph)

@docs Json, Atom, Composed, Tagged


## create

@docs fromValue


## transform

@docs toValue

-}

import Array
import Decimal exposing (Decimal)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Value exposing (AtomOrComposed(..))


{-| A valid JSON value. `case`able. Elm doesn't crash on `==`.
Can't contain any [spooky impure stuff](Json-Morph#JsValueMagic)
-}
type alias Json =
    AtomOrComposed Atom Composed


{-| json atom. null/bool/[number](Decimal#Decimal) or string
-}
type Atom
    = Null ()
    | Bool Bool
    | Number Decimal
    | String String


{-| json structure. record/object/dict or array
-}
type Composed
    = Array (Array.Array Json)
    | Object (List Tagged)


{-| tag-[value](#Json) pair used to represent a field
-}
type alias Tagged =
    RecordWithoutConstructorFunction
        { tag : String, value : Json }


{-| Convert from a [generic representation of an elm value](Value#Value)
-}
fromValue : Value.Value String -> Json
fromValue =
    \json ->
        case json of
            Atom atom ->
                atom |> atomFromValue |> Atom

            Composed composed ->
                composed |> composedFromValue |> Composed


atomFromValue : Value.Atom -> Atom
atomFromValue =
    \atom ->
        case atom of
            Value.Unit () ->
                Null ()

            Value.String stringAtom ->
                stringAtom |> String

            Value.Number decimal ->
                decimal |> Number


composedFromValue : Value.Composed String -> Composed
composedFromValue =
    \composed ->
        case composed of
            Value.List list ->
                list |> List.map fromValue |> Array.fromList |> Array

            Value.Record record ->
                record
                    |> List.map
                        (\field ->
                            { tag = field.tag
                            , value = field.value |> fromValue
                            }
                        )
                    |> Object

            Value.Variant variant ->
                { tag = variant.tag, value = variant.value |> fromValue }
                    |> List.singleton
                    |> Object


{-| Convert to a [generic representation of an elm value](Value#Value)
-}
toValue : Json -> Value.Value String
toValue =
    \json ->
        case json of
            Atom atom ->
                atom |> atomToValue

            Composed composed ->
                composed |> composedToValue |> Composed


atomToValue : Atom -> Value.Value String
atomToValue =
    \atom ->
        case atom of
            Null unit ->
                unit |> Value.Unit |> Atom

            Number decimal ->
                decimal |> Value.Number |> Atom

            String string_ ->
                string_ |> Value.String |> Atom

            Bool isTrue ->
                { value = () |> Value.Unit |> Atom
                , tag =
                    if isTrue then
                        "true"

                    else
                        "false"
                }
                    |> Value.Variant
                    |> Composed


composedToValue : Composed -> Value.Composed String
composedToValue =
    \composed ->
        case composed of
            Array array ->
                array |> Array.toList |> List.map toValue |> Value.List

            Object object ->
                object
                    |> List.map
                        (\tagged ->
                            { tag = tagged.tag
                            , value = tagged.value |> toValue
                            }
                        )
                    |> Value.Record
