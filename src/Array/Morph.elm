module Array.Morph exposing
    ( each
    , list, string
    , value
    )

{-| [`Morph`](Morph) for an [`elm/core` `Array element`](https://dark.elm.dmy.fr/packages/elm/core/latest/Array#Array)


## alter

@docs each


## transform

@docs list, string
@docs value

-}

import Array exposing (Array)
import Morph exposing (MorphIndependently, MorphOrError)
import Stack
import Value
import Value.Morph exposing (MorphValue)
import Value.Morph.Internal exposing (MorphValue)


{-| [`Morph.OneToOne`](Morph#OneToOne) from a `List`

    import Array
    import Morph

    [ 0, 1, 2, 3 ]
        |> Morph.mapTo Array.Morph.list
    --> Array.fromList [ 0, 1, 2, 3 ]

[Inverse](Morph#invert) of [`List.Morph.array`](List-Morph#array)

-}
list :
    MorphIndependently
        (List narrowElement -> Result error_ (Array narrowElement))
        (Array broadElement -> List broadElement)
list =
    Morph.oneToOne Array.fromList Array.toList


{-| [`Morph.OneToOne`](Morph#OneToOne) from a `String` to `Array Char`

    import Array
    import Morph

    "0123"
        |> Morph.mapTo Array.Morph.string
    --> Array.fromList [ '0', '1', '2', '3' ]

[Inverse](Morph#invert) of [`String.Morph.array`](String-Morph#array)

-}
string : MorphOrError (Array Char) String error_
string =
    list
        |> Morph.overOneToOne (Morph.oneToOne String.toList String.fromList)



--


{-| `Array` [`MorphValue`](Value-Morph#MorphValue)
-}
value : MorphValue element -> MorphValue (Array element)
value elementMorph =
    each elementMorph
        |> Morph.over
            (Morph.custom "array"
                { toNarrow =
                    \broad ->
                        case broad of
                            Value.List listElements ->
                                listElements |> Array.fromList |> Ok

                            composedOther ->
                                composedOther |> Value.composedKindToString |> Err
                , toBroad = \array -> array |> Array.toList |> Value.List
                }
            )
        |> Morph.over Value.Morph.Internal.toComposed


{-| [`Morph`](Morph#Morph) all elements.
On the narrowing side all [narrowed](Morph#toNarrow) values must be `Ok`
for it to not result in a [`Morph.Error`](Morph#Error)

If the element [`Morph`](Morph#Morph) is [`OneToOne`](Morph#OneToOne),
`each` will always succeed with the type knowing it does

-}
each :
    MorphIndependently
        (beforeToNarrow
         -> Result Morph.Error narrow
        )
        (beforeToBroad -> broad)
    ->
        MorphIndependently
            (Array beforeToNarrow
             ->
                Result
                    Morph.Error
                    (Array narrow)
            )
            (Array beforeToBroad -> Array broad)
each elementMorph =
    Morph.named "each"
        { description =
            Morph.ElementsDescription (elementMorph |> Morph.description)
        , toNarrow =
            \array ->
                array
                    |> Array.foldr
                        (\element { index, collected } ->
                            { collected =
                                case element |> Morph.toNarrow elementMorph of
                                    Ok elementValue ->
                                        collected
                                            |> Result.map (\l -> l |> (::) elementValue)

                                    Err elementError ->
                                        let
                                            errorsSoFar =
                                                case collected of
                                                    Ok _ ->
                                                        []

                                                    Err elementsAtIndexes ->
                                                        elementsAtIndexes |> Stack.toList
                                        in
                                        ( { location = index |> String.fromInt
                                          , error = elementError
                                          }
                                        , errorsSoFar
                                        )
                                            |> Err
                            , index = index - 1
                            }
                        )
                        { collected = [] |> Ok
                        , index = (array |> Array.length) - 1
                        }
                    |> .collected
                    |> Result.map Array.fromList
                    |> Result.mapError Morph.ElementsError
        , toBroad =
            \array ->
                array |> Array.map (Morph.toBroad elementMorph)
        }
