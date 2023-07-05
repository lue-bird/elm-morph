module Array.Morph exposing
    ( each
    , list, toList
    , value
    )

{-| [`Morph`](Morph) an `Array`


## alter

@docs each


## transform

@docs list, toList
@docs value

-}

import Array exposing (Array)
import Emptiable
import Morph exposing (MorphIndependently, oneToOne)
import Possibly exposing (Possibly(..))
import Stack
import Value
import Value.Morph.Internal exposing (MorphValue)


{-| [`Morph.OneToOne`](Morph#OneToOne) from `List` to `Array`

    import Array

    [ 0, 1, 2, 3 ]
        |> Morph.mapTo Array.Morph.list
    --> Array.fromList [ 0, 1, 2, 3 ]

-}
list :
    MorphIndependently
        (List narrowElement -> Result error_ (Array narrowElement))
        (Array broadElement -> List broadElement)
list =
    oneToOne Array.fromList Array.toList


{-| [`Morph.OneToOne`](Morph#OneToOne) from `Array` to `List`

    import Array

    Array.fromList [ 0, 1, 2, 3 ]
        |> Morph.mapTo Array.Morph.toList
    --> [ 0, 1, 2, 3 ]

-}
toList :
    MorphIndependently
        (Array narrowElement -> Result error_ (List narrowElement))
        (List element -> Array element)
toList =
    oneToOne Array.toList Array.fromList



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
        |> Morph.over Value.Morph.Internal.composed


{-| [`Morph`](Morph#Morph) all elements.
On the narrowing side all [narrowed](Morph#toNarrow) values must be `Ok`
for it to not result in a [`Morph.Error`](Morph#Error)

If the element [`Morph`](Morph#Morph) is [`OneToOne`](Morph#OneToOne),
`each` will always succeed with the type knowing it does

-}
each :
    MorphIndependently
        (beforeToNarrow
         -> Result (Morph.ErrorWithDeadEnd deadEnd) narrow
        )
        (beforeToBroad -> broad)
    ->
        MorphIndependently
            (Array beforeToNarrow
             ->
                Result
                    (Morph.ErrorWithDeadEnd deadEnd)
                    (Array narrow)
            )
            (Array beforeToBroad -> Array broad)
each elementMorph =
    Morph.named "all"
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
                                                        Emptiable.empty

                                                    Err elementsAtIndexes ->
                                                        elementsAtIndexes |> Emptiable.emptyAdapt (\_ -> Possible)
                                        in
                                        errorsSoFar
                                            |> Stack.onTopLay
                                                { location = index |> String.fromInt
                                                , error = elementError
                                                }
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
