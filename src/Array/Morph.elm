module Array.Morph exposing
    ( each
    , list, stack, arraySized, string
    , value
    )

{-| [`Morph`](Morph) an `Array`


## alter

@docs each


## transform

@docs list, stack, arraySized, string
@docs value

-}

import Array exposing (Array)
import ArraySized exposing (ArraySized)
import Emptiable exposing (Emptiable)
import Morph exposing (MorphIndependently, MorphOrError)
import N exposing (Min, Up0)
import Possibly exposing (Possibly(..))
import Stack exposing (Stacked)
import Value
import Value.Morph.Internal exposing (MorphValue)


{-| [`Morph.OneToOne`](Morph#OneToOne) from `List` to `Array`

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


{-| [`Morph.OneToOne`](Morph#OneToOne) from `List` to `Array`

    import Morph
    import Array
    import Stack

    [ 0, 1, 2, 3 ]
        |> Morph.mapTo Array.Morph.stack
    --> Array.fromList [ 0, 1, 2, 3 ]

[Inverse](Morph#invert) of [`Stack.Morph.array`](Stack-Morph#array)

-}
stack :
    MorphIndependently
        (Emptiable (Stacked narrowElement) possiblyOrNever_
         -> Result error_ (Array narrowElement)
        )
        (Array broadElement -> Emptiable (Stacked broadElement) Possibly)
stack =
    list
        |> Morph.over (Morph.oneToOne Stack.toList Stack.fromList)
        |> Morph.narrowErrorMap Morph.deadEndNever


{-| [`Morph.OneToOne`](Morph#OneToOne) from `String` to `Array Char`

    import Array

    "0123"
        |> Morph.mapTo Array.Morph.string
    --> Array.fromList [ 0, 1, 2, 3 ]

[Inverse](Morph#invert) of [`String.Morph.array`](String-Morph#array)

-}
string : MorphOrError (Array Char) String error_
string =
    list
        |> Morph.over (Morph.oneToOne String.toList String.fromList)
        |> Morph.narrowErrorMap Morph.deadEndNever


{-| [`Morph.OneToOne`](Morph#OneToOne) from `ArraySized` to `Array`

    import ArraySized
    import Array

    ArraySized.l4 0 1 2 3
        |> Morph.mapTo Array.Morph.arraySized
    --> Array.fromList [ 0, 1, 2, 3 ]

[Inverse](Morph#invert) of [`ArraySized.Morph.array`](ArraySized-Morph#array)

-}
arraySized :
    MorphIndependently
        (ArraySized narrowElement narrowRange_
         -> Result error_ (Array narrowElement)
        )
        (Array broadElement
         -> ArraySized broadElement (Min (Up0 broadX_))
        )
arraySized =
    Morph.oneToOne ArraySized.toArray ArraySized.fromArray



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
