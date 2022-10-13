module Array.Morph exposing
    ( eachElement
    , list, toList
    , value
    )

{-| [`Morph`](Morph) an `Array`


## alter

@docs eachElement

Also try [`toggle`](Morph#toggle) [`Array.Extra.reverse`](https://dark.elm.dmy.fr/packages/elm-community/array-extra/latest/Array-Extra#reverse)


## transform

@docs list, toList
@docs value

-}

import Array exposing (Array)
import Emptiable exposing (filled)
import Morph exposing (ErrorWithDeadEnd, Morph, MorphIndependently, Translate, translate, translateOn)
import Possibly exposing (Possibly(..))
import Stack
import Value exposing (MorphValue)
import Value.PackageInternal


{-| [`Translate`](Morph#Translate) from `List` to `Array`

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
    translate Array.fromList Array.toList


{-| [`Translate`](Morph#Translate) from `Array` to `List`

    import Array

    Array.fromList [ 0, 1, 2, 3 ]
        |> Morph.mapTo Array.Morph.list
    --> [ 0, 1, 2, 3 ]

-}
toList :
    MorphIndependently
        (Array narrowElement -> Result error_ (List narrowElement))
        (List element -> Array element)
toList =
    translate Array.toList Array.fromList



--


{-| `Array` [`MorphValue`](Value#MorphValue)
-}
value : MorphValue element -> MorphValue (Array element)
value elementMorph =
    eachElement elementMorph
        |> Morph.over
            (Morph.value "Array"
                { narrow =
                    \broad ->
                        case broad of
                            Value.Array arrayElements ->
                                arrayElements |> Ok

                            Value.List listElements ->
                                listElements |> Array.fromList |> Ok

                            structureExceptArrayAndList ->
                                structureExceptArrayAndList
                                    |> Value.PackageInternal.structureKindToString
                                    |> Err
                , broaden = Value.Array
                }
            )
        |> Morph.over Value.structure


{-| [`Morph`](Morph#Morph) all elements in sequence.
On the narrowing side all [narrowed](Morph#narrowTo) values must be `Ok`
for it to not result in a [`Morph.Error`](Morph#Error)

If the element [`Morph`](Morph#Morph) is a [`Translate`](Morph#Translate),
`eachElement` will be equivalent to

    Morph.translateOn ( Array.map, Array.map )

which always succeeds with the type knowing it does

-}
eachElement :
    MorphIndependently
        (beforeNarrow
         -> Result (Morph.ErrorWithDeadEnd deadEnd) narrow
        )
        (beforeBroaden -> broad)
    ->
        MorphIndependently
            (Array beforeNarrow
             ->
                Result
                    (Morph.ErrorWithDeadEnd deadEnd)
                    (Array narrow)
            )
            (Array beforeBroaden -> Array broad)
eachElement elementMorph =
    { description =
        { custom = Stack.only "each"
        , inner =
            Morph.Elements (elementMorph |> Morph.description)
                |> filled
        }
    , narrow =
        \array ->
            array
                |> Array.foldr
                    (\element { index, collected } ->
                        { collected =
                            case element |> Morph.narrowTo elementMorph of
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
                                            { index = index
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
                |> Result.mapError Morph.Parts
    , broaden =
        \array ->
            array |> Array.map (Morph.broadenFrom elementMorph)
    }
