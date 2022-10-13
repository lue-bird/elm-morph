module Set.Morph exposing
    ( eachElement
    , list, toList
    , value
    )

{-| [`Morph`](Morph) a `Set`


## alter

@docs eachElement


## transform

@docs list, toList
@docs value

-}

import Emptiable exposing (filled)
import List.Morph
import Morph exposing (ErrorWithDeadEnd, Morph, MorphIndependently, Translate, translate, translateOn)
import Possibly exposing (Possibly(..))
import Set exposing (Set)
import Stack
import Value exposing (MorphValue)


{-| [`Translate`](Morph#Translate) from `List` to `Set`.

    import Set

    [ 0, 1, 2, 3 ]
        |> (Set.Morph.fromList |> Morph.map)
    --> Set.fromList [ 0, 1, 2, 3 ]

-}
list :
    MorphIndependently
        (List comparableNarrowElement
         -> Result error_ (Set comparableNarrowElement)
        )
        (Set broadElement -> List broadElement)
list =
    translate Set.fromList Set.toList


{-| [`Translate`](Morph#Translate) from `Set` to `List`.

    import Set

    Set.fromList [ 0, 1, 2, 3 ]
        |> (Set.Morph.toList |> Morph.map)
    --> [ 0, 1, 2, 3 ]

-}
toList :
    MorphIndependently
        (Set narrowElement
         -> Result error_ (List narrowElement)
        )
        (List comparableBroadElement
         -> Set comparableBroadElement
        )
toList =
    translate Set.toList Set.fromList



--


{-| [`Morph`](Morph#Morph) all elements in sequence.
On the narrowing side all [narrowed](Morph#narrowTo) values must be `Ok`
for it to not result in a [`Morph.Error`](Morph#Error)

If the given element [`Morph`](Morph#Morph) is a [`Translate`](Morph#Translate),
`eachElement` will be equivalent to

    Morph.translateOn ( Set.map, Set.map )

which always succeeds with the type knowing it does

-}
eachElement :
    MorphIndependently
        (comparableBeforeNarrow
         -> Result (Morph.ErrorWithDeadEnd deadEnd) comparableNarrow
        )
        (comparableBeforeBroaden -> comparableBroad)
    ->
        MorphIndependently
            (Set comparableBeforeNarrow
             ->
                Result
                    (Morph.ErrorWithDeadEnd deadEnd)
                    (Set comparableNarrow)
            )
            (Set comparableBeforeBroaden -> Set comparableBroad)
eachElement elementMorph =
    { description =
        { custom = Stack.only "each"
        , inner =
            Morph.Elements (elementMorph |> Morph.description)
                |> filled
        }
    , narrow =
        \set ->
            set
                |> Set.foldl
                    (\element { index, collected } ->
                        { collected =
                            case element |> Morph.narrowTo elementMorph of
                                Ok elementValue ->
                                    collected
                                        |> Result.map (\l -> l |> Set.insert elementValue)

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
                    { collected = Set.empty |> Ok
                    , index = (set |> Set.size) - 1
                    }
                |> .collected
                |> Result.mapError Morph.Parts
    , broaden =
        \set ->
            set |> Set.map (Morph.broadenFrom elementMorph)
    }



--


{-| `Set` [`MorphValue`](Value#MorphValue)
-}
value :
    MorphValue comparableElement
    -> MorphValue (Set comparableElement)
value elementMorph =
    list |> Morph.over (List.Morph.value elementMorph)
