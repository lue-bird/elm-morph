module Set.Morph exposing
    ( each
    , list, toList
    , value
    )

{-| [`Morph`](Morph) a `Set`


## alter

@docs each


## transform

@docs list, toList
@docs value

-}

import Emptiable
import List.Morph
import Morph exposing (MorphIndependently, translate)
import Possibly exposing (Possibly(..))
import Set exposing (Set)
import Stack
import StructureMorph
import Value


{-| [`Translate`](Morph#Translate) from `List` to `Set`

    import Set

    [ 0, 1, 2, 3 ]
        |> Morph.mapTo Set.Morph.list
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


{-| [`Translate`](Morph#Translate) from `Set` to `List`

    import Set

    Set.fromList [ 0, 1, 2, 3 ]
        |> Morph.mapTo Set.Morph.toList
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
On the narrowing side all [narrowed](Morph#toNarrow) values must be `Ok`
for it to not result in a [`Morph.Error`](Morph#Error)

If the given element [`Morph`](Morph#Morph) is a [`Translate`](Morph#Translate),
`each` will always succeed with the type knowing it does

-}
each :
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
each elementMorph =
    StructureMorph.for "each" morphEachElement
        |> StructureMorph.add elementMorph
        |> StructureMorph.finish


morphEachElement :
    MorphIndependently
        (comparableBeforeNarrow
         -> Result (Morph.ErrorWithDeadEnd deadEnd) comparableNarrow
        )
        (comparableBeforeBroaden -> comparableBroad)
    ->
        { narrow :
            Set comparableBeforeNarrow
            ->
                Result
                    (Morph.ErrorWithDeadEnd deadEnd)
                    (Set comparableNarrow)
        , broaden : Set comparableBeforeBroaden -> Set comparableBroad
        }
morphEachElement elementMorph =
    { narrow =
        \set ->
            set
                |> Set.foldl
                    (\element { index, collected } ->
                        { collected =
                            case element |> Morph.toNarrow elementMorph of
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
                |> Result.mapError Morph.GroupError
    , broaden =
        \set ->
            set |> Set.map (Morph.toBroad elementMorph)
    }



--


{-| `Set` [`Value.Morph`](Value#Morph)
-}
value :
    Value.Morph comparableElement
    -> Value.Morph (Set comparableElement)
value elementMorph =
    list |> Morph.over (List.Morph.value elementMorph)
