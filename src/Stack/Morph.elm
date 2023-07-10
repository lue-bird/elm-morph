module Stack.Morph exposing
    ( each
    , list, array, arraySized, string
    )

{-| [`Morph`](Morph#Morph) a [`Stack`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Stack)


## alter

@docs each


## transform

@docs list, array, arraySized, string

-}

import Array exposing (Array)
import ArraySized exposing (ArraySized)
import Emptiable exposing (Emptiable, filled)
import Linear exposing (Direction(..))
import Morph exposing (MorphIndependently, MorphOrError)
import N exposing (In, Min, N0, N0OrAdd1, On)
import Possibly exposing (Possibly(..))
import Stack exposing (Stacked)



-- alter


{-| [`Morph`](Morph#Morph) all stacked elements

If the given [`Morph`](Morph#Morph) is [`OneToOne`](Morph#OneToOne),
[`each`](#each) will also be a [`OneToOne`](Morph#OneToOne)

-}
each :
    MorphIndependently
        (beforeToNarrow
         -> Result (Morph.ErrorWithDeadEnd deadEnd) narrow
        )
        (beforeToBroad -> broad)
    ->
        MorphIndependently
            (Emptiable (Stacked beforeToNarrow) broadEmptiablePossiblyOrNever
             ->
                Result
                    (Morph.ErrorWithDeadEnd deadEnd)
                    (Emptiable (Stacked narrow) broadEmptiablePossiblyOrNever)
            )
            (Emptiable (Stacked beforeToBroad) narrowPossiblyOrNever
             -> Emptiable (Stacked broad) narrowPossiblyOrNever
            )
each elementMorph =
    Morph.named "all"
        { description =
            Morph.ElementsDescription (elementMorph |> Morph.description)
        , toNarrow =
            \stack ->
                case stack of
                    Emptiable.Empty emptyPossiblyOrNever ->
                        Emptiable.Empty emptyPossiblyOrNever
                            |> Ok

                    Emptiable.Filled stacked ->
                        let
                            reversed =
                                stacked |> filled |> Stack.reverse

                            lastIndex =
                                (reversed |> Stack.length) - 1
                        in
                        reversed
                            |> Stack.removeTop
                            |> Stack.foldFrom
                                { collected =
                                    case reversed |> Stack.top |> Morph.toNarrow elementMorph of
                                        Err topError ->
                                            { index = lastIndex, error = topError }
                                                |> Stack.one
                                                |> Err

                                        Ok topNarrow ->
                                            topNarrow
                                                |> Stack.one
                                                |> Ok
                                , index = lastIndex
                                }
                                Up
                                (\element { index, collected } ->
                                    { collected =
                                        case element |> Morph.toNarrow elementMorph of
                                            Ok elementValue ->
                                                collected
                                                    |> Result.map
                                                        (\l -> l |> Stack.onTopLay elementValue)

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
                            |> .collected
                            |> Result.mapError Morph.PartsError
        , toBroad =
            Stack.map (\_ -> Morph.toBroad elementMorph)
        }



-- transform


{-| [`Morph.OneToOne`](Morph#OneToOne) from `List` to a stack.

    import Stack
    import Morph

    [ 0, 12, 3 ]
        |> Morph.mapTo Stack.Morph.list
    --> Stack.topBelow 0 [ 12, 3 ]
    --: Emptiable (Stacked Int) Possibly

[Inverse](Morph#invert) of [`List.Morph.stack`](List-Morph#stack)

-}
list :
    MorphIndependently
        (List broadElement
         ->
            Result
                error_
                (Emptiable (Stacked broadElement) Possibly)
        )
        (Emptiable (Stacked narrowElement) Possibly
         -> List narrowElement
        )
list =
    Morph.oneToOne Stack.fromList Stack.toList


{-| [`Morph.OneToOne`](Morph#OneToOne) from `List` to a stack.

    import Stack
    import Array
    import Morph

    Array.fromList [ 0, 12, 3 ]
        |> Morph.mapTo Stack.Morph.array
    --> Stack.topBelow 0 [ 12, 3 ]
    --: Emptiable (Stacked Int) Possibly

[Inverse](Morph#invert) of [`Array.Morph.stack`](Array-Morph#stack)

-}
array :
    MorphIndependently
        (Array broadElement
         ->
            Result
                error_
                (Emptiable (Stacked broadElement) Possibly)
        )
        (Emptiable (Stacked narrowElement) Possibly
         -> Array narrowElement
        )
array =
    list
        |> Morph.over (Morph.oneToOne Array.toList Array.fromList)
        |> Morph.narrowErrorMap Morph.deadEndNever


{-| [`Morph.OneToOne`](Morph#OneToOne) from `ArraySized` to `Emptiable (Stacked ...) ...`

    import ArraySized
    import Morph
    import Stack

    ArraySized.l4 0 1 2 3
        |> Morph.mapTo Stack.Morph.arraySized
    --> Stack.topBelow 0 [ 1, 2, 3 ]

[Inverse](Morph#invert) of [`ArraySized.Morph.stack`](ArraySized-Morph#stack)

-}
arraySized :
    MorphIndependently
        (ArraySized narrowElement (In (On (N0OrAdd1 narrowPossiblyOrNever minFrom1_)) max_)
         ->
            Result
                error_
                (Emptiable (Stacked narrowElement) narrowPossiblyOrNever)
        )
        (Emptiable (Stacked broadElement) broadPossiblyOrNever
         -> ArraySized broadElement (Min (On (N0OrAdd1 broadPossiblyOrNever N0)))
        )
arraySized =
    Morph.oneToOne ArraySized.toStack ArraySized.fromStack


{-| [`Morph.OneToOne`](Morph#OneToOne) from `String` to a stack of `Char`s.

    import Stack
    import Morph

    "012" |> Morph.mapTo Stack.Morph.string
    --> Stack.fromList [ '0', '1', '2' ]

[Inverse](Morph#invert) of [`String.Morph.stack`](String-Morph#stack)

-}
string :
    MorphOrError
        (Emptiable (Stacked Char) Possibly)
        String
        error_
string =
    Morph.oneToOne Stack.fromString Stack.toString
