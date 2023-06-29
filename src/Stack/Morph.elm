module Stack.Morph exposing
    ( each
    , list, toList
    , string, toString
    )

{-| [`Morph`](Morph#Morph) a [`Stack`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Stack)


## alter

@docs each


## transform

@docs list, toList
@docs string, toString

-}

import Emptiable exposing (Emptiable, filled)
import Linear exposing (Direction(..))
import Morph exposing (MorphIndependently, MorphOrError, oneToOne)
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


{-| [`Morph.OneToOne`](Morph#OneToOne) from a stack to a `List`

    import Stack
    import Stack.Morph
    import Morph

    Stack.topBelow 0 [ 12, 3 ]
        |> Morph.mapTo Stack.Morph.toList
    --> [ 0, 12, 3 ]

-}
toList :
    MorphIndependently
        (Emptiable (Stacked broadElement) Possibly
         -> Result error_ (List broadElement)
        )
        (List narrowElement
         -> Emptiable (Stacked narrowElement) Possibly
        )
toList =
    Morph.invert list


{-| [`Morph.OneToOne`](Morph#OneToOne) from `List` to a stack.

    import Stack
    import Stack.Morph
    import Morph

    [ 0, 12, 3 ]
        |> Morph.mapTo Stack.Morph.list
    --> Stack.topBelow 0 [ 12, 3 ]
    --: Emptiable (Stacked Int) Possibly

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
    oneToOne Stack.fromList Stack.toList


{-| [`Morph.OneToOne`](Morph#OneToOne) from a stack of `Char`s to a `String`.

    import Stack
    import Morph

    Stack.topBelow '0' [ '1', '2' ]
        |> Morph.mapTo Stack.Morph.toString
    --> "012"

-}
toString :
    MorphOrError
        String
        (Emptiable (Stacked Char) Possibly)
        error_
toString =
    Morph.invert string


{-| [`Morph.OneToOne`](Morph#OneToOne) from `String` to a stack of `Char`s.

    import Stack
    import Morph

    "012" |> Morph.mapTo Stack.Morph.string
    --> Stack.fromList [ '0', '1', '2' ]

-}
string :
    MorphOrError
        (Emptiable (Stacked Char) Possibly)
        String
        error_
string =
    oneToOne Stack.fromString Stack.toString
