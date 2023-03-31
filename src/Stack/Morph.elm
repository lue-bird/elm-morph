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
import Morph exposing (MorphIndependently, MorphOrError, translate)
import Possibly exposing (Possibly(..))
import Stack exposing (Stacked)
import StructureMorph



-- alter


{-| [`Morph`](Morph#Morph) each stacked element

If the given [`Morph`](Morph#Morph) is a [`Translate`](Morph#Translate),
[`each`](#each) will also be a [`Translate`](Morph#Translate)

-}
each :
    MorphIndependently
        (beforeNarrow
         -> Result (Morph.ErrorWithDeadEnd deadEnd) narrow
        )
        (beforeBroaden -> broad)
    ->
        MorphIndependently
            (Emptiable (Stacked beforeNarrow) broadEmptiablePossiblyOrNever
             ->
                Result
                    (Morph.ErrorWithDeadEnd deadEnd)
                    (Emptiable (Stacked narrow) broadEmptiablePossiblyOrNever)
            )
            (Emptiable (Stacked beforeBroaden) narrowPossiblyOrNever
             -> Emptiable (Stacked broad) narrowPossiblyOrNever
            )
each elementMorph =
    StructureMorph.for "each" morphEachElement
        |> StructureMorph.add elementMorph
        |> StructureMorph.finish


morphEachElement :
    MorphIndependently
        (beforeNarrow
         -> Result (Morph.ErrorWithDeadEnd deadEnd) narrow
        )
        (beforeBroaden -> broad)
    ->
        { narrow :
            Emptiable (Stacked beforeNarrow) broadEmptiablePossiblyOrNever
            ->
                Result
                    (Morph.ErrorWithDeadEnd deadEnd)
                    (Emptiable (Stacked narrow) broadEmptiablePossiblyOrNever)
        , broaden :
            Emptiable (Stacked beforeBroaden) narrowPossiblyOrNever
            -> Emptiable (Stacked broad) narrowPossiblyOrNever
        }
morphEachElement elementMorph =
    { narrow =
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
                        |> Result.mapError Morph.GroupError
    , broaden =
        Stack.map (\_ -> Morph.toBroad elementMorph)
    }



-- transform


{-| [`Translate`](Morph#Translate) from a stack to a `List`

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


{-| [`Translate`](Morph#Translate) from `List` to a stack.

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
    translate Stack.fromList Stack.toList


{-| [`Translate`](Morph#Translate) from a stack of `Char`s to a `String`.

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


{-| [`Translate`](Morph#Translate) from `String` to a stack of `Char`s.

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
    translate Stack.fromString Stack.toString
