module Stack.Morph exposing
    ( reverse, eachElement
    , list, toList
    , string, toString
    )

{-| [`Morph`](Morph#Morph) a [`Stack`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Stack)


## alter

@docs reverse, eachElement


## transform

@docs list, toList
@docs string, toString

-}

import Emptiable exposing (Emptiable, filled)
import Linear exposing (Direction(..))
import List.Morph
import Morph exposing (ErrorWithDeadEnd, Morph, MorphIndependently, MorphOrError, Translate, translate, translateOn)
import Possibly exposing (Possibly(..))
import Set exposing (Set)
import Stack exposing (StackTopBelow, Stacked)



-- alter


reverse :
    MorphIndependently
        (List narrowElement
         -> Result error_ (List narrowElement)
        )
        (List broadElement -> List broadElement)
reverse =
    translate List.reverse List.reverse


{-| [`Morph`](Morph#Morph) each stacked element

If the given [`Morph`](Morph#Morph) is a [`Translate`](Morph#Translate),
[`eachBelowTop`](#eachBelowTop) is equivalent to

    Morph.translateOn
        ( Stack.belowTopMap (\_ -> elementMap)
        , Stack.belowTopMap (\_ -> elementMap)
        )

-}
eachElement :
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
eachElement elementMorph =
    { description =
        { custom = Stack.only "each"
        , inner =
            Morph.Elements (elementMorph |> Morph.description)
                |> filled
        }
    , narrow =
        \stack ->
            case stack of
                Emptiable.Empty emptyPossiblyOrNever ->
                    Emptiable.Empty emptyPossiblyOrNever

                Emptiable.Filled (Stack.TopDown top belowTop) ->
                    stack
                        |> Stack.toList
                        |> List.Morph.eachElement elementMorph
                        |> Result.map
                            (\list ->
                                case list of
                                    -- is this branch is reached,
                                    -- List.Morph.eachElement has a bug
                                    [] ->
                                        case top |> 
                            )
    , broaden =
        Stack.map (\_ -> Morph.broadenWith elementMorph)
    }



-- transform


{-| [`Translate`](Morph#Translate) from a stack to a `List`.

    import Stack

    Stack.topDown 0 [ 12, 3 ]
        |> Morph.map Stack.Morph.toList
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
    translate Stack.toList Stack.fromList


{-| [`Translate`](Morph#Translate) from `List` to a stack.

    import Stack

    [ 0, 12, 3 ]
        |> Morph.mapWith Stack.Morph.list
    --> Stack.topDown 0 [ 12, 3 ]

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

    Stack.topDown '0' [ '1', '2' ]
        |> Morph.map Stack.Morph.toString
    --> "012"

-}
toString :
    MorphOrError
        String
        (Emptiable (Stacked Char) Possibly)
        error_
toString =
    translate Stack.toText Stack.fromText


{-| [`Translate`](Morph#Translate) from `String` to a stack of `Char`s.

    import Stack

    "012" |> Morph.mapWith Stack.Morph.string
    --> Stack.fromList [ '0', '1', '2' ]

-}
string :
    MorphOrError
        (Emptiable (Stacked Char) Possibly)
        String
        error_
string =
    translate Stack.fromText Stack.toText
