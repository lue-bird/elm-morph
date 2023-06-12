module List.Morph exposing
    ( each
    , for, forBroad
    , value
    )

{-| [`Morph`](Morph#Morph) to and from a `List`


## alter

@docs each


## sequence

@docs for, forBroad


## transform

@docs value

-}

import Array
import Emptiable exposing (Emptiable)
import Linear exposing (Direction(..))
import List.Linear
import Morph exposing (MorphIndependently, MorphRow, broad, toBroad, toNarrow)
import Morph.Internal
import PartialOrComplete exposing (PartialOrComplete(..))
import Possibly exposing (Possibly(..))
import Rope
import Stack exposing (Stacked)
import Value



-- sequence


{-| Match broad [`MorphRow`](Morph#MorphRow)s
(those that can always [produce its broad value](Morph#toBroad))
based given input elements in sequence

This can get verbose, so create helpers with it where you see common patterns!

    import Morph
    import Morph.Error

    textOnly : String -> MorphRow Char ()
    textOnly stringConstant =
        Morph.forBroad
            (Char.Morph.only >> Morph.one)
            (stringConstant |> String.toList)

    -- Match a specific character, case sensitive
    "abc"
        |> Text.toNarrow (textOnly "abc")
    --> Ok ()

    -- It fails if it's not _exactly_ the same
    "abC"
        |> Text.toNarrow (textOnly "abC")
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:1: I was expecting the character 'a'. I got stuck when I got the character 'A'."

-}
forBroad :
    (element
     -> MorphRow () broadElement
    )
    -> List element
    -> MorphRow () broadElement
forBroad morphRowByElement expectedConstantInputList =
    broad
        (List.repeat
            (expectedConstantInputList |> List.length)
            ()
        )
        |> Morph.overRow
            (expectedConstantInputList
                |> for morphRowByElement
            )


{-| [`Morph.grab`](Morph#grab) the elements of a given `List` of [`MorphRow`](Morph#MorphRow)s in order

Some also call this "traverse"

Don't try to be clever with this.

    import Morph
    import Char.Morph

    "AB"
        |> Morph.toNarrow
            (List.Morph.for (Char.Morph.caseNo >> Morph.one) [ 'a', 'b' ]
                |> Morph.rowFinish
                |> Morph.over Stack.Morph.string
            )
    --> Ok [ 'a', 'b' ]

The usual [`Morph.succeed`](Morph#succeed)`(\... -> ...) |>`[`grab`](Morph#grab)-[`match`](Morph#match) chain
is often more explicit, descriptive and type-safe.

Because of this, `List.Morph` only exposes `for`, not `sequence`,
making misuse a bit more obvious.

-}
for :
    (element
     -> MorphRow narrow broadElement
    )
    -> List element
    -> MorphRow (List narrow) broadElement
for morphRowByElement elementsToTraverseInSequence =
    elementsToTraverseInSequence
        |> List.map morphRowByElement
        |> sequence


sequence :
    List (MorphRow element broadElement)
    -> MorphRow (List element) broadElement
sequence toSequence =
    case toSequence of
        [] ->
            Morph.succeed []

        toSequence0 :: toSequence1Up ->
            { description =
                Morph.Internal.sequenceDescriptionFromStack
                    (Stack.topBelow toSequence0 toSequence1Up
                        |> Stack.map (\_ -> Morph.description)
                    )
            , toNarrow =
                let
                    step :
                        MorphRow element broadElement
                        ->
                            { broad : Emptiable (Stacked broadElement) Possibly
                            , narrow : List element
                            , startsDown : Emptiable (Stacked Int) Never
                            }
                        ->
                            PartialOrComplete
                                { broad : Emptiable (Stacked broadElement) Possibly
                                , narrow : List element
                                , startsDown : Emptiable (Stacked Int) Never
                                }
                                { error : Morph.Error
                                , startsDown : Emptiable (Stacked Int) Never
                                }
                    step sequenceMorphRow =
                        \soFar ->
                            case soFar.broad |> toNarrow sequenceMorphRow of
                                Ok stepParsed ->
                                    { broad = stepParsed.broad
                                    , narrow =
                                        soFar.narrow |> (::) stepParsed.narrow
                                    , startsDown =
                                        soFar.startsDown
                                            |> Stack.onTopLay (stepParsed.broad |> Stack.length)
                                    }
                                        |> Partial

                                Err error ->
                                    { startsDown = soFar.startsDown, error = error }
                                        |> Complete
                in
                \initialInput ->
                    let
                        traversed =
                            (toSequence0 :: toSequence1Up)
                                |> List.Linear.foldUntilCompleteFrom
                                    { narrow = []
                                    , broad = initialInput
                                    , startsDown = initialInput |> Stack.length |> Stack.one
                                    }
                                    Up
                                    (\sequenceMorphRow statusOk -> statusOk |> step sequenceMorphRow)
                    in
                    case traversed of
                        Partial ok ->
                            { narrow = ok.narrow, broad = ok.broad } |> Ok

                        Complete error ->
                            case toSequence0 :: toSequence1Up |> List.length of
                                1 ->
                                    error.error |> Err

                                _ ->
                                    Morph.Internal.inSequenceErrorWith error |> Err
            , toBroad =
                \beforeToBroadSequence ->
                    List.map2
                        (\morphInSequence narrowElement -> narrowElement |> toBroad morphInSequence)
                        (toSequence0 :: toSequence1Up)
                        beforeToBroadSequence
                        |> Rope.fromList
                        |> Rope.concat
            }



--


{-| `List` [`Value.Morph`](Value#Morph)
-}
value : Value.Morph element -> Value.Morph (List element)
value elementMorph =
    each elementMorph
        |> Morph.over
            (Morph.custom "List"
                { toNarrow =
                    \broad ->
                        case broad of
                            Value.List listElements ->
                                listElements |> Ok

                            Value.Array arrayElements ->
                                arrayElements |> Array.toList |> Ok

                            composedExceptList ->
                                composedExceptList
                                    |> Value.composedKindToString
                                    |> Err
                , toBroad = Value.List
                }
            )
        |> Morph.over Value.composed


{-| [`Morph`](Morph#Morph) all elements.
On the narrowing side all [narrowed](Morph#toNarrow) values must be `Ok`
for it to not result in a [`Morph.Error`](Morph#Error)

If the element [`Morph`](Morph#Morph) is a [`Translate`](Morph#Translate),
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
            (List beforeToNarrow
             ->
                Result
                    (Morph.ErrorWithDeadEnd deadEnd)
                    (List narrow)
            )
            (List beforeToBroad -> List broad)
each elementMorph =
    { description =
        { custom = Stack.one "all"
        , inner = Morph.ElementsDescription (elementMorph |> Morph.description)
        }
    , toNarrow =
        \list ->
            list
                |> List.foldr
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
                                            { index = index
                                            , error = elementError
                                            }
                                        |> Err
                        , index = index - 1
                        }
                    )
                    { collected = [] |> Ok
                    , index = (list |> List.length) - 1
                    }
                |> .collected
                |> Result.mapError Morph.GroupError
    , toBroad =
        \list ->
            list |> List.map (Morph.toBroad elementMorph)
    }
