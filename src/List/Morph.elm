module List.Morph exposing
    ( eachElement
    , for, forBroad
    , value
    )

{-| [`Morph`](Morph#Morph) to and from a `List`


## alter

@docs eachElement


## sequence

@docs for, forBroad


## transform

@docs value

-}

import Array
import ArraySized
import Emptiable exposing (filled)
import Linear exposing (Direction(..))
import Morph exposing (MorphIndependently, MorphRow, broad, broadenFrom, narrowTo)
import Possibly exposing (Possibly(..))
import Stack
import Value



-- sequence


{-| Match broad [`MorphRow`](Morph#MorphRow)s
(those that can always [produce its broad value](Morph#broadenFrom))
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
        |> Text.narrowTo (textOnly "abc")
    --> Ok ()

    -- It fails if it's not _exactly_ the same
    "abC"
        |> Text.narrowTo (textOnly "abC")
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
    import Char.Morph as Char
    import String.Morph as Text

    "AB"
        |> narrow
            (Morph.for (Char.Morph.caseNo >> Morph.one) [ 'a', 'b' ]
                |> Morph.rowFinish
                |> Morph.over Stack.Morph.string
            )
    --> Ok [ 'a', 'b' ]

The usual [`Morph.succeed`](Morph#succeed)`(\... -> ...) |>`[`grab`](Morph#grab)-[`match`](Morph#match) chain
is often more explicit, descriptive and type-safe.

Because of this, `MorphRow` only exposes `for`, not `sequence`,
making misuse a bit more obvious.

-}
for :
    (element
     -> MorphRow narrow broadElement
    )
    -> List element
    -> MorphRow (List narrow) broadElement
for morphRowByElement elementsToTraverseInSequence =
    { description =
        case elementsToTraverseInSequence of
            [] ->
                { inner = Emptiable.empty, custom = Emptiable.empty }

            [ only1 ] ->
                only1 |> morphRowByElement |> Morph.description

            element0 :: element1 :: elements2Up ->
                { custom = Emptiable.empty
                , inner =
                    ArraySized.l2 element0 element1
                        |> ArraySized.attachMin Up
                            (elements2Up |> ArraySized.fromList)
                        |> ArraySized.map
                            (morphRowByElement >> Morph.description)
                        |> ArraySized.maxToInfinity
                        |> Morph.GroupDescription
                        |> Emptiable.filled
                }
    , narrow =
        let
            stepFrom traversedElement =
                \soFar ->
                    soFar.broad
                        |> narrowTo (traversedElement |> morphRowByElement)
                        |> Result.map
                            (\stepParsed ->
                                { broad = stepParsed.broad
                                , narrow =
                                    soFar.narrow
                                        |> (::) stepParsed.narrow
                                }
                            )
        in
        \initialInput ->
            elementsToTraverseInSequence
                |> List.foldl
                    (\stepElement ->
                        Result.andThen (stepFrom stepElement)
                    )
                    ({ narrow = []
                     , broad = initialInput
                     }
                        |> Ok
                    )
    , broaden =
        \narrowSequence ->
            List.map2
                (\morphInSequence ->
                    broadenFrom (morphInSequence |> morphRowByElement)
                )
                elementsToTraverseInSequence
                narrowSequence
                |> List.concatMap Stack.toList
                |> Stack.fromList
    }



--


{-| `List` [`Value.Morph`](Value#Morph)
-}
value : Value.Morph element -> Value.Morph (List element)
value elementMorph =
    eachElement elementMorph
        |> Morph.over
            (Morph.value "List"
                { narrow =
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
                , broaden = Value.List
                }
            )
        |> Morph.over Value.composed


{-| [`Morph`](Morph#Morph) all elements in sequence.
On the narrowing side all [narrowed](Morph#narrowTo) values must be `Ok`
for it to not result in a [`Morph.Error`](Morph#Error)

If the element [`Morph`](Morph#Morph) is a [`Translate`](Morph#Translate),
`eachElement` will always succeeds with the type knowing it does

-}
eachElement :
    MorphIndependently
        (beforeNarrow
         -> Result (Morph.ErrorWithDeadEnd deadEnd) narrow
        )
        (beforeBroaden -> broad)
    ->
        MorphIndependently
            (List beforeNarrow
             ->
                Result
                    (Morph.ErrorWithDeadEnd deadEnd)
                    (List narrow)
            )
            (List beforeBroaden -> List broad)
eachElement elementMorph =
    { description =
        { custom = Stack.one "each"
        , inner =
            Morph.ElementsDescription (elementMorph |> Morph.description)
                |> filled
        }
    , narrow =
        \list ->
            list
                |> List.foldr
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
                    , index = (list |> List.length) - 1
                    }
                |> .collected
                |> Result.mapError Morph.GroupError
    , broaden =
        \list ->
            list |> List.map (Morph.broadenFrom elementMorph)
    }
