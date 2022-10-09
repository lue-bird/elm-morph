module List.Morph exposing
    ( reverse, eachElement
    , for, forBroad
    , value
    )

{-| [`Morph`](Morph#Morph) to and from a `List`


## alter

@docs reverse, eachElement


## sequence

@docs for, forBroad


## transform

@docs value

-}

import Array
import ArraySized
import Emptiable exposing (Emptiable, filled)
import Linear exposing (Direction(..))
import Morph exposing (ErrorWithDeadEnd, Morph, MorphIndependently, MorphOrError, MorphRow, Translate, broad, broadenWith, narrowWith, translate, translateOn)
import N exposing (Up)
import Possibly exposing (Possibly(..))
import Stack exposing (Stacked)
import Value exposing (MorphValue)
import Value.PackageInternal



-- alter


{-| Flip the elements' order
-}
reverse :
    MorphIndependently
        (List broadElement -> Result error_ (List broadElement))
        (List narrowElement -> List narrowElement)
reverse =
    translate List.reverse List.reverse



-- sequence


{-| Match broad [`MorphRow`](#MorphRow)s
(those that can always [produce its broad value](#broadenWith))
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
        |> Text.narrowWith (textOnly "abc")
    --> Ok ()

    -- It fails if it's not _exactly_ the same
    "abC"
        |> Text.narrowWith (textOnly "abC")
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:1: I was expecting the character 'a'. I got stuck when I got the character 'A'."

-}
forBroad :
    (element -> MorphRow broadElement ())
    -> List element
    -> MorphRow broadElement ()
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


{-| [`grab`](#grab) the elements of a given `List` of [`MorphRow`](#MorphRow)s in order

Some also call this "traverse"

Don't try to be clever with this.

    import Morph exposing (one)
    import Char.Morph as Char
    import String.Morph as Text

    "AB"
        |> narrow
            (Morph.for (Char.Morph.caseNo >> one) [ 'a', 'b' ]
                |> Morph.rowFinish
                |> Morph.over Stack.Morph.fromText
            )
    --> Ok [ 'a', 'b' ]

The usual [`Morph.succeed`](#Morph.succeed)`(\... -> ...) |>`[`grab`](#grab)-[`skip`](#skip) chain
is often more explicit, descriptive and type-safe.

Because of this, `MorphRow` only exposes `for`, not `sequence`,
making misuse a bit more obvious.

-}
for :
    (element -> MorphRow broadElement narrow)
    -> List element
    -> MorphRow broadElement (List narrow)
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
                        |> ArraySized.glueMin Up
                            (elements2Up |> ArraySized.fromList)
                        |> ArraySized.map
                            (morphRowByElement >> Morph.description)
                        |> ArraySized.maxToInfinity
                        |> Morph.Group
                        |> Emptiable.filled
                }
    , narrow =
        let
            stepFrom traversedElement =
                \soFar ->
                    soFar.broad
                        |> narrowWith (traversedElement |> morphRowByElement)
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
                    broadenWith (morphInSequence |> morphRowByElement)
                )
                elementsToTraverseInSequence
                narrowSequence
                |> List.concatMap Stack.toList
                |> Stack.fromList
    }



--


{-| `List` [`Morph`](#Morph)
-}
value : MorphValue element -> MorphValue (List element)
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

                            structureExceptList ->
                                structureExceptList
                                    |> Value.PackageInternal.structureKindToString
                                    |> Err
                , broaden = Value.List
                }
            )
        |> Morph.over Value.structure


{-| [`Morph`](Morph#Morph) all elements in sequence.
On the narrowing side all [narrowed](Morph#narrowWith) values must be `Ok`
for it to not result in a [`Morph.Error`](Morph#Error)

If the element [`Morph`](Morph#Morph) is a [`Translate`](Morph#Translate),
`eachElement` will be equivalent to

    Morph.translateOn ( List.map, List.map )

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
            (List beforeNarrow
             ->
                Result
                    (Morph.ErrorWithDeadEnd deadEnd)
                    (List narrow)
            )
            (List beforeBroaden -> List broad)
eachElement elementMorph =
    { description =
        { custom = Stack.only "each"
        , inner =
            Morph.Elements (elementMorph |> Morph.description)
                |> filled
        }
    , narrow =
        \list ->
            list
                |> List.foldr
                    (\element { index, collected } ->
                        { collected =
                            case element |> Morph.narrowWith elementMorph of
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
                |> Result.mapError Morph.Parts
    , broaden =
        \list ->
            list |> List.map (Morph.broadenWith elementMorph)
    }
