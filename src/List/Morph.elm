module List.Morph exposing
    ( reverse, elementTranslate
    , for, forBroad
    )

{-| [`Morph`](Morph#Morph) to and from a `List`


## alter

@docs reverse, elementTranslate


## sequence

for, forBroad

-}

import ArraySized
import Emptiable
import Linear exposing (DirectionLinear(..))
import Morph exposing (ErrorWithDeadEnd, Morph, MorphIndependently, MorphOrError, MorphRow, Translate, broad, broadenWith, narrowWith, translate, translateOn)
import N exposing (Up)
import Stack



-- alter


{-| Flip the elements' order
-}
reverse :
    MorphIndependently
        (List broadElement -> Result error_ (List broadElement))
        (List narrowElement -> List narrowElement)
reverse =
    translate List.reverse List.reverse


{-| [`Translate`](Morph#Translate) each element in a `List`
-}
elementTranslate :
    MorphIndependently
        (beforeMapElement -> Result (ErrorWithDeadEnd Never) mappedElement)
        (beforeUnmapElement -> unmappedElement)
    ->
        MorphIndependently
            (List beforeMapElement
             -> Result error_ (List mappedElement)
            )
            (List beforeUnmapElement -> List unmappedElement)
elementTranslate elementTranslate_ =
    translateOn ( List.map, List.map ) elementTranslate_



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

The usual [`succeed`](#succeed)`(\... -> ...) |>`[`grab`](#grab)-[`skip`](#skip) chain
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
                        |> ArraySized.minGlue Up
                            (elements2Up |> ArraySized.fromList)
                        |> ArraySized.map
                            (morphRowByElement >> Morph.description)
                        |> ArraySized.maxNo
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
