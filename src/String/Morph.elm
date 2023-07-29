module String.Morph exposing
    ( each
    , only
    , list, stack, array, arraySized, value
    , sequenceMap, broadSequenceMap
    )

{-| [`Morph`](Morph#Morph) for an [`elm/core` `String`](https://dark.elm.dmy.fr/packages/elm/core/latest/String#String)


## alter

@docs each
@docs only


### transform

@docs list, stack, array, arraySized, value


## sequence

@docs sequenceMap, broadSequenceMap

-}

import Array exposing (Array)
import ArraySized exposing (ArraySized)
import Char.Morph
import Emptiable exposing (Emptiable)
import List.Morph
import Morph exposing (MorphIndependently, MorphOrError, MorphRow)
import N exposing (Min, Up0)
import Possibly exposing (Possibly)
import Stack exposing (Stacked)
import String.Morph.Internal
import Value.Morph.Internal exposing (MorphValue)


{-| [`Morph.OneToOne`](Morph#OneToOne) from a `List Char`.

    import Morph

    [ '0', '1', '2', '3' ] |> Morph.mapTo String.Morph.list
    --> "0123"

    "0123" |> Morph.toBroad String.Morph.list
    --> [ '0', '1', '2', '3' ]

[Inverse](Morph#invert) of [`String.Morph.list`](String-Morph#list)

To use a [`MorphRow ... Char`](Morph#MorphRow) with a `String`,
use [`Morph.toNarrow`](Morph#toNarrow)/[`toBroad`](Morph#toBroad)
with

    yourData
        |> Morph.toNarrowOrBroad
            (yourMorphRow
                |> Morph.rowFinish
                |> Morph.over String.Morph.List
            )

-}
list : MorphOrError String (List Char) error_
list =
    Morph.oneToOne String.fromList String.toList


{-| [`Morph.OneToOne`](Morph#OneToOne) from a
[stack](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Stack) of `Char`s.

    import Stack
    import Morph

    Stack.topBelow '0' [ '1', '2' ]
        |> Morph.mapTo String.Morph.stack
    --> "012"

[Inverse](Morph#invert) of [`Stack.Morph.string`](Stack-Morph#string)

-}
stack :
    MorphOrError
        String
        (Emptiable (Stacked Char) Possibly)
        error_
stack =
    Morph.oneToOne Stack.toString Stack.fromString


{-| [`Morph.OneToOne`](Morph#OneToOne) from an
[`ArraySized`](https://dark.elm.dmy.fr/packages/lue-bird/elm-typesafe-array/latest/) of `Char`s

    import ArraySized
    import Morph

    ArraySized.l4 '0' '1' '2' '3'
        |> Morph.mapTo String.Morph.arraySized
    --> "0123"

[Inverse](Morph#invert) of [`ArraySized.Morph.string`](ArraySized-Morph#string)

-}
arraySized :
    MorphIndependently
        (ArraySized Char narrowRange_
         -> Result error_ String
        )
        (String
         -> ArraySized Char (Min (Up0 broadX_))
        )
arraySized =
    Morph.oneToOne ArraySized.toString ArraySized.fromString


{-| [`Morph.OneToOne`](Morph#OneToOne) from an `Array Char`

    import Array
    import Morph

    Array.fromList [ '0', '1', '2', '3' ]
        |> Morph.mapTo String.Morph.array
    --> "0123"

[Inverse](Morph#invert) of [`Array.Morph.string`](Array-Morph#string)

-}
array : MorphOrError String (Array Char) error_
array =
    list
        |> Morph.over (Morph.oneToOne Array.toList Array.fromList)
        |> Morph.narrowErrorMap Morph.deadEndNever



-- transform


{-| [`Morph.OneToOne`](Morph#OneToOne) each `Char` in a `String`

For fallible transformations etc,
morph to other structures (but generic ones) like a list first
and use its `each` morph.

-}
each :
    Morph.OneToOne Char Char
    -> MorphOrError String String error_
each elementCharTranslate =
    Morph.oneToOneOn ( String.map, String.map ) elementCharTranslate



--


{-| Match a specific given `String` and nothing else.
This is case sensitive.

    import Morph
    import List.Morph

    -- match an exact text
    "abc"
        |> Morph.toNarrow
            (String.Morph.only "abc" |> Morph.rowFinish |> Morph.over List.Morph.string)
    --> Ok ()

    -- case sensitive
    "abC"
        |> Morph.toNarrow
            (String.Morph.only "abc" |> Morph.rowFinish |> Morph.over List.Morph.string)
        |> Result.toMaybe
    --> Nothing

    -- if there is still input remaining, fail
    "abcdef"
        |> Morph.toNarrow
            (String.Morph.only "abc" |> Morph.rowFinish |> Morph.over List.Morph.string)
        |> Result.toMaybe
    --> Nothing

See [`sequenceMap`](#sequenceMap), [`broadSequenceMap`](#broadSequenceMap) to control what to [morph](Morph#MorphRow) for each `Char`.

-}
only : String -> MorphRow () Char
only specificValidBroadString =
    let
        nameIfMultiple =
            if (specificValidBroadString |> String.length) <= 1 then
                identity

            else
                Morph.named ([ "\"", specificValidBroadString, "\"" ] |> String.concat)
    in
    nameIfMultiple
        (broadSequenceMap
            (Char.Morph.only >> Morph.one)
            specificValidBroadString
        )


{-| Match broad [`MorphRow`](Morph#MorphRow)s
(those that can always produce the same broad value)
based on given input elements in sequence.

More details → [`List.Morph.broadSequenceMap`](List-Morph#broadSequenceMap)

-}
broadSequenceMap :
    (Char
     -> MorphRow () broadElement
    )
    -> String
    -> MorphRow () broadElement
broadSequenceMap charMorphRow expectedText =
    List.Morph.broadSequenceMap charMorphRow
        (expectedText |> String.toList)


{-| From the chars in a given `String`,
create [`MorphRow`](Morph#MorphRow)s
that will be run in the same order, one after the other.

More details → [`List.Morph.sequenceMap`](List-Morph#sequenceMap)

-}
sequenceMap :
    (Char -> MorphRow narrowElement broadElement)
    -> String
    -> MorphRow (List narrowElement) broadElement
sequenceMap charMorphRow expectedText =
    List.Morph.sequenceMap charMorphRow
        (expectedText |> String.toList)



--


{-| `String` [`MorphValue`](Value-Morph#MorphValue)
-}
value : MorphValue String
value =
    String.Morph.Internal.value
