module String.Morph exposing
    ( each
    , only
    , toList, list, value
    , for, forBroad
    )

{-| [`Morph`](Morph#Morph)s from and to a `String`.


## alter

@docs each
@docs only


### transform

@docs toList, list, value


## sequence

@docs for, forBroad

-}

import Char.Morph
import List.Morph
import Morph exposing (MorphOrError, MorphRow, OneToOne, oneToOne, oneToOneOn)
import String.Morph.Internal
import Value.Morph exposing (MorphValue)


{-| [`OneToOne`](Morph#OneToOne) from a `String` to a `List Char`.

    "0123" |> (String.Morph.toList |> Morph.map)
    --> [ '0', '1', '2', '3' ]

-}
toList : MorphOrError (List Char) String error_
toList =
    oneToOne String.toList String.fromList


{-| [`OneToOne`](Morph#OneToOne) from `List Char` to a `String`.

    "0123" |> Morph.mapTo String.Morph.list
    --> [ '0', '1', '2', '3' ]

Parse-build a `String` →
Use [`toNarrow`](Morph#toNarrow), [`toBroad`](Morph#toBroad)
with [`Morph.rowFinish`](Morph#rowFinish) `|> over` [`String.Morph.List`](#list)

-}
list : MorphOrError String (List Char) error_
list =
    oneToOne String.fromList String.toList



-- transform


{-| [`OneToOne`](Morph#OneToOne) each `Char` in a `String`

For fallible transformations etc,
morph to other structures (but generic ones) like a list first
and use its `each` morph.

-}
each :
    OneToOne Char Char
    -> MorphOrError String String error_
each elementCharTranslate =
    oneToOneOn ( String.map, String.map ) elementCharTranslate



--


{-| Match a specific given `String` and nothing else.
This is case sensitive.

    import Morph

    -- match an exact text, case sensitive
    "abc" |> Morph.toNarrow (String.Morph.only "abc")
    --> Ok ()

    -- match an exact text, case sensitive
    "abcdef" |> Morph.toNarrow (String.Morph.only "abc")
    --> Err (Morph.DeadEnd "TODO")

    -- but anything else makes it fail
    "abCDEF"
        |> Text.toNarrow (text "abc")
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:3: I was expecting the text 'abc'. I got stuck when I got the character 'C'."

See [`for`](#for), [`forBroad`](#forBroad) to control what to [morph](Morph#MorphRow) for each `Char`.

-}
only : String -> MorphRow () Char
only expectedText =
    Morph.named ([ "\"", expectedText, "\"" ] |> String.concat)
        (forBroad
            (Char.Morph.only >> Morph.one)
            expectedText
        )


{-| Match broad [`MorphRow`](Morph#MorphRow)s
(those that can always [produce its broad value](Morph#toBroad))
based a given `String`s `Char`s in sequence

For more details, look at [`ArraySized.Morph.forBroad`](ArraySized-Morph#forBroad)

-}
forBroad :
    (Char
     -> MorphRow () broadElement
    )
    -> String
    -> MorphRow () broadElement
forBroad charMorphRow expectedText =
    List.Morph.forBroad charMorphRow
        (expectedText |> String.toList)


{-| Traverse a `String`
-}
for :
    (Char -> MorphRow narrowElement broadElement)
    -> String
    -> MorphRow (List narrowElement) broadElement
for charMorphRow expectedText =
    List.Morph.for charMorphRow
        (expectedText |> String.toList)



--


{-| `String` [`MorphValue`](Value-Morph#MorphValue)
-}
value : MorphValue String
value =
    String.Morph.Internal.value
