module String.Morph exposing
    ( elementTranslate
    , only
    , toList, list, value
    , for, forBroad
    )

{-| [`Morph`](Morph#Morph)s from and to a `String`.

Necessary because a `String` isn't a `List` internally :(


## alter

Also available: [`toggle`](Morph#toggle) `String.reverse`

@docs elementTranslate
@docs only


### transform

@docs toList, list, value


## sequence

@docs for, forBroad

-}

import Char.Morph
import List.Morph
import Morph exposing (Morph, MorphOrError, MorphRow, Translate, broad, one, translate, translateOn)
import Value exposing (MorphValue)
import Value.PackageInternal


{-| [`Translate`](Morph#Translate) from a `String` to a `List Char`.

    "0123" |> (String.Morph.toList |> Morph.map)
    --> [ '0', '1', '2', '3' ]

-}
toList : MorphOrError (List Char) String error_
toList =
    translate String.toList String.fromList


{-| [`Translate`](Morph#Translate) from `List Char` to a `String`.

    "0123" |> Morph.mapWith String.Morph.fromList
    --> [ '0', '1', '2', '3' ]

Parse-build a `String` →
Use [`narrowWith`](Morph#narrow), [`broadenWith`](Morph#broaden)
with [`Morph.rowFinish`](MorphRow#finish) `|> over` [`Text.fromList`](#fromList)

-}
list : MorphOrError String (List Char) error_
list =
    translate String.fromList String.toList



-- transform


{-| [`Translate`](Morph#Translate) each element in a `String`
-}
elementTranslate :
    Translate Char Char
    -> MorphOrError String String error_
elementTranslate elementCharTranslate =
    translateOn ( String.map, String.map ) elementCharTranslate



--


{-| Match a specific text string and nothing else.
This is case sensitive.

    import Morph.Error

    -- match an exact text, case sensitive
    "abcdef" |> Text.narrowWith (text "abc") --> Ok "abc"

    -- but anything else makes it fail
    "abCDEF"
        |> Text.narrowWith (text "abc")
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:3: I was expecting the text 'abc'. I got stuck when I got the character 'C'."

See [`for`](#for), [`forBroad`](#forBroad) to control what to [morph](MorphRow#Row) for each `Char`.

-}
only : String -> MorphRow Char ()
only expectedText =
    Morph.to ([ "\"", expectedText, "\"" ] |> String.concat)
        (forBroad
            (Char.Morph.only >> Morph.one)
            expectedText
        )


{-| Match broad [`MorphRow`](#MorphRow)s
(those that can always [produce its broad value](Morph#broadenWith))
based a given `String`s `Char`s in sequence

See [`Morph.forBroad`](Morph#forBroad)

-}
forBroad :
    (Char -> MorphRow broadElement ())
    -> String
    -> MorphRow broadElement ()
forBroad charMorphRow expectedText =
    List.Morph.forBroad charMorphRow
        (expectedText |> String.toList)


{-| Traverse a `String`: See [`Morph.for`](#for)
-}
for :
    (Char -> MorphRow broadElement narrowElement)
    -> String
    -> MorphRow broadElement (List narrowElement)
for charMorphRow expectedText =
    List.Morph.for charMorphRow
        (expectedText |> String.toList)



--


{-| `String` [`MorphValue`](Value#MorphValue)
-}
value : MorphValue String
value =
    Morph.value "String"
        { broaden = Value.String
        , narrow =
            \valueBroad ->
                case valueBroad of
                    Value.String stringNarrow ->
                        stringNarrow |> Ok

                    literalExceptString ->
                        literalExceptString |> Value.PackageInternal.literalKindToString |> Err
        }
        |> Morph.over Value.literal
