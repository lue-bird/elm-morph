module List.Morph exposing
    ( each
    , sequenceMap, broadSequenceMap
    , array, string, set, dict
    , bytes, value
    )

{-| [`Morph`](Morph#Morph) for an [`elm/core` `List element`](https://dark.elm.dmy.fr/packages/elm/core/latest/List)


## alter

@docs each


## sequence

@docs sequenceMap, broadSequenceMap


## transform

@docs array, string, set, dict
@docs bytes, value

-}

import Array exposing (Array)
import Bit exposing (Bit)
import Bits
import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import Dict exposing (Dict)
import Linear exposing (Direction(..))
import List.Linear
import Morph exposing (MorphIndependently, MorphOrError, MorphRow, broad, toBroad, toNarrow)
import Morph.Internal
import PartialOrComplete exposing (PartialOrComplete(..))
import Rope
import Set exposing (Set)
import Stack exposing (Stacked)
import Value
import Value.Morph.Internal exposing (MorphValue)



-- sequence


{-| Match broad [`MorphRow`](Morph#MorphRow)s
(those that can always produce the same broad value)
based on given input elements in sequence.

This can get verbose, so create helpers with it where you see common patterns!

    import Morph exposing (MorphRow)
    import Char.Morph
    import List.Morph

    textOnly : String -> MorphRow () Char
    textOnly stringConstant =
        List.Morph.broadSequenceMap
            (Char.Morph.only >> Morph.one)
            (stringConstant |> String.toList)

    -- Match a specific character, case sensitive
    "abc"
        |> Morph.toNarrow
            (textOnly "abc" |> Morph.rowFinish |> Morph.over List.Morph.string)
    --> Ok ()

    -- It fails if it's not _exactly_ the same
    "abC"
        |> Morph.toNarrow
            (textOnly "abc" |> Morph.rowFinish |> Morph.over List.Morph.string)
        |> Result.toMaybe
    --> Nothing

Note that `textOnly` is available as `String.Morph.only`.
Others aren't, tho, like when matching only a specific
sequence of [`Bit`](https://dark.elm.dmy.fr/packages/lue-bird/elm-bits/latest/)s

-}
broadSequenceMap :
    (element -> MorphRow () broadElement)
    -> List element
    -> MorphRow () broadElement
broadSequenceMap morphRowByElement expectedConstantInputList =
    broad
        (List.repeat
            (expectedConstantInputList |> List.length)
            ()
        )
        |> Morph.overRow
            (expectedConstantInputList
                |> sequenceMap morphRowByElement
            )


{-| From the elements in a given `List`,
create [`MorphRow`](Morph#MorphRow)s
that will be run in the same order, one after the other.

Some also call this "traverse" (or "for" when the arguments are flipped)

    import Morph exposing (MorphRow)
    import String.Morph
    import AToZ

    "helloTHEREcooorwhat"
        |> Morph.toNarrow
            (List.Morph.sequenceMap casedStringOnly [ "hello", "there", "coo", "or", "what" ]
                |> Morph.rowFinish
                |> Morph.over List.Morph.string
            )
    --> Ok [ AToZ.CaseLower, AToZ.CaseUpper, AToZ.CaseLower, AToZ.CaseLower, AToZ.CaseLower ]

    casedStringOnly : String -> MorphRow AToZ.Case Char
    casedStringOnly string =
        Morph.choice
            (\lower upper cased ->
                case cased of
                    AToZ.CaseLower -> lower ()
                    AToZ.CaseUpper -> upper ()
            )
            |> Morph.rowTry (\() -> AToZ.CaseLower)
                (String.Morph.only (string |> String.toLower))
            |> Morph.rowTry (\() -> AToZ.CaseUpper)
                (String.Morph.only (string |> String.toUpper))
            |> Morph.choiceFinish

**Don't try to be clever with this.**

The usual [`Morph.narrow`](Morph#narrow)`(\... -> ...) |>`[`grab`](Morph#grab)-[`match`](Morph#match) chain
is often more explicit, descriptive and type-safe.

Because of this, `List.Morph` only exposes `sequenceMap`, not `sequence`,
making misuse a bit more obvious.

If each element's [`MorphRow`](Morph#MorphRow)
will always produce the same broad value like [`String.Morph.only`](String-Morph#only),
use [`broadSequenceMap`](#broadSequenceMap)

-}
sequenceMap :
    (element
     -> MorphRow narrow broadElement
    )
    -> List element
    -> MorphRow (List narrow) broadElement
sequenceMap morphRowByElement elementsToTraverseInSequence =
    elementsToTraverseInSequence
        |> List.map morphRowByElement
        |> sequence


sequence :
    List (MorphRow element broadElement)
    -> MorphRow (List element) broadElement
sequence toSequence =
    case toSequence of
        [] ->
            Morph.narrow []

        toSequence0 :: toSequence1Up ->
            { description =
                Morph.Internal.sequenceDescriptionFromStack
                    (( toSequence0, toSequence1Up )
                        |> Stack.map Morph.description
                    )
            , toNarrow =
                let
                    step :
                        MorphRow element broadElement
                        ->
                            { broad : List broadElement
                            , narrow : List element
                            , startsDown : Stacked Int
                            }
                        ->
                            PartialOrComplete
                                { broad : List broadElement
                                , narrow : List element
                                , startsDown : Stacked Int
                                }
                                { error : Morph.Error
                                , startsDown : Stacked Int
                                }
                    step sequenceMorphRow soFar =
                        case soFar.broad |> toNarrow sequenceMorphRow of
                            Ok stepParsed ->
                                { broad = stepParsed.broad
                                , narrow =
                                    soFar.narrow |> (::) stepParsed.narrow
                                , startsDown =
                                    soFar.startsDown
                                        |> Stack.cons (stepParsed.broad |> List.length)
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
                                    , startsDown = initialInput |> List.length |> Stack.one
                                    }
                                    Up
                                    (\sequenceMorphRow statusOk -> statusOk |> step sequenceMorphRow)
                    in
                    case traversed of
                        Partial ok ->
                            { narrow = ok.narrow |> List.reverse, broad = ok.broad } |> Ok

                        Complete error ->
                            case toSequence1Up of
                                [] ->
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


{-| [`Morph.OneToOne`](Morph#OneToOne) from an `Array`

    import Array
    import Morph

    Array.fromList [ 0, 1, 2, 3 ]
        |> Morph.mapTo List.Morph.array
    --> [ 0, 1, 2, 3 ]

[Inverse](Morph#invert) of [`Array.Morph.list`](Array-Morph#list)

-}
array :
    MorphIndependently
        (Array narrowElement -> Result error_ (List narrowElement))
        (List element -> Array element)
array =
    Morph.oneToOne Array.toList Array.fromList


{-| [`Morph.OneToOne`](Morph#OneToOne) from a `String` to a `List Char`.

[Inverse](Morph#invert) of [`String.Morph.list`](String-Morph#list)

-}
string : MorphOrError (List Char) String error_
string =
    Morph.oneToOne String.toList String.fromList


{-| [`Morph.OneToOne`](Morph#OneToOne) from a `Set`

    import Set
    import Morph

    Set.fromList [ 0, 1, 2, 3 ]
        |> Morph.mapTo List.Morph.set
    --> [ 0, 1, 2, 3 ]

[Inverse](Morph#invert) of [`Set.Morph.list`](Set-Morph#list)

-}
set :
    MorphIndependently
        (Set narrowElement
         -> Result error_ (List narrowElement)
        )
        (List comparableBroadElement
         -> Set comparableBroadElement
        )
set =
    Morph.oneToOne Set.toList Set.fromList


{-| [`Morph.OneToOne`](Morph#OneToOne) from a `Dict key value` to a `List { key : key, value : value }`.

    import Dict
    import List.Morph
    import Morph

    Dict.empty
        |> Dict.insert 0 'a'
        |> Dict.insert 1 'b'
        |> Morph.mapTo List.Morph.dict
    --> [ { key = 0, value = 'a' }, { key = 1, value = 'b' } ]

[Inverse](Morph#invert) of [`Dict.Morph.list`](Dict-Morph#list)

-}
dict :
    MorphIndependently
        (Dict broadKey broadValue
         -> Result error_ (List { key : broadKey, value : broadValue })
        )
        (List { key : comparableNarrowKey, value : narrowValue }
         -> Dict comparableNarrowKey narrowValue
        )
dict =
    Morph.oneToOne
        (\dict_ ->
            dict_
                |> Dict.foldr
                    (\key value_ -> (::) { key = key, value = value_ })
                    []
        )
        (\dict_ ->
            dict_
                |> List.foldl
                    (\entry -> Dict.insert entry.key entry.value)
                    Dict.empty
        )


{-| `List` [`MorphValue`](Value-Morph#MorphValue)
-}
value : MorphValue element -> MorphValue (List element)
value elementMorph =
    each elementMorph
        |> Morph.over
            (Morph.custom "list"
                { toNarrow =
                    \broad ->
                        case broad of
                            Value.List listElements ->
                                listElements |> Ok

                            composedExceptList ->
                                composedExceptList
                                    |> Value.composedKindToString
                                    |> Err
                , toBroad = Value.List
                }
            )
        |> Morph.over Value.Morph.Internal.toComposed


{-| [`Morph`](Morph#Morph) all elements.
On the narrowing side all [narrowed](Morph#toNarrow) values must be `Ok`
for it to not result in a [`Morph.Error`](Morph#Error)

If the element [`Morph`](Morph#Morph) is [`OneToOne`](Morph#OneToOne),
`each` will always succeed with the type knowing it does

-}
each :
    MorphIndependently
        (beforeToNarrow
         -> Result Morph.Error narrow
        )
        (beforeToBroad -> broad)
    ->
        MorphIndependently
            (List beforeToNarrow
             ->
                Result
                    Morph.Error
                    (List narrow)
            )
            (List beforeToBroad -> List broad)
each elementMorph =
    Morph.named "each"
        { description =
            Morph.ElementsDescription (elementMorph |> Morph.description)
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
                                            errorsSoFar : List { index : Int, error : Morph.Error }
                                            errorsSoFar =
                                                case collected of
                                                    Ok _ ->
                                                        []

                                                    Err elementsAtIndexes ->
                                                        elementsAtIndexes |> Stack.toList
                                        in
                                        Err
                                            ( { index = index
                                              , error = elementError
                                              }
                                            , errorsSoFar
                                            )
                            , index = index - 1
                            }
                        )
                        { collected = Ok []
                        , index = (list |> List.length) - 1
                        }
                    |> .collected
                    |> Result.mapError Morph.PartsError
        , toBroad =
            \list -> list |> List.map (Morph.toBroad elementMorph)
        }


{-| [`Morph.OneToOne`](Morph#OneToOne) from [`Bytes`](https://dark.elm.dmy.fr/packages/elm/bytes/latest/)
to a list of individual bits.
Now you can [morph them as a row](Morph#MorphRow)!
-}
bytes : MorphOrError (List Bit) Bytes error_
bytes =
    Morph.oneToOne Bits.fromBytes Bits.toBytes
