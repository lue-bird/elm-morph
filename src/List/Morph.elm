module List.Morph exposing
    ( each
    , sequenceMap, broadSequenceMap
    , stack, array, arraySized, string, set, dict
    , bytes, value
    )

{-| [`Morph`](Morph#Morph) to and from a `List`


## alter

@docs each


## sequence

@docs sequenceMap, broadSequenceMap


## transform

@docs stack, array, arraySized, string, set, dict
@docs bytes, value

-}

import Array exposing (Array)
import ArraySized exposing (ArraySized)
import Bit exposing (Bit)
import BitArray
import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import Dict exposing (Dict)
import Emptiable exposing (Emptiable)
import Linear exposing (Direction(..))
import List.Linear
import Morph exposing (MorphIndependently, MorphOrError, MorphRow, broad, toBroad, toNarrow)
import Morph.Internal
import N exposing (Min, Up0, n0, n8)
import PartialOrComplete exposing (PartialOrComplete(..))
import Possibly exposing (Possibly(..))
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

    import Morph
    import List.Morph

    textOnly : String -> MorphRow Char ()
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
    (element
     -> MorphRow () broadElement
    )
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

    import Morph
    import String.Morph
    import AToZ exposing (AToZ(..))

    "helloTherecoo"
        |> Morph.toNarrow
            (List.Morph.sequenceMap casedStringOnly [ "hello", "there", "coo" ]
                |> Morph.rowFinish
                |> Morph.over List.Morph.string
            )
    --> Ok [ AToZ.CaseLower, AToZ.CaseUpper, AToZ.CaseLower ]

    casedStringOnly : String -> MorphRow AToZ.Case Char
    casedStringOnly string =
        Morph.choice
            (\lower upper cased ->
                case cased of
                    AToZ.CaseLower -> lower ()
                    AToZ.CaseUpper -> upper ()
            )
            |> Morph.tryRow (\() -> AToZ.CharLower)
                (String.Morph.only (string |> String.toLower))
            |> Morph.tryRow (\() -> AToZ.CharUpper)
                (String.Morph.only (string |> String.toUpper))
            |> Morph.choiceFinish

**Don't try to be clever with this.**

The usual [`Morph.succeed`](Morph#succeed)`(\... -> ...) |>`[`grab`](Morph#grab)-[`match`](Morph#match) chain
is often more explicit, descriptive and type-safe.

Because of this, `List.Morph` only exposes `sequenceMap`, not `sequence`,
making misuse a bit more obvious.

If each element's [`MorphRow`](Morph#MorphRow)
will always produce the same broad value like `String.Morph.only`, use [`broadSequenceMap`](#broadSequenceMap)

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
                            { broad : List broadElement
                            , narrow : List element
                            , startsDown : Emptiable (Stacked Int) Never
                            }
                        ->
                            PartialOrComplete
                                { broad : List broadElement
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
                                            |> Stack.onTopLay (stepParsed.broad |> List.length)
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


{-| [`Morph.OneToOne`](Morph#OneToOne) from a stack to a `List`

    import Stack
    import Stack.Morph
    import Morph

    Stack.topBelow 0 [ 12, 3 ]
        |> Morph.mapTo List.Morph.stack
    --> [ 0, 12, 3 ]

[Inverse](Morph#invert) of [`Stack.Morph.list`](Stack-Morph#list)

-}
stack :
    MorphIndependently
        (Emptiable (Stacked broadElement) Possibly
         -> Result error_ (List broadElement)
        )
        (List narrowElement
         -> Emptiable (Stacked narrowElement) Possibly
        )
stack =
    Morph.oneToOne Stack.toList Stack.fromList


{-| [`Morph.OneToOne`](Morph#OneToOne) from `Array` to `List`

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


{-| [`Morph.OneToOne`](Morph#OneToOne) from `ArraySized` to `List`

    import ArraySized
    import Morph

    ArraySized.l4 0 1 2 3
        |> Morph.mapTo List.Morph.arraySized
    --> [ 0, 1, 2, 3 ]

[Inverse](Morph#invert) of [`ArraySized.Morph.list`](ArraySized-Morph#list)

-}
arraySized :
    MorphIndependently
        (ArraySized narrowElement narrowRange_
         -> Result error_ (List narrowElement)
        )
        (List broadElement
         -> ArraySized broadElement (Min (Up0 broadX_))
        )
arraySized =
    Morph.oneToOne ArraySized.toList ArraySized.fromList


{-| [`Morph.OneToOne`](Morph#OneToOne) from a `String` to a `List` of `Char`s.

[Inverse](Morph#invert) of [`String.Morph.list`](String-Morph#list)

-}
string : MorphOrError (List Char) String error_
string =
    Morph.oneToOne String.toList String.fromList


{-| [`Morph.OneToOne`](Morph#OneToOne) from `Set` to `List`

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
        |> Morph.over Value.Morph.Internal.composed


{-| [`Morph`](Morph#Morph) all elements.
On the narrowing side all [narrowed](Morph#toNarrow) values must be `Ok`
for it to not result in a [`Morph.Error`](Morph#Error)

If the element [`Morph`](Morph#Morph) is [`OneToOne`](Morph#OneToOne),
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
    Morph.named "all"
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
                    |> Result.mapError Morph.PartsError
        , toBroad =
            \list ->
                list |> List.map (Morph.toBroad elementMorph)
        }


{-| [`Morph.OneToOne`](Morph#OneToOne) [`Bytes`](https://dark.elm.dmy.fr/packages/elm/bytes/latest/)
to a list of individual bits.
Now you can [morph them as a row](Morph#MorphRow)!
-}
bytes : MorphOrError (Emptiable (Stacked Bit) Possibly) Bytes error_
bytes =
    Morph.oneToOne
        (\bytes_ ->
            bytes_
                |> Bytes.Decode.decode (byteList (bytes_ |> Bytes.width))
                |> Maybe.withDefault []
                |> Stack.fromList
        )
        (\bits ->
            let
                bytes_ =
                    bits |> Stack.toList |> List.Linear.toChunksOf Up 8
            in
            bytes_.chunks
                ++ [ bytes_.remainder ]
                |> List.map
                    (\unsignedInt8Bits ->
                        unsignedInt8Bits
                            |> ArraySized.fromList
                            |> BitArray.toN
                            |> N.toInt
                            |> Bytes.Encode.unsignedInt8
                    )
                |> Bytes.Encode.sequence
                |> Bytes.Encode.encode
        )


byteList : Int -> Bytes.Decode.Decoder (List Bit)
byteList length =
    Bytes.Decode.loop ( length, [] ) byteListStep


byteListStep :
    ( Int, List Bit )
    ->
        Bytes.Decode.Decoder
            (Bytes.Decode.Step ( Int, List Bit ) (List Bit))
byteListStep ( n, elements ) =
    if n <= 0 then
        Bytes.Decode.succeed (Bytes.Decode.Done (List.reverse elements))

    else
        Bytes.Decode.map
            (\unsignedInt8 ->
                Bytes.Decode.Loop
                    ( n - 1
                    , (unsignedInt8
                        |> N.intToAtLeast n0
                        |> BitArray.fromN n8
                        |> ArraySized.toList
                      )
                        ++ elements
                    )
            )
            Bytes.Decode.unsignedInt8
