module List.Morph exposing
    ( each
    , for, forBroad
    , string, toString, value, bytes
    )

{-| [`Morph`](Morph#Morph) to and from a `List`


## alter

@docs each


## sequence

@docs for, forBroad


## transform

@docs string, toString, value, bytes

-}

import Array
import ArraySized
import Bit exposing (Bit)
import BitArray
import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import Emptiable exposing (Emptiable)
import Linear exposing (Direction(..))
import List.Linear
import Morph exposing (MorphIndependently, MorphOrError, MorphRow, broad, toBroad, toNarrow)
import Morph.Internal
import N exposing (n0, n8)
import PartialOrComplete exposing (PartialOrComplete(..))
import Possibly exposing (Possibly(..))
import Rope
import Stack exposing (Stacked)
import Value
import Value.Morph exposing (MorphValue)



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


{-| [`Morph.OneToOne`](Morph#OneToOne) from a `String` to a `List` of `Char`s
-}
string : MorphOrError (List Char) String error_
string =
    Morph.oneToOne String.toList String.fromList


{-| [`Morph.OneToOne`](Morph#OneToOne) from a `List` of `Char`s to a `String`
-}
toString : MorphOrError String (List Char) error_
toString =
    Morph.invert string


{-| `List` [`MorphValue`](Value-Morph#MorphValue)
-}
value : MorphValue element -> MorphValue (List element)
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
        |> Morph.over Value.Morph.composed


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
