module ArraySized.Morph exposing
    ( value, toValue
    , list, toList
    , stackEmptiable, toStackEmptiable
    , stackFilled, toStackFilled
    , elementTranslate
    , for, forBroad
    )

{-| [`Morph`](Morph) on an `Array`


## alter

@docs value, toValue


## structure

@docs list, toList
@docs stackEmptiable, toStackEmptiable
@docs stackFilled, toStackFilled


## transform

Also available: [`toggle`](Morph#toggle) [`Array.Extra.reverse`](https://dark.elm.dmy.fr/packages/elm-community/array-extra/latest/Array-Extra#reverse)

@docs elementTranslate


## sequence

@docs for, forBroad

-}

import ArraySized exposing (ArraySized)
import Emptiable exposing (Emptiable)
import Linear exposing (DirectionLinear(..))
import Morph exposing (ErrorWithDeadEnd, Morph, MorphIndependently, MorphOrError, MorphRow, Translate, broad, broadenWith, narrowWith, translate, translateOn)
import N exposing (Add1, Fixed, In, InFixed, InValue, Min, N0, N1, To, Up, n0, n1)
import Possibly exposing (Possibly)
import Stack exposing (Stacked)


value :
    MorphIndependently
        (ArraySized (InValue narrowMin narrowMax) narrowElement
         -> Result error_ (ArraySized (InFixed narrowMin narrowMax) narrowElement)
        )
        (ArraySized (InFixed broadMin broadMax) broadElement
         -> ArraySized (InValue broadMin broadMax) broadElement
        )
value =
    translate ArraySized.fromValue ArraySized.toValue


toValue :
    MorphIndependently
        (ArraySized (InFixed narrowMin narrowMax) narrowElement
         -> Result error_ (ArraySized (InValue narrowMin narrowMax) narrowElement)
        )
        (ArraySized (InValue broadMin broadMax) broadElement
         -> ArraySized (InFixed broadMin broadMax) broadElement
        )
toValue =
    translate ArraySized.toValue ArraySized.fromValue


{-| [`Translate`](Morph#Translate) from `List` to `ArraySized`

    import N exposing (n0)
    import ArraySized

    [ 0, 1, 2, 3 ]
        |> Morph.mapWith ArraySized.Morph.fromList
    --: ArraySized (Min (Up x To x)) number_

-}
list :
    MorphIndependently
        (List narrowElement
         ->
            Result
                error_
                (ArraySized (Min (Up narrowX To narrowX)) narrowElement)
        )
        (ArraySized broadRange_ broadElement
         -> List broadElement
        )
list =
    translate ArraySized.fromList ArraySized.toList


{-| [`Translate`](Morph#Translate) from `ArraySized` to `List`

    import N exposing (n0)
    import ArraySized

    ArraySized.l4 0 1 2 3
        |> ArraySized.minLower n0
        |> Morph.map ArraySized.Morph.toList
    --> [ 0, 1, 2, 3 ]

-}
toList :
    MorphIndependently
        (ArraySized narrowRange_ narrowElement
         -> Result error_ (List narrowElement)
        )
        (List broadElement
         -> ArraySized (Min (Up broadX To broadX)) broadElement
        )
toList =
    translate ArraySized.toList ArraySized.fromList


{-| [`Translate`](Morph#Translate) from `Emptiable (Stacked ...) Possibly` to `ArraySized`

    import N exposing (n0)
    import ArraySized

    Stack.topDown 0 [ 1, 2, 3, 4 ]
        |> Morph.mapWith ArraySized.Morph.stackEmptiable
    --: ArraySized (Min (Up x To x)) number_

Have `>= 1` element (`Emptiable (Stacked ...) Never`)? → [`fromStackFilled`](#fromStackFilled)

-}
stackEmptiable :
    MorphIndependently
        (Emptiable (Stacked narrowElement) Possibly
         ->
            Result
                error_
                (ArraySized (Min (Up narrowX To narrowX)) narrowElement)
        )
        (ArraySized (Min broadRange_) broadElement
         -> Emptiable (Stacked broadElement) Possibly
        )
stackEmptiable =
    translate ArraySized.fromStackEmptiable ArraySized.toStackEmptiable


{-| [`Translate`](Morph#Translate) from `ArraySized` to `Emptiable (Stacked ...) Possibly`

    import N exposing (n0)
    import ArraySized

    ArraySized.l4 0 1 2 3
        |> ArraySized.minLower n0
        |> (ArraySized.Morph.toStackEmptiable |> Morph.map)
    --> Stack.topDown 0 [ 1, 2, 3 ]
    --: Emptiable (Stacked number_) Possibly

Have `>= 1` element (`Emptiable (Stacked ...) Never`)? → [`toStackFilled`](#toStackFilled)

-}
toStackEmptiable :
    MorphIndependently
        (ArraySized (Min narrowRange_) narrowElement
         ->
            Result
                error_
                (Emptiable (Stacked narrowElement) Possibly)
        )
        (Emptiable (Stacked broadElement) Possibly
         -> ArraySized (Min (Up broadX To broadX)) broadElement
        )
toStackEmptiable =
    translate ArraySized.toStackEmptiable ArraySized.fromStackEmptiable


{-| [`Translate`](Morph#Translate) from `Emptiable (Stacked ...) Never` to `ArraySized`

    import N exposing (n0)
    import ArraySized

    Stack.topDown 0 [ 1, 2, 3, 4 ]
        |> Morph.mapWith ArraySized.Morph.stackFilled
    --: ArraySized (Min (Up x To (Add1 x))) number_

Have `>= 1` element (`Emptiable (Stacked ...) Possibly`)? → [`fromStackEmptiable`](#fromStackEmptiable)

-}
stackFilled :
    MorphIndependently
        (Emptiable (Stacked narrowElement) Never
         ->
            Result
                error_
                (ArraySized (Min (Up narrowX To (Add1 narrowX))) narrowElement)
        )
        (ArraySized
            (In (Fixed (Add1 beforeUnmapMinMinus1_)) beforeUnmapMax_)
            broadElement
         -> Emptiable (Stacked broadElement) unmappedNever_
        )
stackFilled =
    translate ArraySized.fromStackFilled ArraySized.toStackFilled


{-| [`Translate`](Morph#Translate) from `ArraySized` to `Emptiable (Stacked ...) Never`

    import N exposing (n0)
    import ArraySized

    ArraySized.l4 0 1 2 3
        |> ArraySized.minLower n0
        |> Morph.map ArraySized.Morph.toStackFilled
    --> Stack.topDown 0 [ 1, 2, 3 ]
    --: Emptiable (Stacked number_) never_

Have `>= 1` element (`Emptiable (Stacked ...) Possibly`)? → [`toStackEmptiable`](#toStackEmptiable)

-}
toStackFilled :
    MorphIndependently
        (ArraySized (In (Fixed (Add1 beforeMapMinMinus1_)) beforeMapMax_) narrowElement
         ->
            Result
                error_
                (Emptiable (Stacked narrowElement) narrowNever_)
        )
        (Emptiable (Stacked broadElement) Never
         -> ArraySized (Min (Up broadX To (Add1 broadX))) broadElement
        )
toStackFilled =
    translate ArraySized.toStackFilled ArraySized.fromStackFilled



--


{-| [`Translate`](Morph#Translate) each element in an `Array`
-}
elementTranslate :
    MorphIndependently
        (narrowBeforeMap -> Result (ErrorWithDeadEnd Never) narrowMapped)
        (broadBeforeUnmap -> broadUnmapped)
    ->
        MorphIndependently
            (ArraySized narrowRange narrowBeforeMap
             ->
                Result
                    error_
                    (ArraySized narrowRange narrowMapped)
            )
            (ArraySized broadRange broadBeforeUnmap
             -> ArraySized broadRange broadUnmapped
            )
elementTranslate elementTranslate_ =
    translateOn ( ArraySized.map, ArraySized.map ) elementTranslate_



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
    -> ArraySized (InFixed min_ max_) element
    -> MorphRow broadElement ()
forBroad morphRowByElement expectedConstantInputArraySized =
    broad
        (ArraySized.repeat ()
            (expectedConstantInputArraySized |> ArraySized.length)
        )
        |> Morph.overRow
            (expectedConstantInputArraySized
                |> for morphRowByElement
            )


type ResultOrSoFar result soFar
    = Result result
    | Partial soFar


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
    -> ArraySized (InFixed min max) element
    -> MorphRow broadElement (ArraySized (InFixed min max) narrow)
for morphRowByElement elementsToTraverseInSequence =
    { description =
        case elementsToTraverseInSequence |> ArraySized.has n1 of
            Err (N.Below _) ->
                { inner = Emptiable.empty, custom = Emptiable.empty }

            Ok only1 ->
                only1 |> ArraySized.to1 |> morphRowByElement |> Morph.description

            Err (N.Above atLeast2) ->
                { custom = Emptiable.empty
                , inner =
                    atLeast2
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
                    case
                        soFar.broad
                            |> narrowWith (traversedElement |> morphRowByElement)
                    of
                        Err error ->
                            error |> Err |> Result

                        Ok stepParsed ->
                            let
                                withStepParsed =
                                    soFar.narrow
                                        |> ArraySized.minPush stepParsed.narrow
                                        |> ArraySized.min n0

                                traversalDone =
                                    withStepParsed
                                        |> ArraySized.hasAtLeast
                                            (elementsToTraverseInSequence
                                                |> ArraySized.length
                                                |> N.maxUp n1
                                            )
                            in
                            case traversalDone of
                                Err notAllTraversed ->
                                    { broad = stepParsed.broad
                                    , narrow = notAllTraversed |> ArraySized.maxNo
                                    }
                                        |> Partial

                                Ok traversed ->
                                    { broad = stepParsed.broad
                                    , narrow =
                                        traversed
                                            |> ArraySized.take
                                                ( Up
                                                , elementsToTraverseInSequence |> ArraySized.length
                                                , { atLeast =
                                                        elementsToTraverseInSequence
                                                            |> ArraySized.length
                                                            |> N.minimumAsDifference
                                                            |> N.specific
                                                  }
                                                )
                                    }
                                        |> Ok
                                        |> Result
        in
        \initialInput ->
            let
                traversed =
                    elementsToTraverseInSequence
                        |> ArraySized.foldFrom
                            ({ narrow = ArraySized.empty |> ArraySized.maxNo
                             , broad = initialInput
                             }
                                |> Partial
                            )
                            Up
                            (\stepElement soFar ->
                                case soFar of
                                    Result result ->
                                        result |> Result

                                    Partial partial ->
                                        partial |> stepFrom stepElement
                            )
            in
            case traversed of
                Result result ->
                    result

                Partial _ ->
                    "lue-bird/elm-morph: `Morph.for` bug! Please open an issue with details on the given `ArraySized`."
                        |> Morph.DeadEnd
                        |> Err
    , broaden =
        \narrowSequence ->
            List.map2
                (\morphInSequence ->
                    broadenWith (morphInSequence |> morphRowByElement)
                )
                (elementsToTraverseInSequence |> ArraySized.toList)
                (narrowSequence |> ArraySized.toList)
                |> List.concatMap Stack.toList
                |> Stack.fromList
    }
