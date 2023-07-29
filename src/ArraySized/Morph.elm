module ArraySized.Morph exposing
    ( each
    , array, list, stack, string
    , exactly, exactlyWith, atLeast, in_
    , sequenceMap, broadSequenceMap
    )

{-| [`Morph`](Morph) for an [`ArraySized element lengthRange`](https://dark.elm.dmy.fr/packages/lue-bird/elm-typesafe-array/latest/)


## alter

@docs each


## structure

@docs array, list, stack, string


## row

@docs exactly, exactlyWith, atLeast, in_
@docs sequenceMap, broadSequenceMap

-}

import Array exposing (Array)
import ArraySized exposing (ArraySized)
import Emptiable exposing (Emptiable)
import Linear exposing (Direction(..))
import Morph exposing (ErrorWithDeadEnd, MorphIndependently, MorphRow, MorphRowIndependently, broad, grab)
import Morph.Internal
import N exposing (In, Min, N, N0, N0OrAdd1, On, To, Up, Up0, n0)
import Rope
import Stack exposing (Stacked)


{-| [`Morph.OneToOne`](Morph#OneToOne) from an `Array`

    import ArraySized
    import Array

    Array.fromList [ 0, 1, 2, 3 ]
        |> Morph.mapTo ArraySized.Morph.fromArray
    --: ArraySized (Min (Up0 x_)) number_

[Inverse](Morph#invert) of [`Array.Morph.arraySized`](Array-Morph#arraySized)

-}
array :
    MorphIndependently
        (Array narrowElement
         ->
            Result
                error_
                (ArraySized narrowElement (Min (Up0 narrowX_)))
        )
        (ArraySized broadElement broadRange_
         -> Array broadElement
        )
array =
    Morph.oneToOne ArraySized.fromArray ArraySized.toArray


{-| [`Morph.OneToOne`](Morph#OneToOne) from a `List`

    import ArraySized
    import Morph

    [ 0, 1, 2, 3 ]
        |> Morph.mapTo ArraySized.Morph.list
    --: ArraySized (Min (Up0 x_)) number_

[Inverse](Morph#invert) of [`List.Morph.arraySized`](List-Morph#arraySized)

-}
list :
    MorphIndependently
        (List narrowElement
         ->
            Result
                error_
                (ArraySized narrowElement (Min (Up0 narrowX_)))
        )
        (ArraySized broadElement broadRange_
         -> List broadElement
        )
list =
    Morph.oneToOne ArraySized.fromList ArraySized.toList


{-| [`Morph.OneToOne`](Morph#OneToOne) from a `String` to `ArraySized ... Char`

    import ArraySized
    import Morph

    "0123"
        |> Morph.mapTo ArraySized.Morph.string
    --: ArraySized (Min (Up0 x_)) number_

[Inverse](Morph#invert) of [`String.Morph.arraySized`](String-Morph#arraySized)

-}
string :
    MorphIndependently
        (String
         ->
            Result
                error_
                (ArraySized Char (Min (Up0 narrowX_)))
        )
        (ArraySized Char broadRange_
         -> String
        )
string =
    Morph.oneToOne ArraySized.fromString ArraySized.toString


{-| [`Morph.OneToOne`](Morph#OneToOne) from a [stack](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Stack)

    import ArraySized
    import Morph
    import Stack

    Stack.topBelow 0 [ 1, 2, 3, 4 ]
        |> Morph.mapTo ArraySized.Morph.stack
    --: ArraySized (Min (Up1 x_)) number_

[Inverse](Morph#invert) of [`Stack.Morph.arraySized`](Stack-Morph#arraySized)

-}
stack :
    MorphIndependently
        (Emptiable (Stacked narrowElement) narrowPossiblyOrNever
         ->
            Result
                error_
                (ArraySized narrowElement (Min (On (N0OrAdd1 narrowPossiblyOrNever N0))))
        )
        (ArraySized broadElement (In (On (N0OrAdd1 broadPossiblyOrNever minFrom1_)) max_)
         -> Emptiable (Stacked broadElement) broadPossiblyOrNever
        )
stack =
    Morph.oneToOne ArraySized.fromStack ArraySized.toStack



--


{-| Morph each element in an `ArraySized`
-}
each :
    MorphIndependently
        (narrowBeforeMap
         -> Result (ErrorWithDeadEnd deadEnd) narrowMapped
        )
        (broadBeforeUnmap -> broadUnmapped)
    ->
        MorphIndependently
            (ArraySized narrowBeforeMap (In narrowMin narrowMax)
             ->
                Result
                    (ErrorWithDeadEnd deadEnd)
                    (ArraySized narrowMapped (In narrowMin narrowMax))
            )
            (ArraySized broadBeforeUnmap broadRange
             -> ArraySized broadUnmapped broadRange
            )
each elementMorph =
    Morph.named "all"
        { description =
            Morph.ElementsDescription (elementMorph |> Morph.description)
        , toNarrow =
            \arraySized ->
                arraySized
                    |> ArraySized.minToOn
                    |> ArraySized.andIndexes
                    |> ArraySized.map
                        (\el ->
                            el.element
                                |> Morph.toNarrow elementMorph
                                |> Result.mapError (\err -> { index = el.index, error = err })
                        )
                    |> ArraySized.minToNumber
                    |> ArraySized.allOk
                    |> Result.mapError
                        (\elements ->
                            elements
                                |> Stack.map
                                    (\_ element ->
                                        { location = element.index |> N.toInt |> String.fromInt
                                        , error = element.error
                                        }
                                    )
                                |> Morph.ElementsError
                        )
        , toBroad =
            \arraySized ->
                arraySized |> ArraySized.map (Morph.toBroad elementMorph)
        }



-- sequence


{-| Match broad [`MorphRow`](Morph#MorphRow)s
(those that can always produce the same broad value)
based on given input elements in sequence.

More details → [`List.Morph.broadSequenceMap`](List-Morph#broadSequenceMap)

-}
broadSequenceMap :
    (element -> MorphRow () broadElement)
    -> ArraySized element (In (On min_) (On max_))
    -> MorphRow () broadElement
broadSequenceMap morphRowByElement expectedConstantInputArraySized =
    broad
        (ArraySized.repeat ()
            (expectedConstantInputArraySized |> ArraySized.length)
        )
        |> Morph.overRow
            (sequenceMap morphRowByElement expectedConstantInputArraySized)


sequence :
    ArraySized
        (MorphRowIndependently
            elementBeforeToBroad
            elementNarrow
            broadElement
        )
        (In min max)
    ->
        MorphRowIndependently
            (ArraySized elementBeforeToBroad (In min max))
            (ArraySized elementNarrow (In min max))
            broadElement
sequence toSequence =
    { description =
        case toSequence |> ArraySized.toList of
            [] ->
                Morph.succeed ArraySized.empty |> Morph.description

            inSequence0 :: inSequence1Up ->
                Morph.Internal.sequenceDescriptionFromStack
                    (Stack.topBelow inSequence0 inSequence1Up |> Stack.map (\_ -> Morph.description))
    , toNarrow =
        \initialInput ->
            let
                traversed =
                    toSequence
                        |> ArraySized.mapFoldFrom
                            { broad = initialInput, startsDown = initialInput |> List.length |> Stack.one }
                            Up
                            (\state ->
                                case state.folded.broad |> Morph.toNarrow state.element of
                                    Ok parsed ->
                                        { element = parsed.narrow |> Ok
                                        , folded =
                                            { broad = parsed.broad
                                            , startsDown =
                                                state.folded.startsDown
                                                    |> Stack.onTopLay (parsed.broad |> List.length)
                                            }
                                        }

                                    Err error ->
                                        { element = error |> Err
                                        , folded =
                                            { broad = state.folded.broad
                                            , startsDown = state.folded.startsDown
                                            }
                                        }
                            )
            in
            case traversed.mapped |> ArraySized.allOk of
                Err error ->
                    case toSequence |> ArraySized.length |> N.toInt of
                        1 ->
                            error |> Stack.top |> Err

                        _ ->
                            Morph.Internal.inSequenceErrorWith
                                { startsDown = traversed.folded.startsDown
                                , error = error |> Stack.top
                                }
                                |> Err

                Ok sequenceArraySized ->
                    { narrow = sequenceArraySized, broad = traversed.folded.broad } |> Ok
    , toBroad =
        \narrowSequence ->
            List.map2
                (\morphRowSequence narrow -> narrow |> Morph.toBroad morphRowSequence)
                (toSequence |> ArraySized.toList)
                (narrowSequence |> ArraySized.toList)
                |> Rope.fromList
                |> Rope.concat
    }


{-| From the elements in a given `ArraySized`,
create [`MorphRow`](Morph#MorphRow)s
that will be run in the same order, one after the other.

More details → [`List.Morph.sequenceMap`](List-Morph#sequenceMap)

-}
sequenceMap :
    (element
     -> MorphRow elementNarrow broadElement
    )
    -> ArraySized element (In min max)
    ->
        MorphRow
            (ArraySized elementNarrow (In min max))
            broadElement
sequenceMap morphRowByElement elementsToTraverseInSequence =
    elementsToTraverseInSequence |> ArraySized.map morphRowByElement |> sequence



--- row


{-| Match a value a given number of times
and return them as an [`ArraySized`](https://package.elm-lang.org/packages/lue-bird/elm-typesafe-array/latest/ArraySized)

> ℹ️ Equivalent regular expression: `{n}`

    import Morph
    import Char.Morph
    import String.Morph
    import AToZ exposing (AToZ(..))
    import AToZ.Morph
    import N exposing (n3)
    import ArraySized
    import List.Morph

    -- we want `exactly 3` letters
    "abc"
        |> Morph.toNarrow
            (exactly n3 (AToZ.Morph.lowerChar |> Morph.one)
                |> Morph.rowFinish
                |> Morph.over List.Morph.string
            )
        |> Result.map ArraySized.toList
    --> Ok [ A, B, C ]

    -- not 2 or 4, we want 3
    "ab"
        |> Morph.toNarrow
            (exactly n3 (AToZ.Morph.lowerChar |> Morph.one)
                |> Morph.rowFinish
                |> Morph.over List.Morph.string
            )
        |> Result.toMaybe
    --> Nothing

-}
exactly :
    N (In min max)
    -> MorphRowIndependently elementBeforeToBroad elementNarrow broadElement
    ->
        MorphRowIndependently
            (ArraySized elementBeforeToBroad (In min max))
            (ArraySized elementNarrow (In min max))
            broadElement
exactly repeatCount repeatedMorphRow =
    sequence (ArraySized.repeat repeatedMorphRow repeatCount)


{-| Depending on the number parsed by a given morph,
morph exactly that amount of elements.

To for example morph string bits
where the first 16 bits represent the amount of code points that follow:

    ArraySized.Morph.exactlyWith
        (N.Morph.natural |> Morph.overRow (Natural.Morph.bits n16 Bytes.BE))
        Char.Morph.bits

The name is pretty horrible, so suggestions are welcome!

If you need to parse a format like

    [ length ] [ other necessary data you want to grab ] [ elements ]

Please open an issue and I'll add a version for it.
This isn't exposed currently because the API for this would be jAnKy and the use-case kinda obscure.


#### 🐦 Developer's note: Why there is no generic version like

    using :
        (complete -> toDecide)
        -> MorphRow toDecide broadElement
        -> (toDecide -> MorphRow complete broadElement)
        -> MorphRow complete broadElement

This might look like the happy "andThen" that you love
but it's more evil than that!

For example, you'd typically use "andThen" for versioning etc. – not with morphs.
Use [`Morph.choice`](Morph#choice) where each possibility expects a specific number.

`using` is only unavoidable
when certain data that tells you how to proceed can have infinite possibilities.

Here's some ugly consequences if we use `using` instead of a less generic `exactlyWith`:

  - Its [`description`](Morph#description) can barely tell users anything,
    as our main morph is hidden behind a function :(

    Adding [`Morph.named`](Morph#named) to the whole `using` morph helps but the quality is still notably lower.

  - Instead of recursively using `using`, you should use
    [`untilNext`](Morph#untilNext), [`untilLast`](Morph#untilNext), [`untilNextFold`](Morph#untilNextFold), [`untilLastFold`](Morph#untilLastFold)
    to not blow the stack and get a nicer [`description`](Morph#description)

  - Its harder to reason about

  - It's less composable / less "independent"

  - It's easier than ever to get your [`toBroad`](Morph#toBroad) and [`toNarrow`](Morph#toNarrow)
    functions out of sync

-}
exactlyWith :
    MorphRow (N range) broadElement
    ->
        MorphRowIndependently
            elementNarrow
            elementBeforeToBroad
            broadElement
    ->
        MorphRowIndependently
            (ArraySized elementNarrow range)
            (ArraySized elementBeforeToBroad range)
            broadElement
exactlyWith lengthMorphRow elementMorphRow =
    { description =
        Morph.SequenceDescription
            { early =
                lengthMorphRow |> Morph.description
            , late = elementMorphRow |> Morph.description
            }
    , toBroad =
        \beforeToBroad ->
            (beforeToBroad |> ArraySized.length)
                |> Morph.toBroad lengthMorphRow
                |> Rope.prependTo
                    (beforeToBroad
                        |> ArraySized.toList
                        |> List.map (Morph.toBroad elementMorphRow)
                        |> Rope.fromList
                        |> Rope.concat
                    )
    , toNarrow =
        \initialBeforeToNarrow ->
            (case initialBeforeToNarrow |> Morph.toNarrow lengthMorphRow of
                Err lengthError ->
                    Morph.CountError lengthError |> Err

                Ok length ->
                    let
                        traversed :
                            { mapped : ArraySized (Result Morph.Error elementNarrow) range
                            , folded : { broad : List broadElement, startsDown : Emptiable (Stacked Int) never_ }
                            }
                        traversed =
                            ArraySized.repeat elementMorphRow length.narrow
                                |> ArraySized.mapFoldFrom
                                    { broad = length.broad
                                    , startsDown = length.broad |> List.length |> Stack.one
                                    }
                                    Up
                                    (\state ->
                                        case state.folded.broad |> Morph.toNarrow state.element of
                                            Ok parsed ->
                                                { element = parsed.narrow |> Ok
                                                , folded =
                                                    { broad = parsed.broad
                                                    , startsDown =
                                                        state.folded.startsDown
                                                            |> Stack.onTopLay (parsed.broad |> List.length)
                                                    }
                                                }

                                            Err error ->
                                                { element = error |> Err
                                                , folded =
                                                    { broad = state.folded.broad
                                                    , startsDown = state.folded.startsDown
                                                    }
                                                }
                                    )
                    in
                    case traversed.mapped |> ArraySized.allOk of
                        Err error ->
                            { startsDownInBroadList = traversed.folded.startsDown
                            , error = error |> Stack.top
                            }
                                |> Morph.ExactlyCountElementSequenceError
                                |> Err

                        Ok sequenceArraySized ->
                            { narrow = sequenceArraySized, broad = traversed.folded.broad } |> Ok
            )
                |> Result.mapError Morph.CountAndExactlyElementSequenceError
    }


{-| Match a value at least a given number of times
and return them as an [`ArraySized`](https://package.elm-lang.org/packages/lue-bird/elm-typesafe-array/latest/ArraySized).

> ℹ️ Equivalent regular expression: `{min,}`

    import Morph
    import List.Morph
    import AToZ.Morph
    import AToZ
    import ArraySized
    import N exposing (n3)

    -- we want at least three letters, we are okay with more than three
    "abcdef"
        |> Morph.toNarrow
            (ArraySized.Morph.atLeast n3 (AToZ.Morph.lowerChar |> Morph.one)
                |> Morph.rowFinish
                |> Morph.over List.Morph.string
            )
        |> Result.map ArraySized.toList
    --> Ok [ A, B, C, D, E, F ]

    -- but not two, that's sacrilegious
    "ab"
        |> Morph.toNarrow
            (ArraySized.Morph.atLeast n3 (AToZ.Morph.lowerChar |> Morph.one)
                |> Morph.rowFinish
                |> Morph.over List.Morph.string
            )
        |> Result.toMaybe
    --> Nothing


### `atLeast n0`

> ℹ️ Equivalent regular expression: `*`

    import Morph
    import List.Morph
    import AToZ.Morph
    import AToZ exposing (AToZ(..))
    import ArraySized
    import N exposing (n0)

    -- We want as many letters as there are.
    "abc"
        |> Morph.toNarrow
            (ArraySized.Morph.atLeast n0 (AToZ.Morph.lowerChar |> Morph.one)
                |> Morph.rowFinish
                |> Morph.over List.Morph.string
            )
        |> Result.map ArraySized.toList
    --> Ok [ A, B, C ]

    -- even zero letters is okay
    ""
        |> Morph.toNarrow
            (ArraySized.Morph.atLeast n0 (AToZ.Morph.lowerChar |> Morph.one)
                |> Morph.rowFinish
                |> Morph.over List.Morph.string
            )
        |> Result.map ArraySized.toList
    --> Ok []

As you can see, `atLeast n0` will never fail.

If you want a to morph a `List` instead of an `ArraySized ... (Min (On N0))`,
you might as well use [`Morph.whilePossible`](Morph#whilePossible) instead of

    ArraySized.toList
        |> Morph.overRow (ArraySized.Morph.atLeast n0)


### `atLeast n1`

> ℹ️ Equivalent regular expression: `+`

    import Morph
    import List.Morph
    import AToZ.Morph
    import AToZ exposing (AToZ(..))
    import ArraySized
    import N exposing (n1)

    -- we want as many letters as there are
    "abc"
        |> Morph.toNarrow
            (ArraySized.Morph.atLeast n1 (AToZ.Morph.lowerChar |> Morph.one)
                |> Morph.rowFinish
                |> Morph.over List.Morph.string
            )
        |> Result.map ArraySized.toList
    --> Ok [ A, B, C ]

    -- but we want at least one
    ""
        |> Morph.toNarrow
            (ArraySized.Morph.atLeast n1 (AToZ.Morph.lowerChar |> Morph.one)
                |> Morph.rowFinish
                |> Morph.over List.Morph.string
            )
        |> Result.toMaybe
    --> Nothing


### example: interspersed separators

    import Stack
    import Morph
    import ArraySized.Morph exposing (atLeast)
    import List.Morph
    import AToZ.Morph
    import AToZ exposing (AToZ(..))
    import N exposing (n0)


    tag =
        atLeast n1 (AToZ.Morph.lowerChar |> Morph.one)

    tags =
        Morph.succeed Stack.onTopLay
            |> grab Stack.top tag
            |> grab Stack.removeTop
                (List.Morph.arraySized
                    |> Morph.overRow
                        (atLeast n0
                            (Morph.succeed (\tag -> tag)
                                |> match separator
                                |> grab (\tag -> tag) tag
                            )
                        )
                )

    "a,bc,def"
        |> Morph.toNarrow (tags |> Morph.rowFinish |> Morph.over List.Morph.string)
        |> Result.map (Stack.map (\_ -> ArraySized.toList))
    --→ Ok
    --→     (Stack.topBelow [ A ]
    --→         [ [ B, C ]
    --→         , [ D, E, F ]
    --→         ]
    --→     )


### anti-example: parsing infinitely

    Morph.succeed ...
        |> grab ... (atLeast n0 (Morph.keep |> Morph.one))
        |> grab ...

would only parse the first part until the end
because it always [`succeed`](Morph#succeed)s.
Nothing after would ever be parsed, making the whole thing fail.


### minimum type explanation

The maximum of the lower limit argument enables what's shown in the following example:
"match any number of spaces and broaden to 1"

    broad (ArraySized.one ())
        |> Morph.overRow (atLeast n0 (String.Morph.only " "))

In this case, the minimum of the given "seed" before broadening `ArraySized.one ()` is 1,
whereas the narrow result of `atLeast n0` will have a minimum length of 0.

The maximum of the lower limit argument is a type-level proof that the "seed" minimum is greater
or equal to the resulting narrow minimum length.

↓ for example will lead to a compile time error:

    broad ArraySized.empty
        |> Morph.overRow (atLeast n1 (String.Morph.only " "))

> The argument to `|>` is of type:
>
>     ... ArraySized () (In #(On N0)# (Up0 maxX_)) ...
>
> But it needs to be:
>
>     ... ArraySized () (In #(On N1)# (Up0 maxX_)) ...

-}
atLeast :
    N (In (On lowerLimit) (Up lowerLimitToBroad_ To broadLowerLimit))
    -> MorphRowIndependently narrow beforeToBroad broadElement
    ->
        MorphRowIndependently
            (ArraySized narrow (Min (On lowerLimit)))
            (ArraySized beforeToBroad (In (On broadLowerLimit) max_))
            broadElement
atLeast minimum elementStepMorphRow =
    Morph.oneToOne identity ArraySized.maxToInfinity
        |> Morph.overRow
            (Morph.succeed
                (\minimumArraySized overMinimum ->
                    minimumArraySized
                        |> ArraySized.attachMin Up overMinimum
                )
                |> grab
                    (ArraySized.take Up { atLeast = minimum } minimum)
                    (exactly minimum elementStepMorphRow)
                |> grab
                    (\arr -> arr |> ArraySized.dropMin Up minimum |> ArraySized.minTo0)
                    (list |> Morph.overRow (Morph.whilePossible elementStepMorphRow))
            )


{-| Match a value between a minimum and maximum number of times
and return them as an [`ArraySized`](https://package.elm-lang.org/packages/lue-bird/elm-typesafe-array/latest/ArraySized).

> ℹ️ Equivalent regular expression: `{min,max}`

    import Morph
    import AToZ.Morph
    import AToZ exposing (AToZ(..))
    import List.Morph
    import ArraySized
    import N exposing (n2, n4)

    -- we want between two and four letters
    "abc"
        |> Morph.toNarrow
            (in_ ( n2, n4 ) (AToZ.Morph.lowerChar |> Morph.one)
                |> Morph.rowFinish
                |> Morph.over List.Morph.string
            )
        |> Result.map ArraySized.toList
    --> Ok [ A, B, C ]


    "a"
        |> Morph.toNarrow
            (in_ ( n2, n4 ) (AToZ.Morph.lowerChar |> Morph.one)
                |> Morph.rowFinish
                |> Morph.over List.Morph.string
            )
        |> Result.toMaybe
    --> Nothing


### `in_ ( n0, n1 )`

Alternative to [`Maybe.Morph.row`](Maybe-Morph#row) which instead returns an `ArraySized`.

> ℹ️ Equivalent regular expression: `?`


### example: at most

> ℹ️ Equivalent regular expression: `{0,max}`

    import Morph
    import AToZ exposing (AToZ(..))
    import AToZ.Morph
    import List.Morph
    import ArraySized
    import N exposing (n0, n3)

    -- we want a maximum of three letters
    "ab"
        |> Morph.toNarrow
            (in_ ( n0, n3 ) (AToZ.Morph.lowerChar |> Morph.one)
                |> Morph.rowFinish
                |> Morph.over List.Morph.string
            )
        |> Result.map ArraySized.toList
    --> Ok [ A, B ]

    -- even zero letters are fine
    ""
        |> Morph.toNarrow
            (in_ ( n0, n3 ) (AToZ.Morph.lowerChar |> Morph.one)
                |> Morph.rowFinish
                |> Morph.over List.Morph.string
            )
        |> Result.map ArraySized.toList
    --> Ok []

-}
in_ :
    ( N (In min (Up minToMaxMin_ To maxMin))
    , N (In (On maxMin) max)
    )
    -> MorphRow element broadElement
    ->
        MorphRow
            (ArraySized element (In min max))
            broadElement
in_ ( lowerLimit, upperLimit ) repeatedElementMorphRow =
    Morph.oneToOne identity ArraySized.maxToOn
        |> Morph.overRow
            (Morph.succeed
                (\minimumList overMinimum ->
                    minimumList
                        |> ArraySized.minToOn
                        |> ArraySized.attachMin Up overMinimum
                        |> ArraySized.minToNumber
                        |> ArraySized.takeCurrentMin Up (upperLimit |> N.minTo lowerLimit)
                )
                |> grab
                    (ArraySized.takeCurrentMin Up lowerLimit)
                    (exactly lowerLimit repeatedElementMorphRow)
                |> grab
                    (\arraySized ->
                        arraySized |> ArraySized.dropMax Up (lowerLimit |> N.minTo0)
                    )
                    (atMost
                        (upperLimit
                            |> N.maxToOn
                            |> N.subtractMax (lowerLimit |> N.minTo0)
                        )
                        repeatedElementMorphRow
                    )
            )


{-| morph a given element less or equal to a given number of times

**Shouldn't be exposed**

-}
atMost :
    N (In (On upperLimitMin_) (Up maxX To maxPlusX))
    -> MorphRow element broadElement
    ->
        MorphRowIndependently
            (ArraySized element (In (Up0 narrowMinX_) (Up maxX To maxPlusX)))
            (ArraySized element (In beforeToNarrowMin_ (Up maxX To maxPlusX)))
            broadElement
atMost upperLimit element =
    Morph.named
        ([ "repeating ≤", upperLimit |> N.toString ]
            |> String.concat
        )
        (Morph.oneToOne identity ArraySized.minTo0
            |> Morph.overRow
                (let
                    possibleCounts : List (N (In (Up0 nMinX_) (Up maxX To maxPlusX)))
                    possibleCounts =
                        ArraySized.n1To upperLimit
                            |> ArraySized.map N.minTo0
                            |> ArraySized.toList
                            |> List.reverse
                 in
                 Morph.oneToOne .info (\value -> { tag = n0 |> N.maxTo upperLimit, info = value })
                    |> Morph.over
                        (Morph.tryTopToBottom
                            (\count -> exactly count element)
                            (Stack.fromList possibleCounts
                                |> Stack.attachAdapt Up (Stack.one (n0 |> N.maxTo upperLimit))
                            )
                        )
                )
        )
