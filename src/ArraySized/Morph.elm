module ArraySized.Morph exposing
    ( each
    , array, toArray
    , list, toList
    , stack, toStack
    , string, toString
    , for, forBroad
    , exactly, exactlyWith, atLeast, in_
    )

{-| [`Morph`](Morph) on an `Array`


## alter

@docs each


## structure

@docs array, toArray
@docs list, toList
@docs stack, toStack
@docs string, toString


## row

@docs for, forBroad
@docs exactly, exactlyWith, atLeast, in_

-}

import Array exposing (Array)
import ArraySized exposing (ArraySized)
import Emptiable exposing (Emptiable)
import Linear exposing (Direction(..))
import Morph exposing (ErrorWithDeadEnd, MorphIndependently, MorphRow, MorphRowIndependently, broad, grab, oneToOne, toBroad, toNarrow)
import Morph.Internal
import N exposing (Exactly, In, Min, N, N0, N0OrAdd1, On, To, Up, Up0, n0)
import Rope
import Stack exposing (Stacked)


{-| [`OneToOne`](Morph#OneToOne) from `Array` to `ArraySized`

    import ArraySized
    import Array

    Array.fromList [ 0, 1, 2, 3 ]
        |> Morph.mapTo ArraySized.Morph.fromArray
    --: ArraySized (Min (Up0 x_)) number_

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
    oneToOne ArraySized.fromArray ArraySized.toArray


{-| [`OneToOne`](Morph#OneToOne) from `ArraySized` to `Array`

    import ArraySized
    import Array

    ArraySized.l4 0 1 2 3
        |> Morph.mapTo ArraySized.Morph.toArray
    --> Array.fromList [ 0, 1, 2, 3 ]

-}
toArray :
    MorphIndependently
        (ArraySized narrowElement narrowRange_
         -> Result error_ (Array narrowElement)
        )
        (Array broadElement
         -> ArraySized broadElement (Min (Up0 broadX_))
        )
toArray =
    Morph.invert array


{-| [`OneToOne`](Morph#OneToOne) from `List` to `ArraySized`

    import ArraySized

    [ 0, 1, 2, 3 ]
        |> Morph.mapTo ArraySized.Morph.list
    --: ArraySized (Min (Up0 x_)) number_

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
    oneToOne ArraySized.fromList ArraySized.toList


{-| [`OneToOne`](Morph#OneToOne) from `ArraySized` to `List`

    import ArraySized

    ArraySized.l4 0 1 2 3
        |> Morph.mapTo ArraySized.Morph.toList
    --> [ 0, 1, 2, 3 ]

-}
toList :
    MorphIndependently
        (ArraySized narrowElement narrowRange_
         -> Result error_ (List narrowElement)
        )
        (List broadElement
         -> ArraySized broadElement (Min (Up0 broadX_))
        )
toList =
    Morph.invert list


{-| [`OneToOne`](Morph#OneToOne) from `String` to `ArraySized`

    import ArraySized

    "0123"
        |> Morph.mapTo ArraySized.Morph.string
    --: ArraySized (Min (Up0 x_)) number_

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
    oneToOne ArraySized.fromString ArraySized.toString


{-| [`OneToOne`](Morph#OneToOne) from `ArraySized` to `String`

    import ArraySized

    ArraySized.l4 0 1 2 3
        |> Morph.mapTo ArraySized.Morph.toString
    --> "0123"

-}
toString :
    MorphIndependently
        (ArraySized Char narrowRange_
         -> Result error_ String
        )
        (String
         -> ArraySized Char (Min (Up0 broadX_))
        )
toString =
    Morph.invert string


{-| [`OneToOne`](Morph#OneToOne) from `Emptiable (Stacked ...) ...` to `ArraySized`

    import ArraySized

    Stack.topBelow 0 [ 1, 2, 3, 4 ]
        |> Morph.mapTo ArraySized.Morph.stack
    --: ArraySized (Min (Up1 x_)) number_

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
    oneToOne ArraySized.fromStack ArraySized.toStack


{-| [`OneToOne`](Morph#OneToOne) from `ArraySized` to `Emptiable (Stacked ...) ...`

    import ArraySized

    ArraySized.l4 0 1 2 3
        |> Morph.mapTo ArraySized.Morph.toStack
    --> Stack.topBelow 0 [ 1, 2, 3 ]

-}
toStack :
    MorphIndependently
        (ArraySized narrowElement (In (On (N0OrAdd1 narrowPossiblyOrNever minFrom1_)) max_)
         ->
            Result
                error_
                (Emptiable (Stacked narrowElement) narrowPossiblyOrNever)
        )
        (Emptiable (Stacked broadElement) broadPossiblyOrNever
         -> ArraySized broadElement (Min (On (N0OrAdd1 broadPossiblyOrNever N0)))
        )
toStack =
    Morph.invert stack



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
    { description =
        { custom = Stack.one "all"
        , inner = Morph.ElementsDescription (elementMorph |> Morph.description)
        }
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
    (element -> MorphRow () broadElement)
    -> ArraySized element (In (On min_) (On max_))
    -> MorphRow () broadElement
forBroad morphRowByElement expectedConstantInputArraySized =
    broad
        (ArraySized.repeat ()
            (expectedConstantInputArraySized |> ArraySized.length)
        )
        |> Morph.overRow
            (for morphRowByElement expectedConstantInputArraySized)


sequence :
    ArraySized (MorphRow elementNarrow broadElement) (In min max)
    ->
        MorphRow
            (ArraySized elementNarrow (In min max))
            broadElement
sequence toSequence =
    -- this implementation looks a bit... unfortunate
    -- it could be simplified by either
    --     - allowing empty and singleton sequence (this is a bit ugly)
    --     - developing a better mechanism for ArraySized length matching (this might be hard)
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
                                case state.folded.broad |> toNarrow state.element of
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
                (\morphRowSequence -> toBroad morphRowSequence)
                (toSequence |> ArraySized.toList)
                (narrowSequence |> ArraySized.toList)
                |> Rope.fromList
                |> Rope.concat
    }


{-| [`grab`](Morph#grab) the elements of a given `List` of [`MorphRow`](Morph#MorphRow)s in order

Some also call this "traverse"

Don't try to be clever with this.

    import Morph exposing (one)
    import Char.Morph as Char
    import String.Morph as Text

    "AB"
        |> narrow
            (Morph.named (Char.Morph.caseNo >> one) [ 'a', 'b' ]
                |> Morph.rowFinish
                |> Morph.over Stack.Morph.string
            )
    --> Ok [ 'a', 'b' ]

The usual [`Morph.succeed`](Morph#succeed)`(\... -> ...) |>`[`grab`](Morph#grab)-[`match`](Morph#match) chain
is often more explicit, descriptive and type-safe.

Because of this, `MorphRow` only exposes `for`, not `sequence`,
making misuse a bit more obvious.

-}
for :
    (element
     -> MorphRow elementNarrow broadElement
    )
    -> ArraySized element (In min max)
    ->
        MorphRow
            (ArraySized elementNarrow (In min max))
            broadElement
for morphRowByElement elementsToTraverseInSequence =
    elementsToTraverseInSequence |> ArraySized.map morphRowByElement |> sequence



--- row


{-| Match a value a given number of times
and return them as an [`ArraySized`](https://package.elm-lang.org/packages/lue-bird/elm-typesafe-array/latest/ArraySized)

> ℹ️ Equivalent regular expression: `{n}`

    import Morph.Error
    import Char.Morph as Char
    import String.Morph as Text
    import N exposing (n3)

    -- we want `exactly 3` letters
    "abcdef" |> narrow (map Text.fromList (exactly n3 AToZ.char))
    --> Ok [ 'a', 'b', 'c' ]

    -- not 2 or 4, we want 3
    "ab_def"
        |> narrow (map Text.fromList (exactly n3 AToZ.char))
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:3: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '_'."

-}
exactly :
    N (In min max)
    -> MorphRow element broadElement
    ->
        MorphRow
            (ArraySized element (In min max))
            broadElement
exactly repeatCount repeatedMorphRow =
    Morph.named ([ "repeating ", repeatCount |> N.toString ] |> String.concat)
        (sequence (ArraySized.repeat repeatedMorphRow repeatCount))


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

  - Instead of recursively using `using`, you should use [`until`](Morph#until), [`untilFold`](Morph#untilFold)
    to not blow the stack and get a nicer [`description`](Morph#description)

  - Its harder to reason about

  - It's less composable / less "independent"

  - It's easier than ever to get your [`toBroad`](Morph#toBroad) and [`toNarrow`](Morph#toNarrow)
    functions out of sync

-}
exactlyWith :
    MorphRow (N range) broadElement
    -> MorphRow element broadElement
    -> MorphRow (ArraySized element range) broadElement
exactlyWith lengthMorphRow elementMorphRow =
    { description =
        { inner =
            Morph.SequenceDescription
                { early =
                    lengthMorphRow |> Morph.description
                , late = elementMorphRow |> Morph.description
                }
        , custom = Emptiable.empty
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
                            { mapped : ArraySized (Result Morph.Error element) range
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
                                        case state.folded.broad |> toNarrow state.element of
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

    import Morph.Error
    import Char.Morph as Char
    import String.Morph as Text

    -- we want at least three letters, we are okay with more than three
    "abcdef"
        |> Text.toNarrow (atLeast n3 AToZ.char)
    --> Ok [ 'a', 'b', 'c', 'd', 'e', 'f' ]

    -- but not two, that's sacrilegious
    "ab_def"
        |> Text.toNarrow (atLeast n3 AToZ.char)
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:3: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '_'."


### `atLeast n0`

> ℹ️ Equivalent regular expression: `*`

    import Char.Morph as Char
    import String.Morph as Text

    -- We want as many letters as there are.
    "abc" |> Text.toNarrow (atLeast n0 AToZ.char)
    --> Ok [ 'a', 'b', 'c' ]

    "abc123" |> Text.toNarrow (atLeast n0 AToZ.char)
    --> Ok [ 'a', 'b', 'c' ]

    -- even zero letters is okay
    "123abc" |> Text.toNarrow (atLeast n0 AToZ.char)
    --> Ok []


### `atLeast n1`

> ℹ️ Equivalent regular expression: `+`

    import N exposing (n1)
    import Morph.Error
    import Char.Morph as Char
    import String.Morph as Text

    -- we want as many letters as there are
    "abc" |> Text.toNarrow (atLeast n1 AToZ.char)
    --> Ok [ 'a', 'b', 'c' ]

    "abc123" |> Text.toNarrow (atLeast n1 AToZ.char)
    --> Ok [ 'a', 'b', 'c' ]

    -- but we want at least one
    "123abc"
        |> Text.toNarrow (atLeast n1 AToZ.char)
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:1: I was expecting a letter a|..|z or A|...|Z. I got stuck when I got the character '1'."


### example: interspersed separators

    import Stack
    import Morph exposing (separatedBy, atLeast, one)
    import String.Morph as Text exposing (text)
    import Char.Morph as Char


    tag =
        atLeast n0 (Morph.AToZ.caseAnyLower |> one)

    tags =
        Morph.succeed Stack.onTopLay
            |> grab Stack.top tag
            |> grab Stack.removeTop
                (ArraySized.Morph.toStack
                    |> Morph.overRow
                        (atLeast n0
                            (Morph.succeed (\tag -> tag)
                                |> match separator
                                |> grab (\tag -> tag) tag
                            )
                        )
                )

    -- note that both values and separators must be of the same type
    "a,bc,def"
        |> Text.toNarrow tags
    --> Ok
    -->     { first = [ 'a' ]
    -->     , afterFirst =
    -->         [ { separator = (), part = [ 'b', 'c' ] }
    -->         , { separator = (), part = [ 'd', 'e', 'f' ] }
    -->         ]
    -->     }

    ",a,,"
        |> Text.toNarrow tags
    --> Ok
    -->     (Stack.topBelow
    -->         []
    -->         [ { separator = (), part = [ 'a' ] }
    -->         , { separator = (), part = [] }
    -->         , { separator = (), part = [] }
    -->         ]
    -->     )

    -- an empty input text gives a single element from an empty string
    ""
        |> Text.toNarrow tags
    --> Ok (topDown [] [])


### anti-example: parsing infinitely

    Morph.succeed ...
        |> grab (atLeast n0 (Morph.keep |> Morph.one))
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
    -> MorphRow narrow broadElement
    ->
        MorphRowIndependently
            (ArraySized narrow (In (On broadLowerLimit) max_))
            (ArraySized narrow (Min (On lowerLimit)))
            broadElement
atLeast minimum elementStepMorphRow =
    Morph.named
        ([ "repeating ≥ ", minimum |> N.toString ] |> String.concat)
        (Morph.oneToOne identity ArraySized.maxToInfinity
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
        )


{-| Match a value between a minimum and maximum number of times
and return them as an [`ArraySized`](https://package.elm-lang.org/packages/lue-bird/elm-typesafe-array/latest/ArraySized).

> ℹ️ Equivalent regular expression: `{min,max}`

    import Morph.Error
    import Char.Morph as Char
    import String.Morph as Text

    -- we want between two and four letters
    "abcdef" |> Text.toNarrow (in_ ( n2, n4 ) AToZ.char)
    --> Ok [ 'a', 'b', 'c', 'd' ]

    "abc_ef" |> Text.toNarrow (in_ ( n2, n4 ) AToZ.char)
    --> Ok [ 'a', 'b', 'c' ]

    "ab_def" |> Text.toNarrow (in_ ( n2, n4 ) AToZ.char)
    --> Ok [ 'a', 'b' ]


    -- but less than that is not cool
    "i_am_here"
        |> Text.toNarrow (in_ ( n2, n3 ) letter)
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:2: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '_'."


### example: `in_ ( n0, n1 )`

Alternative to [`Maybe.Morph.row`](Maybe-Morph#row) which instead returns a `List`.

> ℹ️ Equivalent regular expression: `?`

    import Char.Morph as Char
    import String.Morph as Text

    -- we want one letter, optionally
    "abc" |> Text.toNarrow (in_ ( n0, n1 ) AToZ.char)
    --> Ok [ 'a' ]

    -- if we don't get any, that's still okay
    "123abc" |> Text.toNarrow (in_ ( n0, n1 ) AToZ.char)
    --> Ok []


### example: at most

> ℹ️ Equivalent regular expression: `{0,max}`

    import Morph
    import Char.Morph as Char
    import String.Morph as Text

    -- we want a maximum of three letters
    "abcdef" |> Text.toNarrow (in_ ( n0, n3 ) AToZ.char)
    --> Ok [ 'a', 'b', 'c' ]

    -- less than that is also okay
    "ab_def" |> Text.toNarrow (in_ ( n0, n3 ) AToZ.char)
    --> Ok [ 'a', 'b' ]

    -- even zero letters are fine
    "_underscore" |> Text.toNarrow (in_ ( n0, n3 ) AToZ.char)
    --> Ok []

    -- make sure we don't consume more than three letters
    "abcdef"
        |> Text.toNarrow
            (Morph.succeed (\letters -> letters)
                |> grab (in_ ( n0, n3 ) AToZ.char)
                |> match (one 'd')
            )
    --> Ok [ 'a', 'b', 'c' ]

-}
in_ :
    ( N (Exactly (On min))
    , N (In (On min) (On max))
    )
    -> MorphRow element broadElement
    ->
        MorphRow
            (ArraySized element (In (On min) (On max)))
            broadElement
in_ ( lowerLimit, upperLimit ) repeatedElementMorphRow =
    Morph.named
        ([ "repeating "
         , lowerLimit |> N.toString
         , ".."
         , upperLimit |> N.toString
         ]
            |> String.concat
        )
        (oneToOne identity (ArraySized.minTo lowerLimit)
            |> Morph.overRow
                (Morph.succeed
                    (\minimumList overMinimum ->
                        minimumList
                            |> ArraySized.attachMin Up
                                (overMinimum |> ArraySized.minTo n0)
                            |> ArraySized.minTo lowerLimit
                            |> ArraySized.take Up { atLeast = lowerLimit } upperLimit
                    )
                    |> grab
                        (ArraySized.take Up { atLeast = lowerLimit } lowerLimit)
                        (exactly lowerLimit repeatedElementMorphRow)
                    |> grab
                        (\arraySized ->
                            arraySized
                                |> ArraySized.dropMin Up lowerLimit
                                |> ArraySized.maxToInfinity
                                |> ArraySized.maxToOn
                        )
                        (atMost
                            ((upperLimit |> N.toInt)
                                - (lowerLimit |> N.toInt)
                                |> N.intToAtLeast n0
                                |> N.maxToOn
                            )
                            repeatedElementMorphRow
                        )
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
            (ArraySized element (In beforeToNarrowMin_ (Up maxX To maxPlusX)))
            (ArraySized element (In (Up0 narrowMinX_) (Up maxX To maxPlusX)))
            broadElement
atMost upperLimit element =
    Morph.named
        ([ "repeating ≤", upperLimit |> N.toInt |> String.fromInt ]
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
                 Morph.choiceEquivalent
                    (\count -> exactly count element)
                    { tryEarly = possibleCounts
                    , broad = n0 |> N.maxTo upperLimit
                    , tryLate = []
                    }
                )
        )
