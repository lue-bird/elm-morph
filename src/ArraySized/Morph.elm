module ArraySized.Morph exposing
    ( inNumber, inOn
    , list, toList
    , stack, toStack
    , elementTranslate
    , for, forBroad
    , exactly, atLeast, in_
    )

{-| [`Morph`](Morph) on an `Array`


## alter

@docs inNumber, inOn


## structure

@docs list, toList
@docs stack, toStack


## transform

Also available: [`toggle`](Morph#toggle) [`Array.Extra.reverse`](https://dark.elm.dmy.fr/packages/elm-community/array-extra/latest/Array-Extra#reverse)

@docs elementTranslate


## sequence

@docs for, forBroad


## row

@docs exactly, atLeast, in_

-}

import ArraySized exposing (ArraySized)
import Emptiable exposing (Emptiable)
import Linear exposing (Direction(..))
import Morph exposing (Error, ErrorWithDeadEnd, MorphIndependently, MorphRow, MorphRowIndependently, broad, broadenFrom, grab, narrowTo, translate, translateOn)
import N exposing (Exactly, In, Min, N, N0, N0OrAdd1, On, To, Up, Up0, n0, n1)
import Possibly exposing (Possibly)
import Stack exposing (Stacked)


{-| [`Morph`](Morph#Morph) from an `ArraySized` with an equatable range `In`
to an `In (On ...) (On ...)` to operate on it
-}
inOn :
    MorphIndependently
        (ArraySized narrowElement (In (On narrowMin) (On narrowMax))
         ->
            Result
                error_
                (ArraySized narrowElement (In narrowMin narrowMax))
        )
        (ArraySized broadElement (In broadMin broadMax)
         -> ArraySized broadElement (In (On broadMin) (On broadMax))
        )
inOn =
    translate ArraySized.inToNumber ArraySized.inToOn


{-| [`Morph`](Morph#Morph) from an `ArraySized` with a range `In (On ...) (On ...)`
to an `In` to make it equatable
-}
inNumber :
    MorphIndependently
        (ArraySized narrowElement (In narrowMin narrowMax)
         ->
            Result
                error_
                (ArraySized narrowElement (In (On narrowMin) (On narrowMax)))
        )
        (ArraySized broadElement (In (On broadMin) (On broadMax))
         -> ArraySized broadElement (In broadMin broadMax)
        )
inNumber =
    translate ArraySized.inToOn ArraySized.inToNumber


{-| [`Translate`](Morph#Translate) from `List` to `ArraySized`

    import N exposing (n0)
    import ArraySized

    [ 0, 1, 2, 3 ]
        |> Morph.mapTo ArraySized.Morph.fromList
    --: ArraySized (Min (Up x To x)) number_

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
        (ArraySized narrowElement narrowRange_
         -> Result error_ (List narrowElement)
        )
        (List broadElement
         -> ArraySized broadElement (Min (Up0 broadX_))
        )
toList =
    translate ArraySized.toList ArraySized.fromList


{-| [`Translate`](Morph#Translate) from `Emptiable (Stacked ...) ...` to `ArraySized`

    import N exposing (n0)
    import ArraySized

    Stack.topBelow 0 [ 1, 2, 3, 4 ]
        |> Morph.mapTo ArraySized.Morph.stack
    --: ArraySized (Min (Up1 x)) number_

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
    translate ArraySized.fromStack ArraySized.toStack


{-| [`Translate`](Morph#Translate) from `ArraySized` to `Emptiable (Stacked ...) ...`

    import N exposing (n0)
    import ArraySized

    ArraySized.l4 0 1 2 3
        |> ArraySized.minLower n0
        |> Morph.map ArraySized.Morph.toStack
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


{-| [`Translate`](Morph#Translate) each element in an `Array`
-}
elementTranslate :
    MorphIndependently
        (narrowBeforeMap
         -> Result (ErrorWithDeadEnd Never) narrowMapped
        )
        (broadBeforeUnmap -> broadUnmapped)
    ->
        MorphIndependently
            (ArraySized narrowBeforeMap narrowRange
             ->
                Result
                    error_
                    (ArraySized narrowMapped narrowRange)
            )
            (ArraySized broadBeforeUnmap broadRange
             -> ArraySized broadUnmapped broadRange
            )
elementTranslate elementTranslate_ =
    translateOn ( ArraySized.map, ArraySized.map ) elementTranslate_



-- sequence


{-| Match broad [`MorphRow`](Morph#MorphRow)s
(those that can always [produce its broad value](Morph#broadenFrom))
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
        |> Text.narrowTo (textOnly "abc")
    --> Ok ()

    -- It fails if it's not _exactly_ the same
    "abC"
        |> Text.narrowTo (textOnly "abC")
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
            (expectedConstantInputArraySized
                |> for morphRowByElement
            )


{-| [`grab`](Morph#grab) the elements of a given `List` of [`MorphRow`](Morph#MorphRow)s in order

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

The usual [`Morph.succeed`](Morph#succeed)`(\... -> ...) |>`[`grab`](Morph#grab)-[`skip`](Morph#skip) chain
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
    { description =
        case elementsToTraverseInSequence |> ArraySized.has n1 of
            Err (N.Below _) ->
                { inner = Emptiable.empty, custom = Emptiable.empty }

            Ok only1 ->
                only1 |> ArraySized.toOne |> morphRowByElement |> Morph.description

            Err (N.Above atLeast2) ->
                { custom = Emptiable.empty
                , inner =
                    atLeast2
                        |> ArraySized.map
                            (\elementToTraverse ->
                                elementToTraverse
                                    |> morphRowByElement
                                    |> Morph.description
                            )
                        |> ArraySized.maxToInfinity
                        |> Morph.Group
                        |> Emptiable.filled
                }
    , narrow =
        \initialInput ->
            let
                traversed =
                    elementsToTraverseInSequence
                        |> ArraySized.mapFoldFrom
                            (initialInput |> Ok)
                            Up
                            (\state ->
                                case state.folded of
                                    Ok broadSoFar ->
                                        case broadSoFar |> narrowTo (state.element |> morphRowByElement) of
                                            Ok parsed ->
                                                { element = parsed.narrow |> Emptiable.filled
                                                , folded = parsed.broad |> Ok
                                                }

                                            Err error ->
                                                { element = Emptiable.empty
                                                , folded = error |> Err
                                                }

                                    Err error ->
                                        { element = Emptiable.empty
                                        , folded = error |> Err
                                        }
                            )
            in
            case traversed.folded of
                Err error ->
                    error |> Err

                Ok broadRemaining ->
                    case traversed.mapped |> ArraySized.allFill of
                        Emptiable.Empty _ ->
                            "lue-bird/elm-morph: ArraySized.Morph.for bug: narrowing succeeded but not all parsed elements found! Please open an issue with details on the given ArraySized."
                                |> Morph.DeadEnd
                                |> Err

                        Emptiable.Filled arraySizedNarrow ->
                            { narrow = arraySizedNarrow, broad = broadRemaining } |> Ok
    , broaden =
        \narrowSequence ->
            List.map2
                (\morphInSequence ->
                    broadenFrom (morphInSequence |> morphRowByElement)
                )
                (elementsToTraverseInSequence |> ArraySized.toList)
                (narrowSequence |> ArraySized.toList)
                |> List.concatMap Stack.toList
                |> Stack.fromList
    }



--- row


{-| Match a value `exactly` a number of times
and return them as a [`ArraySized`](https://package.elm-lang.org/packages/lue-bird/elm-typesafe-array/latest/ArraySized)

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
    Morph.to
        ([ "exactly ", repeatCount |> N.toString ]
            |> String.concat
        )
        (for (\() -> repeatedMorphRow) (ArraySized.repeat () repeatCount))


{-| Match a value at least a number of times and returns them as a `List`.

> ℹ️ Equivalent regular expression: `{min,}`

    import Morph.Error
    import Char.Morph as Char
    import String.Morph as Text

    -- we want at least three letters, we are okay with more than three
    "abcdef"
        |> Text.narrowTo (atLeast n3 AToZ.char)
    --> Ok [ 'a', 'b', 'c', 'd', 'e', 'f' ]

    -- but not two, that's sacrilegious
    "ab_def"
        |> Text.narrowTo (atLeast n3 AToZ.char)
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:3: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '_'."


## `atLeast ... n0`

> ℹ️ Equivalent regular expression: `*`

    import Char.Morph as Char
    import String.Morph as Text

    -- We want as many letters as there are.
    "abc" |> Text.narrowTo (atLeast AToZ.char n0)
    --> Ok [ 'a', 'b', 'c' ]

    "abc123" |> Text.narrowTo (atLeast AToZ.char n0)
    --> Ok [ 'a', 'b', 'c' ]

    -- even zero letters is okay
    "123abc" |> Text.narrowTo (atLeast AToZ.char n0)
    --> Ok []


### `atLeast n1`

> ℹ️ Equivalent regular expression: `+`

    import N exposing (n1)
    import Morph.Error
    import Char.Morph as Char
    import String.Morph as Text

    -- we want as many letters as there are
    "abc" |> Text.narrowTo (atLeast AToZ.char n1)
    --> Ok [ 'a', 'b', 'c' ]

    "abc123" |> Text.narrowTo (atLeast AToZ.char n1)
    --> Ok [ 'a', 'b', 'c' ]

    -- but we want at least one
    "123abc"
        |> Text.narrowTo (atLeast n1 AToZ.char)
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:1: I was expecting a letter a|..|z or A|...|Z. I got stuck when I got the character '1'."


### example: interspersed separators

    import Stack
    import Morph exposing (separatedBy, atLeast, one)
    import String.Morph as Text exposing (text)
    import Char.Morph as Char


    tag =
        atLeast (Morph.AToZ.caseAnyLower |> one) n0

    tags =
        Morph.succeed Stack.onTopLay
            |> grab Stack.top tag
            |> grab Stack.removeTop
                (ArraySized.Morph.toStack
                    |> Morph.overRow
                        (atLeast
                            (Morph.succeed (\tag -> tag)
                                |> skip separator
                                |> grab (\tag -> tag) tag
                            )
                            n0
                        )
                )

    -- note that both values and separators must be of the same type
    "a,bc,def"
        |> Text.narrowTo tags
    --> Ok
    -->     { first = [ 'a' ]
    -->     , afterFirst =
    -->         [ { separator = (), part = [ 'b', 'c' ] }
    -->         , { separator = (), part = [ 'd', 'e', 'f' ] }
    -->         ]
    -->     }

    ",a,,"
        |> Text.narrowTo tags
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
        |> Text.narrowTo tags
    --> Ok (topDown [] [])


### anti-example: parsing infinitely

    Morph.succeed ...
        |> grab (atLeast (Morph.keep |> Morph.one) n0)
        |> grab ...

would only parse the first part until the end
because it always [`Morph.succeed`](Morph#succeed)s.
Nothing after would ever be parsed, making the whole thing fail.

-}
atLeast :
    MorphRow narrow broadElement
    -> N (Exactly (On min))
    ->
        MorphRowIndependently
            (ArraySized narrow (In (On min) max_))
            (ArraySized narrow (Min (On min)))
            broadElement
atLeast elementStepMorphRow minimum =
    Morph.broaden ArraySized.maxToInfinity
        |> Morph.overRow
            (Morph.succeed
                (\minimumArraySized overMinimum ->
                    minimumArraySized
                        |> ArraySized.attachMin Up
                            (overMinimum |> ArraySized.minTo n0)
                )
                |> grab
                    (ArraySized.take Up { atLeast = minimum } minimum)
                    (exactly minimum elementStepMorphRow)
                |> grab
                    (ArraySized.dropMin Up minimum)
                    (list
                        |> Morph.overRow (untilFail elementStepMorphRow)
                    )
            )


{-| How are [`atLeast`](#atLeast), ... defined?

    import Morph exposing (Morph.choice, validate)
    import Morph exposing (MorphRow, one, Morph.succeed, atLeast, take, drop, whileAccumulate)
    import Char.Morph
    import String.Morph
    import Number.Morph

    sumWhileLessThan : Float -> MorphRow Char (List Number)
    sumWhileLessThan max =
        whileAccumulate
            { initial = 0
            , step =
                \element stepped ->
                    let
                        floats =
                            stepped + (element |> Morph.map Number.Morph.toFloat)
                    in
                    if floats >= max then
                        Err ()
                    else
                        floats |> Ok
            , element =
                Morph.succeed (\n -> n)
                    |> grab (\n -> n) Number.Morph.text
                    |> skip (atLeast (String.Morph.only " ") n0)
            }

    -- stops before we reach a maximum of 6 in the sum
    "2 3 4"
        |> narrow
            (String.Morph.fromList
                |> Morph.overRow
                    (Morph.succeed (\numbers -> numbers)
                        |> grab (\numbers -> numbers) (sumWhileLessThan 6)
                        |> skip (String.Morph.only "4")
                    )
            )
    --> Ok 5

-}
whileAccumulate :
    { initial : accumulationValue
    , step :
        goOnElement
        ->
            (accumulationValue
             -> Result () accumulationValue
            )
    , element : MorphRow goOnElement broadElement
    }
    -> MorphRow (List goOnElement) broadElement
whileAccumulate { initial, step, element } =
    { description =
        { custom = Emptiable.empty
        , inner =
            Morph.While (element |> Morph.description)
                |> Emptiable.filled
        }
    , broaden =
        \list_ ->
            list_
                |> List.map (broadenFrom element)
                |> Stack.fromList
                |> Stack.flatten
    , narrow =
        let
            loopNarrowStep :
                { accumulationValue : accumulationValue }
                ->
                    (Emptiable (Stacked broadElement) Possibly
                     ->
                        Result
                            Error
                            { narrow : List goOnElement
                            , broad : Emptiable (Stacked broadElement) Possibly
                            }
                    )
            loopNarrowStep { accumulationValue } =
                \broad_ ->
                    broad_
                        |> narrowTo element
                        |> Result.andThen
                            (\stepped ->
                                case accumulationValue |> step stepped.narrow of
                                    Err () ->
                                        { broad = broad_
                                        , narrow = []
                                        }
                                            |> Ok

                                    Ok accumulationValueAltered ->
                                        stepped.broad
                                            |> loopNarrowStep
                                                { accumulationValue = accumulationValueAltered }
                                            |> Result.map
                                                (\tail ->
                                                    { broad = tail.broad
                                                    , narrow =
                                                        tail.narrow
                                                            |> (::) stepped.narrow
                                                    }
                                                )
                            )
        in
        loopNarrowStep { accumulationValue = initial }
    }


untilFail :
    MorphRow element broadElement
    -> MorphRow (List element) broadElement
untilFail elementStepMorphRow =
    whileAccumulate
        { initial = ()
        , step = \_ () -> () |> Ok
        , element = elementStepMorphRow
        }


{-| Match a value between a range of times and returns them as a `List`.

> ℹ️ Equivalent regular expression: `{min,max}`

    import Morph.Error
    import Char.Morph as Char
    import String.Morph as Text

    -- we want between two and four letters
    "abcdef" |> Text.narrowTo (in_ AToZ.char ( n2, n4 ))
    --> Ok [ 'a', 'b', 'c', 'd' ]

    "abc_ef" |> Text.narrowTo (in_ AToZ.char ( n2, n4 ))
    --> Ok [ 'a', 'b', 'c' ]

    "ab_def" |> Text.narrowTo (in_ AToZ.char ( n2, n4 ))
    --> Ok [ 'a', 'b' ]


    -- but less than that is not cool
    "i_am_here"
        |> Text.narrowTo (in_ letter ( n2, n3 ))
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:2: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '_'."


### example: `in_ ... ( n0, n1 )`

Alternative to [`Maybe.Morph.row`](Maybe-Morph#row) which instead returns a `List`.

> ℹ️ Equivalent regular expression: `?`

    import Char.Morph as Char
    import String.Morph as Text

    -- we want one letter, optionally
    "abc" |> Text.narrowTo (in_ AToZ.char ( n0, n1 ))
    --> Ok [ 'a' ]

    -- if we don't get any, that's still okay
    "123abc" |> Text.narrowTo (in_ AToZ.char ( n0, n1 ))
    --> Ok []


### example: at most

> ℹ️ Equivalent regular expression: `{0,max}`

    import Morph
    import Char.Morph as Char
    import String.Morph as Text

    -- we want a maximum of three letters
    "abcdef" |> Text.narrowTo (in_ AToZ.char ( n0, n3 ))
    --> Ok [ 'a', 'b', 'c' ]

    -- less than that is also okay
    "ab_def" |> Text.narrowTo (in_ AToZ.char ( n0, n3 ))
    --> Ok [ 'a', 'b' ]

    -- even zero letters are fine
    "_underscore" |> Text.narrowTo (in_ AToZ.char ( n0, n3 ))
    --> Ok []

    -- make sure we don't consume more than three letters
    "abcdef"
        |> Text.narrowTo
            (Morph.succeed (\letters -> letters)
                |> grab (in_ AToZ.char ( n0, n3 ))
                |> skip (one 'd')
            )
    --> Ok [ 'a', 'b', 'c' ]

-}
in_ :
    MorphRow element broadElement
    ->
        ( N (Exactly (On min))
        , N (In (On min) (On max))
        )
    ->
        MorphRow
            (ArraySized element (In (On min) (On max)))
            broadElement
in_ repeatedElementMorphRow ( lowerLimit, upperLimit ) =
    translate identity
        (ArraySized.minTo lowerLimit)
        |> Morph.overRow
            (let
                lowerLimitExactly =
                    lowerLimit
             in
             Morph.succeed
                (\minimumList overMinimum ->
                    minimumList
                        |> ArraySized.attachMin Up
                            (overMinimum |> ArraySized.minTo n0)
                        |> ArraySized.minTo lowerLimitExactly
                        |> ArraySized.take Up { atLeast = lowerLimit } upperLimit
                )
                |> grab
                    (ArraySized.take Up { atLeast = lowerLimitExactly } lowerLimitExactly)
                    (exactly lowerLimitExactly repeatedElementMorphRow)
                |> grab
                    (\arraySized ->
                        arraySized
                            |> ArraySized.dropMin Up lowerLimitExactly
                            |> ArraySized.maxToInfinity
                    )
                    (atMostLoop repeatedElementMorphRow
                        ((upperLimit |> N.toInt)
                            - (lowerLimit |> N.toInt)
                            |> N.intToAtLeast n0
                            |> N.maxToOn
                        )
                    )
            )


{-| Match a value at less or equal a number of times

**Shouldn't be exposed**

-}
atMostLoop :
    MorphRow narrow broadElement
    ->
        N
            (In
                upperLimitMin_
                (Up upperLimitMaxX_ To upperLimitMaxPlusX_)
            )
    ->
        MorphRowIndependently
            (ArraySized narrow (In min_ max_))
            (ArraySized narrow (Min (Up narrowX To narrowX)))
            broadElement
atMostLoop elementStepMorphRow upperLimit =
    Morph.to
        ([ "<= ", upperLimit |> N.toInt |> String.fromInt ]
            |> String.concat
        )
        (list
            |> Morph.overRow
                (whileAccumulate
                    { initial = { length = n0 |> N.maxToInfinity }
                    , step =
                        \_ soFar ->
                            case soFar.length |> N.isAtLeast (upperLimit |> N.maxAdd n1) of
                                Ok _ ->
                                    Err ()

                                Err _ ->
                                    { length = soFar.length |> N.addMin n1 } |> Ok
                    , element = elementStepMorphRow
                    }
                )
        )
