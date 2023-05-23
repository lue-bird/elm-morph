module ArraySized.Morph exposing
    ( inNumber, inOn
    , array, toArray
    , list, toList
    , stack, toStack
    , string, toString
    , each
    , for, forBroad
    , exactly, atLeast, in_
    )

{-| [`Morph`](Morph) on an `Array`


## alter

@docs inNumber, inOn


## structure

@docs array, toArray
@docs list, toList
@docs stack, toStack
@docs string, toString


## transform

@docs each


## sequence

@docs for, forBroad


## row

@docs exactly, atLeast, in_

-}

import Array exposing (Array)
import ArraySized exposing (ArraySized)
import Emptiable exposing (Emptiable)
import Linear exposing (Direction(..))
import Morph exposing (Error, ErrorWithDeadEnd, MorphIndependently, MorphRow, MorphRowIndependently, broad, grab, toBroad, toNarrow, translate, translateOn)
import N exposing (Exactly, In, Min, N, N0, N0OrAdd1, On, To, Up, Up0, n0, n1)
import Possibly exposing (Possibly(..))
import Stack exposing (Stacked)
import StructureMorph


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


{-| [`Translate`](Morph#Translate) from `Array` to `ArraySized`

    import N exposing (n0)
    import ArraySized
    import Array

    Array.fromList [ 0, 1, 2, 3 ]
        |> Morph.mapTo ArraySized.Morph.fromArray
    --: ArraySized (Min (Up x To x)) number_

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
    translate ArraySized.fromArray ArraySized.toArray


{-| [`Translate`](Morph#Translate) from `ArraySized` to `Array`

    import N exposing (n0)
    import ArraySized

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


{-| [`Translate`](Morph#Translate) from `List` to `ArraySized`

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
    translate ArraySized.fromList ArraySized.toList


{-| [`Translate`](Morph#Translate) from `ArraySized` to `List`

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


{-| [`Translate`](Morph#Translate) from `String` to `ArraySized`

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
    translate ArraySized.fromString ArraySized.toString


{-| [`Translate`](Morph#Translate) from `ArraySized` to `String`

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


{-| [`Translate`](Morph#Translate) from `Emptiable (Stacked ...) ...` to `ArraySized`

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


{-| [`Translate`](Morph#Translate) each element in an `ArraySized`

TODO Make sequence. See List.Morph.each

-}
each :
    MorphIndependently
        (narrowBeforeMap
         -> Result (ErrorWithDeadEnd deadEnd) narrowMapped
        )
        (broadBeforeUnmap -> broadUnmapped)
    ->
        MorphIndependently
            (ArraySized narrowBeforeMap narrowRange
             ->
                Result
                    (ErrorWithDeadEnd deadEnd)
                    (ArraySized narrowMapped narrowRange)
            )
            (ArraySized broadBeforeUnmap broadRange
             -> ArraySized broadUnmapped broadRange
            )
each elementMorph =
    StructureMorph.for "each" morphEachElement
        |> StructureMorph.add elementMorph
        |> StructureMorph.finish


morphEachElement :
    MorphIndependently
        (narrowBeforeMap
         -> Result (ErrorWithDeadEnd deadEnd) narrowMapped
        )
        (broadBeforeUnmap -> broadUnmapped)
    ->
        { toNarrow :
            ArraySized narrowBeforeMap narrowRange
            ->
                Result
                    (ErrorWithDeadEnd deadEnd)
                    (ArraySized narrowMapped narrowRange)
        , toBroad :
            ArraySized broadBeforeUnmap broadRange
            -> ArraySized broadUnmapped broadRange
        }
morphEachElement elementMorph =
    { toNarrow =
        \arraySized ->
            arraySized
                |> ArraySized.map (Morph.toNarrow elementMorph)
                |> ArraySized.allOk
                |> Result.mapError Morph.GroupError
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


sequence :
    ArraySized (MorphRow elementNarrow broadElement) (In min max)
    ->
        MorphRow
            (ArraySized elementNarrow (In min max))
            broadElement
sequence toSequence =
    sequenceNamed "sequence" toSequence


sequenceNamed :
    String
    -> ArraySized (MorphRow elementNarrow broadElement) (In min max)
    ->
        MorphRow
            (ArraySized elementNarrow (In min max))
            broadElement
sequenceNamed structureName toSequence =
    { description =
        case toSequence |> ArraySized.toList of
            [] ->
                Morph.succeed ArraySized.empty |> Morph.description

            inSequence0 :: inSequence1Up ->
                { custom = Emptiable.empty
                , inner =
                    Morph.StructureDescription "sequence"
                        (Stack.topBelow inSequence0 inSequence1Up |> Stack.map (\_ -> Morph.description))
                }
    , toNarrow =
        \initialInput ->
            let
                traversed =
                    toSequence
                        |> ArraySized.mapFoldFrom
                            { index = 0, broad = initialInput }
                            Up
                            (\state ->
                                case state.folded.broad |> toNarrow state.element of
                                    Ok parsed ->
                                        { element = parsed.toNarrow |> Ok
                                        , folded =
                                            { index = state.folded.index + 1
                                            , broad = parsed.broad
                                            }
                                        }

                                    Err error ->
                                        { element =
                                            Morph.InStructureError { index = state.folded.index, error = error }
                                                |> Err
                                        , folded =
                                            { index = state.folded.index + 1
                                            , broad = state.folded.broad
                                            }
                                        }
                            )
            in
            case traversed.mapped |> ArraySized.allOk of
                Err error ->
                    error |> Morph.GroupError |> Err

                Ok sequenceArraySized ->
                    { toNarrow = sequenceArraySized, broad = traversed.folded.broad } |> Ok
    , toBroad =
        \narrowSequence ->
            List.map2
                (\morphRowSequence ->
                    toBroad morphRowSequence
                )
                (toSequence |> ArraySized.toList)
                (narrowSequence |> ArraySized.toList)
                |> List.concatMap Stack.toList
                |> Stack.fromList
    }



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
    sequenceNamed
        ([ "repeating ", repeatCount |> N.toString ] |> String.concat)
        (ArraySized.repeat repeatedMorphRow repeatCount)


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
    Morph.to
        ([ "repeating ≥ ", minimum |> N.toString ] |> String.concat)
        (Morph.toBroad ArraySized.maxToInfinity
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
                        (\arr -> arr |> ArraySized.dropMin Up minimum |> ArraySized.minTo n0)
                        (list |> Morph.overRow (untilFail elementStepMorphRow))
                )
        )


{-| How are [`atLeast`](#atLeast), ... defined?

    import Morph exposing (Morph.choice, validate)
    import Morph exposing (MorphRow, one, Morph.succeed, atLeast, take, drop, whileAccumulate)
    import Char.Morph
    import String.Morph
    import Number.Morph

    sumWhileLessThan : Float -> MorphRow Char (List (OrException Decimal))
    sumWhileLessThan max =
        whileAccumulate
            { initial = 0
            , step =
                \element stepped ->
                    let
                        floats =
                            stepped + (element |> Morph.mapTo DecimalOrException.Morph.toFloat)
                    in
                    if floats >= max then
                        Err ()
                    else
                        floats |> Ok
            , element =
                Morph.succeed (\n -> n)
                    |> grab (\n -> n) Number.Morph.text
                    |> match (atLeast n0 (String.Morph.only " "))
            }

    -- stops before we reach a maximum of 6 in the sum
    "2 3 4"
        |> narrow
            (String.Morph.list
                |> Morph.overRow
                    (Morph.succeed (\numbers -> numbers)
                        |> grab (\numbers -> numbers) (sumWhileLessThan 6)
                        |> match (String.Morph.only "4")
                    )
            )
    --> Ok 5

-}
whileFold :
    { initial : folded
    , step : goOnElement -> (folded -> Maybe folded)
    , goOnDescription : String
    }
    -> MorphRow goOnElement broadElement
    -> MorphRow (List goOnElement) broadElement
whileFold foldConfig element =
    StructureMorph.for ("repeating while " ++ foldConfig.goOnDescription)
        (morphWhileFold { initial = foldConfig.initial, step = foldConfig.step })
        |> StructureMorph.add element
        |> StructureMorph.finish


morphWhileFold :
    { initial : folded
    , step : goOnElement -> (folded -> Maybe folded)
    }
    -> MorphRow goOnElement broadElement
    ->
        { toNarrow :
            Emptiable (Stacked broadElement) Possibly
            ->
                Result
                    error_
                    { toNarrow : List goOnElement
                    , broad : Emptiable (Stacked broadElement) Possibly
                    }
        , toBroad : List goOnElement -> Emptiable (Stacked broadElement) Possibly
        }
morphWhileFold { initial, step } element =
    { toBroad =
        \list_ ->
            list_
                |> List.map (toBroad element)
                |> Stack.fromList
                |> Stack.flatten
    , toNarrow =
        let
            loopNarrowStep :
                { folded : folded }
                ->
                    (Emptiable (Stacked broadElement) Possibly
                     ->
                        { toNarrow : List goOnElement
                        , broad : Emptiable (Stacked broadElement) Possibly
                        }
                    )
            loopNarrowStep { folded } =
                \broad_ ->
                    case broad_ |> toNarrow element of
                        Err _ ->
                            { broad = broad_, toNarrow = [] }

                        Ok stepped ->
                            case folded |> step stepped.toNarrow of
                                Nothing ->
                                    { broad = broad_, toNarrow = [] }

                                Just foldedAltered ->
                                    let
                                        tail :
                                            { toNarrow : List goOnElement
                                            , broad : Emptiable (Stacked broadElement) Possibly
                                            }
                                        tail =
                                            stepped.broad
                                                |> loopNarrowStep { folded = foldedAltered }
                                    in
                                    { broad = tail.broad
                                    , toNarrow = tail.toNarrow |> (::) stepped.toNarrow
                                    }
        in
        \initialBroad ->
            initialBroad |> loopNarrowStep { folded = initial } |> Ok
    }


untilFail :
    MorphRow element broadElement
    -> MorphRow (List element) broadElement
untilFail elementStepMorphRow =
    whileFold
        { goOnDescription = "possible"
        , initial = ()
        , step = \_ -> Just
        }
        elementStepMorphRow


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
    StructureMorph.for
        ([ "repeating "
         , lowerLimit |> N.toString
         , ".."
         , upperLimit |> N.toString
         ]
            |> String.concat
        )
        (inMorph ( lowerLimit, upperLimit ))
        |> StructureMorph.add repeatedElementMorphRow
        |> StructureMorph.finish


inMorph :
    ( N (Exactly (On min))
    , N (In (On min) (On max))
    )
    -> MorphRow element broadElement
    ->
        MorphRow
            (ArraySized element (In (On min) (On max)))
            broadElement
inMorph ( lowerLimit, upperLimit ) repeatedElementMorphRow =
    translate identity
        (ArraySized.minTo lowerLimit)
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
                    )
                    (atMostLoop
                        ((upperLimit |> N.toInt)
                            - (lowerLimit |> N.toInt)
                            |> N.intToAtLeast n0
                            |> N.maxToOn
                        )
                        repeatedElementMorphRow
                    )
            )


{-| Match a value at less or equal a number of times

**Shouldn't be exposed**

-}
atMostLoop :
    N
        (In
            upperLimitMin_
            (Up upperLimitMaxX_ To upperLimitMaxPlusX_)
        )
    -> MorphRow narrow broadElement
    ->
        MorphRowIndependently
            (ArraySized narrow (In min_ max_))
            (ArraySized narrow (Min (Up narrowX To narrowX)))
            broadElement
atMostLoop upperLimit elementStepMorphRow =
    StructureMorph.for
        ([ "repeating ≤", upperLimit |> N.toInt |> String.fromInt ]
            |> String.concat
        )
        (morphAtMost upperLimit)
        |> StructureMorph.add elementStepMorphRow
        |> StructureMorph.finish


morphAtMost :
    N
        (In
            upperLimitMin_
            (Up upperLimitMaxX_ To upperLimitMaxPlusX_)
        )
    -> MorphRow narrow broadElement
    ->
        MorphRowIndependently
            (ArraySized narrow (In min_ max_))
            (ArraySized narrow (Min (Up narrowX To narrowX)))
            broadElement
morphAtMost upperLimit elementStepMorphRow =
    list
        |> Morph.overRow
            (whileFold
                { goOnDescription = "amount ≤ " ++ (upperLimit |> N.toString)
                , initial = n0 |> N.maxToInfinity
                , step =
                    \_ soFarLength ->
                        case soFarLength |> N.isAtLeast (upperLimit |> N.maxAdd n1) of
                            Ok _ ->
                                Nothing

                            Err _ ->
                                soFarLength |> N.addMin n1 |> Just
                }
                elementStepMorphRow
            )
