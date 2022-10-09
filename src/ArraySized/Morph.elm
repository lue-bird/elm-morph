module ArraySized.Morph exposing
    ( value, toValue
    , list, toList
    , stackEmptiable, toStackEmptiable
    , stackFilled, toStackFilled
    , elementTranslate
    , for, forBroad
    , exactly, atLeast, in_
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


## row

@docs exactly, atLeast, in_

-}

import ArraySized exposing (ArraySized)
import Emptiable exposing (Emptiable)
import Group exposing (grab)
import Linear exposing (Direction(..))
import Morph exposing (Error, ErrorWithDeadEnd, Morph, MorphIndependently, MorphOrError, MorphRow, MorphRowIndependently, Translate, broad, broadenWith, narrowWith, translate, translateOn)
import N exposing (Add1, Exactly, Fixed, In, InFixed, InValue, Min, N, N0, N1, To, Up, n0, n1, n2)
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

The usual [`Morph.succeed`](#Morph.succeed)`(\... -> ...) |>`[`grab`](#grab)-[`skip`](#skip) chain
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
                        |> ArraySized.maxToInfinity
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
                                        |> ArraySized.pushMin stepParsed.narrow
                                        |> ArraySized.minTo n0

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
                                    , narrow = notAllTraversed |> ArraySized.maxToInfinity
                                    }
                                        |> Partial

                                Ok traversed ->
                                    { broad = stepParsed.broad
                                    , narrow =
                                        traversed
                                            {- ArraySized.minTo
                                               (elementsToTraverseInSequence
                                                   |> ArraySized.length
                                                   |> N.min
                                                   |> N.exactly
                                               )
                                            -}
                                            |> ArraySized.take
                                                ( Up
                                                , elementsToTraverseInSequence |> ArraySized.length
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
                            ({ narrow = ArraySized.empty |> ArraySized.maxToInfinity
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



--- row


{-| Match a value `exactly` a number of times
and return them as a [`ArraySized`](https://package.elm-lang.org/packages/lue-bird/elm-typesafe-array/latest/ArraySized)

> ℹ️ Equivalent regular expression: `{n}`

    import Morph.Error
    import Char.Morph as Char
    import String.Morph as Text
    import N exposing (n3)

    -- we want `exactly 3` letters
    "abcdef" |> narrow (map Text.fromList (exactly n3 AToZ.Morph.char))
    --> Ok [ 'a', 'b', 'c' ]

    -- not 2 or 4, we want 3
    "ab_def"
        |> narrow (map Text.fromList (exactly n3 AToZ.Morph.char))
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:3: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '_'."

-}
exactly :
    N (Exactly howMany)
    -> MorphRow broadElement element
    ->
        MorphRow
            broadElement
            (ArraySized (Exactly howMany) element)
exactly repeatCount repeatedMorphRow =
    { description =
        case repeatCount |> N.is n1 of
            Err (N.Below _) ->
                { custom = Emptiable.empty
                , inner = Emptiable.empty
                }

            Ok _ ->
                repeatedMorphRow |> Morph.description

            Err (N.Above repeatCountAtLeast2) ->
                { custom =
                    Stack.only
                        ([ "exactly "
                         , repeatCountAtLeast2 |> N.toInt |> String.fromInt
                         ]
                            |> String.concat
                        )
                , inner =
                    ArraySized.repeat
                        (repeatedMorphRow |> Morph.description)
                        repeatCountAtLeast2
                        |> ArraySized.maxToInfinity
                        |> ArraySized.minTo n2
                        |> Morph.Group
                        |> Emptiable.filled
                }
    , narrow =
        let
            narrowRepeatStep :
                { soFar : ArraySized (Min (Fixed N0)) element }
                ->
                    (Emptiable (Stacked broadElement) Possibly
                     ->
                        Result
                            Error
                            { narrow : ArraySized (Exactly howMany) element
                            , broad : Emptiable (Stacked broadElement) Possibly
                            }
                    )
            narrowRepeatStep { soFar } =
                \broad_ ->
                    case soFar |> ArraySized.hasAtLeast (repeatCount |> N.maxUp n1) of
                        Ok arraySizedAtLeastHowOften ->
                            { narrow =
                                arraySizedAtLeastHowOften
                                    |> ArraySized.take ( Up, repeatCount )
                            , broad = broad_
                            }
                                |> Ok

                        Err _ ->
                            case broad_ |> narrowWith repeatedMorphRow of
                                Err error ->
                                    error |> Err

                                Ok parsed ->
                                    -- does this blow the stack?
                                    narrowRepeatStep
                                        { soFar =
                                            ArraySized.minDown n1
                                                (ArraySized.pushMin parsed.narrow soFar)
                                        }
                                        parsed.broad
        in
        narrowRepeatStep { soFar = ArraySized.empty |> ArraySized.maxToInfinity }
    , broaden =
        \repeated ->
            repeated
                |> ArraySized.toList
                |> Stack.fromList
                |> Stack.map (\_ -> broadenWith repeatedMorphRow)
                |> Stack.flatten
    }


{-| Match a value at least a number of times and returns them as a `List`.

> ℹ️ Equivalent regular expression: `{min,}`

    import Morph.Error
    import Char.Morph as Char
    import String.Morph as Text

    -- we want at least three letters, we are okay with more than three
    "abcdef"
        |> Text.narrowWith (atLeast n3 AToZ.Morph.char)
    --> Ok [ 'a', 'b', 'c', 'd', 'e', 'f' ]

    -- but not two, that's sacrilegious
    "ab_def"
        |> Text.narrowWith (atLeast n3 AToZ.Morph.char)
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:3: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '_'."


## `atLeast n0`

> ℹ️ Equivalent regular expression: `*`

    import Char.Morph as Char
    import String.Morph as Text

    -- We want as many letters as there are.
    "abc" |> Text.narrowWith (atLeast n0 AToZ.Morph.char)
    --> Ok [ 'a', 'b', 'c' ]

    "abc123" |> Text.narrowWith (atLeast n0 AToZ.Morph.char)
    --> Ok [ 'a', 'b', 'c' ]

    -- even zero letters is okay
    "123abc" |> Text.narrowWith (atLeast n0 AToZ.Morph.char)
    --> Ok []


### `atLeast n1`

> ℹ️ Equivalent regular expression: `+`

    import N exposing (n1)
    import Morph.Error
    import Char.Morph as Char
    import String.Morph as Text

    -- we want as many letters as there are
    "abc" |> Text.narrowWith (atLeast n1 AToZ.Morph.char)
    --> Ok [ 'a', 'b', 'c' ]

    "abc123" |> Text.narrowWith (atLeast n1 AToZ.Morph.char)
    --> Ok [ 'a', 'b', 'c' ]

    -- but we want at least one
    "123abc"
        |> Text.narrowWith (atLeast n1 AToZ.Morph.char)
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
        Morph.succeed (\first afterFirst -> Stack.topDown first afterFirst)
            |> grab .first tag
            |> grab .afterFirst
                (ArraySized.Morph.toList
                    |> Morph.overRow
                        (atLeast n0
                            (Morph.succeed (\part -> part)
                                |> skip separator
                                |> grab .tag tag
                            )
                        )
                )

    -- note that both values and separators must be of the same type
    "a,bc,def"
        |> Text.narrowWith tags
    --> Ok
    -->     { first = [ 'a' ]
    -->     , afterFirst =
    -->         [ { separator = (), part = [ 'b', 'c' ] }
    -->         , { separator = (), part = [ 'd', 'e', 'f' ] }
    -->         ]
    -->     }

    ",a,,"
        |> Text.narrowWith tags
    --> Ok
    -->     (Stack.topDown
    -->         []
    -->         [ { separator = (), part = [ 'a' ] }
    -->         , { separator = (), part = [] }
    -->         , { separator = (), part = [] }
    -->         ]
    -->     )

    -- an empty input text gives a single element from an empty string
    ""
        |> Text.narrowWith tags
    --> Ok (topDown [] [])


### anti-example: parsing infinitely

    Morph.succeed ...
        |> grab (atLeast n0 (Morph.keep |> Morph.one))
        |> grab ...

would only parse the first part until the end
because it always [`Morph.succeed`](#Morph.succeed)s.
Nothing after would ever be parsed, making the whole thing fail.

-}
atLeast :
    N (In (Fixed lowerLimitMin) (Up minNewMaxToMin_ To min))
    -> MorphRow broadElement narrow
    ->
        MorphRowIndependently
            broadElement
            (ArraySized (In (Fixed min) max_) narrow)
            (ArraySized (Min (Fixed lowerLimitMin)) narrow)
atLeast minimum =
    \elementStepMorphRow ->
        Morph.broaden (ArraySized.minTo minimum >> ArraySized.maxToInfinity)
            |> Morph.overRow
                (let
                    minimumExactly =
                        minimum |> N.min |> N.exactly
                 in
                 Morph.succeed
                    (\minimumArraySized overMinimum ->
                        minimumArraySized
                            |> ArraySized.glueMin Up
                                (overMinimum |> ArraySized.minTo n0)
                    )
                    |> grab
                        (ArraySized.take ( Up, minimumExactly ))
                        (exactly minimumExactly elementStepMorphRow)
                    |> grab
                        (ArraySized.dropMin ( Up, minimumExactly ))
                        (list
                            |> Morph.overRow (untilFail elementStepMorphRow)
                        )
                )


untilFail :
    MorphRow broadElement element
    -> MorphRow broadElement (List element)
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
    "abcdef" |> Text.narrowWith (in_ 2 4 AToZ.Morph.char)
    --> Ok [ 'a', 'b', 'c', 'd' ]

    "abc_ef" |> Text.narrowWith (in_ ( n2, n4 ) AToZ.Morph.char)
    --> Ok [ 'a', 'b', 'c' ]

    "ab_def" |> Text.narrowWith (in_ ( n2, n4 ) AToZ.Morph.char)
    --> Ok [ 'a', 'b' ]


    -- but less than that is not cool
    "i_am_here"
        |> Text.narrowWith (in_ ( n2, n3 ) letter)
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:2: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '_'."


### example: `in_ ( n0, n1 )`

Alternative to [`maybe`](#maybe) which instead returns a `List`.

> ℹ️ Equivalent regular expression: `?`

    import Char.Morph as Char
    import String.Morph as Text

    -- we want one letter, optionally
    "abc" |> Text.narrowWith (in_ ( n0, n1 ) AToZ.Morph.char)
    --> Ok [ 'a' ]

    -- if we don't get any, that's still okay
    "123abc" |> Text.narrowWith (in_ ( n0, n1 ) AToZ.Morph.char)
    --> Ok []


### example: at most

> ℹ️ Equivalent regular expression: `{0,max}`

    import Morph
    import Char.Morph as Char
    import String.Morph as Text

    -- we want a maximum of three letters
    "abcdef" |> Text.narrowWith (in_ ( n0, n3 ) AToZ.Morph.char)
    --> Ok [ 'a', 'b', 'c' ]

    -- less than that is also okay
    "ab_def" |> Text.narrowWith (in_ ( n0, n3 ) AToZ.Morph.char)
    --> Ok [ 'a', 'b' ]

    -- even zero letters are fine
    "_underscore" |> Text.narrowWith (in_ ( n0, n3 ) AToZ.Morph.char)
    --> Ok []

    -- make sure we don't consume more than three letters
    "abcdef"
        |> Text.narrowWith
            (Morph.succeed (\letters -> letters)
                |> grab (in_ ( n0, n3 ) AToZ.Morph.char)
                |> skip (one 'd')
            )
    --> Ok [ 'a', 'b', 'c' ]

-}
in_ :
    ( N (In (Fixed lowerLimitMin) (Up minToLowerLimitMin_ To min))
    , N (In (Fixed lowerLimitMin) (Fixed upperLimitMax))
    )
    -> MorphRow broadElement element
    ->
        MorphRowIndependently
            broadElement
            (ArraySized
                (In (Fixed min) (Up maxToUpperLimitMax_ To upperLimitMax))
                element
            )
            (ArraySized
                (InFixed lowerLimitMin upperLimitMax)
                element
            )
in_ ( lowerLimit, upperLimit ) repeatedElementMorphRow =
    translate identity
        (ArraySized.minTo lowerLimit)
        |> Morph.overRow
            (let
                lowerLimitExactly =
                    lowerLimit |> N.min |> N.exactly
             in
             Morph.succeed
                (\minimumList overMinimum ->
                    minimumList
                        |> ArraySized.glueMin Up (overMinimum |> ArraySized.minTo n0)
                        |> ArraySized.minTo lowerLimitExactly
                        |> ArraySized.take ( Up, upperLimit )
                )
                |> grab
                    (ArraySized.take
                        ( Up
                        , lowerLimitExactly
                        )
                    )
                    (exactly lowerLimitExactly repeatedElementMorphRow)
                |> grab
                    (ArraySized.dropMin ( Up, lowerLimitExactly )
                        >> ArraySized.maxToInfinity
                    )
                    (atMostLoop
                        ((upperLimit |> N.toInt)
                            - (lowerLimit |> N.toInt)
                            |> N.intAtLeast n0
                        )
                        repeatedElementMorphRow
                    )
            )


{-| Match a value at less or equal a number of times.

**Shouldn't be exposed**

-}
atMostLoop :
    N
        (In
            upperLimitMin_
            (Up upperLimitMaxX_ To upperLimitMaxPlusX_)
        )
    -> MorphRow broadElement narrow
    ->
        MorphRowIndependently
            broadElement
            (ArraySized (In min_ max_) narrow)
            (ArraySized (Min (Up narrowX To narrowX)) narrow)
atMostLoop upperLimit elementStepMorphRow =
    Morph.to
        ([ "<= ", upperLimit |> N.toInt |> String.fromInt ]
            |> String.concat
        )
        (translate ArraySized.fromList ArraySized.toList
            |> Morph.overRow
                (whileAccumulate
                    { initial = { length = n0 |> N.maxToInfinity }
                    , step =
                        \_ soFar ->
                            case soFar.length |> N.isAtLeast (upperLimit |> N.maxUp n1) of
                                Ok _ ->
                                    Err ()

                                Err _ ->
                                    { length = soFar.length |> N.addMin n1 } |> Ok
                    , element = elementStepMorphRow
                    }
                )
        )


{-| How are [`atLeast`](#atLeast), ... defined?

    import Morph exposing (Choice.between, validate)
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
                    |> skip (atLeast n0 (String.Morph.only " "))
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
        -> accumulationValue
        -> Result () accumulationValue
    , element : MorphRow broadElement goOnElement
    }
    -> MorphRow broadElement (List goOnElement)
whileAccumulate { initial, step, element } =
    { description =
        { custom = Emptiable.empty
        , inner =
            Morph.While (element |> Morph.description)
                |> Emptiable.filled
        }
    , broaden =
        List.map (broadenWith element)
            >> Stack.fromList
            >> Stack.flatten
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
                        |> narrowWith element
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
