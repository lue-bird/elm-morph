module Morph exposing
    ( Morph, Translate, MorphOrError, MorphIndependently
    , Description, DescriptionInner(..)
    , Error, ErrorWithDeadEnd(..), PartsError
    , broaden
    , value, only, validate
    , translate, broad, toggle, keep, translateOn
    , lazy
    , end, one, succeed, grab, skip
    , to
    , invert
    , deadEndMap
    , deadEndNever, narrowErrorMap
    , errorToLines
    , description
    , broadenFrom, narrowTo, mapTo
    , over, overRow
    , GroupMorphNoPart
    , groupToFrom, part, groupFinish
    , choiceEquivalent
    , choiceEquivalentRow
    , ChoiceMorphNoVariant, choiceToFrom, variant, choiceToFromFinish
    , choice
    , ChoiceMorphNoTry, try, choiceFinish
    , ChoiceMorphRowNoTry, tryRow, choiceRowFinish
    , MorphRow, MorphRowIndependently, rowFinish
    , before
    )

{-| Call it Codec, ToFrom, ParserBuilder, Convert-, Transform-, Shape-, FormReversible, ...
We call it

@docs Morph, Translate, MorphOrError, MorphIndependently
@docs Description, DescriptionInner
@docs Error, ErrorWithDeadEnd, PartsError


## create

@docs broaden
@docs value, only, validate
@docs translate, broad, toggle, keep, translateOn

@docs lazy


### create row

@docs end, one, succeed, grab, skip


## alter

@docs to
@docs invert
@docs deadEndMap
@docs deadEndNever, narrowErrorMap


## transform

@docs errorToLines


## scan

@docs description
@docs broadenFrom, narrowTo, mapTo


## chain

@docs over, overRow


## group

@docs GroupMorphNoPart
@docs groupToFrom, part, groupFinish


## choice

[`Morph`](#Morph) a tagged union `type`

@docs choiceEquivalent
@docs choiceEquivalentRow


### morph by variant

@docs ChoiceMorphNoVariant, choiceToFrom, variant, choiceToFromFinish
@docs choice
@docs ChoiceMorphNoTry, try, choiceFinish


## choice [`MorphRow`](#MorphRow)

@docs ChoiceMorphRowNoTry, tryRow, choiceRowFinish


## row

@docs MorphRow, MorphRowIndependently, rowFinish


### sequence row

  - optional → [`Maybe.Morph.row`](Maybe-Morph#row)
  - [`atLeast`](ArraySized-Morph-atLeast)
  - [`exactly`](ArraySized-Morph-exactly)
  - between → [`ArraySized.Morph.in_`](ArraySized-Morph-exactly)

@docs before

`whileAccumulate`, `until` aren't exposed for simplicity.
Have a need for them? → issue

---

Up for a challenge? implement & PR

  - `date`, `time`, `datetime`
  - `pathUnix`, `pathWindows`
  - `uri`
  - `ipV4`, `ipV6`

-}

import ArraySized exposing (ArraySized)
import Emptiable exposing (Emptiable, filled)
import Linear exposing (Direction(..))
import N exposing (Add1, Exactly, In, Min, N, N0, N2, On, To, Up, n0, n1, n2)
import Possibly exposing (Possibly(..))
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Stack exposing (Stacked)
import Util exposing (restoreTry)



{- dev notes and zombie comments

   ### loop

   Powerful ways to recurse over [`MorphRow`](#MorphRow)s:

   situation: One [`possibility`](#try) matches, next the new argument is taken to call the whole [`MorphRow`](#MorphRow) recursively.

   This grows the stack, so you cannot do it indefinitely.
   ↓ enable tail-call elimination so you can have as many repeats you want.

   @docs whileAccumulate, until


   ## performance optimizations

   - `commit` to set a path as non-backtracking add
   - use `StackedWithLength` as input so that `Error.Row` paths don't re-evaluate `List.length` (O(n))

-}


{-| Conversion functions from a more general to
a more specific format and back.

There's no use-case implied.
You can always chain, [group](#groups), [choose](#choices), ...

👀 Each type `Morph narrow broad`,
for example `Morph Email String`, can


### `broaden : narrow -> broad`

  - example: `Email -> String`
  - going from a specific type to a general one
  - any specific value can be turned back successfully
  - can loose information on its way


### `narrow : broad -> Result error narrow`

  - example: `String -> Result Morph.Error Email`
      - ↑ is exactly how running your typical parser looks
  - going from a general type to a specific one
  - the result can always be turned back successfully
  - 📰 [blog post "A Broader Take on Parsing" by Joël Quenneville](https://thoughtbot.com/blog/a-broader-take-on-parsing?utm_campaign=Elm%20Weekly&utm_medium=email&utm_source=Revue%20newsletter)
    captures the essence of narrowing and gives more examples
  - 📰 [blog post "Shaping Values with Types" by Josh Clayton](https://thoughtbot.com/blog/shaping-values-with-types?utm_campaign=Elm%20Weekly&utm_medium=email&utm_source=Revue%20newsletter)
    following the example of an employee id,
    shows how simply wrapping broad data into an opaque type
      - doesn't bring safety and peace of mind
          - for example,
            you won't have to fix a bug from months ago
            that interrupts the work when you're swamped with today
      - opaque types don't save complexity in validation, tests, documentation anyway
          - doesn't communicate business rules (requirements, TODO)
              - including for other developers,
                "allowing for improved reasoning across the codebase"
  - 🎙️ [podcast "Parse, don't validate"](https://elm-radio.com/episode/parse-dont-validate/)


#### Why `Morph.Error` in `Morph` this when I could [use custom errors](#MorphOrError) everywhere?

Errors with more narrow structural information are mostly useful for recovery based on what went wrong.

You _can_ use [`MorphOrError`](#MorphOrError) in these cases (TODO recovering).

Without needing to recover, benefits of having narrow error types for every interaction
aren't worth

  - making new structure-specific types for
  - the extra type variable which decreases simplicity


## limits

This can only fail one way and that might not fit the actual narrow-ness of parts.

An example is translating one programming language to another,
where both can represent stuff the other can't.
This can fail in both directions

I haven't done something like that,
but the answer to these questions is often to have

    Morph LanguageBSubset LanguageA
    -- LanguageBSubset -> LanguageA will always work

and

    Morph LanguageBSubset LanguageB
    -- LanguageBSubset -> LanguageB will always work

-}
type alias Morph narrow broad =
    MorphIndependently
        (broad -> Result Error narrow)
        (narrow -> broad)


{-| Sometimes, you'll see the most general version of [`Morph`](#Morph):

    : MorphIndependently narrow broaden

where

  - [`narrow`](#narrow)ed value types can't necessarily be [`broaden`](#broaden)ed
  - [`broaden`](#broaden)ed value types can't necessarily be [`narrow`](#narrow)ed

This general form is helpful to describe a step in building an incomplete [`Morph`](#Morph).

TODO: add error as type parameter to allow translate

TODO: dream:
Choice by group/Morph.choice/... associating errors and description

-}
type alias MorphIndependently narrow broaden =
    RecordWithoutConstructorFunction
        { description : Description
        , narrow : narrow
        , broaden : broaden
        }


{-| [`Morph`](#Morph) that can [narrow](#narrowTo)
to an error that can be different from the default [`Error`](#Error)

    type alias Translate mapped unmapped =
        MorphOrError mapped unmapped (ErrorWithDeadEnd Never)

-}
type alias MorphOrError narrow broad error =
    MorphIndependently
        (broad -> Result error narrow)
        (narrow -> broad)


{-| Describing what the Morph [narrows to](#narrowTo) and [broadens from](#broadenFrom)

  - custom description of the context
  - maybe [a description depending on structure](#DescriptionInner)

-}
type alias Description =
    RecordWithoutConstructorFunction
        { custom : Emptiable (Stacked String) Possibly
        , inner : Emptiable DescriptionInner Possibly
        }


{-| Description of a structure

  - chained morphs
  - narrow group of multiple
  - narrow Morph.choice between multiple

-}
type DescriptionInner
    = Translate
    | Over { narrow : Description, broad : Description }
    | Group (ArraySized Description (Min (On N2)))
    | Choice (ArraySized Description (Min (On N2)))
    | Elements Description
      -- row
    | While Description
    | Until { end : Description, element : Description }


{-| Where [narrowing](#narrowTo) has failed.

`String` is not enough for display?
→ use [`MorphOrError`](#MorphOrError) [`ErrorWithDeadEnd`](#ErrorWithDeadEnd) doing [`mapDeadEnd`](#mapDeadEnd)
on [`Morph`](#Morph) that are returned

Have trouble doing so because some API is too strict on errors? → issue

-}
type alias Error =
    ErrorWithDeadEnd String


{-| [`Error`](#Error) with a custom value on `DeadEnd`

    type alias Translate mapped unmapped =
        MorphOrError mapped unmapped (ErrorWithDeadEnd Never)

`deadEnd` could also be formatted text for display.
For that, use [`MorphOrError`](#MorphOrError) [`ErrorWithDeadEnd`](#ErrorWithDeadEnd) doing [`mapDeadEnd`](#mapDeadEnd)
on [`Morph`](#Morph) that are returned.

Have trouble doing so because some API is too strict on errors? → issue


### Why use text instead of a more narrow type for dead ends?

Different cases shouldn't invoke different responses.
Types give the benefit that all error conditions are explicit and transparent and that users are only able to produce those kinds of errors.
Then again: They don't have to be.
**Structured** text is enough while being so much easier to work with (mostly on the developer side)

-}
type ErrorWithDeadEnd deadEnd
    = DeadEnd deadEnd
    | Row { startDown : Int, error : ErrorWithDeadEnd deadEnd }
      --? TODO | Variant { index : Int, error : ErrorWithDeadEnd deadEnd }
    | Parts (PartsError (ErrorWithDeadEnd deadEnd))
    | Tries
        (Emptiable
            (Stacked (ErrorWithDeadEnd deadEnd))
            Never
        )


{-| A group's part [`Error`](#Error)s, each with their part index
-}
type alias PartsError partError =
    Emptiable
        (Stacked { index : Int, error : partError })
        Never


{-| Create a message from the [`Error`](#Error)

TODO: update examples

    import Morph
    import String.Morph as Text
    import Char.Morph exposing (letter)

    -- Getting a digit instead of a letter
    "123"
        |> Text.narrowTo letter
        |> Result.mapError Morph.errorToString
    --> Err "1:1: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '1'."

    -- Running out of input characters
    ""
        |> Text.narrowTo letter
        |> Result.mapError Morph.errorToString
    --> Err "1:0: I was expecting a letter [a-zA-Z]. I reached the end of the input."

-}
errorToLines : Error -> Emptiable (Stacked String) never_
errorToLines =
    \expected ->
        case expected of
            DeadEnd unexpectedDescription ->
                Stack.one unexpectedDescription

            Row row ->
                Stack.onTopLay
                    ([ row.startDown |> String.fromInt
                     , " elements from the last"
                     ]
                        |> String.concat
                    )
                    (Stack.onTopLay "" (row.error |> errorToLines))

            Parts parts ->
                Stack.onTopLay
                    "in the parts"
                    (parts
                        |> Emptiable.emptyAdapt never
                        |> Stack.map
                            (\_ part_ ->
                                Stack.onTopLay
                                    ([ "part ", part_.index |> String.fromInt ]
                                        |> String.concat
                                    )
                                    (Stack.onTopLay "" (part_.error |> errorToLines))
                                    |> markdownElement
                            )
                        |> Stack.flatten
                    )

            Tries possibilities ->
                Stack.onTopLay
                    "i tried"
                    (possibilities
                        |> Emptiable.emptyAdapt never
                        |> Stack.map
                            (\_ possibility ->
                                Stack.onTopLay
                                    "possibility:"
                                    (possibility |> errorToLines)
                                    |> markdownElement
                            )
                        |> Stack.flatten
                    )


indent =
    \line -> "    " ++ line


markdownElement :
    Emptiable (Stacked String) Never
    -> Emptiable (Stacked String) never_
markdownElement =
    \elementMarkdown ->
        Stack.onTopLay
            ([ "  - ", elementMarkdown |> Stack.top ] |> String.concat)
            (elementMarkdown
                |> Stack.removeTop
                |> Stack.map (\_ -> indent)
            )


{-| Describe the context to improve error messages.

    import Morph.Error
    import Char.Morph as Char
    import String.Morph as Text

    -- we can redefine an error message if something goes wrong
    "123"
        |> Text.narrowTo
            (Morph.to "variable name"
                (atLeast n1 AToZ.char)
            )
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:1: I was expecting a name consisting of letters. I got stuck when I got '1'."


    import Morph exposing (take, drop, Morph.succeed, expect, one)
    import String.Morph as Text

    type alias Point =
        -- makes `Point` function unavailable:
        -- https://dark.elm.dmy.fr/packages/lue-bird/elm-no-record-type-alias-constructor-function/latest/
        RecordWithoutConstructorFunction
            { x : Float
            , y : Float
            }

    -- we can use `expect` to have more context when an error happens
    point : MorphRow Point
    point =
        Morph.to "point"
            (Morph.succeed (\x y -> { x = x, y = y })
                |> skip (Char.Morph.only '(' |> one)
                |> grab .x Text.number
                |> skip (Char.Morph.only ',' |> one)
                |> grab .y Text.number
                |> skip (Char.Morph.only ')' |> one)
            )

    "(12,34)" |> narrow (map Text.fromList point)
    --> Ok { x = 12, y = 34 }

    -- we can get the error context stack as well as where they started matching
    "(a,b)" |> narrow (map Text.fromList point)
        |> Result.mapError .expected
    --> Err [ ExpectedCustom "point" ]

-}
to :
    String
    ->
        (MorphIndependently narrow broaden
         -> MorphIndependently narrow broaden
        )
to expectationCustomDescription morphToDescribe =
    { morphToDescribe
        | description =
            morphToDescribe.description
                |> (\description_ ->
                        { description_
                            | custom =
                                description_.custom
                                    |> Stack.onTopLay
                                        expectationCustomDescription
                        }
                   )
    }



--


{-| The morph's [`Description`](#Description).

Add custom ones via [`Morph.to`](#to)

-}
description : MorphIndependently narrow_ broaden_ -> Description
description =
    .description


{-| Its transformation that turns `narrow` into `broad`.
Some call it "build"
-}
broadenFrom : MorphIndependently narrow_ broaden -> broaden
broadenFrom =
    .broaden


{-| Its transformation that turns `broad` into `narrow` or an `error`.
Some call it "parse"
-}
narrowTo : MorphIndependently narrow broaden_ -> narrow
narrowTo =
    .narrow


{-| Convert values of the arbitrarily chosen types `unmapped -> mapped`.

    "3456" |> |> Morph.mapTo String.Morph.toList
    --> [ '3', '4', '5', '6' ]

-}
mapTo :
    MorphIndependently
        (unmapped -> Result (ErrorWithDeadEnd Never) mapped)
        broaden_
    -> (unmapped -> mapped)
mapTo translate_ =
    \unmapped ->
        case unmapped |> narrowTo translate_ of
            Ok mappedNarrow ->
                mappedNarrow

            Err error ->
                error |> deadEndNever



--


{-| Filter specific values.

In general, try to narrow down the type when limiting values:
["Parse, don't validate"](https://elm-radio.com/episode/parse-dont-validate/).
That's a core idea in elm. You'll find lots of legendary resources on this topic.

Narrowing gives you

  - a better error description out of the box
  - a more descriptive and correct type
  - building invalid values becomes impossible

```
printable : Morph LocalSymbolPrintable Char (Morph.Error Char)
printable =
    Morph.choice
        (\exclamationMark numberSign dollarSign percentSign ampersand asterisk lowLine hyphenMinus backSlash printable ->
            case printable of
                ExclamationMark ->
                    exclamationMark ()

                NumberSign ->
                    numberSign ()

                DollarSign ->
                    dollarSign ()

                PercentSign ->
                    percentSign ()

                Ampersand ->
                    ampersand ()

                Asterisk ->
                    asterisk ()

                LowLine ->
                    lowLine ()

                HyphenMinus ->
                    hyphenMinus ()
        )
        |> Morph.try (\() -> ExclamationMark) (Char.Morph.only '!')
        |> Morph.try (\() -> NumberSign) (Char.Morph.only '#')
        |> Morph.try (\() -> DollarSign) (Char.Morph.only '$')
        |> Morph.try (\() -> PercentSign) (Char.Morph.only '%')
        |> Morph.try (\() -> Ampersand) (Char.Morph.only '&')
        |> Morph.try (\() -> Asterisk) (Char.Morph.only '*')
        |> Morph.try (\() -> LowLine) (Char.Morph.only '_')
        |> Morph.try (\() -> HyphenMinus) (Char.Morph.only '-')
        |> Morph.choiceFinish
```

-}
validate :
    String
    -> (narrow -> Result deadEnd narrow)
    ->
        MorphIndependently
            (narrow -> Result (ErrorWithDeadEnd deadEnd) narrow)
            (broad -> broad)
validate descriptionCustom narrowConvert =
    value descriptionCustom
        { narrow = narrowConvert
        , broaden = identity
        }


{-| Mutual [`Morph`](#Morph) between representations
that have the same structural information
and can be mapped 1:1 into each other.
[narrowing](#mapTo) can `Never` fail

Examples:

  - some [`Morph`](#Morph) needs a different type

        translate Set.toList Set.fromList
            |> Morph.over
                (Value.list elementMorph)

      - [`Array.Morph.toList`](Array-#toList), [`Array.Morph.fromList`](Array-#fromList)
      - [`Stack.Morph.toString`](Stack-#toString), [`Stack.Morph.fromString`](Stack-#fromString)

  - strip unnecessary information
    ~`{ end : (), before :`~`List element`~`}`~

        translate .before
            (\before_ -> { before = before_, end = () })

Only use [`Translate`](#Translate) to annotate consumed inputs, for results,

    MorphOrError (List Char) String error_

allows it to be used in more general [`Morph`](#Morph) chains where the target value can be a concrete error.

Both type arguments are really equal in "narrowness",
so choosing one as the `mapped` and one as the `unmapped` is rather arbitrary.

That's the reason it's a good idea to always expose 2 versions: `aToB` & `bToA`.

**!** Information can get lost on the way:

    dictFromListMorph =
        Morph.translate Dict.fromList Dict.toList

Still, there's no narrowing necessary to translate one state to the other

-}
type alias Translate narrow broad =
    MorphOrError narrow broad (ErrorWithDeadEnd Never)


{-| Switch between 2 opposite representations. Examples:

    toggle List.reverse

    toggle not

    toggle negate

    toggle (\n -> n ^ -1)

    toggle Linear.opposite

If you want to allow both directions to [`MorphIndependently`](#MorphIndependently),
opt for `translate v v` instead of `toggle v`!

-}
toggle :
    (value -> value)
    ->
        MorphIndependently
            (value -> Result error_ value)
            (value -> value)
toggle changeToOpposite =
    translate changeToOpposite changeToOpposite


{-| A [`Morph`](#Morph) that doesn't transform anything.
Any possible input stays, remains the same. A no-op.

Same as writing:

  - [`map`](#map)`identity`
  - [`unmap`](#unmap)`identity`
  - [`validate`](#validate)`Ok`
  - [`translate`](#translate)`identity identity`
  - `{ broaden = identity, narrow = Ok }`
  - [`toggle`](#toggle)`identity` when broad and narrow types match

-}
keep :
    MorphIndependently
        (narrow -> Result error_ narrow)
        (broad -> broad)
keep =
    translate identity identity


{-| Create a [`Translate`](#Translate)

    stringToListMorph : Morph (List Char) String error_
    stringToListMorph =
        Morph.translate String.toList String.fromList

See the type's documentation for more detail

-}
translate :
    (beforeMap -> mapped)
    -> (beforeUnmap -> unmapped)
    ->
        MorphIndependently
            (beforeMap -> Result error_ mapped)
            (beforeUnmap -> unmapped)
translate map unmap =
    { description =
        { custom = Emptiable.empty, inner = Emptiable.empty }
    , narrow = map >> Ok
    , broaden = unmap
    }


{-| Only broadens (unmaps), doesn't narrow.
What comes out as the broad thing will be transformed but input doesn't.

What is great is using this to make inputs more "user-usable":

    ArraySized.Morph.maxToInfinity :
        MorphIndependently
            (narrow -> Result error_ narrow)
            (ArraySized element (In (On min) max_)
             -> ArraySized element (In (On min) Infinity)
            )
    ArraySized.Morph.maxToInfinity =
        Morph.broaden ArraySized.maxToInfinity

However! This can also often be an anti-pattern. See [`validate`](#validate).

    "WOW"
        |> Morph.broadenFrom
            (Morph.broaden String.toLower
                |> Morph.over stringValidation
            )
    --→ "wow"

-}
broaden :
    (beforeBroaden -> broad)
    ->
        MorphIndependently
            (narrow -> Result error_ narrow)
            (beforeBroaden -> broad)
broaden broadenFromNarrow =
    translate identity broadenFromNarrow


{-| [`Morph`](#Morph) that always [`broaden`](#broaden)s to a given constant.

For any more complex [`broaden`](#broaden)ing process, use [`translate`](#translate)

-}
broad :
    broadConstant
    ->
        MorphIndependently
            (beforeNarrow_ -> Result error_ ())
            (() -> broadConstant)
broad broadConstantSeed =
    translate (\_ -> ()) (\() -> broadConstantSeed)


{-| Match only the specific given broad input.

Make helpers for each type of constant for convenience

    Char.Morph.only broadCharConstant =
        Morph.only String.fromChar broadCharConstant

-}
only :
    (broadConstant -> String)
    -> broadConstant
    -> Morph () broadConstant
only broadConstantToString broadConstant =
    value
        ([ "only '", broadConstant |> broadConstantToString, "'" ] |> String.concat)
        { narrow =
            \broadValue ->
                if broadValue == broadConstant then
                    () |> Ok

                else
                    broadValue
                        |> broadConstantToString
                        |> Err
        , broaden =
            \() -> broadConstant
        }


{-| Create a custom morph for a value by explicitly specifying

  - a `String` description
  - `narrow`: a transformation that can fail with a `String` error
  - `broaden`: a transformation that can build the parsed value back to what a value that can be parsed

-}
value :
    String
    ->
        { narrow : beforeNarrow -> Result deadEnd narrow
        , broaden : beforeBroaden -> broad
        }
    ->
        MorphIndependently
            (beforeNarrow -> Result (ErrorWithDeadEnd deadEnd) narrow)
            (beforeBroaden -> broad)
value descriptionCustom morphTransformations =
    { description =
        { custom = Stack.one descriptionCustom
        , inner = Emptiable.empty
        }
    , narrow =
        morphTransformations.narrow
            >> Result.mapError DeadEnd
    , broaden = morphTransformations.broaden
    }



--


{-| To reference a [`Morph`](#Morph) in recursive definitions

    import Morph exposing (grab, skip, one)
    import Integer.Morph
    import String.Morph

    type LazyList
        = End
        | Next ( Int, LazyList )

    lazyList : MorphRow LazyList
    lazyList =
        Morph.choice
            (\endVariant nextVariant lazyListNarrow ->
                case lazyListNarrow of
                    End ->
                        endVariant ()
                    Next next ->
                        nextVariant next
            )
            |> Morph.try (\() -> End)
                (String.Morph.only "[]")
            |> Morph.try Next
                (Morph.succeed Tuple.pair
                    |> grab
                        (Integer.Morph.toInt
                            |> Morph.overRow Integer.Morph.text
                        )
                    |> skip
                        (broad [ () ]
                            |> Morph.overRow
                                (atLeast n1 (String.Morph.only " "))
                        )
                    |> skip (String.Morph.only "::")
                    |> skip
                        (broad [ () ]
                            |> Morph.overRow
                                (atLeast n1 (String.Morph.only " "))
                        )
                    |> grab (Morph.lazy (\() -> lazyList))
                )

    "[]" |> Text.narrowTo lazyList
    --> Ok End

    "a :: []" |> Text.narrowTo lazyList
    --> Ok (Next 'a' End)

    "a :: b :: []" |> Text.narrowTo lazyList
    --> Ok (Next 'a' (Next 'b' End))

Without `lazy`, you would get an error like:

>     The `lazyList` definition is causing a very tricky infinite loop.
>
>     The `lazyList` value depends on itself through the following chain of
>     definitions:
>
>           ┌─────┐
>           │    lazyList
>           │     ↓
>           │    lazyListNext
>           └─────┘

Read more about why this limitation exists
in [compiler hint "bad recursion"](https://github.com/elm/compiler/blob/master/hints/bad-recursion.md#tricky-recursion)
up until the end

-}
lazy :
    (()
     ->
        MorphIndependently
            (beforeNarrow -> Result error narrow)
            (beforeBroaden -> broad)
    )
    ->
        MorphIndependently
            (beforeNarrow -> Result error narrow)
            (beforeBroaden -> broad)
lazy morphLazy =
    { description = morphLazy () |> description
    , narrow =
        \broadValue ->
            broadValue |> narrowTo (morphLazy ())
    , broaden =
        \narrowValue ->
            narrowValue |> broadenFrom (morphLazy ())
    }


{-| [`Morph`](#Morph) on groups in progress.
Start with [`group`](#group), complete with [`part`](#part), finally [`groupFinish`](#groupFinish)
-}
type alias GroupMorphNoPart noPartPossiblyOrNever narrow broaden =
    RecordWithoutConstructorFunction
        { description :
            -- parts
            Emptiable (Stacked Description) noPartPossiblyOrNever
        , narrow : narrow
        , broaden : broaden
        }


{-| Assemble a group from narrow and broad [`part`](#part)s

Use [`groupToFrom`](#groupToFrom)
when each broad, narrow [`part`](#part) always has their respective counterpart

    ( "4", "5" )
        |> Morph.narrowTo
            (Morph.groupToFrom
                ( \x y -> { x = x, y = y }
                , \x y -> ( x, y )
                )
                |> Group.part ( .x, Tuple.first )
                    (Integer.Morph.toInt
                        |> Morph.overRow Integer.Morph.rowChar
                        |> Morph.rowFinish
                    )
                |> Group.part ( .y, Tuple.second )
                    (Integer.Morph.toInt
                        |> Morph.over (Integer.Morph.bitSizeAtMost n32)
                        |> Morph.overRow Integer.Morph.rowChar
                        |> Morph.rowFinish
                    )
                |> Group.finish
            )
    --> Ok { x = 4, y = 5 }

-}
groupToFrom :
    ( narrowAssemble
    , broadAssemble
    )
    ->
        GroupMorphNoPart
            Possibly
            (broad_
             -> Result error_ narrowAssemble
            )
            (groupNarrow_ -> broadAssemble)
groupToFrom ( narrowAssemble, broadAssemble ) =
    { description = Emptiable.empty
    , narrow = \_ -> narrowAssemble |> Ok
    , broaden = \_ -> broadAssemble
    }


{-| The [`Morph`](#Morph) of the next part in a [`group`](#group).

    Group.build
        ( \nameFirst nameLast email ->
            { nameFirst = nameFirst, nameLast = nameLast, email = email }
        , \nameFirst nameLast email ->
            { nameFirst = nameFirst, nameLast = nameLast, email = email }
        )
        |> Group.part ( .nameFirst, .nameFirst ) remain
        |> Group.part ( .nameLast, .nameLast ) remain
        |> Group.part ( .email, .email ) emailMorph
        |> Group.finish

-}
part :
    ( groupNarrow -> partNarrow
    , groupBroad -> partBroad
    )
    -> MorphOrError partNarrow partBroad partError
    ->
        (GroupMorphNoPart
            noPartPossiblyOrNever_
            (groupBroad
             ->
                Result
                    (PartsError partError)
                    (partNarrow -> groupNarrowFurther)
            )
            (groupNarrow -> (partBroad -> groupBroadenFurther))
         ->
            GroupMorphNoPart
                noPartNever_
                (groupBroad
                 ->
                    Result
                        (PartsError partError)
                        groupNarrowFurther
                )
                (groupNarrow -> groupBroadenFurther)
        )
part ( narrowPartAccess, broadPartAccess ) partMorph =
    \groupMorphSoFar ->
        { description =
            groupMorphSoFar.description
                |> Stack.onTopLay partMorph.description
        , narrow =
            groupMorphSoFar.narrow
                |> narrowPart
                    (groupMorphSoFar.description |> Stack.length)
                    broadPartAccess
                    (narrowTo partMorph)
        , broaden =
            groupMorphSoFar.broaden
                |> broadenPart narrowPartAccess (broadenFrom partMorph)
        }


broadenPart :
    (groupNarrow -> partNarrow)
    -> (partNarrow -> partBroad)
    ->
        ((groupNarrow -> (partBroad -> groupBroadenFurther))
         -> (groupNarrow -> groupBroadenFurther)
        )
broadenPart narrowPartAccess broadenPartMorph =
    \groupMorphSoFarBroaden ->
        \groupNarrow ->
            (groupNarrow |> groupMorphSoFarBroaden)
                (groupNarrow
                    |> narrowPartAccess
                    |> broadenPartMorph
                )


narrowPart :
    Int
    -> (groupBroad -> partBroad)
    -> (partBroad -> Result partError partNarrow)
    ->
        ((groupBroad
          ->
            Result
                (PartsError partError)
                (partNarrow -> groupNarrowFurther)
         )
         ->
            (groupBroad
             ->
                Result
                    (PartsError partError)
                    groupNarrowFurther
            )
        )
narrowPart index broadPartAccess narrowPartMorph =
    \groupMorphSoFarNarrow ->
        \groupBroad ->
            let
                narrowPartOrError : Result partError partNarrow
                narrowPartOrError =
                    groupBroad
                        |> broadPartAccess
                        |> narrowPartMorph
            in
            case ( groupBroad |> groupMorphSoFarNarrow, narrowPartOrError ) of
                ( Ok groupMorphSoFarEat, Ok partNarrow ) ->
                    groupMorphSoFarEat partNarrow |> Ok

                ( Ok _, Err partError ) ->
                    { index = index, error = partError }
                        |> Stack.one
                        |> Err

                ( Err partsSoFarErrors, Ok _ ) ->
                    partsSoFarErrors |> Err

                ( Err partsSoFarErrors, Err partError ) ->
                    partsSoFarErrors
                        |> Stack.onTopLay { index = index, error = partError }
                        |> Err


{-| Conclude a [`Group.build`](#group) |> [`Group.part`](#part) chain
-}
groupFinish :
    GroupMorphNoPart
        Never
        (beforeNarrow
         ->
            Result
                (PartsError (ErrorWithDeadEnd deadEnd))
                narrowed
        )
        (beforeBroaden -> broadened)
    ->
        MorphIndependently
            (beforeNarrow
             -> Result (ErrorWithDeadEnd deadEnd) narrowed
            )
            (beforeBroaden -> broadened)
groupFinish =
    \groupMorphInProgress ->
        { description =
            case groupMorphInProgress.description |> Emptiable.fill of
                Stack.TopBelow ( part0, part1 :: parts2Up ) ->
                    { inner =
                        ArraySized.l2 part0 part1
                            |> ArraySized.attachMin Up
                                (parts2Up |> ArraySized.fromList)
                            |> Group
                            |> Emptiable.filled
                    , custom = Emptiable.empty
                    }

                Stack.TopBelow ( partOnly, [] ) ->
                    partOnly
        , narrow =
            \broad_ ->
                broad_
                    |> groupMorphInProgress.narrow
                    |> Result.mapError Parts
        , broaden = groupMorphInProgress.broaden
        }


{-| Go over an additional step of [`Morph`](#Morph) on its broad type

Chaining

  - `<<` on the broad side
  - `<< Result.andThen` on the narrow side

This can be used to, for example

  - [`Translate`](#Translate) what was [narrowed](#narrowTo)
  - narrow only one variant,
    then of that variant's value type one of their variants

-}
over :
    MorphIndependently
        (beforeBeforeNarrow -> Result error beforeNarrow)
        (beforeBroaden -> broad)
    ->
        (MorphIndependently
            (beforeNarrow -> Result error narrow)
            (beforeBeforeBroaden -> beforeBroaden)
         ->
            MorphIndependently
                (beforeBeforeNarrow -> Result error narrow)
                (beforeBeforeBroaden -> broad)
        )
over morphNarrowBroad =
    \morph ->
        { description =
            { custom = Emptiable.empty
            , inner =
                Over
                    { narrow = morph |> description
                    , broad = morphNarrowBroad |> description
                    }
                    |> Emptiable.filled
            }
        , broaden =
            broadenFrom morph
                >> broadenFrom morphNarrowBroad
        , narrow =
            narrowTo morphNarrowBroad
                >> Result.andThen (narrowTo morph)
        }


{-| `Translate a <-> b`
by swapping the functions [`map`](#map) <-> [`unmap`](#unmap).

    [ 'O', 'h', 'a', 'y', 'o' ]
        |> Morph.map
           (Text.toList |> Morph.invert)
    --> "Ohayo"

This can be used to easily create a `fromX`/`toX` pair

    module Stack.Morph exposing (fromListNonEmpty)

    import Emptiable exposing (Emptiable)
    import List.NonEmpty
    import Stack exposing (Stacked)

    fromListNonEmpty :
        MorphIndependently
            (List.NonEmpty.NonEmpty element
             -> Result error_ (Emptiable (Stacked element) never_)
            )
            (Emptiable (Stacked element) Never
             -> List.NonEmpty.NonEmpty element
            )
    fromListNonEmpty =
        toListNonEmpty |> Morph.invert

    toListNonEmpty :
        MorphIndependently
            (Emptiable (Stacked element) Never
             -> Result error_ (List.NonEmpty.NonEmpty element)
            )
            (List.NonEmpty.NonEmpty element
             -> Emptiable (Stacked element) never_
            )
    toListNonEmpty =
        translate Stack.toListNonEmpty Stack.fromListNonEmpty

[`unmap`](#unmap) `...` is equivalent to `map (... |> reverse)`.

-}
invert :
    MorphIndependently
        (beforeMap -> Result (ErrorWithDeadEnd Never) mapped)
        (beforeUnmap -> unmapped)
    ->
        MorphIndependently
            (beforeUnmap -> Result error_ unmapped)
            (beforeMap -> mapped)
invert =
    \translate_ ->
        { description = translate_ |> description
        , narrow =
            \unmapped ->
                unmapped |> broadenFrom translate_ |> Ok
        , broaden = mapTo translate_
        }


{-| Change all [`DeadEnd`](#ErrorWithDeadEnd)s based on their current values.

`deadEnd` can for example be changed to formatted text for display.
For that, use [`MorphOrError`](#MorphOrError) [`ErrorWithDeadEnd`](#ErrorWithDeadEnd) doing [`mapDeadEnd`](#mapDeadEnd)
on [`Morph`](#Morph) that are returned.

Have trouble doing so because some API is too strict on errors? → issue

See also: [`deadEndNever`](#deadEndNever)

-}
deadEndMap :
    (deadEnd -> deadEndMapped)
    ->
        (ErrorWithDeadEnd deadEnd
         -> ErrorWithDeadEnd deadEndMapped
        )
deadEndMap deadEndChange =
    \error ->
        case error of
            DeadEnd deadEnd ->
                deadEnd |> deadEndChange |> DeadEnd

            Row row ->
                { startDown = row.startDown
                , error = row.error |> deadEndMap deadEndChange
                }
                    |> Row

            Parts parts ->
                parts
                    |> Stack.map
                        (\_ partError ->
                            { index = partError.index
                            , error = partError.error |> deadEndMap deadEndChange
                            }
                        )
                    |> Parts

            Tries possibilities ->
                possibilities
                    |> Stack.map
                        (\_ -> deadEndMap deadEndChange)
                    |> Tries


{-| An [`Error`](#ErrorWithDeadEnd) where running into a dead end is impossible can't be created.
Therefore, you can treat it as _any_ value.

Under the hood, only [`Basics.never`] is used so it's completely safe to use.

-}
deadEndNever : ErrorWithDeadEnd Never -> any_
deadEndNever =
    \error ->
        case error of
            DeadEnd deadEnd ->
                deadEnd |> never

            Row row ->
                row.error |> deadEndNever

            Parts parts ->
                parts
                    |> Stack.top
                    |> .error
                    |> deadEndNever

            Tries possibilities ->
                possibilities
                    |> Stack.top
                    |> deadEndNever


{-| Change the potential [`Error`](#Error) TODO This is usually used with either

  - [`deadEndNever : ErrorWithDeadEnd Never -> any_`](#deadEndNever)
  - [`deadEndMap`](#deadEndMap)

-}
narrowErrorMap :
    (error -> errorMapped)
    ->
        MorphIndependently
            (beforeNarrow -> Result error narrowed)
            (beforeBroaden -> broadened)
    ->
        MorphIndependently
            (beforeNarrow -> Result errorMapped narrowed)
            (beforeBroaden -> broadened)
narrowErrorMap errorChange =
    \morph ->
        { description = morph |> description
        , broaden = broadenFrom morph
        , narrow =
            narrowTo morph
                >> Result.mapError errorChange
        }



--


{-| Morph the structure's elements

    List.Morph.elementTranslate elementTranslate =
        translateOn ( List.map, List.map ) elementTranslate

-}
translateOn :
    ( (elementBeforeMap -> elementMapped)
      -> (structureBeforeMap -> structureMapped)
    , (elementBeforeUnmap -> elementUnmapped)
      -> (structureBeforeUnmap -> structureUnmapped)
    )
    ->
        (MorphIndependently
            (elementBeforeMap
             -> Result (ErrorWithDeadEnd Never) elementMapped
            )
            (elementBeforeUnmap -> elementUnmapped)
         ->
            MorphIndependently
                (structureBeforeMap -> Result error_ structureMapped)
                (structureBeforeUnmap -> structureUnmapped)
        )
translateOn ( structureMap, structureUnmap ) elementTranslate =
    { description =
        { custom = Emptiable.empty
        , inner = Emptiable.empty
        }
    , narrow =
        \broad_ ->
            broad_
                |> structureMap (mapTo elementTranslate)
                |> Ok
    , broaden =
        structureUnmap (broadenFrom elementTranslate)
    }



-- row


{-| Parser-builder:

  - grab some elements from an input stack,
    and return either a value or else an [`Error`](#Error)

  - take the value and turn it back into an input stack

```
{-| [`MorphRow`](#MorphRow) on input characters
-}
type alias MorphText narrow =
    MorphRow narrow Char
```

[`MorphRow`](#MorphRow) is inspired by [`lambda-phi/parser`](https://dark.elm.dmy.fr/packages/lambda-phi/parser/latest/)


## example: 2D point

    import Morph exposing (MorphRow, atLeast, skip, into, Morph.succeed, grab, one)
    import Char.Morph as Char
    import String.Morph as Text exposing (number)
    import Morph.Error

    type alias Point =
        -- makes `Point` function unavailable:
        -- https://dark.elm.dmy.fr/packages/lue-bird/elm-no-record-type-alias-constructor-function/latest/
        RecordWithoutConstructorFunction
            { x : Float
            , y : Float
            }

    -- successful parsing looks like
    "(2.71, 3.14)" |> narrow (listToString |> over point)
    --> Ok { x = 2.71, y = 3.14 }

    -- building always works
    { x = 2.71, y = 3.14 } |> broad (listToString |> over point)
    --> "( 2.71, 3.14 )"

    point : MorphRow Point Char
    point =
        Morph.to "point"
            (Morph.succeed (\x y -> { x = x, y = y })
                |> skip (String.Morph.only "(")
                |> skip
                    (broad (ArraySIzed.one ())
                        |> Morph.overRow (atLeast (String.Morph.only " ") n0)
                    )
                |> grab number
                |> skip
                    (broad ArraySIzed.empty
                        |> Morph.overRow (atLeast (String.Morph.only " ") n0)
                    )
                |> skip (String.Morph.only ",")
                |> skip
                    (broad (ArraySIzed.one ())
                        |> Morph.overRow (atLeast (String.Morph.only " ") n0)
                    )
                |> grab .x Number.Morph.text
                |> skip
                    (broad (ArraySIzed.one ())
                        |> Morph.overRow (atLeast (String.Morph.only " ") n0)
                    )
                |> skip (String.Morph.only ")")
            )

    -- we can get a nice error message if it fails
    "(2.71, x)"
        |> Text.narrowTo point
        |> Result.mapError (Morph.Error.dump "filename.txt")
    --> Err
    -->     [ "[ERROR] filename.txt: line 1:8: I was expecting a digit [0-9]. I got stuck when I got 'x'."
    -->     , "  in Point at line 1:1"
    -->     , ""
    -->     , "1|(2.71, x)"
    -->     , "  ~~~~~~~^"
    -->     ]

Note before we start:
`MorphRow` _always backtracks_ and never commits to a specific path!

  - 👍 improves readability

    crucial so we don't experience reports like

    > "If it compiles it runs"
    >
    > Unless you are writing a parser.
    >
    > The parser doesn't care.
    >
    > The parser will compile and then murder you for breakfast.

    – xarvh (Francesco Orsenigo) on slack

  - 👍 error messages will always show all options and why they failed,
    showing those that came the furthest first

  - 👎 performs worse as there are more [possibilities](Choice#try) to parse to know it failed

-}
type alias MorphRow narrow broadElement =
    MorphIndependently
        (Emptiable (Stacked broadElement) Possibly
         ->
            Result
                Error
                { narrow : narrow
                , broad : Emptiable (Stacked broadElement) Possibly
                }
        )
        (narrow -> Emptiable (Stacked broadElement) Possibly)


{-| Incomplete [`MorphRow`](#MorphRow) for a thing composed of multiple parts = group.
It's what you supply during a [`Morph.succeed`](#Morph.succeed)`|>`[`grab`](#grab)/[`skip`](#skip) build
-}
type alias MorphRowIndependently beforeBroaden narrowed broadElement =
    MorphIndependently
        (Emptiable (Stacked broadElement) Possibly
         ->
            Result
                Error
                { narrow : narrowed
                , broad : Emptiable (Stacked broadElement) Possibly
                }
        )
        (beforeBroaden
         -> Emptiable (Stacked broadElement) Possibly
        )


{-| [`MorphRow`](#MorphRow) from and to a single broad input.


## `Morph.keep |> Morph.one`

> ℹ️ Equivalent regular expression: `.`

    import Morph
    import Morph.Error
    import String.Morph as Text

    -- can match any character
    "a" |> Text.narrowTo (Morph.keep |> one)
    --> Ok 'a'

    "#" |> Text.narrowTo (Morph.keep |> one)
    --> Ok '#'

    -- only fails if we run out of inputs
    ""
        |> Text.narrowTo (Morph.keep |> one)
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:0: I was expecting a character. I reached the end of the input."

-}
one :
    Morph narrow broadElement
    -> MorphRow narrow broadElement
one =
    \morph ->
        { description =
            morph |> description
        , narrow =
            \broad_ ->
                case broad_ of
                    Emptiable.Empty _ ->
                        { error = "end of input" |> DeadEnd
                        , startDown = 0
                        }
                            |> Row
                            |> Err

                    Emptiable.Filled (Stack.TopBelow ( nextBroadElement, afterNextBroadElement )) ->
                        case
                            nextBroadElement
                                |> narrowTo morph
                        of
                            Ok narrowNarrow ->
                                { narrow = narrowNarrow
                                , broad =
                                    afterNextBroadElement
                                        |> Stack.fromList
                                }
                                    |> Ok

                            Err error ->
                                { error = error
                                , startDown = broad_ |> Stack.length
                                }
                                    |> Row
                                    |> Err
        , broaden =
            broadenFrom morph
                >> Stack.one
        }


{-| Never consumes anything.
Always returns the given narrow constant.
Never fails.

For anything composed of multiple parts,
first declaratively describes what you expect to get in the end,
then [taking](#grab) and [dropping](#skip) what you need to parse

    import Morph exposing (Morph.succeed, one)
    import String.Morph exposing (integer)

    type alias Point =
        -- makes `Point` function unavailable:
        -- https://dark.elm.dmy.fr/packages/lue-bird/elm-no-record-type-alias-constructor-function/latest/
        RecordWithoutConstructorFunction
            { x : Int
            , y : Int
            }

    point : MorphRow Char Point
    point =
        Morph.succeed (\x y -> { x = x, y = y })
            |> grab .x integer
            |> skip (MorphRow.only [ ',' ])
            |> grab .y integer

    "12,34" |> Text.narrowTo point
    --> Ok { x = 12, y = 34 }


### `Morph.succeed` anti-patterns

One example you'll run into when using other parsers is using

    Morph.succeed identity
        |> skip ...
        |> skip ...
        |> grab ...
        |> skip ...

it get's pretty hard to read as you have to jump around the code to know what you're actually producing

    Morph.succeed (\sum -> sum) |> ...

is already nicer

-}
succeed :
    narrowConstant
    -> MorphRowIndependently beforeBroaden_ narrowConstant broadElement_
succeed narrowConstant =
    { description =
        { custom = Emptiable.empty
        , inner = Emptiable.empty
        }
    , narrow =
        \broad_ ->
            { narrow = narrowConstant
            , broad = broad_
            }
                |> Ok
    , broaden =
        \_ -> Emptiable.empty
    }


{-| Take what we get from [converting](#MorphRow) the next section
and channel it back up to the [`Morph.succeed`](#Morph.succeed) grouping
-}
grab :
    (groupNarrow -> partNextNarrow)
    -> MorphRow partNextNarrow broadElement
    ->
        (MorphRowIndependently
            groupNarrow
            (partNextNarrow -> groupNarrowFurther)
            broadElement
         ->
            MorphRowIndependently
                groupNarrow
                groupNarrowFurther
                broadElement
        )
grab partAccess grabbedNextMorphRow =
    \groupMorphRowSoFar ->
        { description = groupMorphRowSoFar |> description
        , narrow =
            \broad_ ->
                broad_
                    |> narrowTo groupMorphRowSoFar
                    |> Result.andThen
                        (\result ->
                            result.broad
                                |> narrowTo grabbedNextMorphRow
                                |> Result.map
                                    (\nextParsed ->
                                        { narrow = result.narrow nextParsed.narrow
                                        , broad = nextParsed.broad
                                        }
                                    )
                        )
        , broaden =
            \groupNarrow ->
                groupNarrow
                    |> partAccess
                    |> grabbedNextMorphRow.broaden
                    |> Stack.attach Down
                        (groupNarrow
                            |> groupMorphRowSoFar.broaden
                        )
        }


{-| Require values to be matched next to continue but ignore the result.

    import String.Morph exposing (text)
    import Morph exposing (Morph.succeed, atLeast, take, drop)

    -- parse a simple email, but we're only interested in the username
    "user@example.com"
        |> Text.narrowTo
            (Morph.succeed (\userName -> { username = userName })
                |> grab .username (atLeast n1 aToZ)
                |> skip (one '@')
                |> skip
                    (Text.fromList
                        |> Morph.overRow (atLeast n1 aToZ)
                        |> broad "example"
                    )
                |> skip (text ".com")
            )
    --> Ok { username = "user" }

[`broad`](#broad) `... |>` [`Morph.overRow`](MorphRow#over) is cool:
when multiple kinds of input can be dropped,
it allows choosing a default possibility for building.

-}
skip :
    MorphRow () broadElement
    ->
        (MorphRowIndependently groupNarrow narrow broadElement
         -> MorphRowIndependently groupNarrow narrow broadElement
        )
skip ignoredNext =
    \groupMorphRowSoFar ->
        { description = groupMorphRowSoFar |> description
        , narrow =
            \broad_ ->
                broad_
                    |> narrowTo groupMorphRowSoFar
                    |> Result.andThen
                        (\result ->
                            result.broad
                                |> narrowTo ignoredNext
                                |> Result.map
                                    (\nextParsed ->
                                        { narrow = result.narrow
                                        , broad = nextParsed.broad
                                        }
                                    )
                        )
        , broaden =
            \groupNarrow ->
                (() |> ignoredNext.broaden)
                    |> Stack.attach Down
                        (groupNarrow |> groupMorphRowSoFar.broaden)
        }



-- chain
{-

   `next` existed once:

       next :
           (narrow -> broad)
           -> (broad -> MorphRow broadElement narrow)
           ->
               (Row broadElement broad
               -> MorphRow broadElement narrow
               )

   After [converting](#MorphRow) the previous section,
   it formed another [morph](#MorphRow) with the value we got.

   It allowed using the last value for the next [`MorphRow`](#MorphRow) like a backreference.

   But!

     - one should [`Morph.overRow`](#over) to narrow parsed values
     - one should [loop](#loop) instead of recursively recursively [`next`](#next)
     - one should try to know what to morph by tracking context,
       independent of what narrow result the last morph gave
         - for example, don't use [`next`](#next) for versioning etc.
           Use [`Morph.choice`](#Morph.choice) where each [`possibility`](#try) expects a specific number


-}


{-| Describe how to reach an even broader type.

  - try to keep [`Morph.overRow`](#over) filters/validations to a minimum to get
      - a better error description out of the box
      - a more descriptive and correct type
      - building invalid values becomes impossible

↓

    import Morph exposing (toggle)
    import Morph exposing (map, atLeast)
    import Char.Morph as Char
    import Text

    -- get some letters, make them lowercase
    "ABC"
        |> Text.narrowTo
            (atLeast n1 AToZ.char
                |> map Text.fromList
                |> map (toggle String.toLower)
            )
    --> Ok "abc"

-}
overRow :
    MorphRowIndependently beforeBroaden beforeNarrow broadElement
    ->
        (MorphIndependently
            (beforeNarrow -> Result Error narrow)
            (beforeBeforeBroaden -> beforeBroaden)
         -> MorphRowIndependently beforeBeforeBroaden narrow broadElement
        )
overRow morphRowBeforeMorph =
    \narrowMorph ->
        { description =
            { custom = Emptiable.empty
            , inner =
                Over
                    { narrow = narrowMorph |> description
                    , broad = morphRowBeforeMorph |> description
                    }
                    |> Emptiable.filled
            }
        , narrow =
            \broad_ ->
                broad_
                    |> narrowTo morphRowBeforeMorph
                    |> Result.andThen
                        (\narrowed ->
                            narrowed.narrow
                                |> narrowTo narrowMorph
                                |> Result.map
                                    (\narrowNarrow ->
                                        { narrow = narrowNarrow
                                        , broad = narrowed.broad
                                        }
                                    )
                                |> Result.mapError
                                    (\error ->
                                        { error = error
                                        , startDown = broad_ |> Stack.length
                                        }
                                            |> Row
                                    )
                        )
        , broaden =
            broadenFrom narrowMorph
                >> broadenFrom morphRowBeforeMorph
        }



-- sequence


{-| [Morph](#MorphRow) multiple elements from now to when `end` matches.

    decoderNameSubject : MorphRow String Char expectationCustom
    decoderNameSubject =
        Text.fromList
            |> Morph.overRow
                (MorphRow.before
                    { end =
                        MorphRow.Morph.succeed ()
                            |> skip (String.Morph.only "Decoder")
                            |> skip Morph.end
                    , goOn = Morph.keep |> Morph.one
                    }
                )

You might think: Why not use

    decoderNameSubject : MorphRow String Char expectationCustom
    decoderNameSubject =
        MorphRow.Morph.succeed (\subject -> subject)
            |> grab (\subject -> subject)
                (atLeast (Morph.keep |> Morph.one) n0)
            |> skip (String.Morph.only "Decoder")
            |> skip Morph.end

Problem is: This will never Morph.succeed.
`atLeast (Morph.keep |> Morph.one) n0` always goes on.
We never reach the necessary [`skip`](#skip)ped things.

-}
before :
    { end : MorphRow () broadElement
    , goOn : MorphRow goOnElement broadElement
    }
    -> MorphRow (List goOnElement) broadElement
before untilStep =
    until
        { commit =
            translate .before
                (\before_ -> { before = before_, end = () })
        , end = untilStep.end
        , goOn = untilStep.goOn
        }


{-| How to continue a loop.
Either continue with a partial result or return with a complete value
-}
type LoopStep partial complete
    = GoOn partial
    | Commit complete


{-| How are [`in_`](#in_), ... defined?

    decoderNameSubject : MorphRow String Char expectationCustom
    decoderNameSubject =
        Text.fromList
            |> Morph.overRow
                (MorphRow.until
                    { commit =
                        translate .before
                            (\before -> { before = before, end = () })
                    , end =
                        MorphRow.Morph.succeed ()
                            |> skip (String.Morph.only "Decoder")
                            |> skip Morph.end
                    , goOn = Morph.keep |> Morph.one
                    }
                )

↑ can be simplified with [`before`](#before)

Any kind of structure validation that if it fails should proceed to `goOn`
must be in `commit`

-}
until :
    { commit :
        Morph
            commitResult
            { end : endElement
            , before : List goOnElement
            }
    , end : MorphRow endElement broadElement
    , goOn : MorphRow goOnElement broadElement
    }
    -> MorphRow commitResult broadElement
until untilStep =
    let
        loopStep =
            choice
                (\commit goOn loopStepNarrow ->
                    case loopStepNarrow of
                        Commit commitElement ->
                            commit commitElement

                        GoOn goOnELement ->
                            goOn goOnELement
                )
                |> tryRow Commit untilStep.end
                |> tryRow GoOn untilStep.goOn
                |> choiceRowFinish
    in
    { description =
        { custom = Emptiable.empty
        , inner =
            Over
                { narrow = untilStep.commit |> description
                , broad =
                    { custom = Emptiable.empty
                    , inner =
                        Until
                            { end = untilStep.end |> description
                            , element = untilStep.goOn |> description
                            }
                            |> Emptiable.filled
                    }
                }
                |> Emptiable.filled
        }
    , broaden =
        let
            broadenStepBack :
                ()
                ->
                    (List goOnElement
                     -> Emptiable (Stacked broadElement) Possibly
                    )
            broadenStepBack () =
                \toStep ->
                    case toStep of
                        [] ->
                            Emptiable.empty

                        top :: tail ->
                            (top |> GoOn)
                                |> broadenFrom loopStep
                                |> Stack.attach Down
                                    (tail |> broadenStepBack ())
        in
        \commitResultNarrow ->
            let
                committedBack =
                    commitResultNarrow
                        |> broadenFrom untilStep.commit
            in
            (committedBack.before
                |> List.reverse
                |> broadenStepBack ()
            )
                |> Stack.attach Down
                    ((committedBack.end |> Commit)
                        |> broadenFrom loopStep
                    )
    , narrow =
        let
            loopNarrowStep :
                ()
                ->
                    (List goOnElement
                     ->
                        (Emptiable (Stacked broadElement) Possibly
                         ->
                            Result
                                Error
                                { narrow : commitResult
                                , broad : Emptiable (Stacked broadElement) Possibly
                                }
                        )
                    )
            loopNarrowStep () =
                \before_ ->
                    narrowTo loopStep
                        >> Result.andThen
                            (\stepped ->
                                case stepped.narrow of
                                    Commit committed ->
                                        case
                                            { before = before_, end = committed }
                                                |> narrowTo untilStep.commit
                                        of
                                            Err error ->
                                                { error = error
                                                , startDown = stepped.broad |> Stack.length
                                                }
                                                    |> Row
                                                    |> Err

                                            Ok commitResult ->
                                                { broad = stepped.broad
                                                , narrow = commitResult
                                                }
                                                    |> Ok

                                    GoOn goOnElement ->
                                        stepped.broad
                                            |> (before_ |> (::) goOnElement |> loopNarrowStep ())
                            )
        in
        [] |> loopNarrowStep ()
    }



--


{-| Only matches when there's no further broad input afterwards.

This is not required for [`narrow`](#narrow)ing to Morph.succeed.

It can, however simplify checking for specific endings:

    decoderNameSubject : MorphRow String Char expectationCustom
    decoderNameSubject =
        Text.fromList
            |> Morph.overRow
                (MorphRow.until
                    { commit =
                        translate .before
                            (\before -> { before = before, end = () })
                    , end =
                        MorphRow.Morph.succeed ()
                            |> skip (String.Morph.only "Decoder")
                            |> skip Morph.end
                    , goOn = Morph.keep |> Morph.one
                    }
                )

-}
end : MorphRow () broadElement_
end =
    { description =
        { custom = Stack.one "end"
        , inner = Emptiable.empty
        }
    , narrow =
        \broad_ ->
            case broad_ of
                Emptiable.Empty _ ->
                    { narrow = ()
                    , broad = Emptiable.empty
                    }
                        |> Ok

                Emptiable.Filled stacked ->
                    { startDown = stacked |> filled |> Stack.length
                    , error = "remaining input" |> DeadEnd
                    }
                        |> Row
                        |> Err
    , broaden =
        \() -> Emptiable.empty
    }


{-| Final step before running a [`MorphRow`](#MorphRow),
transforming it into a [`Morph`](#Morph) on the full stack of input elements.

    fromString =
        narrowTo
            (Point.morphRowChar
                |> Morph.rowFinish
                |> Morph.over Stack.Morph.toString
            )

-}
rowFinish :
    MorphRow narrow broadElement
    -> Morph narrow (Emptiable (Stacked broadElement) Possibly)
rowFinish =
    \morphRow ->
        { description = morphRow.description
        , narrow =
            \broadElements ->
                broadElements
                    |> narrowTo morphRow
                    |> Result.andThen
                        (\result ->
                            result.broad
                                |> narrowTo end
                                |> Result.map
                                    (\_ -> result.narrow)
                        )
        , broaden =
            \narrow -> narrow |> broadenFrom morphRow
        }



--


{-| Possibly incomplete [`Morph`](Morph#Morph) a choice from a [`Value`](Value#Value).
See [`Morph.choice`](#Morph.choice), [`variantValue`](#variantValue), [`finishValue`](#finishValue)
-}
type alias ChoiceMorphNoTry noTryPossiblyOrNever choiceNarrow choiceBeforeNarrow choiceBroaden error =
    ChoiceMorphNoVariant
        noTryPossiblyOrNever
        (choiceBeforeNarrow
         ->
            Result
                (-- tries
                 Emptiable (Stacked error) noTryPossiblyOrNever
                )
                choiceNarrow
        )
        choiceBroaden


{-| Word in an incomplete morph in progress. For example

    Morph.choiceFinish :
        Choice.MorphNoTry
            Never
            (N (In N0 N9)))
            Char
            (N (In N0 N9)) -> Char)
        -> Morph (N (In N0 N9)) Char

-}
type NoTry
    = NoTryTag Never


{-| Discriminate into possibilities

    {-| Invisible spacing character
    -}
    type Blank
        = Space
        | Tab
        | Return Line.Return
        | FormFeed

    blankChar : Morph Blank Char (Morph.Error Char)
    blankChar =
        Morph.to "blank"
            (Morph.choice
                (\spaceVariant tabVariant returnVariant formFeedVariant blankNarrow ->
                    case blankNarrow of
                        Space ->
                            spaceVariant ()

                        Tab ->
                            tabVariant ()

                        Return return_ ->
                            returnVariant return_

                        FormFeed ->
                            formFeedVariant ()
                )
                |> Morph.try (\() -> Space) (Char.Morph.only ' ')
                |> Morph.try (\() -> Tab) (Char.Morph.only '\t')
                |> Morph.try Return Line.returnChar
                |> Morph.try (\() -> FormFeed)
                    -- \f
                    (Char.Morph.only '\u{000C}')
                |> Morph.choiceFinish
            )

    {-| Line break character
    -}
    type Return
        = NewLine
        | CarriageReturn

    {-| Match a line break character: Either

      - new line `'\n'`
      - carriage return `'\r'`

    > ℹ️ Equivalent regular expression: `[\n\r]`

        import Morph.Error
        import String.Morph as Text

        -- match a blank
        "\n\t abc" |> Text.narrowTo blank --> Ok '\n'

        -- anything else makes it fail
        "abc"
            |> Text.narrowTo blank
            |> Result.mapError Morph.Error.textMessage
        --> Err "1:1: I was expecting a blank space or new line. I got stuck when I got 'a'."

    -}
    returnChar : Morph Return Char (Morph.Error Char)
    returnChar =
        Morph.choice
            (\newLineVariant carriageReturnVariant returnNarrow ->
                case returnNarrow of
                    NewLine ->
                        newLineVariant ()

                    CarriageReturn ->
                        carriageReturnVariant ()
            )
            |> Morph.try (\() -> NewLine)
                (Char.Morph.only '\n')
            |> Morph.try (\() -> CarriageReturn)
                -- \r
                (Char.Morph.only '\u{000D}')
            |> Morph.choiceFinish

    {-| The end of a text line:
    either a [return character](Return#Return) or the end of the whole text.
    -}
    type LineEnd
        = InputEnd
        | Return Return

    {-| Consume the end of the current line or Morph.succeed if there are
    no more remaining characters in the input text.

    > ℹ️ Equivalent regular expression: `$`

    -}
    endText : MorphRow Char LineEnd
    endText =
        Morph.choice
            (\returnVariant inputEndVariant maybeChoice ->
                case maybeChoice of
                    Return returnValue ->
                        returnVariant returnValue

                    InputEnd ->
                        inputEndVariant ()
            )
            |> Morph.tryRow Return
                (returnChar |> Morph.one)
            |> Morph.tryRow (\() -> InputEnd)
                Morph.end
            |> Morph.choiceRowFinish

-}
choice :
    broadenByPossibility
    ->
        ChoiceMorphNoTry
            Possibly
            choiceNarrow_
            choiceBroad_
            broadenByPossibility
            error_
choice choiceBroadenDiscriminatedByPossibility =
    { description = Emptiable.empty
    , narrow =
        \_ ->
            Emptiable.empty |> Err
    , broaden = choiceBroadenDiscriminatedByPossibility
    }


{-| Offer alternative [`Morph`](Morph#Morph) possibilities to a given preferred one.
Functionally, it's the same as [`Choice.equivalent`](#equivalent) with an optimization
as shown in ["Fast parsing of String Sets in Elm" by Marcelo Lazaroni](https://lazamar.github.io/fast-parsing-of-string-sets-in-elm/)
published as [`dict-parser`](https://dark.elm.dmy.fr/packages/lazamar/dict-parser/latest/Parser-Dict)

Usually, you'll be better off with a [`Morph.choice`](#between)
an explicit custom tagged union
because you'll have the option to preserve what was [narrowed](Morph#narrowTo).
(Remember: you can always discard that info and set a preferred option with [`Morph.broad`](Morph#broad))

Go [`Choice.equivalent`](Choice#equivalent) if you have a dynamic list of aliases/morphs to treat equally.
An example is defined variable names

    import Order

    Choice.equivalentRow String.Morph.only
        { broad = "∨"
        , alternatives = [ "|", "or" ]
        , order = Order.string { case_ = Order.lowerUpper }
        }

    Choice.equivalentRow String.Morph.only
        { broad = "±"
        , alternatives = [ "pm", "plusminus" ]
        , order = Order.string { case_ = Order.lowerUpper }
        }

TODO: optimize

-}
choiceEquivalentRow :
    (broadElement
     ->
        MorphIndependently
            (beforeNarrow
             -> Result (ErrorWithDeadEnd deadEnd) narrow
            )
            broaden
    )
    ->
        { broad : Emptiable (Stacked broadElement) broadPossibilityEmptyPossiblyOrNever_
        , alternatives :
            List
                (Emptiable (Stacked broadElement) alternativePossibilityEmptyPossiblyOrNever_)
        , order : broadElement -> broadElement -> Order
        }
    ->
        MorphIndependently
            (beforeNarrow
             -> Result (ErrorWithDeadEnd deadEnd) narrow
            )
            broaden
choiceEquivalentRow possibilityMorph possibilitiesOrdered =
    Debug.todo ""


{-| Offer alternative [`Morph`](Morph#Morph) possibilities to a given preferred one.

Usually, you'll be better off with a [`Morph.choice`](#between)
an explicit custom tagged union
because you'll have the option to preserve what was [narrowed](Morph#narrowTo).
(Remember: you can always discard that info and set a preferred option with [`Morph.broad`](Morph#broad))

Go [`Choice.equivalent`](Choice#equivalent) if you have a dynamic list of aliases/morphs to treat equally.
An example is defined variable names

    Choice.equivalent Char.Morph.only { broad = '∨', alternatives = [ '|' ] }

Use [`Choice.equivalentRow`](#equivalentRow) for strings etc.

-}
choiceEquivalent :
    (element
     ->
        MorphIndependently
            (beforeNarrow
             -> Result (ErrorWithDeadEnd deadEnd) narrow
            )
            broaden
    )
    ->
        { broad : element
        , alternatives : List element
        }
    ->
        MorphIndependently
            (beforeNarrow
             -> Result (ErrorWithDeadEnd deadEnd) narrow
            )
            broaden
choiceEquivalent traversePossibility possibilities =
    case possibilities.alternatives of
        [] ->
            traversePossibility possibilities.broad

        alternative0 :: alternatives1Up ->
            { description =
                { custom = Emptiable.empty
                , inner =
                    ArraySized.l2 possibilities.broad alternative0
                        |> ArraySized.attachMin Up (alternatives1Up |> ArraySized.fromList)
                        |> ArraySized.map traversePossibility
                        |> ArraySized.map description
                        |> Choice
                        |> Emptiable.filled
                }
            , narrow =
                \beforeNarrow ->
                    beforeNarrow
                        |> choiceEquivalentTryNarrow traversePossibility
                            (Stack.topBelow possibilities.broad
                                (alternative0 :: alternatives1Up)
                            )
            , broaden =
                (traversePossibility possibilities.broad).broaden
            }


choiceEquivalentTryNarrow :
    (element
     ->
        MorphIndependently
            (beforeNarrow
             -> Result (ErrorWithDeadEnd deadEnd) narrow
            )
            broaden
    )
    -> Emptiable (Stacked element) Never
    ->
        (beforeNarrow
         -> Result (ErrorWithDeadEnd deadEnd) narrow
        )
choiceEquivalentTryNarrow traverseTry tries =
    \beforeNarrow ->
        tries
            |> Stack.removeTop
            |> Stack.foldFrom
                (beforeNarrow
                    |> narrowTo
                        (traverseTry (tries |> Stack.top))
                    |> Result.mapError Stack.one
                )
                Up
                (\elementForMorph resultSoFar ->
                    resultSoFar
                        |> restoreTry
                            (\errorsSoFar ->
                                beforeNarrow
                                    |> narrowTo (traverseTry elementForMorph)
                                    |> Result.mapError
                                        (\error -> errorsSoFar |> Stack.onTopLay error)
                            )
                )
            |> Result.mapError Tries


{-| Builder for a [`Morph`](#Morph) to a choice. Possibly incomplete

Initialize with [`Morph.choiceToFrom`](#toFrom)

-}
type alias ChoiceMorphNoVariant noTryPossiblyOrNever narrow broaden =
    RecordWithoutConstructorFunction
        { description : Emptiable (Stacked Description) noTryPossiblyOrNever
        , narrow : narrow
        , broaden : broaden
        }



-- maybe variant


{-| If the previous [`possibility`](#try) fails
try this [`Morph`](#Morph).

> ℹ️ Equivalent regular expression: `|`

    import Char.Morph as Char
    import Morph.Error
    import AToZ exposing (AToZ)

    type UnderscoreOrLetter
        = Underscore
        | Letter Char

    underscoreOrLetter : Morph UnderscoreOrLetter Char
    underscoreOrLetter =
        Morph.choice
            (\underscore letter underscoreOrLetter ->
                case underscoreOrLetter of
                    Underscore ->
                        underscore ()

                    Letter aToZ ->
                        letter aToZ
            )
            |> try Underscore (Char.Morph.only '_')
            |> try Letter AToZ.char

    -- try the first possibility
    "_" |> Text.narrowTo (underscoreOrLetter |> one)
    --> Ok Underscore

    -- if it fails, try the next
    "a" |> Text.narrowTo (underscoreOrLetter |> one)
    --> Ok 'a'

    -- if none work, we get the error from all possible steps
    "1"
        |> Text.narrowTo (underscoreOrLetter |> one)
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:1: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '1'."

-}
try :
    (possibilityNarrow -> narrowChoice)
    ->
        MorphIndependently
            (possibilityBeforeNarrow
             -> Result error possibilityNarrow
            )
            (possibilityBeforeBroaden -> possibilityBroad)
    ->
        (ChoiceMorphNoTry
            noTryPossiblyOrNever_
            narrowChoice
            possibilityBeforeNarrow
            ((possibilityBeforeBroaden -> possibilityBroad)
             -> choiceBroadenFurther
            )
            error
         ->
            ChoiceMorphNoTry
                noTryNever_
                narrowChoice
                possibilityBeforeNarrow
                choiceBroadenFurther
                error
        )
try possibilityToChoice possibilityMorph =
    \choiceMorphSoFar ->
        { description =
            choiceMorphSoFar.description
                |> Stack.onTopLay possibilityMorph.description
        , narrow =
            \broadValue ->
                broadValue
                    |> choiceMorphSoFar.narrow
                    |> restoreTry
                        (\soFarTryErrors ->
                            case broadValue |> narrowTo possibilityMorph of
                                Ok possibilityNarrow ->
                                    possibilityNarrow
                                        |> possibilityToChoice
                                        |> Ok

                                Err tryError ->
                                    soFarTryErrors
                                        |> Stack.onTopLay tryError
                                        |> Err
                        )
        , broaden =
            choiceMorphSoFar.broaden
                (broadenFrom possibilityMorph)
        }



-- each variant


{-| Initialize a [choice morph](Choice#MorphNoTry)
by discriminating `(` the broad`,` the narrow `)` choices,
then `|>` [`Morph.try`](Choice#try)ing each possibility,
concluding the builder with [`Morph.choiceFinish`](#finish)

A use case is [morphing](Morph#Morph) from and to an internal type

    absoluteInternal : MorphOrError Absolute Decimal.Internal.Absolute error_
    absoluteInternal =
        Morph.choiceToFrom
            ( \variantFraction variantAtLeast1 decimal ->
                case decimal of
                    Decimal.Internal.Fraction fractionValue ->
                        variantFraction fractionValue

                    Decimal.Internal.AtLeast1 atLeast1Value ->
                        variantAtLeast1 atLeast1Value
            , \variantFraction variantAtLeast1 decimal ->
                case decimal of
                    Fraction fractionValue ->
                        variantFraction fractionValue

                    AtLeast1 atLeast1Value ->
                        variantAtLeast1 atLeast1Value
            )
            |> Choice.tryToFrom ( Fraction, Decimal.Internal.Fraction ) fractionInternal
            |> Choice.tryToFrom ( AtLeast1, Decimal.Internal.AtLeast1 ) atLeast1Internal
            |> Morph.choiceFinish

For morphing choices with simple variants without values (enums),
a simple [`translate`](Morph#translate) also does the job

    signInternal : MorphOrError Sign Sign.Internal.Sign error_
    signInternal =
        Morph.translate
            (\signInternalBeforeNarrow ->
                case signInternalBeforeNarrow of
                    Sign.Internal.Negative ->
                        Sign.Negative

                    Sign.Internal.Positive ->
                        Sign.Positive
            )
            (\signBeforeBroaden ->
                case signBeforeBroaden of
                    Sign.Negative ->
                        Sign.Internal.Negative

                    Sign.Positive ->
                        Sign.Internal.Positive
            )

-}
choiceToFrom :
    ( narrowByPossibility
    , broadenByPossibility
    )
    ->
        ChoiceMorphNoVariant
            Possibly
            narrowByPossibility
            broadenByPossibility
choiceToFrom ( narrowByPossibility, broadenByPossibility ) =
    { description = Emptiable.empty
    , narrow = narrowByPossibility
    , broaden = broadenByPossibility
    }


{-| [`Morph`](Morph#Morph) the next variant value.
Finish with [`Morph.choiceToFromFinish`](#finishToFrom)
-}
variant :
    ( narrowVariantValue -> narrowChoice
    , possibilityBroad -> broadChoice
    )
    ->
        MorphIndependently
            (beforeNarrowVariantValue
             -> Result error narrowVariantValue
            )
            (beforeBroadVariantValue -> possibilityBroad)
    ->
        (ChoiceMorphNoVariant
            noTryPossiblyOrNever_
            ((beforeNarrowVariantValue
              -> Result error narrowChoice
             )
             -> narrowChoiceFurther
            )
            ((beforeBroadVariantValue -> broadChoice)
             -> broadenChoiceFurther
            )
         ->
            ChoiceMorphNoVariant
                noTryNever_
                narrowChoiceFurther
                broadenChoiceFurther
        )
variant ( possibilityToChoice, possibilityFromChoice ) possibilityMorph =
    \choiceMorphSoFar ->
        { description =
            choiceMorphSoFar.description
                |> Stack.onTopLay possibilityMorph.description
        , narrow =
            choiceMorphSoFar.narrow
                (\broad_ ->
                    broad_
                        |> narrowTo possibilityMorph
                        |> Result.map possibilityToChoice
                )
        , broaden =
            choiceMorphSoFar.broaden
                (\narrow ->
                    narrow
                        |> broadenFrom possibilityMorph
                        |> possibilityFromChoice
                )
        }


{-| Conclude a [`Morph.choiceToFrom`](Choice#toFrom) `|>` [`Choice.tryToFrom`](Choice#tryToFrom) builder
-}
choiceToFromFinish :
    ChoiceMorphNoVariant Never narrow broaden
    -> MorphIndependently narrow broaden
choiceToFromFinish =
    \choiceMorphComplete ->
        { description =
            case choiceMorphComplete.description |> Emptiable.fill of
                Stack.TopBelow ( variantOnly, [] ) ->
                    variantOnly

                Stack.TopBelow ( variant0, variant1 :: variants2Up ) ->
                    { custom = Emptiable.empty
                    , inner =
                        ArraySized.l2 variant0 variant1
                            |> ArraySized.attachMin Up
                                (variants2Up |> ArraySized.fromList)
                            |> Choice
                            |> Emptiable.filled
                    }
        , narrow = choiceMorphComplete.narrow
        , broaden = choiceMorphComplete.broaden
        }



-- MorphRow


{-| Possibly incomplete [`MorphRow`](#MorphRow) to and from a Morph.choice.
See [`Morph.choice`](#Morph.choice), [`Morph.tryRow`](#try), [`MorphRow.choiceFinish`](#choiceFinish)
-}
type alias ChoiceMorphRowNoTry noTryPossiblyOrNever choiceNarrow choiceBroaden broadElement =
    ChoiceMorphNoVariant
        noTryPossiblyOrNever
        (Emptiable (Stacked broadElement) Possibly
         ->
            Result
                (-- tries
                 Emptiable (Stacked Error) noTryPossiblyOrNever
                )
                { narrow : choiceNarrow
                , broad : Emptiable (Stacked broadElement) Possibly
                }
        )
        choiceBroaden


{-| If the previous [`possibility`](#try) fails
try this [`MorphRow`](#MorphRow).

> ℹ️ Equivalent regular expression: `|`

    import Morph
    import Char.Morph as Char
    import Morph.Error

    type UnderscoreOrLetter
        = Underscore
        | Letter Char

    underscoreOrLetter : MorphRow Char UnderscoreOrLetter
    underscoreOrLetter =
        Morph.choice
            (\underscoreVariant letterVariant underscoreOrLetterNarrow ->
                case underscoreOrLetterNarrow of
                    Underscore ->
                        underscoreVariant ()

                    Letter letter ->
                        letterVariant letter
            )
            |> try (\() -> Underscore) (Char.Morph.only '_')
            |> try Letter AToZ.caseAny
            |> choiceFinish

    -- try the first possibility
    "_"
        |> Text.narrowTo underscoreOrLetter
    --> Ok Underscore

    -- if it fails, try the next
    "a"
        |> Text.narrowTo underscoreOrLetter
    --> Ok 'a'

    -- if none work, we get the error from all possible steps
    "1"
        |> Text.narrowTo (onFailDown [ one '_', AToZ.char ])
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:1: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '1'."


### example: fallback step if the previous step fails

    import Morph
    import Char.Morph as Char
    import Morph.Error

    type AlphaNum
        = Digits (List (N (In N0 N9)))
        | Letters String

    alphaNum : MorphRow Char AlphaNum
    alphaNum =
        Morph.choice
            (\digit letter alphaNum ->
                case alphaNum of
                    Digits int ->
                        digit int

                    Letters char ->
                        letter char
            )
            |> Morph.try Letter
                (map String.Morph.fromList
                    (atLeast n1 AToZ.char)
                )
            |> Morph.try Digit
                (atLeast n1 Digit.n0To9)
            |> MorphRow.choiceFinish

    -- try letters, or else give me some digits
    "abc"
        |> Text.narrowTo alphaNum
    --> Ok "abc"

    -- we didn't get letters, but we still got digits
    "123"
        |> Text.narrowTo alphaNum
    --> Ok "123"

    -- but if we still fail, give the expectations of all steps
    "_"
        |> Text.narrowTo alphaNum
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:1: I was expecting at least 1 digit [0-9]. I got stuck when I got the character '_'."

-}
tryRow :
    (possibilityNarrow -> choiceNarrow)
    -> MorphRow possibilityNarrow broadElement
    ->
        (ChoiceMorphRowNoTry
            noTryPossiblyOrNever_
            choiceNarrow
            ((possibilityNarrow -> Emptiable (Stacked broadElement) Possibly)
             -> choiceBroadenFurther
            )
            broadElement
         ->
            ChoiceMorphRowNoTry
                never_
                choiceNarrow
                choiceBroadenFurther
                broadElement
        )
tryRow possibilityToChoice possibilityMorph =
    \choiceMorphSoFar ->
        { description =
            choiceMorphSoFar.description
                |> Stack.onTopLay (possibilityMorph |> description)
        , narrow =
            \choiceBroad ->
                choiceBroad
                    |> choiceMorphSoFar.narrow
                    |> restoreTry
                        (\soFarErrorPossibilities ->
                            case choiceBroad |> narrowTo possibilityMorph of
                                Ok possibilityParsed ->
                                    { broad = possibilityParsed.broad
                                    , narrow =
                                        possibilityParsed.narrow
                                            |> possibilityToChoice
                                    }
                                        |> Ok

                                Err possibilityExpectation ->
                                    soFarErrorPossibilities
                                        |> Stack.onTopLay possibilityExpectation
                                        |> Err
                        )
        , broaden =
            choiceMorphSoFar.broaden
                (broadenFrom possibilityMorph)
        }


{-| Always the last step in a [`Morph.choice`](#choice) `|>` [`Morph.tryRow`](#tryRow) build process
-}
choiceRowFinish :
    ChoiceMorphRowNoTry
        Never
        choiceNarrow
        (choiceNarrow
         -> Emptiable (Stacked broadElement) Possibly
        )
        broadElement
    -> MorphRow choiceNarrow broadElement
choiceRowFinish =
    \choiceMorphRowComplete ->
        { description =
            case choiceMorphRowComplete.description |> Emptiable.fill of
                Stack.TopBelow ( descriptionOnly, [] ) ->
                    descriptionOnly

                Stack.TopBelow ( description0, description1 :: description2Up ) ->
                    { custom = Emptiable.empty
                    , inner =
                        ArraySized.l2 description0 description1
                            |> ArraySized.attachMin Up
                                (description2Up |> ArraySized.fromList)
                            |> Group
                            |> Emptiable.filled
                    }
        , narrow =
            \broad_ ->
                broad_
                    |> choiceMorphRowComplete.narrow
                    |> Result.mapError
                        (\errorPossibilities ->
                            { startDown = broad_ |> Stack.length
                            , error = errorPossibilities |> Tries
                            }
                                |> Row
                        )
        , broaden = choiceMorphRowComplete.broaden
        }


{-| Conclude a [`Morph.choice`](Choice#between) `|>` [`Morph.try`](Choice#try) builder
-}
choiceFinish :
    ChoiceMorphNoTry
        Never
        choiceNarrow
        choiceBeforeNarrow
        (choiceBeforeBroaden -> choiceBroad)
        (ErrorWithDeadEnd deadEnd)
    ->
        MorphIndependently
            (choiceBeforeNarrow
             -> Result (ErrorWithDeadEnd deadEnd) choiceNarrow
            )
            (choiceBeforeBroaden -> choiceBroad)
choiceFinish =
    \choiceMorphComplete ->
        { description =
            case choiceMorphComplete.description |> Emptiable.fill of
                Stack.TopBelow ( variantOnly, [] ) ->
                    variantOnly

                Stack.TopBelow ( variant0, variant1 :: variants2Up ) ->
                    { custom = Emptiable.empty
                    , inner =
                        ArraySized.l2 variant0 variant1
                            |> ArraySized.attachMin Up
                                (variants2Up |> ArraySized.fromList)
                            |> Choice
                            |> Emptiable.filled
                    }
        , narrow =
            choiceMorphComplete.narrow
                >> Result.mapError Tries
        , broaden =
            choiceMorphComplete.broaden
        }
