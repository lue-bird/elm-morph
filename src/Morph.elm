module Morph exposing
    ( Morph, Translate, MorphOrError, MorphIndependently
    , Description, DescriptionInner(..)
    , Error, ErrorWithDeadEnd(..), GroupError
    , broaden
    , value, only, validate
    , translate, broad, toggle, keep, translateOn
    , lazy
    , end, one, succeed
    , to
    , reverse
    , deadEndMap
    , deadEndNever, narrowErrorMap
    , description
    , broadenWith, narrowWith, mapWith
    , over, overRow
    , MorphRow, MorphRowIndependently, rowFinish
    , before
    )

{-| Call it Codec, Conversion, Transformation, Shape, PrismReversible, ParseBuild

@docs Morph, Translate, MorphOrError, MorphIndependently
@docs Description, DescriptionInner
@docs Error, ErrorWithDeadEnd, GroupError


## create

@docs broaden
@docs value, only, validate
@docs translate, broad, toggle, keep, translateOn

@docs lazy


### create row

@docs end, one, succeed


## alter

@docs to
@docs reverse
@docs deadEndMap
@docs deadEndNever, narrowErrorMap


## scan

@docs description
@docs broadenWith, narrowWith, mapWith


## chain

@docs over, overRow


## group

[`Group.Morph`](Group#Morph)


## Choice.between

[`Choice.Morph`](Choice#Morph)


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
import N exposing (Add1, Exactly, Fixed, In, InFixed, Min, N, N0, N2, To, Up, n0, n1, n2)
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
Choice by group/Choice.between/... associating errors and description

-}
type alias MorphIndependently narrow broaden =
    RecordWithoutConstructorFunction
        { description : Description
        , narrow : narrow
        , broaden : broaden
        }


{-| [`Morph`](#Morph) that can [narrow](#narrowWith)
to an error that can be different from the default [`Error`](#Error)

    type alias Translate mapped unmapped =
        MorphOrError mapped unmapped (ErrorWithDeadEnd Never)

-}
type alias MorphOrError narrow broad error =
    MorphIndependently
        (broad -> Result error narrow)
        (narrow -> broad)


{-| Describing what the Morph [narrows to](#narrowWith) and [broadens from](#broadenWith)

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
  - narrow Choice.between between multiple

-}
type DescriptionInner
    = Over { narrow : Description, broad : Description }
    | Group (ArraySized (Min (Fixed N2)) Description)
    | Choice (ArraySized (Min (Fixed N2)) Description)
    | Elements Description
      -- row
    | While Description
    | Until { end : Description, element : Description }


{-| Where [narrowing](#narrowWith) has failed.

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
    | Parts (GroupError (ErrorWithDeadEnd deadEnd))
    | -- TODO: Stack → ArraySized (Min (Fixed N2))
      -- TODO: error → { index : Int, error : error }
      Possibilities (Emptiable (Stacked (ErrorWithDeadEnd deadEnd)) Never)


{-| A group's part [`Error`](#Error)s with their locations
-}
type alias GroupError partError =
    Emptiable
        (Stacked
            { index : Int
            , error : partError
            }
        )
        Never


{-| Create a message from the [`Error`](#Error)

TODO: update examples

    import Morph
    import String.Morph as Text
    import Char.Morph exposing (letter)

    -- Getting a digit instead of a letter
    "123"
        |> Text.narrowWith letter
        |> Result.mapError Morph.errorToString
    --> Err "1:1: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '1'."

    -- Running out of input characters
    ""
        |> Text.narrowWith letter
        |> Result.mapError Morph.errorToString
    --> Err "1:0: I was expecting a letter [a-zA-Z]. I reached the end of the input."

-}
errorToString : Error -> Emptiable (Stacked String) Never
errorToString =
    \expected ->
        case expected of
            DeadEnd unexpectedDescription ->
                Stack.only unexpectedDescription

            Row row ->
                Stack.onTopLay
                    ([ row.startDown |> String.fromInt
                     , " elements from the last"
                     ]
                        |> String.concat
                    )
                    (row.error |> errorToString)

            Parts parts ->
                Stack.onTopLay
                    "in the parts"
                    (parts
                        |> Emptiable.emptyAdapt never
                        |> Stack.map
                            (\_ part ->
                                Stack.onTopLay
                                    ([ "part ", part.index |> String.fromInt, ":" ]
                                        |> String.concat
                                    )
                                    (part.error |> errorToString)
                                    |> markdownElement
                            )
                        |> Stack.flatten
                    )

            Possibilities possibilities ->
                Stack.onTopLay
                    "i tried"
                    (possibilities
                        |> Emptiable.emptyAdapt never
                        |> Stack.map
                            (\_ possibility ->
                                Stack.onTopLay
                                    ([ "possibility:" ]
                                        |> String.concat
                                    )
                                    (possibility |> errorToString)
                                    |> markdownElement
                            )
                        |> Stack.flatten
                    )


markdownElement :
    Emptiable (Stacked String) Never
    -> Emptiable (Stacked String) never_
markdownElement =
    \elementMarkdown ->
        Stack.onTopLay
            ([ "  - ", elementMarkdown |> Stack.top ] |> String.concat)
            (elementMarkdown
                |> Stack.topRemove
                |> Stack.map (\_ possibilityLine -> "    " ++ possibilityLine)
            )


{-| Describe the context to improve error messages.

    import Morph.Error
    import Char.Morph as Char
    import String.Morph as Text

    -- we can redefine an error message if something goes wrong
    "123"
        |> Text.narrowWith
            (Morph.to "variable name"
                (atLeast n1 AToZ.Morph.char)
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
broadenWith : MorphIndependently narrow_ broaden -> broaden
broadenWith =
    .broaden


{-| Its transformation that turns `broad` into `narrow` or an `error`.
Some call it "parse"
-}
narrowWith : MorphIndependently narrow broaden_ -> narrow
narrowWith =
    .narrow


{-| Convert values of the arbitrarily chosen types `unmapped -> mapped`.

    "3456" |> |> Morph.mapWith String.Morph.toList
    --> [ '3', '4', '5', '6' ]

-}
mapWith :
    MorphIndependently
        (unmapped -> Result (ErrorWithDeadEnd Never) mapped)
        broaden_
    -> (unmapped -> mapped)
mapWith translate_ =
    \unmapped ->
        case unmapped |> narrowWith translate_ of
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
    Choice.between
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
        |> Choice.try (\() -> ExclamationMark) (Char.Morph.only '!')
        |> Choice.try (\() -> NumberSign) (Char.Morph.only '#')
        |> Choice.try (\() -> DollarSign) (Char.Morph.only '$')
        |> Choice.try (\() -> PercentSign) (Char.Morph.only '%')
        |> Choice.try (\() -> Ampersand) (Char.Morph.only '&')
        |> Choice.try (\() -> Asterisk) (Char.Morph.only '*')
        |> Choice.try (\() -> LowLine) (Char.Morph.only '_')
        |> Choice.try (\() -> HyphenMinus) (Char.Morph.only '-')
        |> Choice.finish
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
[narrowing](#mapWith) can `Never` fail

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
translate mapTo unmapFrom =
    { description =
        { custom = Emptiable.empty, inner = Emptiable.empty }
    , narrow = mapTo >> Ok
    , broaden = unmapFrom
    }


{-| Only broadens (unmaps), doesn't narrow.
What comes out as the broad thing will be transformed but input doesn't.

What is great is using this to make inputs more "user-usable":

    ArraySized.Morph.maxToInfinity :
        MorphIndependently
            (narrow -> Result error_ narrow)
            (ArraySized (In (Fixed min) max_)
             -> ArraySized (In (Fixed min) max_)
            )
    ArraySized.Morph.maxToInfinity =
        Morph.broaden ArraySized.maxToInfinity

However! This can also often be an anti-pattern. See [`validate`](#validate).

    "WOW"
        |> Morph.broadenWith
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
broaden broadenFrom =
    translate identity broadenFrom


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

Make helpers for each type of constant for convenience!

    Char.Morph.only broadCharConstant =
        Morph.only
            (\char ->
                [ "'", char |> String.fromChar, "'" ]
                    |> String.concat
            )
            broadCharConstant

-}
only :
    (broadConstant -> String)
    -> broadConstant
    -> Morph () broadConstant
only broadConstantToString broadConstant =
    value
        (broadConstant |> broadConstantToString)
        { narrow =
            \broadValue ->
                if broadValue == broadConstant then
                    () |> Ok

                else
                    broadConstant
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
        { narrow : beforeNarrow -> Result deadEnd narrowed
        , broaden : beforeBroaden -> broadened
        }
    ->
        MorphIndependently
            (beforeNarrow -> Result (ErrorWithDeadEnd deadEnd) narrowed)
            (beforeBroaden -> broadened)
value descriptionCustom morphTransformations =
    { description =
        { custom = Stack.only descriptionCustom
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
        Choice.between
            (\endVariant nextVariant lazyListNarrow ->
                case lazyListNarrow of
                    End ->
                        endVariant ()
                    Next next ->
                        nextVariant next
            )
            |> Choice.try (\() -> End)
                (String.Morph.only "[]")
            |> Choice.try Next
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

    "[]" |> Text.narrowWith lazyList
    --> Ok End

    "a :: []" |> Text.narrowWith lazyList
    --> Ok (Next 'a' End)

    "a :: b :: []" |> Text.narrowWith lazyList
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
            broadValue |> narrowWith (morphLazy ())
    , broaden =
        \narrowValue ->
            narrowValue |> broadenWith (morphLazy ())
    }


{-| Go over an additional step of [`Morph`](#Morph) on its broad type

Chaining

  - `<<` on the broad side
  - `<< Result.andThen` on the narrow side

This can be used to, for example

  - [`Translate`](#Translate) what was [narrowed](#narrowWith)
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
            broadenWith morph
                >> broadenWith morphNarrowBroad
        , narrow =
            narrowWith morphNarrowBroad
                >> Result.andThen (narrowWith morph)
        }


{-| `Translate a <-> b`
by swapping the functions [`map`](#map) <-> [`unmap`](#unmap).

    [ 'O', 'h', 'a', 'y', 'o' ]
        |> Morph.map
           (Text.toList |> Morph.reverse)
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
        toListNonEmpty |> Morph.reverse

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
reverse :
    MorphIndependently
        (beforeMap -> Result (ErrorWithDeadEnd Never) mapped)
        (beforeUnmap -> unmapped)
    ->
        MorphIndependently
            (beforeUnmap -> Result error_ unmapped)
            (beforeMap -> mapped)
reverse =
    \translate_ ->
        { description = translate_ |> description
        , narrow =
            \unmapped ->
                unmapped |> broadenWith translate_ |> Ok
        , broaden = mapWith translate_
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

            Possibilities possibilities ->
                possibilities
                    |> Stack.map
                        (\_ -> deadEndMap deadEndChange)
                    |> Possibilities


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

            Possibilities possibilities ->
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
        , broaden = broadenWith morph
        , narrow =
            narrowWith morph
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
            (elementBeforeMap -> Result (ErrorWithDeadEnd Never) elementMapped)
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
        structureMap (mapWith elementTranslate)
            >> Ok
    , broaden =
        structureUnmap (broadenWith elementTranslate)
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
    MorphRow Char narrow
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

    point : MorphRow Point
    point =
        Morph.to "point"
            (Morph.succeed (\x y -> { x = x, y = y })
                |> skip (String.Morph.only "(")
                |> skip
                    (broad [ () ]
                        |> Morph.overRow (atLeast n0 (String.Morph.only " "))
                    )
                |> grab number
                |> skip
                    (broad []
                        |> Morph.overRow (atLeast n0 (String.Morph.only " "))
                    )
                |> skip (String.Morph.only ",")
                |> skip
                    (broad [ () ]
                        |> Morph.overRow (atLeast n0 (String.Morph.only " "))
                    )
                |> grab .x Number.Morph.text
                |> skip
                    (broad [ () ]
                        |> Morph.overRow (atLeast n0 (String.Morph.only " "))
                    )
                |> skip (String.Morph.only ")")
            )

    -- we can get a nice error message if it fails
    "(2.71, x)"
        |> Text.narrowWith point
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

  - 👎 performs worse as there are more [possibilities](#try) to parse to know it failed

-}
type alias MorphRow broadElement narrow =
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
type alias MorphRowIndependently broadElement beforeBroaden narrowed =
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
    "a" |> Text.narrowWith (Morph.keep |> one)
    --> Ok 'a'

    "#" |> Text.narrowWith (Morph.keep |> one)
    --> Ok '#'

    -- only fails if we run out of inputs
    ""
        |> Text.narrowWith (Morph.keep |> one)
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:0: I was expecting a character. I reached the end of the input."

-}
one :
    Morph narrow element
    -> MorphRow element narrow
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

                    Emptiable.Filled (Stack.TopDown nextBroadElement afterNextBroadElement) ->
                        case
                            nextBroadElement
                                |> narrowWith morph
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
            broadenWith morph
                >> Stack.only
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

    "12,34" |> Text.narrowWith point
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
    ->
        MorphRowIndependently
            broadElement_
            beforeBroaden_
            narrowConstant
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
           Use [`Choice.between`](#Choice.between) where each [`possibility`](#try) expects a specific number


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
        |> Text.narrowWith
            (atLeast n1 AToZ.Morph.char
                |> map Text.fromList
                |> map (toggle String.toLower)
            )
    --> Ok "abc"

-}
overRow :
    MorphRowIndependently
        broadElement
        beforeBroaden
        beforeNarrow
    ->
        (MorphIndependently
            (beforeNarrow -> Result Error narrow)
            (beforeBeforeBroaden -> beforeBroaden)
         ->
            MorphRowIndependently
                broadElement
                beforeBeforeBroaden
                narrow
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
                    |> narrowWith morphRowBeforeMorph
                    |> Result.andThen
                        (\narrowed ->
                            narrowed.narrow
                                |> narrowWith narrowMorph
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
            broadenWith narrowMorph
                >> broadenWith morphRowBeforeMorph
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
                (atLeast n0 (Morph.keep |> Morph.one))
            |> skip (String.Morph.only "Decoder")
            |> skip Morph.end

Problem is: This will never Morph.succeed.
`atLeast n0 (Morph.keep |> Morph.one)` always goes on.
We never reach the necessary [`skip`](#skip)ped things.

-}
before :
    { end : MorphRow broadElement ()
    , goOn : MorphRow broadElement goOnElement
    }
    -> MorphRow broadElement (List goOnElement)
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
    , end : MorphRow broadElement endElement
    , goOn : MorphRow broadElement goOnElement
    }
    -> MorphRow broadElement commitResult
until untilStep =
    let
        loopStep =
            choiceBetween
                (\commit goOn loopStepNarrow ->
                    case loopStepNarrow of
                        Commit commitElement ->
                            commit commitElement

                        GoOn goOnELement ->
                            goOn goOnELement
                )
                |> choiceTryRow Commit untilStep.end
                |> choiceTryRow GoOn untilStep.goOn
                |> choiceFinishRow
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
                                |> broadenWith loopStep
                                |> Stack.onTopStack
                                    (tail |> broadenStepBack ())
        in
        \commitResultNarrow ->
            let
                committedBack =
                    commitResultNarrow
                        |> broadenWith untilStep.commit
            in
            (committedBack.before
                |> List.reverse
                |> broadenStepBack ()
            )
                |> Stack.onTopStack
                    ((committedBack.end |> Commit)
                        |> broadenWith loopStep
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
                    narrowWith loopStep
                        >> Result.andThen
                            (\stepped ->
                                case stepped.narrow of
                                    Commit committed ->
                                        case
                                            { before = before_, end = committed }
                                                |> narrowWith untilStep.commit
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



-- copied from module Choice to avoid cyclic import


type alias MorphNoTry noTryPossiblyOrNever choiceNarrow choiceBroad choiceBroaden error =
    RecordWithoutConstructorFunction
        { description :
            -- possibilities
            Emptiable (Stacked Description) noTryPossiblyOrNever
        , narrow :
            choiceBroad
            ->
                Result
                    (-- tries
                     Emptiable (Stacked error) noTryPossiblyOrNever
                    )
                    choiceNarrow
        , broaden : choiceBroaden
        }


choiceBetween :
    choiceBroadenByPossibility
    ->
        MorphNoTry
            Possibly
            choiceNarrow_
            choiceBroad_
            choiceBroadenByPossibility
            error_
choiceBetween choiceBroadenDiscriminatedByPossibility =
    { description = Emptiable.empty
    , narrow =
        \_ ->
            Emptiable.empty |> Err
    , broaden = choiceBroadenDiscriminatedByPossibility
    }


type alias MorphRowNoTry noTryPossiblyOrNever broadElement choiceNarrow choiceBroaden =
    MorphNoTry
        noTryPossiblyOrNever
        { narrow : choiceNarrow
        , broad : Emptiable (Stacked broadElement) Possibly
        }
        (Emptiable (Stacked broadElement) Possibly)
        choiceBroaden
        Error


choiceTryRow :
    (possibilityNarrow -> choiceNarrow)
    -> MorphRow broadElement possibilityNarrow
    ->
        (MorphRowNoTry
            noTryPossiblyOrNever_
            broadElement
            choiceNarrow
            ((possibilityNarrow -> Emptiable (Stacked broadElement) Possibly)
             -> choiceBroadenFurther
            )
         ->
            MorphRowNoTry
                never_
                broadElement
                choiceNarrow
                choiceBroadenFurther
        )
choiceTryRow possibilityToChoice possibilityMorph =
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
                            case choiceBroad |> narrowWith possibilityMorph of
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
                (broadenWith possibilityMorph)
        }


{-| Always the last step in a [`Choice.between`](#Choice.between) `|>` [`Choice.tryRow`](#try) build process
-}
choiceFinishRow :
    MorphRowNoTry
        Never
        broadElement
        choiceNarrow
        (choiceNarrow -> Emptiable (Stacked broadElement) Possibly)
    -> MorphRow broadElement choiceNarrow
choiceFinishRow =
    \choiceMorphRowComplete ->
        { description =
            case choiceMorphRowComplete.description |> Emptiable.fill of
                Stack.TopDown descriptionOnly [] ->
                    descriptionOnly

                Stack.TopDown description0 (description1 :: description2Up) ->
                    { custom = Emptiable.empty
                    , inner =
                        ArraySized.l2 description0 description1
                            |> ArraySized.glueMin Up
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
                            , error = errorPossibilities |> Possibilities
                            }
                                |> Row
                        )
        , broaden = choiceMorphRowComplete.broaden
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
end : MorphRow broadElement_ ()
end =
    { description =
        { custom = Stack.only "end"
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
        narrowWith
            (Point.morphRowChar
                |> Morph.rowFinish
                |> Morph.over Stack.Morph.toString
            )

-}
rowFinish :
    MorphRow broadElement narrow
    -> Morph narrow (Emptiable (Stacked broadElement) Possibly)
rowFinish =
    \morphRow ->
        { description = morphRow.description
        , narrow =
            \broadElements ->
                broadElements
                    |> narrowWith morphRow
                    |> Result.andThen
                        (\result ->
                            result.broad
                                |> narrowWith end
                                |> Result.map
                                    (\_ -> result.narrow)
                        )
        , broaden =
            \narrow -> narrow |> broadenWith morphRow
        }
