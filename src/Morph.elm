module Morph exposing
    ( Morph, Translate, MorphOrError, MorphIndependently
    , Description, DescriptionInner(..)
    , broaden
    , value, only, validate
    , translate, broad, toggle, keep, translateOn
    , lazy
    , end, one, succeed, grab, match
    , to
    , invert
    , deadEndMap
    , deadEndNever, narrowErrorMap
    , Error, ErrorWithDeadEnd(..), GroupError
    , Label, LabelKind(..), descriptionAndErrorToTree
    , description, descriptionToTree
    , treeToLines
    , toBroad, toNarrow, mapTo
    , over, overRow
    , PartsMorphEmptiable
    , parts, part, partsFinish
    , choiceEquivalent
    , VariantsMorphEmptiable, variants, variant, variantsFinish
    , choice
    , ChoiceMorphEmptiable, try, choiceFinish
    , ChoiceMorphRowEmptiable, tryRow, choiceRowFinish
    , MorphRow, MorphRowIndependently, rowFinish
    , before
    )

{-| Call it Codec, Coder, ParserBuilder, Convert-, Transform-, Shape-, FormReversible, ToFrom, ...
We call it

@docs Morph, Translate, MorphOrError, MorphIndependently
@docs Description, DescriptionInner


## create

@docs broaden
@docs value, only, validate
@docs translate, broad, toggle, keep, translateOn

@docs lazy


### create row

@docs end, one, succeed, grab, match


## alter

@docs to
@docs invert
@docs deadEndMap
@docs deadEndNever, narrowErrorMap


## error

@docs Error, ErrorWithDeadEnd, GroupError
@docs Label, LabelKind, descriptionAndErrorToTree


## describe

@docs description, descriptionToTree


## visualize

@docs treeToLines


## scan

@docs toBroad, toNarrow, mapTo


## chain

@docs over, overRow


## group

@docs PartsMorphEmptiable
@docs parts, part, partsFinish


## choice [`Morph`](Morph#Morph)

[`Morph`](#Morph) a union `type`

@docs choiceEquivalent


### morph by variant

@docs VariantsMorphEmptiable, variants, variant, variantsFinish
@docs choice
@docs ChoiceMorphEmptiable, try, choiceFinish


## choice [`MorphRow`](#MorphRow)

@docs ChoiceMorphRowEmptiable, tryRow, choiceRowFinish


## row

@docs MorphRow, MorphRowIndependently, rowFinish


### sequence row

  - optional → [`Maybe.Morph.row`](Maybe-Morph#row)
  - [`atLeast`](ArraySized-Morph#atLeast)
  - [`exactly`](ArraySized-Morph#exactly)
  - between → [`ArraySized.Morph.in_`](ArraySized-Morph#exactly)

@docs before

`whileAccumulate`, `until` aren't exposed for simplicity.
Have a need for them? → issue


### other art

  - [`invertible-syntax`](https://hackage.haskell.org/package/invertible-syntax) same idea in the haskell world
  - parse-build an enum over a String: [`jmpavlick/bimap`](https://dark.elm.dmy.fr/packages/jmpavlick/bimap/latest/), [`toastal/select-prism`](https://package.elm-lang.org/packages/toastal/select-prism/latest/), [`Herteby/enum`](https://package.elm-lang.org/packages/Herteby/enum/latest), [`genthaler/elm-enum`](https://package.elm-lang.org/packages/genthaler/elm-enum/latest/), [`the-sett/elm-refine` `Enum`](https://package.elm-lang.org/packages/the-sett/elm-refine/latest/Enum)
  - equivalent to [`Translate`](#Translate): [`arturopala/elm-monocle` `Monocle.Iso`](https://package.elm-lang.org/packages/arturopala/elm-monocle/latest/Monocle-Iso), [`Heimdell/elm-optics` `Optics.Core.Iso`](https://package.elm-lang.org/packages/Heimdell/elm-optics/latest/Optics-Core#Iso), [`erlandsona/elm-accessors` `Accessors.Iso`](https://dark.elm.dmy.fr/packages/erlandsona/elm-accessors/latest/Accessors#Iso), [`fujiy/elm-json-convert` `Json.Convert.Iso`](https://package.elm-lang.org/packages/fujiy/elm-json-convert/latest/Json-Convert#Iso)

---

Up for a challenge? implement & PR

  - `date`, `time`, `datetime`
  - `pathUnix`, `pathWindows`
  - `uri`
  - `ipV4`, `ipV6`

-}

import ArraySized exposing (ArraySized)
import Emptiable exposing (Emptiable, filled)
import Json.Decode exposing (Error)
import Linear exposing (Direction(..))
import N exposing (Min, N2, On)
import Possibly exposing (Possibly)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Rope exposing (Rope)
import Stack exposing (Stacked)
import Tree exposing (Tree)
import Util exposing (recoverTry)



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
You can always chain, [group](#group), [choose](#choice), ...

👀 Each type `Morph narrow broad`,
for example `Morph Email String`, can


### `broaden : narrow -> broad`

  - example: `Email -> String`
  - going from a specific type to a general one
  - any specific value can be turned back successfully
  - can loose information on its way


### `narrow : broad -> Result error narrow`

  - example: `String -> Result Morph.Error Email`
      - ↑ is exactly how running your typical parser looks like
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
          - doesn't communicate business rules (requirements, ...)
              - including for other developers,
                "allowing for improved reasoning across the codebase"
  - 🎙️ [podcast "Parse, don't validate"](https://elm-radio.com/episode/parse-dont-validate/)


#### Why `Morph.Error` in `Morph` this when I could [use custom errors](#MorphOrError) everywhere?

Errors with more narrow structural information are mostly useful for recovery based on what went wrong.

You _can_ use [`MorphOrError`](#MorphOrError) in these cases.

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
    MorphOrError narrow broad Error


{-| Sometimes, you'll see the most general version of [`Morph`](#Morph):

    : MorphIndependently narrow broaden

where

  - [`toNarrow`](#toNarrow) result types can't necessarily be used as input for [`toBroad`](#toBroad)
  - [`toBroad`](#toBroad) result types can't necessarily be used as input for [`toNarrow`](#toNarrow)

For example:

  - [`Value.Morph`](Value#Morph): [`toBroad`](#toBroad) returns a value where we know
    **both index and name** for each field/variant,
    whereas [`toNarrow`](#toNarrow) allows **either index or name** for each field/variant.
    This allows us to choose whether we want a [`descriptive`](Value#descriptive) or [`compact`](Value#compact)
    view at the end, being able to switch anytime or use both for different situations.
  - [`Stack.Morph.list`](Stack-Morph#list) allows different element types for both directions.
    This is not necessary at all but allows it to be used more generally.
  - [`MorphRow`](#MorphRow): [`toNarrow`](#toNarrow) accepts a row of elements
    but [`toBroad`](#toBroad) results in a [`Rope`](https://dark.elm.dmy.fr/packages/miniBill/elm-rope/latest/) for better performance

-}
type alias MorphIndependently narrow broaden =
    RecordWithoutConstructorFunction
        { description : Description
        , narrow : narrow
        , broaden : broaden
        }


{-| [`Morph`](#Morph) that can [narrow](#toNarrow)
to an error that can be different from the default [`Error`](#Error)

    type alias Translate mapped unmapped =
        MorphOrError mapped unmapped (ErrorWithDeadEnd Never)

-}
type alias MorphOrError narrow broad error =
    MorphIndependently
        (broad -> Result error narrow)
        (narrow -> broad)


{-| Describing what the Morph [narrows to](#toNarrow) and [broadens from](#toBroad)

  - custom description of the context
  - maybe [a description depending on structure](#DescriptionInner)

-}
type alias Description =
    RecordWithoutConstructorFunction
        { custom : Emptiable (Stacked String) Possibly
        , inner : DescriptionInner
        }


{-| Description of a structure

  - chained morphs
  - narrow group of multiple
  - narrow Morph.choice between multiple

-}
type DescriptionInner
    = CustomDescription
    | OnlyDescription String
    | -- e.g. over, sequence, while, inverse, elements, atLeast exactly, in
      StructureDescription String (Emptiable (Stacked Description) Possibly)
    | GroupDescription (ArraySized Description (Min (On N2)))
    | PartsDescription (Emptiable (Stacked { tag : String, value : Description }) Never)
    | ChoiceDescription (ArraySized Description (Min (On N2)))
    | VariantsDescription (Emptiable (Stacked { tag : String, value : Description }) Never)
      -- row
    | EndDescription


{-| Create a simple markdown-formatted message from a tree.
-}
treeToLines : Tree String -> List String
treeToLines =
    \tree ->
        (tree |> Tree.label)
            :: (tree
                    |> Tree.children
                    |> List.concatMap (\child -> child |> treeToLines |> markdownElement)
               )


markdownElement : List String -> List String
markdownElement =
    \elementMarkdown ->
        case elementMarkdown of
            [] ->
                []

            top :: below ->
                ([ "  - ", top ] |> String.concat)
                    :: (below |> List.map indent)


indent : String -> String
indent =
    \line -> "    " ++ line


{-| Create a tree from the structured [`Description`](#Description)
-}
descriptionToTree : Description -> Tree String
descriptionToTree =
    \description_ ->
        descriptionAndErrorToTree description_ Nothing
            |> Tree.map .text


{-| Info of a node in an [error and description tree](#descriptionAndErrorToTree):
Text description and [`LabelKind`].
-}
type alias Label =
    { text : String, kind : LabelKind }


{-| A node in an [error and description tree](#descriptionAndErrorToTree):
Is it from an error, a structure description or custom description?
-}
type LabelKind
    = LabelDescriptionCustom
    | LabelDescriptionStructure
    | LabelError


justWhen : (content -> Bool) -> content -> Maybe content
justWhen passes =
    \content ->
        if content |> passes then
            Just content

        else
            Nothing


{-| Create a tree describing a given [`Error`](#Error) embedded in a given [`Description`](#Description).
-}
descriptionAndErrorToTree : Description -> Maybe Error -> Tree Label
descriptionAndErrorToTree description_ maybeError =
    let
        error =
            case maybeError of
                Nothing ->
                    { always = Nothing, structure = Nothing }

                Just (DeadEnd deadEnd) ->
                    { always = Tree.singleton { kind = LabelError, text = deadEnd } |> Just
                    , structure = Nothing
                    }

                Just (RowError rowError) ->
                    { always =
                        Tree.singleton
                            { kind = LabelError
                            , text = [ "starting at ", rowError.startDown |> String.fromInt, " from last" ] |> String.concat
                            }
                            |> Just
                    , structure = rowError.error |> Just
                    }

                Just structureError ->
                    { always = Nothing, structure = structureError |> Just }

        structureTree : Tree Label
        structureTree =
            case description_.inner of
                CustomDescription ->
                    Tree.singleton { kind = LabelDescriptionStructure, text = "(custom)" }

                OnlyDescription onlyDescription ->
                    Tree.singleton { kind = LabelDescriptionStructure, text = "only " ++ onlyDescription }

                StructureDescription structureName elementDescriptions ->
                    let
                        maybeInStructureError : Maybe { index : Int, error : Error }
                        maybeInStructureError =
                            case error.structure of
                                Nothing ->
                                    Nothing

                                Just (InStructureError inStructureError) ->
                                    let
                                        _ =
                                            Debug.log "located in structure" error.structure
                                    in
                                    Just inStructureError

                                Just _ ->
                                    let
                                        _ =
                                            Debug.log "expected InStructureError but found" error.structure
                                    in
                                    Nothing
                    in
                    Tree.tree { kind = LabelDescriptionStructure, text = structureName }
                        (elementDescriptions
                            |> Stack.toList
                            |> List.indexedMap
                                (\index elementDescription ->
                                    descriptionAndErrorToTree elementDescription
                                        (maybeInStructureError |> maybeWhen (\inSequence -> index == inSequence.index) |> Maybe.map .error)
                                )
                        )

                ChoiceDescription possibilities ->
                    let
                        maybeChoiceError =
                            case error.structure of
                                Nothing ->
                                    Nothing

                                Just (ChoiceError choiceError) ->
                                    let
                                        _ =
                                            Debug.log "located in choice" error.structure
                                    in
                                    Just choiceError

                                Just _ ->
                                    let
                                        _ =
                                            Debug.log "expected ChoiceError but found" error.structure
                                    in
                                    Nothing
                    in
                    Tree.tree { kind = LabelDescriptionStructure, text = "choice between" }
                        (case maybeChoiceError of
                            Nothing ->
                                possibilities
                                    |> ArraySized.toList
                                    |> List.map
                                        (\elementDescription ->
                                            descriptionAndErrorToTree elementDescription Nothing
                                        )

                            Just choiceError ->
                                List.map2
                                    (\elementDescription elementError ->
                                        descriptionAndErrorToTree elementDescription
                                            (elementError |> Just)
                                    )
                                    (possibilities |> ArraySized.toList)
                                    (choiceError |> Stack.toList)
                        )

                GroupDescription elements ->
                    let
                        maybeGroupError =
                            case error.structure of
                                Nothing ->
                                    Nothing

                                Just (GroupError groupError) ->
                                    let
                                        _ =
                                            Debug.log "located in group" error.structure
                                    in
                                    Just groupError

                                Just _ ->
                                    let
                                        _ =
                                            Debug.log "expected GroupError but found" error.structure
                                    in
                                    Nothing
                    in
                    Tree.tree { kind = LabelDescriptionStructure, text = "both" }
                        (case maybeGroupError of
                            Nothing ->
                                elements
                                    |> ArraySized.toList
                                    |> List.map
                                        (\elementDescription ->
                                            descriptionAndErrorToTree elementDescription Nothing
                                        )

                            Just groupError ->
                                List.map3
                                    (\index elementDescription elementError ->
                                        descriptionAndErrorToTree elementDescription
                                            (elementError |> justWhen (\element -> element.index == index) |> Maybe.map .error)
                                    )
                                    (List.range 0 ((elements |> ArraySized.length |> N.toInt) - 1))
                                    (elements |> ArraySized.toList)
                                    (groupError |> Stack.toList)
                        )

                EndDescription ->
                    Tree.singleton { kind = LabelDescriptionStructure, text = "done" }

                PartsDescription partsDescription ->
                    let
                        maybePartError =
                            case error.structure of
                                Nothing ->
                                    Nothing

                                Just (GroupError groupError) ->
                                    let
                                        _ =
                                            Debug.log "located in group" error.structure
                                    in
                                    Just (groupError |> Stack.top)

                                Just _ ->
                                    let
                                        _ =
                                            Debug.log "expected GroupError but found" error.structure
                                    in
                                    Nothing
                    in
                    Tree.tree { kind = LabelDescriptionStructure, text = "parts" }
                        (partsDescription
                            |> Stack.toList
                            |> List.indexedMap
                                (\index partDescription ->
                                    Tree.tree { kind = LabelDescriptionStructure, text = partDescription.tag }
                                        [ descriptionAndErrorToTree partDescription.value
                                            (maybePartError
                                                |> maybeWhen (\partError -> partError.index == index)
                                                |> Maybe.map .error
                                            )
                                        ]
                                )
                        )

                VariantsDescription variantsDescription ->
                    let
                        maybeVariantError =
                            case error.structure of
                                Nothing ->
                                    Nothing

                                Just (VariantError variantError) ->
                                    let
                                        _ =
                                            Debug.log "located in variant" error.structure
                                    in
                                    Just variantError

                                Just _ ->
                                    let
                                        _ =
                                            Debug.log "expected VariantError but found" error.structure
                                    in
                                    Nothing
                    in
                    Tree.tree { kind = LabelDescriptionStructure, text = "variants" }
                        (variantsDescription
                            |> Stack.toList
                            |> List.indexedMap
                                (\index variantDescription ->
                                    Tree.tree { kind = LabelDescriptionStructure, text = variantDescription.tag }
                                        [ descriptionAndErrorToTree variantDescription.value
                                            (maybeVariantError
                                                |> maybeWhen (\variantError -> variantError.index == index)
                                                |> Maybe.map .error
                                            )
                                        ]
                                )
                        )

        treeWithoutErrorAlways =
            treeWithCustomDescriptionNest description_.custom structureTree
    in
    case error.always of
        Nothing ->
            treeWithoutErrorAlways

        Just errorAlways ->
            treeWithoutErrorAlways |> Tree.prependChild errorAlways


treeWithCustomDescriptionNest : Emptiable (Stacked String) Possibly -> Tree Label -> Tree Label
treeWithCustomDescriptionNest labelsToNest bottom =
    case labelsToNest of
        Emptiable.Empty _ ->
            bottom

        Emptiable.Filled (Stack.TopBelow ( line0Custom, lines1UpCustom )) ->
            Tree.tree { kind = LabelDescriptionCustom, text = line0Custom }
                [ treeWithCustomDescriptionNest (Stack.fromList lines1UpCustom) bottom ]


maybeWhen : (content -> Bool) -> Maybe content -> Maybe content
maybeWhen passes =
    \maybe ->
        case maybe of
            Nothing ->
                Nothing

            Just content ->
                content |> justWhen passes


{-| Where [narrowing](#toNarrow) has failed.

`String` is not enough for display?
→ use [`MorphOrError`](#MorphOrError) [`ErrorWithDeadEnd`](#ErrorWithDeadEnd) doing [`deadEndMap`](#deadEndMap)
on [`Morph`](#Morph) that are returned

Have trouble doing so because some API is too strict on errors? → issue

-}
type alias Error =
    ErrorWithDeadEnd String


{-| [`Error`](#Error) with a custom value on `DeadEnd`

    type alias Translate mapped unmapped =
        MorphOrError mapped unmapped (ErrorWithDeadEnd Never)

`deadEnd` could also be formatted text for display.
For that, use [`MorphOrError`](#MorphOrError) [`ErrorWithDeadEnd`](#ErrorWithDeadEnd) doing [`deadEndMap`](#deadEndMap)
on [`Morph`](#Morph) that are returned.

Have trouble doing so because some API is too strict on errors? → issue

-}
type ErrorWithDeadEnd deadEnd
    = DeadEnd deadEnd
    | RowError { startDown : Int, error : ErrorWithDeadEnd deadEnd }
    | InStructureError { index : Int, error : ErrorWithDeadEnd deadEnd }
    | VariantError { index : Int, error : ErrorWithDeadEnd deadEnd }
    | GroupError (GroupError (ErrorWithDeadEnd deadEnd))
    | ChoiceError (Emptiable (Stacked (ErrorWithDeadEnd deadEnd)) Never)


{-| A group's part [`Error`](#Error)s, each with their part index
-}
type alias GroupError partError =
    Emptiable
        (Stacked { index : Int, error : partError })
        Never


{-| Describe the context to improve error messages.

    import Morph.Error
    import Char.Morph as Char
    import String.Morph as Text

    -- we can redefine an error message if something goes wrong
    "123"
        |> Text.toNarrow
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
                |> match (Char.Morph.only '(' |> one)
                |> grab .x Text.number
                |> match (Char.Morph.only ',' |> one)
                |> grab .y Text.number
                |> match (Char.Morph.only ')' |> one)
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
toBroad : MorphIndependently narrow_ broaden -> broaden
toBroad =
    .broaden


{-| Its transformation that turns `broad` into `narrow` or an `error`.
Some call it "parse"
-}
toNarrow : MorphIndependently narrow broaden_ -> narrow
toNarrow =
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
        case unmapped |> toNarrow translate_ of
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

      - [`Array.Morph.toList`](Array-Morph#toList), [`Array.Morph.list`](Array-Morph#list)
      - [`Stack.Morph.toString`](Stack-Morph#toString), [`Stack.Morph.string`](Stack-Morph#string)

  - strip unnecessary information
    ~`{ end : (), before :`~`List element`~`}`~

        translate .before
            (\before_ -> { before = before_, end = () })

Only use [`Translate`](#Translate) to annotate arguments, for results,

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

  - [`map`](#mapTo)`identity`
  - [`toBroad`](#toBroad)`identity`
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
        { custom = Emptiable.empty, inner = CustomDescription }
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
        |> Morph.toBroad
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


{-| [`Morph`](#Morph) that when calling [`toBroad`](Morph#toBroad) always returns a given constant.

For any more complex [`toBroad`](#toBroad) process, use [`translate`](#translate)

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
    { description =
        { custom = Emptiable.empty
        , inner = OnlyDescription (broadConstant |> broadConstantToString)
        }
    , narrow =
        \broadValue ->
            if broadValue == broadConstant then
                () |> Ok

            else
                broadValue
                    |> broadConstantToString
                    |> DeadEnd
                    |> Err
    , broaden = \() -> broadConstant
    }


{-| Create a custom morph for a value by explicitly specifying

  - a `String` description
  - `narrow`: a transformation that can fail with a `String` error
  - `toBroad`: a transformation that can build the parsed value back to what a value that can be parsed

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
        , inner = CustomDescription
        }
    , narrow =
        morphTransformations.narrow
            >> Result.mapError DeadEnd
    , broaden = morphTransformations.broaden
    }



--


{-| To reference a [`Morph`](#Morph) in recursive definitions

    import Morph exposing (grab, match, one)
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
                    |> match
                        (broad [ () ]
                            |> Morph.overRow
                                (atLeast n1 (String.Morph.only " "))
                        )
                    |> match (String.Morph.only "::")
                    |> match
                        (broad [ () ]
                            |> Morph.overRow
                                (atLeast n1 (String.Morph.only " "))
                        )
                    |> grab (Morph.lazy (\() -> lazyList))
                )

    "[]" |> Text.toNarrow lazyList
    --> Ok End

    "a :: []" |> Text.toNarrow lazyList
    --> Ok (Next 'a' End)

    "a :: b :: []" |> Text.toNarrow lazyList
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
            broadValue |> toNarrow (morphLazy ())
    , broaden =
        \narrowValue ->
            narrowValue |> toBroad (morphLazy ())
    }


{-| [`Morph`](#Morph) on groups in progress.
Start with [`group`](#group), complete with [`part`](#part), finally [`partsFinish`](#partsFinish)
-}
type alias PartsMorphEmptiable noPartPossiblyOrNever narrow broaden =
    RecordWithoutConstructorFunction
        { description :
            -- parts
            Emptiable (Stacked { tag : String, value : Description }) noPartPossiblyOrNever
        , narrow : narrow
        , broaden : broaden
        }


{-| Assemble a group from narrow and broad [`part`](#part)s

Use [`group`](#group)
when each broad, narrow [`part`](#part) always has their respective counterpart

    ( "4", "5" )
        |> Morph.toNarrow
            (Morph.parts
                ( \x y -> { x = x, y = y }
                , \x y -> ( x, y )
                )
                |> Group.part ( .x, Tuple.first )
                    (Integer.Morph.toInt
                        |> Morph.overRow Integer.Morph.chars
                        |> Morph.rowFinish
                    )
                |> Group.part ( .y, Tuple.second )
                    (Integer.Morph.toInt
                        |> Morph.over (Integer.Morph.bitSizeAtMost n32)
                        |> Morph.overRow Integer.Morph.chars
                        |> Morph.rowFinish
                    )
                |> Group.finish
            )
    --> Ok { x = 4, y = 5 }

-}
parts :
    ( narrowAssemble
    , broadAssemble
    )
    ->
        PartsMorphEmptiable
            Possibly
            (broad_
             -> Result error_ narrowAssemble
            )
            (groupNarrow_ -> broadAssemble)
parts ( narrowAssemble, broadAssemble ) =
    { description = Emptiable.empty
    , narrow = \_ -> narrowAssemble |> Ok
    , broaden = \_ -> broadAssemble
    }


{-| The [`Morph`](#Morph) of the next part in a [`group`](#group).

    Morph.parts
        ( \nameFirst nameLast email ->
            { nameFirst = nameFirst, nameLast = nameLast, email = email }
        , \nameFirst nameLast email ->
            { nameFirst = nameFirst, nameLast = nameLast, email = email }
        )
        |> Morph.part "name first" ( .nameFirst, .nameFirst ) Morph.keep
        |> Morph.part "name last" ( .nameLast, .nameLast ) Morph.keep
        |> Morph.part "email" ( .email, .email ) emailMorph
        |> Morph.finishParts

-}
part :
    String
    ->
        ( groupNarrow -> partNarrow
        , groupBroad -> partBroad
        )
    -> MorphOrError partNarrow partBroad partError
    ->
        (PartsMorphEmptiable
            noPartPossiblyOrNever_
            (groupBroad
             ->
                Result
                    { index : Int, error : partError }
                    (partNarrow -> groupNarrowFurther)
            )
            (groupNarrow -> (partBroad -> groupBroadenFurther))
         ->
            PartsMorphEmptiable
                noPartNever_
                (groupBroad
                 ->
                    Result
                        { index : Int, error : partError }
                        groupNarrowFurther
                )
                (groupNarrow -> groupBroadenFurther)
        )
part partTagName ( narrowPartAccess, broadPartAccess ) partMorph =
    \groupMorphSoFar ->
        { description =
            groupMorphSoFar.description
                |> Stack.onTopLay { tag = partTagName, value = partMorph.description }
        , narrow =
            groupMorphSoFar.narrow
                |> narrowPart
                    (groupMorphSoFar.description |> Stack.length)
                    broadPartAccess
                    (toNarrow partMorph)
        , broaden =
            groupMorphSoFar.broaden
                |> broadenPart narrowPartAccess (toBroad partMorph)
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
                { index : Int, error : partError }
                (partNarrow -> groupNarrowFurther)
         )
         ->
            (groupBroad
             ->
                Result
                    { index : Int, error : partError }
                    groupNarrowFurther
            )
        )
narrowPart index broadPartAccess narrowPartMorph =
    \groupMorphSoFarNarrow ->
        \groupBroad ->
            case groupBroad |> groupMorphSoFarNarrow of
                Err partsSoFarError ->
                    partsSoFarError |> Err

                Ok groupMorphSoFarEat ->
                    case
                        groupBroad
                            |> broadPartAccess
                            |> narrowPartMorph
                    of
                        Ok partNarrow ->
                            groupMorphSoFarEat partNarrow |> Ok

                        Err partError ->
                            { index = index, error = partError }
                                |> Err


{-| Conclude a [`Group.build`](#group) |> [`Group.part`](#part) chain
-}
partsFinish :
    PartsMorphEmptiable
        Never
        (beforeNarrow
         ->
            Result
                { index : Int, error : ErrorWithDeadEnd deadEnd }
                narrowed
        )
        (beforeBroaden -> broadened)
    ->
        MorphIndependently
            (beforeNarrow
             -> Result (ErrorWithDeadEnd deadEnd) narrowed
            )
            (beforeBroaden -> broadened)
partsFinish =
    \groupMorphInProgress ->
        { description =
            { custom = Emptiable.empty
            , inner = groupMorphInProgress.description |> PartsDescription
            }
        , narrow =
            \broad_ ->
                broad_
                    |> groupMorphInProgress.narrow
                    |> Result.mapError (\error -> error |> Stack.one |> GroupError)
        , broaden = groupMorphInProgress.broaden
        }


{-| Go over an additional step of [`Morph`](#Morph) on its broad type

Chaining

  - `<<` on the broad side
  - `<< Result.andThen` on the narrow side

This can be used to, for example

  - [`Translate`](#Translate) what was [narrowed](#toNarrow)
  - narrow only one variant,
    then of that variant's value type one of their variants

-}
over :
    MorphIndependently
        (beforeBeforeNarrow -> Result (ErrorWithDeadEnd deadEnd) beforeNarrow)
        (beforeBroaden -> broad)
    ->
        (MorphIndependently
            (beforeNarrow -> Result (ErrorWithDeadEnd deadEnd) narrow)
            (beforeBeforeBroaden -> beforeBroaden)
         ->
            MorphIndependently
                (beforeBeforeNarrow -> Result (ErrorWithDeadEnd deadEnd) narrow)
                (beforeBeforeBroaden -> broad)
        )
over morphNarrowBroad =
    \morph ->
        chained morphOver morph morphNarrowBroad


morphOver :
    MorphIndependently
        (beforeNarrow -> Result (ErrorWithDeadEnd deadEnd) narrow)
        (beforeBeforeBroaden -> beforeBroaden)
    ->
        MorphIndependently
            (beforeBeforeNarrow -> Result (ErrorWithDeadEnd deadEnd) beforeNarrow)
            (beforeBroaden -> broad)
    ->
        { narrow : beforeBeforeNarrow -> Result (ErrorWithDeadEnd deadEnd) narrow
        , broaden : beforeBeforeBroaden -> broad
        }
morphOver morph morphNarrowBroad =
    { broaden =
        toBroad morph
            >> toBroad morphNarrowBroad
    , narrow =
        \beforeNarrow ->
            beforeNarrow
                |> toNarrow morphNarrowBroad
                |> Result.mapError (\error -> InStructureError { index = 1, error = error })
                |> Result.andThen
                    (\beforeNarrowNarrow ->
                        beforeNarrowNarrow
                            |> toNarrow morph
                            |> Result.mapError (\error -> InStructureError { index = 0, error = error })
                    )
    }


{-| `Translate a <-> b`
by swapping the functions [`map`](#mapTo) <-> [`unmap`](#toBroad).

    [ 'O', 'h', 'a', 'y', 'o' ]
        |> Morph.mapTo String.Morph.list
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

[`unmap`](#toBroad) `...` is equivalent to `map (... |> reverse)`.

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
        structure "invert" translateInvert
            |> structureAdd translate_
            |> structureFinish


translateInvert :
    MorphIndependently
        (beforeMap -> Result (ErrorWithDeadEnd Never) mapped)
        (beforeUnmap -> unmapped)
    ->
        { narrow : beforeUnmap -> Result error unmapped
        , broaden : beforeMap -> mapped
        }
translateInvert translate_ =
    { narrow =
        \unmapped ->
            unmapped |> toBroad translate_ |> Ok
    , broaden = mapTo translate_
    }


{-| Change all [`DeadEnd`](#ErrorWithDeadEnd)s based on their current values.

`deadEnd` can for example be changed to formatted text for display.
For that, use [`MorphOrError`](#MorphOrError) [`ErrorWithDeadEnd`](#ErrorWithDeadEnd) doing [`deadEndMap`](#deadEndMap)
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

            RowError row ->
                { startDown = row.startDown
                , error = row.error |> deadEndMap deadEndChange
                }
                    |> RowError

            InStructureError inStructureError ->
                { index = inStructureError.index
                , error = inStructureError.error |> deadEndMap deadEndChange
                }
                    |> InStructureError

            GroupError parts_ ->
                parts_
                    |> Stack.map
                        (\_ partError ->
                            { index = partError.index
                            , error = partError.error |> deadEndMap deadEndChange
                            }
                        )
                    |> GroupError

            ChoiceError possibilities ->
                possibilities
                    |> Stack.map
                        (\_ -> deadEndMap deadEndChange)
                    |> ChoiceError

            VariantError variantError ->
                { index = variantError.index
                , error = variantError.error |> deadEndMap deadEndChange
                }
                    |> VariantError


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

            RowError row ->
                row.error |> deadEndNever

            InStructureError inStructureError ->
                inStructureError.error |> deadEndNever

            GroupError parts_ ->
                parts_
                    |> Stack.top
                    |> .error
                    |> deadEndNever

            ChoiceError possibilities ->
                possibilities
                    |> Stack.top
                    |> deadEndNever

            VariantError variantError ->
                variantError.error |> deadEndNever


{-| Change the potential [`Error`](#Error). This is usually used with either

  - [`deadEndNever : ErrorWithDeadEnd Never -> any_`](#deadEndNever)
  - [`deadEndMap`](#deadEndMap)

-}
narrowErrorMap :
    (error -> errorMapped)
    ->
        MorphIndependently
            (beforeNarrow -> Result error narrowed)
            toBroad
    ->
        MorphIndependently
            (beforeNarrow -> Result errorMapped narrowed)
            toBroad
narrowErrorMap errorChange =
    \morph ->
        { description = morph |> description
        , broaden = toBroad morph
        , narrow =
            toNarrow morph
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
        , inner = CustomDescription
        }
    , narrow =
        \broad_ ->
            broad_
                |> structureMap (mapTo elementTranslate)
                |> Ok
    , broaden =
        structureUnmap (toBroad elementTranslate)
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

    import Morph exposing (MorphRow, atLeast, match, into, Morph.succeed, grab, one)
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
                |> match (String.Morph.only "(")
                |> match
                    (broad (ArraySIzed.one ())
                        |> Morph.overRow (atLeast n0 (String.Morph.only " "))
                    )
                |> grab number
                |> match
                    (broad ArraySIzed.empty
                        |> Morph.overRow (atLeast n0 (String.Morph.only " "))
                    )
                |> match (String.Morph.only ",")
                |> match
                    (broad (ArraySIzed.one ())
                        |> Morph.overRow (atLeast n0 (String.Morph.only " "))
                    )
                |> grab .x Number.Morph.text
                |> match
                    (broad (ArraySIzed.one ())
                        |> Morph.overRow (atLeast n0 (String.Morph.only " "))
                    )
                |> match (String.Morph.only ")")
            )

    -- we can get a nice error message if it fails
    "(2.71, x)"
        |> Text.toNarrow point
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

  - 👎 performs worse as there are more [possibilities](Morph#try) to parse to know it failed

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
        (narrow
         ->
            -- Rope is like a List that has faster nested concatenation
            -- see https://dark.elm.dmy.fr/packages/miniBill/elm-rope/latest/
            Rope broadElement
        )


{-| Incomplete [`MorphRow`](#MorphRow) for a thing composed of multiple parts = group.
It's what you supply during a [`Morph.succeed`](Morph#succeed)`|>`[`grab`](#grab)/[`match`](#match) build
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
         ->
            -- Rope is like a List that has faster nested concatenation
            -- see https://dark.elm.dmy.fr/packages/miniBill/elm-rope/latest/
            Rope broadElement
        )


{-| [`MorphRow`](#MorphRow) from and to a single broad input.


## `Morph.keep |> Morph.one`

> ℹ️ Equivalent regular expression: `.`

    import Morph
    import Morph.Error
    import String.Morph as Text

    -- can match any character
    "a" |> Text.toNarrow (Morph.keep |> one)
    --> Ok 'a'

    "#" |> Text.toNarrow (Morph.keep |> one)
    --> Ok '#'

    -- only fails if we run out of inputs
    ""
        |> Text.toNarrow (Morph.keep |> one)
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
                            |> RowError
                            |> Err

                    Emptiable.Filled (Stack.TopBelow ( nextBroadElement, afterNextBroadElement )) ->
                        case
                            nextBroadElement
                                |> toNarrow morph
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
                                    |> RowError
                                    |> Err
        , broaden =
            \beforeToBroad ->
                beforeToBroad
                    |> toBroad morph
                    |> Rope.singleton
        }


{-| Never consumes anything.
Always returns the given narrow constant.
Never fails.

For anything composed of multiple parts,
first declaratively describes what you expect to get in the end,
then [grabbing (taking)](#grab) and [matching (dropping/skipping)](#match) what you need

    import Morph exposing (Morph.succeed, one)
    import String.Morph exposing (integer)

    type alias Point =
        -- makes `Point` function unavailable:
        -- https://dark.elm.dmy.fr/packages/lue-bird/elm-no-record-type-alias-constructor-function/latest/
        RecordWithoutConstructorFunction
            { x : Int
            , y : Int
            }

    point : MorphRow Point Char
    point =
        Morph.succeed (\x y -> { x = x, y = y })
            |> grab .x integer
            |> match (String.Morph.only ",")
            |> grab .y integer

    "12,34" |> Text.toNarrow point
    --> Ok { x = 12, y = 34 }


### example: infix-separated elements

    Morph.succeed (\first separatedElements -> { first = first, separatedElements = separatedElements })
        |> grab .first element
        |> grab .separatedElements
            (ArraySized.Morph.atLeast n0
                (Morph.succeed (\separator element -> { element = element, separator = separator })
                    |> grab .separator separator
                    |> grab .element element
                )
            )


### `Morph.succeed` anti-patterns

One example you'll run into when using other parsers is using

    Morph.succeed identity
        |> match ...
        |> match ...
        |> grab ...
        |> match ...

it get's pretty hard to read as you have to jump around the code to know what you're actually producing

    Morph.succeed (\sum -> sum) |> ...

is already nicer

-}
succeed :
    narrowConstant
    -> MorphRowIndependently beforeBroaden_ narrowConstant broadElement_
succeed narrowConstant =
    structure "sequence"
        { narrow =
            \broad_ ->
                { narrow = narrowConstant
                , broad = broad_
                }
                    |> Ok
        , broaden =
            \_ -> Rope.empty
        }
        |> structureFinish


{-| Take what we get from [converting](#MorphRow) the next section
and channel it back up to the [`Morph.succeed`](Morph#succeed) grouping
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
        groupMorphRowSoFar |> next partAccess (|>) grabbedNextMorphRow


{-| Require values to be present next to continue but ignore the result.
On the parsing side, this is often called "skip" or "drop", `elm/parser` uses `|.`

    import String.Morph exposing (text)
    import Morph exposing (match, grab)

    -- parse a simple email, but we're only interested in the username
    "user@example.com"
        |> Text.toNarrow
            (Morph.succeed (\userName -> { username = userName })
                |> grab .username (ArraySized.Morph.atLeast n1 aToZ)
                |> match (String.Morph.only "@")
                |> match
                    (Text.fromList
                        |> Morph.overRow (atLeast n1 aToZ)
                        |> broad "example"
                    )
                |> match (text ".com")
            )
    --> Ok { username = "user" }

[`broad`](#broad) `... |>` [`Morph.overRow`](Morph#overRow) is cool:
when multiple kinds of input can be dropped,
it allows choosing a default possibility for building.

-}
match :
    MorphRow () broadElement
    ->
        (MorphRowIndependently groupNarrow groupNarrowConstruct broadElement
         -> MorphRowIndependently groupNarrow groupNarrowConstruct broadElement
        )
match ignoredNextMorphRow =
    \groupMorphRowSoFar ->
        groupMorphRowSoFar |> next (\_ -> ()) (\() -> identity) ignoredNextMorphRow


{-| See `match` and `grab` implementation
-}
next :
    (groupNarrow -> partNextNarrow)
    -> (partNextNarrow -> (groupNarrowConstruct -> groupNarrowConstructChanged))
    -> MorphRow partNextNarrow broadElement
    ->
        (MorphRowIndependently
            groupNarrow
            groupNarrowConstruct
            broadElement
         ->
            MorphRowIndependently
                groupNarrow
                groupNarrowConstructChanged
                broadElement
        )
next partAccess partChange nextMorphRow =
    \groupMorphRowSoFar ->
        structureBinaryExtension "sequence"
            (morphNext partAccess partChange)
            groupMorphRowSoFar
            nextMorphRow


morphNext :
    (groupNarrow -> partNextNarrow)
    -> (partNextNarrow -> (groupNarrowConstruct -> groupNarrowConstructChanged))
    ->
        MorphRowIndependently
            groupNarrow
            groupNarrowConstruct
            broadElement
    -> MorphRow partNextNarrow broadElement
    ->
        { narrow :
            Emptiable (Stacked broadElement) Possibly
            ->
                Result
                    Error
                    { narrow : groupNarrowConstructChanged
                    , broad : Emptiable (Stacked broadElement) Possibly
                    }
        , broaden :
            groupNarrow
            -> Rope broadElement
        }
morphNext partAccess partChange groupMorphRowSoFar nextMorphRow =
    { narrow =
        \broad_ ->
            broad_
                |> toNarrow groupMorphRowSoFar
                |> Result.andThen
                    (\result ->
                        case result.broad |> toNarrow nextMorphRow of
                            Ok nextParsed ->
                                { narrow = result.narrow |> partChange nextParsed.narrow
                                , broad = nextParsed.broad
                                }
                                    |> Ok

                            Err nextError ->
                                nextError
                                    |> Err
                    )
    , broaden =
        \groupNarrow ->
            groupNarrow
                |> partAccess
                |> toBroad nextMorphRow
                |> Rope.appendTo
                    (groupNarrow
                        |> toBroad groupMorphRowSoFar
                    )
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
           Use [`Morph.choice`](Morph#choice) where each [`possibility`](#try) expects a specific number


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
        |> Text.toNarrow
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
        chained morphOverRow morphRowBeforeMorph narrowMorph


morphOverRow :
    MorphRowIndependently beforeBroaden beforeNarrow broadElement
    ->
        (MorphIndependently
            (beforeNarrow -> Result Error narrow)
            (beforeBeforeBroaden -> beforeBroaden)
         ->
            { narrow :
                Emptiable (Stacked broadElement) Possibly
                ->
                    Result
                        Error
                        { narrow : narrow
                        , broad : Emptiable (Stacked broadElement) Possibly
                        }
            , broaden :
                beforeBeforeBroaden
                -> Rope broadElement
            }
        )
morphOverRow morphRowBeforeMorph narrowMorph =
    { narrow =
        \broad_ ->
            broad_
                |> toNarrow morphRowBeforeMorph
                |> Result.andThen
                    (\narrowed ->
                        narrowed.narrow
                            |> toNarrow narrowMorph
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
                                        |> RowError
                                )
                    )
    , broaden =
        \beforeNarrow ->
            beforeNarrow
                |> toBroad narrowMorph
                |> toBroad morphRowBeforeMorph
    }


structureBinaryExtension :
    String
    ->
        (MorphIndependently (soFarBeforeNarrow -> Result (ErrorWithDeadEnd deadEnd) soFarNarrow) soFarBroaden
         -> MorphIndependently (nextBeforeNarrow -> Result (ErrorWithDeadEnd deadEnd) nextNarrow) nextBroaden
         -> { a | narrow : toNarrow, broaden : toBroad }
        )
    -> MorphIndependently (soFarBeforeNarrow -> Result (ErrorWithDeadEnd deadEnd) soFarNarrow) soFarBroaden
    -> MorphIndependently (nextBeforeNarrow -> Result (ErrorWithDeadEnd deadEnd) nextNarrow) nextBroaden
    -> MorphIndependently toNarrow toBroad
structureBinaryExtension structureName morphExtend soFarMorph nextMorph =
    let
        newStructure () =
            structure structureName morphExtend
                |> structureAdd soFarMorph
                |> structureAdd nextMorph
                |> structureFinish
    in
    case soFarMorph |> description |> .inner of
        StructureDescription soFarStructure soFar ->
            if soFarStructure == structureName then
                let
                    morphExtendedStructure =
                        morphExtend
                            soFarMorph
                            (nextMorph
                                |> narrowErrorMap
                                    (\error ->
                                        InStructureError
                                            { index = (soFar |> Stack.length) + 1
                                            , error = error
                                            }
                                    )
                            )
                in
                { description =
                    { custom = Emptiable.empty
                    , inner =
                        soFar
                            |> Stack.attach Up (Stack.one (nextMorph |> description))
                            |> StructureDescription structureName
                    }
                , narrow = morphExtendedStructure.narrow
                , broaden = morphExtendedStructure.broaden
                }

            else
                newStructure ()

        _ ->
            newStructure ()


chained :
    (MorphIndependently (soFarBeforeNarrow -> Result (ErrorWithDeadEnd deadEnd) soFarNarrow) soFarBroaden
     -> MorphIndependently (nextBeforeNarrow -> Result (ErrorWithDeadEnd deadEnd) nextNarrow) nextBroaden
     -> { a | narrow : toNarrow, broaden : toBroad }
    )
    -> MorphIndependently (soFarBeforeNarrow -> Result (ErrorWithDeadEnd deadEnd) soFarNarrow) soFarBroaden
    -> MorphIndependently (nextBeforeNarrow -> Result (ErrorWithDeadEnd deadEnd) nextNarrow) nextBroaden
    -> MorphIndependently toNarrow toBroad
chained morphChained morphBeforeMorph narrowMorph =
    structureBinaryExtension "chained" morphChained morphBeforeMorph narrowMorph



-- sequence


{-| [Morph](#MorphRow) multiple elements from now to when `end` matches.

    decoderNameSubject : MorphRow String Char expectationCustom
    decoderNameSubject =
        Text.fromList
            |> Morph.overRow
                (MorphRow.before
                    { end =
                        Morph.succeed ()
                            |> match (String.Morph.only "Decoder")
                            |> match Morph.end
                    , goOn = Morph.keep |> Morph.one
                    }
                )

You might think: Why not use

    decoderNameSubject : MorphRow String Char expectationCustom
    decoderNameSubject =
        Morph.succeed (\subject -> subject)
            |> grab (\subject -> subject)
                (atLeast n0 (Morph.keep |> Morph.one))
            |> match (String.Morph.only "Decoder")
            |> match Morph.end

Problem is: This will never (Morph.)succeed.
`atLeast n0 (Morph.keep |> Morph.one)` always goes on.
We never reach the necessary [`match`](#match)ped things.

-}
before :
    { end : MorphRow () broadElement
    , goOn : MorphRow goOnElement broadElement
    }
    -> MorphRow (Emptiable (Stacked goOnElement) Possibly) broadElement
before untilStep =
    until
        { commit =
            translate .before
                (\before_ -> { before = before_, end = () })
        , end = untilStep.end
        , goOn = untilStep.goOn
        }


{-| How are [`ArraySized.Morph.in_`](ArraySized-Morph#in_), ... defined?

    decoderNameSubject : MorphRow String Char
    decoderNameSubject =
        Stack.string
            |> Morph.over
                (translate .before
                    (\before -> { before = before, end = () })
                )
            |> Morph.overRow
                (MorphRow.until
                    { end =
                        Morph.succeed ()
                            |> match (String.Morph.only "Decoder")
                            |> match Morph.end
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
            , before : Emptiable (Stacked goOnElement) Possibly
            }
    , end : MorphRow endElement broadElement
    , goOn : MorphRow goOnElement broadElement
    }
    -> MorphRow commitResult broadElement
until untilStep =
    structure "repeating" morphUntil
        |> structureAdd
            (structure "until" untilStep.commit
                |> structureFinish
            )
        |> structureAdd
            (structure "end" untilStep.end
                |> structureFinish
            )
        |> structureAdd
            (structure "go on" untilStep.goOn
                |> structureFinish
            )
        |> structureFinish


morphUntil :
    Morph
        commitResult
        { end : endElement
        , before : Emptiable (Stacked goOnElement) Possibly
        }
    -> MorphRow endElement broadElement
    -> MorphRow goOnElement broadElement
    ->
        { broaden : commitResult -> Rope broadElement
        , narrow :
            Emptiable (Stacked broadElement) Possibly
            ->
                Result
                    Error
                    { narrow : commitResult
                    , broad : Emptiable (Stacked broadElement) Possibly
                    }
        }
morphUntil commit endStep goOn =
    { broaden =
        \commitResultNarrow ->
            let
                committedBack :
                    { end : endElement
                    , before : Emptiable (Stacked goOnElement) Possibly
                    }
                committedBack =
                    commitResultNarrow |> toBroad commit
            in
            committedBack.before
                |> Stack.toList
                |> List.reverse
                |> Rope.fromList
                |> Rope.concatMap (toBroad goOn)
                |> Rope.appendTo
                    (committedBack.end |> toBroad endStep)
    , narrow =
        let
            loopStep beforeSoFar =
                choice
                    (\variantCommit variantGoOn loopStepNarrow ->
                        case loopStepNarrow of
                            Commit commitElement ->
                                variantCommit commitElement

                            GoOn goOnELement ->
                                variantGoOn goOnELement
                    )
                    |> tryRow Commit
                        (commit
                            |> over (translate (\end_ -> { end = end_, before = beforeSoFar }) .end)
                            |> overRow endStep
                        )
                    |> tryRow GoOn goOn
                    |> choiceRowFinish

            loopNarrowStep :
                Emptiable (Stacked goOnElement) Possibly
                ->
                    (Emptiable (Stacked broadElement) Possibly
                     ->
                        Result
                            Error
                            { narrow : commitResult
                            , broad : Emptiable (Stacked broadElement) Possibly
                            }
                    )
            loopNarrowStep beforeSoFar =
                \broadElements ->
                    broadElements
                        |> toNarrow (loopStep beforeSoFar)
                        |> Result.andThen
                            (\stepped ->
                                case stepped.narrow of
                                    Commit committed ->
                                        { narrow = committed, broad = stepped.broad } |> Ok

                                    GoOn goOnElement ->
                                        stepped.broad
                                            |> loopNarrowStep
                                                (beforeSoFar |> Stack.onTopLay goOnElement)
                            )
        in
        loopNarrowStep Emptiable.empty
    }


{-| How to continue a loop.
Either continue with a partial result or return with a complete value
-}
type LoopStep partial complete
    = GoOn partial
    | Commit complete



--


{-| Only matches when there's no further broad input afterwards.

This is not required for [`narrow`](#toNarrow)ing to Morph.succeed.

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
                        Morph.succeed ()
                            |> match (String.Morph.only "Decoder")
                            |> match Morph.end
                    , goOn = Morph.keep |> Morph.one
                    }
                )

-}
end : MorphRow () broadElement_
end =
    { description =
        { custom = Emptiable.empty
        , inner = EndDescription
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
                        |> RowError
                        |> Err
    , broaden =
        \() -> Rope.empty
    }


{-| Final step before running a [`MorphRow`](#MorphRow),
transforming it into a [`Morph`](#Morph) on the full stack of input elements.

    fromString =
        toNarrow
            (Point.morphChars
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
                    |> toNarrow morphRow
                    |> Result.andThen
                        (\result ->
                            result.broad
                                |> toNarrow end
                                |> Result.map
                                    (\_ -> result.narrow)
                        )
        , broaden =
            \narrow -> narrow |> toBroad morphRow |> Rope.toList |> Stack.fromList
        }



--


{-| Possibly incomplete [`Morph`](Morph#Morph) for a choice type.
See [`Morph.choice`](Morph#choice), [`try`](#try), [`choiceFinish`](#choiceFinish)
-}
type alias ChoiceMorphEmptiable noTryPossiblyOrNever choiceNarrow choiceBeforeNarrow choiceBroaden error =
    RecordWithoutConstructorFunction
        { description :
            Emptiable (Stacked Description) noTryPossiblyOrNever
        , narrow :
            choiceBeforeNarrow
            ->
                Result
                    (-- tries
                     Emptiable (Stacked error) noTryPossiblyOrNever
                    )
                    choiceNarrow
        , broaden : choiceBroaden
        }


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
        "\n\t abc" |> Text.toNarrow blank --> Ok '\n'

        -- anything else makes it fail
        "abc"
            |> Text.toNarrow blank
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
    either a return character or the end of the whole text.
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
        ChoiceMorphEmptiable
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

Usually, you'll be better off with a [`Morph.choice`](#choice)
an explicit custom tagged union
because you'll have the option to preserve what was [narrowed](Morph#toNarrow).
(Remember: you can always discard that info and set a preferred option with [`Morph.broad`](Morph#broad))

Use [`choiceEquivalent`](#choiceEquivalent) if you have a dynamic list of aliases/morphs to treat equally.
An example is defined variable names

    Morph.choiceEquivalent Char.Morph.only
        { broad = '∨'
        , alternatives = [ '|' ]
        }

    Morph.choiceEquivalent String.Morph.only
        { broad = "∨"
        , alternatives = [ "|", "or" ]
        }

    Morph.choiceEquivalent String.Morph.only
        { broad = "±"
        , alternatives = [ "pm", "plusminus" ]
        }

Performance note: This could be optimized
as shown in ["Fast parsing of String Sets in Elm" by Marcelo Lazaroni](https://lazamar.github.io/fast-parsing-of-string-sets-in-elm/)
published as [`dict-parser`](https://dark.elm.dmy.fr/packages/lazamar/dict-parser/latest/Parser-Dict)

Currently, such an optimized version isn't provided because
There is no existing Trie implementation
that can use non-`comparable` constrained elements
which means we would have to write a complete trie implementation from scratch including the Dict part
(`dict-parser` bases it's trie on `Dict`).

Happy to merge your contributions!

-}
choiceEquivalent :
    (broadPossibility
     ->
        MorphIndependently
            (beforeNarrow
             -> Result (ErrorWithDeadEnd deadEnd) narrow
            )
            broaden
    )
    ->
        { broad : broadPossibility
        , alternatives : List broadPossibility
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
                        |> ChoiceDescription
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
            broaden_
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
                    |> toNarrow
                        (traverseTry (tries |> Stack.top))
                    |> Result.mapError Stack.one
                )
                Up
                (\elementForMorph resultSoFar ->
                    resultSoFar
                        |> recoverTry
                            (\errorsSoFar ->
                                beforeNarrow
                                    |> toNarrow (traverseTry elementForMorph)
                                    |> Result.mapError
                                        (\error -> errorsSoFar |> Stack.onTopLay error)
                            )
                )
            |> Result.mapError ChoiceError


{-| Builder for a [`Morph`](#Morph) to a choice. Possibly incomplete

Initialize with [`Morph.variants`](#variants)

-}
type alias VariantsMorphEmptiable noTryPossiblyOrNever narrow broaden =
    RecordWithoutConstructorFunction
        { description :
            Emptiable
                (Stacked { tag : String, value : Description })
                noTryPossiblyOrNever
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
    "_" |> Text.toNarrow (underscoreOrLetter |> one)
    --> Ok Underscore

    -- if it fails, try the next
    "a" |> Text.toNarrow (underscoreOrLetter |> one)
    --> Ok 'a'

    -- if none work, we get the error from all possible steps
    "1"
        |> Text.toNarrow (underscoreOrLetter |> one)
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
        (ChoiceMorphEmptiable
            noTryPossiblyOrNever_
            narrowChoice
            possibilityBeforeNarrow
            ((possibilityBeforeBroaden -> possibilityBroad)
             -> choiceBroadenFurther
            )
            error
         ->
            ChoiceMorphEmptiable
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
                    |> recoverTry
                        (\soFarTryErrors ->
                            case broadValue |> toNarrow possibilityMorph of
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
                (toBroad possibilityMorph)
        }



-- each variant


{-| Initialize a [variants morph](#VariantsMorphEmptiable)
by discriminating `(` the broad`,` the narrow `)` choices,
then `|>` [`Morph.try`](Morph#try)ing each possibility,
concluding the builder with [`Morph.choiceFinish`](#choiceFinish)

A use case is [morphing](Morph#Morph) from and to an internal type

    absoluteInternal : MorphOrError Absolute Decimal.Internal.Absolute error_
    absoluteInternal =
        Morph.variants
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
            |> Morph.variant ( Fraction, Decimal.Internal.Fraction ) fractionInternal
            |> Morph.variant ( AtLeast1, Decimal.Internal.AtLeast1 ) atLeast1Internal
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
variants :
    ( narrowByPossibility
    , broadenByPossibility
    )
    ->
        VariantsMorphEmptiable
            Possibly
            narrowByPossibility
            broadenByPossibility
variants ( narrowByPossibility, broadenByPossibility ) =
    { description = Emptiable.empty
    , narrow = narrowByPossibility
    , broaden = broadenByPossibility
    }


{-| [`Morph`](Morph#Morph) the next variant value.
Finish with [`Morph.variantsFinish`](#variantsFinish)
-}
variant :
    String
    ->
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
        (VariantsMorphEmptiable
            noTryPossiblyOrNever_
            ((beforeNarrowVariantValue
              -> Result { index : Int, error : error } narrowChoice
             )
             -> narrowChoiceFurther
            )
            ((beforeBroadVariantValue -> broadChoice)
             -> broadenChoiceFurther
            )
         ->
            VariantsMorphEmptiable
                noTryNever_
                narrowChoiceFurther
                broadenChoiceFurther
        )
variant variantTagName ( possibilityToChoice, possibilityFromChoice ) possibilityMorph =
    \choiceMorphSoFar ->
        { description =
            choiceMorphSoFar.description
                |> Stack.onTopLay
                    { tag = variantTagName, value = possibilityMorph.description }
        , narrow =
            choiceMorphSoFar.narrow
                (\broad_ ->
                    case broad_ |> toNarrow possibilityMorph of
                        Ok possibility ->
                            possibility |> possibilityToChoice |> Ok

                        Err error ->
                            { index = choiceMorphSoFar.description |> Stack.length, error = error } |> Err
                )
        , broaden =
            choiceMorphSoFar.broaden
                (\narrow ->
                    narrow
                        |> toBroad possibilityMorph
                        |> possibilityFromChoice
                )
        }


{-| Conclude a [`Morph.variants`](Morph#variants) `|>` [`Morph.variant`](Morph#variant) builder
-}
variantsFinish :
    VariantsMorphEmptiable
        Never
        (beforeNarrow
         -> Result { index : Int, error : ErrorWithDeadEnd deadEnd } narrow
        )
        broaden
    ->
        MorphIndependently
            (beforeNarrow
             -> Result (ErrorWithDeadEnd deadEnd) narrow
            )
            broaden
variantsFinish =
    \choiceMorphComplete ->
        { description =
            { custom = Emptiable.empty
            , inner =
                choiceMorphComplete.description
                    |> VariantsDescription
            }
        , narrow =
            \beforeNarrow ->
                beforeNarrow
                    |> choiceMorphComplete.narrow
                    |> Result.mapError VariantError
        , broaden = choiceMorphComplete.broaden
        }



-- MorphRow


{-| Possibly incomplete [`MorphRow`](#MorphRow) to and from a Morph.choice.
See [`Morph.choice`](Morph#choice), [`Morph.tryRow`](#try), [`Morph.choiceRowFinish`](#choiceFinish)
-}
type alias ChoiceMorphRowEmptiable noTryPossiblyOrNever choiceNarrow choiceBroaden broadElement =
    { description :
        Emptiable (Stacked Description) noTryPossiblyOrNever
    , narrow :
        Emptiable (Stacked broadElement) Possibly
        ->
            Result
                (-- tries
                 Emptiable (Stacked Error) noTryPossiblyOrNever
                )
                { narrow : choiceNarrow
                , broad : Emptiable (Stacked broadElement) Possibly
                }
    , broaden : choiceBroaden
    }


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
            |> Morph.try (\() -> Underscore) (Char.Morph.only '_')
            |> Morph.try Letter AToZ.caseAny
            |> Morph.choiceFinish

    -- try the first possibility
    "_"
        |> Text.toNarrow underscoreOrLetter
    --> Ok Underscore

    -- if it fails, try the next
    "a"
        |> Text.toNarrow underscoreOrLetter
    --> Ok 'a'

    -- if none work, we get the error from all possible steps
    "1"
        |> Text.toNarrow (onFailDown [ one '_', AToZ.char ])
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
            |> Morph.tryRow Letter
                (map String.Morph.list
                    (atLeast n1 AToZ.char)
                )
            |> Morph.tryRow Digit
                (atLeast n1 Digit.n0To9)
            |> Morph.choiceRowFinish

    -- try letters, or else give me some digits
    "abc"
        |> Text.toNarrow alphaNum
    --> Ok "abc"

    -- we didn't get letters, but we still got digits
    "123"
        |> Text.toNarrow alphaNum
    --> Ok "123"

    -- but if we still fail, give the expectations of all steps
    "_"
        |> Text.toNarrow alphaNum
        |> Result.mapError Morph.Error.textMessage
    --> Err "1:1: I was expecting at least 1 digit [0-9]. I got stuck when I got the character '_'."

-}
tryRow :
    (possibilityNarrow -> choiceNarrow)
    -> MorphRow possibilityNarrow broadElement
    ->
        (ChoiceMorphRowEmptiable
            noTryPossiblyOrNever_
            choiceNarrow
            ((possibilityNarrow -> Rope broadElement)
             -> choiceBroadenFurther
            )
            broadElement
         ->
            ChoiceMorphRowEmptiable
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
                    |> recoverTry
                        (\soFarErrorPossibilities ->
                            case choiceBroad |> toNarrow possibilityMorph of
                                Ok possibilityParsed ->
                                    { broad = possibilityParsed.broad
                                    , narrow =
                                        possibilityParsed.narrow |> possibilityToChoice
                                    }
                                        |> Ok

                                Err possibilityExpectation ->
                                    soFarErrorPossibilities
                                        |> Stack.onTopLay possibilityExpectation
                                        |> Err
                        )
        , broaden =
            choiceMorphSoFar.broaden
                (toBroad possibilityMorph)
        }


{-| Always the last step in a [`Morph.choice`](#choice) `|>` [`Morph.tryRow`](#tryRow) build process
-}
choiceRowFinish :
    ChoiceMorphRowEmptiable
        Never
        choiceNarrow
        (choiceNarrow -> Rope broadElement)
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
                            |> ChoiceDescription
                    }
        , narrow =
            \broad_ ->
                broad_
                    |> choiceMorphRowComplete.narrow
                    |> Result.mapError
                        (\errorPossibilities ->
                            { startDown = broad_ |> Stack.length
                            , error = errorPossibilities |> ChoiceError
                            }
                                |> RowError
                        )
        , broaden = choiceMorphRowComplete.broaden
        }


{-| Conclude a [`Morph.choice`](Morph#choice) `|>` [`Morph.try`](Morph#try) builder
-}
choiceFinish :
    ChoiceMorphEmptiable
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
                            |> ChoiceDescription
                    }
        , narrow =
            \beforeToNarrow ->
                beforeToNarrow
                    |> choiceMorphComplete.narrow
                    |> Result.mapError ChoiceError
        , broaden =
            choiceMorphComplete.broaden
        }



-- copied from StructureMorph to avoid import cycles with DescriptionInner :(
-- should not be exposed


type alias StructureMorph morph =
    { description :
        { structure : String
        , inner : Emptiable (Stacked Description) Possibly
        }
    , morph : morph
    }


structure :
    String
    -> morph
    -> StructureMorph morph
structure structureName morph =
    { description = { structure = structureName, inner = Emptiable.empty }
    , morph = morph
    }


structureAdd :
    MorphIndependently
        (partBeforeNarrow -> Result (ErrorWithDeadEnd deadEnd) partNarrow)
        partToBroad
    ->
        StructureMorph
            (MorphIndependently
                (partBeforeNarrow -> Result (ErrorWithDeadEnd deadEnd) partNarrow)
                partToBroad
             -> morph
            )
    -> StructureMorph morph
structureAdd partMorph =
    \structureCreatorSoFar ->
        { description =
            { structure = structureCreatorSoFar.description.structure
            , inner =
                structureCreatorSoFar.description.inner
                    |> Stack.onTopLay (partMorph |> description)
            }
        , morph =
            structureCreatorSoFar.morph
                { partMorph
                    | narrow =
                        \beforeNarrow ->
                            beforeNarrow
                                |> toNarrow partMorph
                                |> Result.mapError
                                    (\error ->
                                        InStructureError
                                            { index = structureCreatorSoFar.description.inner |> Stack.length
                                            , error = error
                                            }
                                    )
                }
        }


structureFinish :
    StructureMorph { morph_ | narrow : toNarrow, broaden : toBroad }
    -> MorphIndependently toNarrow toBroad
structureFinish =
    \structureCreator ->
        { description =
            { custom = Emptiable.empty
            , inner =
                StructureDescription
                    structureCreator.description.structure
                    (structureCreator.description.inner |> Stack.reverse)
            }
        , narrow = structureCreator.morph.narrow
        , broaden = structureCreator.morph.broaden
        }
