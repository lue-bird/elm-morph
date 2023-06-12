module Morph exposing
    ( Morph, Translate, MorphOrError, MorphIndependently
    , Description, DescriptionInner(..)
    , toBroadOnly
    , custom, only, validate
    , translate, broad, toggle, keep, translateOn
    , recursive
    , end, one, succeed, grab, match
    , named
    , invert
    , deadEndMap
    , deadEndNever, narrowErrorMap
    , Error, ErrorWithDeadEnd(..), GroupError, InChainPlace(..), InSequencePlace(..)
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
    , ChoiceMorphRowEmptiable, tryRow
    , MorphRow, MorphRowIndependently, rowFinish
    , before
    , until, untilFold, whilePossibleFold
    )

{-| Call it Codec, Coder, ParserBuilder, Convert-, Transform-, Shape-, FormReversible, ToFrom, ...
We call it

@docs Morph, Translate, MorphOrError, MorphIndependently
@docs Description, DescriptionInner


## create

@docs toBroadOnly
@docs custom, only, validate
@docs translate, broad, toggle, keep, translateOn

@docs recursive


### create row

@docs end, one, succeed, grab, match


## alter

@docs named
@docs invert
@docs deadEndMap
@docs deadEndNever, narrowErrorMap


## error

@docs Error, ErrorWithDeadEnd, GroupError, InChainPlace, InSequencePlace
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

@docs ChoiceMorphRowEmptiable, tryRow


## row

@docs MorphRow, MorphRowIndependently, rowFinish


## sequence

  - optional → [`Maybe.Morph.row`](Maybe-Morph#row)
  - [`atLeast`](ArraySized-Morph#atLeast)
  - [`exactly`](ArraySized-Morph#exactly)
  - between → [`ArraySized.Morph.in_`](ArraySized-Morph#exactly)

@docs before


### advanced sequence

@docs until, untilFold, whilePossibleFold


### oh look! other projects do similar things

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

import Emptiable exposing (Emptiable)
import Json.Decode exposing (Error)
import Linear exposing (Direction(..))
import Possibly exposing (Possibly(..))
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Rope exposing (Rope)
import Stack exposing (Stacked)
import Tree exposing (Tree)
import Util exposing (justWhen, maybeWhen, recoverTry)



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
type alias MorphIndependently toNarrow toBroad =
    RecordWithoutConstructorFunction
        { description : Description
        , toNarrow : toNarrow
        , toBroad : toBroad
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


{-| Structural description of what is being morphed
-}
type
    DescriptionInner
    -- Translate
    = InverseDescription Description
    | CustomDescription
      -- MorphRow
    | SucceedDescription
    | EndDescription
    | WhilePossibleDescription Description
    | UntilDescription { commit : Description, end : Description, element : Description }
    | SequenceDescription { early : Description, late : Description }
      -- others
    | OnlyDescription String
    | InnerRecursiveDescription String (() -> Description)
    | ChainDescription { broad : Description, narrow : Description }
      -- group morph
    | GroupDescription (Emptiable (Stacked Description) Never)
    | ElementsDescription Description
    | PartsDescription (Emptiable (Stacked { tag : String, value : Description }) Never)
      -- choice morph
    | ChoiceDescription (Emptiable (Stacked Description) Never)
    | VariantsDescription (Emptiable (Stacked { tag : String, value : Description }) Never)


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


isDescriptive : Description -> Bool
isDescriptive =
    \description_ ->
        case description_.custom of
            Emptiable.Filled _ ->
                True

            Emptiable.Empty _ ->
                case description_.inner of
                    CustomDescription ->
                        False

                    EndDescription ->
                        True

                    SucceedDescription ->
                        False

                    OnlyDescription _ ->
                        True

                    InnerRecursiveDescription _ _ ->
                        True

                    InverseDescription inverseDescription ->
                        inverseDescription |> isDescriptive

                    WhilePossibleDescription _ ->
                        True

                    UntilDescription _ ->
                        True

                    ChainDescription elements ->
                        [ elements.broad, elements.narrow ] |> List.any isDescriptive

                    SequenceDescription elements ->
                        [ elements.early, elements.late ] |> List.any isDescriptive

                    GroupDescription descriptionParts ->
                        descriptionParts |> Stack.toList |> List.any isDescriptive

                    ElementsDescription elementDescription ->
                        elementDescription |> isDescriptive

                    PartsDescription descriptionParts ->
                        descriptionParts |> Stack.toList |> List.any (\part_ -> part_.value |> isDescriptive)

                    ChoiceDescription descriptionPossibilities ->
                        descriptionPossibilities |> Stack.toList |> List.any isDescriptive

                    VariantsDescription descriptionVariants ->
                        descriptionVariants
                            |> Stack.toList
                            |> List.any (\variant_ -> variant_.value |> isDescriptive)


{-| Create a tree describing a given [`Error`](#Error) embedded in a given [`Description`](#Description).
-}
descriptionAndErrorToTree : Description -> Maybe Error -> Tree Label
descriptionAndErrorToTree description_ maybeError =
    let
        startDownLabel : { error_ | startDownInBroadList : Int } -> Tree { kind : LabelKind, text : String }
        startDownLabel =
            \rowError ->
                Tree.singleton
                    { kind = LabelError
                    , text = [ "starting at ", rowError.startDownInBroadList |> String.fromInt, " from last" ] |> String.concat
                    }

        structureTree : Tree Label
        structureTree =
            case description_.inner of
                CustomDescription ->
                    case maybeError of
                        Nothing ->
                            Tree.singleton { kind = LabelDescriptionStructure, text = "(custom)" }

                        Just (DeadEnd deadEnd) ->
                            Tree.singleton { kind = LabelError, text = deadEnd }

                        Just other ->
                            Tree.singleton { kind = LabelError, text = other |> Debug.toString }

                EndDescription ->
                    Tree.singleton { kind = LabelDescriptionStructure, text = "end" }

                InverseDescription inverseDescription ->
                    Tree.tree { kind = LabelDescriptionStructure, text = "inverse" }
                        [ descriptionAndErrorToTree inverseDescription maybeError ]

                SucceedDescription ->
                    Tree.singleton { kind = LabelDescriptionStructure, text = "(always succeeds)" }

                OnlyDescription onlyDescription ->
                    Tree.singleton { kind = LabelDescriptionStructure, text = "only " ++ onlyDescription }

                InnerRecursiveDescription recursiveStructureName lazyDescription ->
                    case maybeError of
                        Nothing ->
                            Tree.singleton { kind = LabelDescriptionStructure, text = "recursive: " ++ recursiveStructureName }

                        Just error ->
                            descriptionAndErrorToTree
                                (lazyDescription ()
                                    |> descriptionCustomNameAlter (\s -> "recursive: " ++ s)
                                )
                                (error |> Just)

                WhilePossibleDescription elementDescription ->
                    Tree.tree { kind = LabelDescriptionStructure, text = "while possible" }
                        [ descriptionAndErrorToTree elementDescription Nothing ]

                UntilDescription untilDescription ->
                    Tree.tree { kind = LabelDescriptionStructure, text = "until" }
                        (case maybeError of
                            Just (UntilError untilError) ->
                                [ Tree.tree { kind = LabelDescriptionStructure, text = "commit" }
                                    [ descriptionAndErrorToTree untilDescription.commit
                                        (case untilError.breakError of
                                            UntilEndError _ ->
                                                Nothing

                                            UntilCommitError commitError ->
                                                commitError |> Just
                                        )
                                    ]
                                , Tree.tree
                                    { kind = LabelDescriptionStructure, text = "end" }
                                    [ descriptionAndErrorToTree untilDescription.end
                                        (case untilError.breakError of
                                            UntilCommitError _ ->
                                                Nothing

                                            UntilEndError endError ->
                                                endError |> Just
                                        )
                                    ]
                                , Tree.tree { kind = LabelDescriptionStructure, text = "element" }
                                    [ descriptionAndErrorToTree untilDescription.element (untilError.elementError |> Just)
                                    , Tree.singleton
                                        { kind = LabelError
                                        , text =
                                            "failed after "
                                                ++ (untilError.elementCount |> String.fromInt)
                                                ++ "elements"
                                        }
                                    , startDownLabel untilError
                                    ]
                                ]

                            Just otherError ->
                                [ Tree.singleton { kind = LabelError, text = "unexpected error kind: " ++ (otherError |> Debug.toString) } ]

                            Nothing ->
                                [ Tree.tree { kind = LabelDescriptionStructure, text = "end" }
                                    [ descriptionAndErrorToTree untilDescription.end Nothing ]
                                , Tree.tree { kind = LabelDescriptionStructure, text = "element" }
                                    [ descriptionAndErrorToTree untilDescription.element Nothing ]
                                ]
                        )

                SequenceDescription elementDescriptions ->
                    let
                        maybeInStructureError :
                            Maybe
                                { place : InSequencePlace
                                , error : Error
                                , startDownInBroadList : Int
                                }
                        maybeInStructureError =
                            case maybeError of
                                Nothing ->
                                    Nothing

                                Just (SequenceError inSequence) ->
                                    Just inSequence

                                Just _ ->
                                    let
                                        _ =
                                            Debug.log "expected SequenceError but found" maybeError
                                    in
                                    Nothing

                        informativeElements =
                            [ ( InSequenceEarly, elementDescriptions.early )
                            , ( InSequenceLate, elementDescriptions.late )
                            ]
                                |> List.filterMap
                                    (\( place, elementDescription ) ->
                                        case maybeInStructureError |> maybeWhen (\inSequence -> place == inSequence.place) of
                                            Just sequenceError ->
                                                descriptionAndErrorToTree elementDescription
                                                    (sequenceError.error |> Just)
                                                    |> Just

                                            Nothing ->
                                                if elementDescription |> isDescriptive then
                                                    descriptionAndErrorToTree elementDescription Nothing |> Just

                                                else
                                                    Nothing
                                    )
                    in
                    case informativeElements of
                        [] ->
                            Tree.singleton { kind = LabelDescriptionStructure, text = "(empty sequence)" }

                        onlyElement :: [] ->
                            onlyElement

                        element0 :: element1 :: elements2Up ->
                            Tree.tree { kind = LabelDescriptionStructure, text = "sequence" }
                                (element0 :: element1 :: elements2Up)

                ChainDescription elementDescriptions ->
                    let
                        maybeInStructureError : Maybe { place : InChainPlace, error : Error }
                        maybeInStructureError =
                            case maybeError of
                                Nothing ->
                                    Nothing

                                Just (ChainError inChain) ->
                                    let
                                        _ =
                                            Debug.log "located in chain" maybeError
                                    in
                                    Just inChain

                                Just _ ->
                                    let
                                        _ =
                                            Debug.log "expected ChainError but found" maybeError
                                    in
                                    Nothing

                        informativeElements =
                            [ ( InChainBroad, elementDescriptions.broad )
                            , ( InChainNarrow, elementDescriptions.narrow )
                            ]
                                |> List.filterMap
                                    (\( index, elementDescription ) ->
                                        case maybeInStructureError |> maybeWhen (\inSequence -> index == inSequence.place) of
                                            Just sequenceError ->
                                                descriptionAndErrorToTree elementDescription
                                                    (sequenceError.error |> Just)
                                                    |> Just

                                            Nothing ->
                                                if elementDescription |> isDescriptive then
                                                    descriptionAndErrorToTree elementDescription Nothing |> Just

                                                else
                                                    Nothing
                                    )
                    in
                    case informativeElements of
                        [] ->
                            Tree.singleton { kind = LabelDescriptionStructure, text = "(empty chain)" }

                        onlyElement :: [] ->
                            onlyElement

                        element0 :: element1 :: elements2Up ->
                            Tree.tree { kind = LabelDescriptionStructure, text = "chained" }
                                (element0 :: element1 :: elements2Up)

                ChoiceDescription possibilities ->
                    let
                        maybeChoiceError =
                            case maybeError of
                                Nothing ->
                                    Nothing

                                Just (ChoiceError choiceError) ->
                                    Just choiceError

                                Just _ ->
                                    let
                                        _ =
                                            Debug.log "expected ChoiceError but found" maybeError
                                    in
                                    Nothing
                    in
                    Tree.tree { kind = LabelDescriptionStructure, text = "choice between" }
                        (case maybeChoiceError of
                            Nothing ->
                                possibilities
                                    |> Stack.toList
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
                                    (possibilities |> Stack.toList)
                                    (choiceError |> Stack.toList)
                        )

                GroupDescription elements ->
                    let
                        maybeGroupError =
                            case maybeError of
                                Nothing ->
                                    Nothing

                                Just (GroupError groupError) ->
                                    let
                                        _ =
                                            Debug.log "located in group" maybeError
                                    in
                                    Just groupError

                                Just _ ->
                                    let
                                        _ =
                                            Debug.log "expected GroupError but found" maybeError
                                    in
                                    Nothing
                    in
                    Tree.tree { kind = LabelDescriptionStructure, text = "both" }
                        (case maybeGroupError of
                            Nothing ->
                                elements
                                    |> Stack.toList
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
                                    (List.range 0 ((elements |> Stack.length) - 1))
                                    (elements |> Stack.toList)
                                    (groupError |> Stack.toList)
                        )

                ElementsDescription elementDescription ->
                    case maybeError of
                        Just (ElementsError inCollectionError) ->
                            Tree.tree { kind = LabelDescriptionStructure, text = "elements" }
                                (inCollectionError
                                    |> Stack.toList
                                    |> List.concatMap
                                        (\elementError ->
                                            [ descriptionAndErrorToTree elementDescription (elementError.error |> Just)
                                            , Tree.singleton { kind = LabelError, text = "at " ++ elementError.location }
                                            ]
                                        )
                                )

                        _ ->
                            Tree.singleton { kind = LabelError, text = "expected InElementsError but found " ++ Debug.toString maybeError }

                PartsDescription partsDescription ->
                    let
                        maybePartError =
                            case maybeError of
                                Nothing ->
                                    Nothing

                                Just (GroupError groupError) ->
                                    Just (groupError |> Stack.top)

                                Just _ ->
                                    let
                                        _ =
                                            Debug.log "expected GroupError but found" maybeError
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
                            case maybeError of
                                Nothing ->
                                    Nothing

                                Just (VariantError variantError) ->
                                    Just variantError

                                Just _ ->
                                    let
                                        _ =
                                            Debug.log "expected VariantError but found" maybeError
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
    in
    treeWithCustomDescriptionNest description_.custom structureTree


descriptionCustomNameAlter : (String -> String) -> (Description -> Description)
descriptionCustomNameAlter nameAlter =
    \description_ ->
        { description_
            | custom = description_.custom |> Stack.topAlter nameAlter
        }


treeWithCustomDescriptionNest : Emptiable (Stacked String) Possibly -> Tree Label -> Tree Label
treeWithCustomDescriptionNest labelsToNest bottom =
    case labelsToNest of
        Emptiable.Empty _ ->
            bottom

        Emptiable.Filled (Stack.TopBelow ( line0Custom, lines1UpCustom )) ->
            Tree.tree { kind = LabelDescriptionCustom, text = line0Custom }
                [ treeWithCustomDescriptionNest (Stack.fromList lines1UpCustom) bottom ]


collapseToDescriptive :
    (DescriptionInner -> Maybe ( Description, Description ))
    -> (DescriptionInner -> List Description)
collapseToDescriptive trySplit =
    \descriptionInner ->
        descriptionInner
            |> collapse trySplit
            |> List.filter isDescriptive


collapse :
    (DescriptionInner -> Maybe ( Description, Description ))
    -> (DescriptionInner -> List Description)
collapse trySplit =
    \descriptionInner ->
        case descriptionInner |> trySplit of
            Nothing ->
                []

            Just ( head, tailBeforeCollapse ) ->
                case tailBeforeCollapse.custom of
                    Emptiable.Filled _ ->
                        [ head, tailBeforeCollapse ]

                    Emptiable.Empty _ ->
                        head :: (tailBeforeCollapse.inner |> collapse trySplit)


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
    | UntilError (UntilError (ErrorWithDeadEnd deadEnd))
    | SequenceError
        { place : InSequencePlace
        , error : ErrorWithDeadEnd deadEnd
        , startDownInBroadList : Int
        }
    | ChainError { place : InChainPlace, error : ErrorWithDeadEnd deadEnd }
    | ElementsError
        (Emptiable
            (Stacked { location : String, error : ErrorWithDeadEnd deadEnd })
            Never
        )
    | GroupError (GroupError (ErrorWithDeadEnd deadEnd))
    | VariantError { index : Int, error : ErrorWithDeadEnd deadEnd }
    | ChoiceError (Emptiable (Stacked (ErrorWithDeadEnd deadEnd)) Never)


{-| Earlier or later in the sequence?
-}
type InSequencePlace
    = InSequenceEarly
    | InSequenceLate


{-| The more narrow or broad morph in an "over" chain?
-}
type InChainPlace
    = InChainBroad
    | InChainNarrow


type alias UntilError partError =
    RecordWithoutConstructorFunction
        { breakError : UntilBreakError partError
        , elementError : partError
        , startDownInBroadList : Int
        , elementCount : Int
        }


type UntilBreakError error
    = -- end parsed successfully but commit failed
      UntilCommitError error
    | -- end element failed to parse
      UntilEndError error


{-| A group's part [`Error`](#Error)s, each with their part index
-}
type alias GroupError partError =
    Emptiable
        (Stacked { index : Int, error : partError })
        Never


{-| Describe what you want to narrow to.
This will make errors and descriptions easier to understand.

A good rule of thumb is to add a [`Morph.named`](#named) to every morph _declaration_
or even more often.

    import Morph.Error
    import Char.Morph as Char
    import String.Morph as Text

    -- we can redefine an error message if something goes wrong
    "123"
        |> Text.toNarrow
            (Morph.named "variable name"
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
        Morph.named "point"
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

Especially for [`translate`](#translate) etc,
adding a description doesn't really add value
as users often don't need to know that you for example converted a [stack to a list](Stack-Morph#toList)

-}
named :
    String
    ->
        (MorphIndependently narrow broaden
         -> MorphIndependently narrow broaden
        )
named expectationCustomDescription morphToDescribe =
    { morphToDescribe
        | description =
            { inner = morphToDescribe.description.inner
            , custom =
                morphToDescribe.description.custom
                    |> Stack.onTopLay expectationCustomDescription
            }
    }



--


{-| The morph's [`Description`](#Description).

Add custom ones via [`Morph.named`](#named)

-}
description : MorphIndependently narrow_ broaden_ -> Description
description =
    .description


{-| Its transformation that turns `narrow` into `broad`.
Some call it "build"
-}
toBroad : MorphIndependently narrow_ broaden -> broaden
toBroad =
    .toBroad


{-| Its transformation that turns `broad` into `narrow` or an `error`.
Some call it "parse"
-}
toNarrow : MorphIndependently narrow broaden_ -> narrow
toNarrow =
    .toNarrow


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
    custom descriptionCustom
        { toNarrow = narrowConvert
        , toBroad = identity
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

  - [`toBroad`](#toBroad)`identity`
  - [`translate`](#translate)`identity identity`
  - [`toggle`](#toggle)`identity` when broad and narrow types match
  - [`validate`](#validate)`Ok`
  - `custom ... { toBroad = identity, toNarrow = Ok }`

-}
keep :
    MorphIndependently
        (narrow -> Result error_ narrow)
        (broad -> broad)
keep =
    translate identity identity


{-| Create a [`Translate`](#Translate)

    String.Morph.toList : MorphOrError (List Char) String error_
    String.Morph.toList =
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
    , toNarrow = \beforeMap -> beforeMap |> map |> Ok
    , toBroad = unmap
    }


{-| Only broadens (unmaps), doesn't narrow.
What comes out as the broad result will be transformed.

What is great is using this to make inputs more "user-usable":

    ArraySized.Morph.maxToInfinity :
        MorphIndependently
            (narrow -> Result error_ narrow)
            (ArraySized element (In (On min) max_)
             -> ArraySized element (In (On min) Infinity)
            )
    ArraySized.Morph.maxToInfinity =
        Morph.toBroad ArraySized.maxToInfinity

However! This can also often be an anti-pattern. See [`validate`](#validate).

    "WOW"
        |> Morph.toBroad
            (Morph.toBroadOnly String.toLower
                |> Morph.over stringValidation
            )
    --→ "wow"

The fact that the name "only" already exists [in a different context](#only) is unfortunate,
suggestions welcome!

-}
toBroadOnly :
    (beforeToBroad -> broad)
    ->
        MorphIndependently
            (narrow -> Result error_ narrow)
            (beforeToBroad -> broad)
toBroadOnly broadenFromNarrow =
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
    , toNarrow =
        \broadValue ->
            if broadValue == broadConstant then
                () |> Ok

            else
                broadValue
                    |> broadConstantToString
                    |> DeadEnd
                    |> Err
    , toBroad = \() -> broadConstant
    }


{-| Create a custom morph for a value by explicitly specifying

  - a `String` description
  - `toNarrow`: a transformation that can fail with any error consistent with your other errors (so most likely a string)
  - `toBroad`: a transformation that can build the parsed value back to what a value that can be parsed

-}
custom :
    String
    ->
        { toNarrow : beforeToNarrow -> Result deadEnd narrow
        , toBroad : beforeToBroad -> broad
        }
    ->
        MorphIndependently
            (beforeToNarrow -> Result (ErrorWithDeadEnd deadEnd) narrow)
            (beforeToBroad -> broad)
custom descriptionCustom morphTransformations =
    { description =
        { custom = Stack.one descriptionCustom
        , inner = CustomDescription
        }
    , toNarrow =
        \beforeToNarrow ->
            beforeToNarrow
                |> morphTransformations.toNarrow
                |> Result.mapError DeadEnd
    , toBroad = morphTransformations.toBroad
    }



--


{-| Define a [`Morph`](#Morph) recursively

    import Morph exposing (grab, match, one)
    import Integer exposing (Integer)
    import Integer.Morph
    import String.Morph

    type IntList
        = End
        | Next { head : Integer, tail : IntList }

    intList : MorphRow IntList
    intList =
        Morph.recursive "int list"
            (\innerIntList ->
                Morph.choice
                    (\endVariant nextVariant intListChoice ->
                        case intListChoice of
                            End ->
                                endVariant ()
                            Next next ->
                                nextVariant next
                    )
                    |> Morph.tryRow (\() -> End) (String.Morph.only "[]")
                    |> Morph.tryRow Next
                        (Morph.succeed (\h t -> { head = h, tail = t })
                            |> grab .head Integer.Morph.rowChar
                            |> match
                                (broad (ArraySized.one ())
                                    |> Morph.overRow
                                        (atLeast n1 (String.Morph.only " "))
                                )
                            |> match (String.Morph.only "::")
                            |> match
                                (broad (ArraySized.one ())
                                    |> Morph.overRow
                                        (atLeast n1 (String.Morph.only " "))
                                )
                            |> grab .tail innerIntList
                        )
            )

    "[]" |> Text.toNarrow intList
    --> Ok End

    "a :: []" |> Text.toNarrow intList
    --> Ok (Next { head = 'a', tail = End })

    "a :: b :: []" |> Text.toNarrow intList
    --> Ok (Next { head = 'a', tail = Next { head = 'b', tail = End })

Without `recursive`, you would get an error like:

>     The `intList` definition is causing a very tricky infinite loop.
>
>     The `intList` value depends on itself

Read more about why this limitation exists
in [compiler hint "bad recursion"](https://github.com/elm/compiler/blob/master/hints/bad-recursion.md#tricky-recursion)
up until the end

Note: in this example you can also simply use [`Morph.before`](#before)

More notes:

  - This would compile:

        intList : () -> MorphRow IntList
        intList =
            Morph.choice ...
                |> Morph.tryRow (\() -> End) ...
                |> Morph.tryRow Next
                    (...
                        |> grab .tail (intList ())
                    )

    This makes the compiler happy, but once we call `intList ()` somewhere in our code,
    the Morph expands itself infinitely leading to a compiler crash 😱

    > RangeError: Maximum call stack size exceeded

  - Other packages like json decoders solve this problem by introducing `lazy`:

        lazy :
            (() -> MorphIndependently toNarrow toBroad)
            -> MorphIndependently toNarrow toBroad
        lazy morphLazy =
            { description = morphLazy () |> description
            , toNarrow = toNarrow (morphLazy ())
            , toBroad = toBroad (morphLazy ())
            }

    This one doesn't crash when we call `intList` or `lazy (\() -> intList)`

    The only reason this really doesn't work with `Morph`s is that we'll get an infinitely nested
    [`description`](#description). Using [`Morph.recursive`](#recursive),
    each inner recursion step just refers back to the outer one.

-}
recursive :
    String
    ->
        (MorphIndependently toNarrow toBroad
         -> MorphIndependently toNarrow toBroad
        )
    -> MorphIndependently toNarrow toBroad
recursive structureName morphLazy =
    let
        innerRecursive : () -> MorphIndependently toNarrow toBroad
        innerRecursive () =
            recursive structureName
                (\step ->
                    { description =
                        { inner =
                            InnerRecursiveDescription structureName
                                (\() -> morphLazy (innerRecursive ()) |> description)
                        , custom = Emptiable.empty
                        }
                    , toNarrow = toNarrow step
                    , toBroad = toBroad step
                    }
                )

        recursiveMorph : MorphIndependently toNarrow toBroad
        recursiveMorph =
            morphLazy (innerRecursive ())
    in
    { description =
        { inner = recursiveMorph |> description |> .inner
        , custom =
            recursiveMorph
                |> description
                |> .custom
                |> Stack.onTopLay structureName
        }
    , toNarrow = toNarrow recursiveMorph
    , toBroad = toBroad recursiveMorph
    }


{-| [`Morph`](#Morph) on groups in progress.
Start with [`group`](#group), complete with [`part`](#part), finally [`partsFinish`](#partsFinish)
-}
type alias PartsMorphEmptiable noPartPossiblyOrNever narrow broaden =
    RecordWithoutConstructorFunction
        { description :
            -- parts
            Emptiable (Stacked { tag : String, value : Description }) noPartPossiblyOrNever
        , toNarrow : narrow
        , toBroad : broaden
        }


{-| Assemble a group from narrow and broad [`part`](#part)s

Use [`group`](#group)
when each broad, toNarrow [`part`](#part) always has their respective counterpart

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
    , toNarrow = \_ -> narrowAssemble |> Ok
    , toBroad = \_ -> broadAssemble
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
                    (GroupError partError)
                    (partNarrow -> groupNarrowFurther)
            )
            (groupNarrow -> (partBroad -> groupBroadenFurther))
         ->
            PartsMorphEmptiable
                noPartNever_
                (groupBroad
                 ->
                    Result
                        (GroupError partError)
                        groupNarrowFurther
                )
                (groupNarrow -> groupBroadenFurther)
        )
part partTagName ( narrowPartAccess, broadPartAccess ) partMorph =
    \groupMorphSoFar ->
        { description =
            groupMorphSoFar.description
                |> Stack.onTopLay { tag = partTagName, value = partMorph.description }
        , toNarrow =
            .toNarrow groupMorphSoFar
                |> narrowPart
                    (groupMorphSoFar.description |> Stack.length)
                    broadPartAccess
                    (toNarrow partMorph)
        , toBroad =
            .toBroad groupMorphSoFar
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
                (GroupError partError)
                (partNarrow -> groupNarrowFurther)
         )
         ->
            (groupBroad
             ->
                Result
                    (GroupError partError)
                    groupNarrowFurther
            )
        )
narrowPart index broadPartAccess narrowPartMorph =
    \groupMorphSoFarNarrow ->
        \groupBroad ->
            case groupBroad |> broadPartAccess |> narrowPartMorph of
                Err partError ->
                    case groupBroad |> groupMorphSoFarNarrow of
                        Ok _ ->
                            { index = index, error = partError }
                                |> Stack.one
                                |> Err

                        Err partsSoFarError ->
                            partsSoFarError
                                |> Stack.onTopLay { index = index, error = partError }
                                |> Err

                Ok partNarrow ->
                    groupBroad
                        |> groupMorphSoFarNarrow
                        |> Result.map (\eat -> eat partNarrow)


{-| Conclude a [`Group.build`](#group) |> [`Group.part`](#part) chain
-}
partsFinish :
    PartsMorphEmptiable
        Never
        (beforeToNarrow
         ->
            Result
                (GroupError (ErrorWithDeadEnd deadEnd))
                narrow
        )
        (beforeToBroad -> broad)
    ->
        MorphIndependently
            (beforeToNarrow
             -> Result (ErrorWithDeadEnd deadEnd) narrow
            )
            (beforeToBroad -> broad)
partsFinish =
    \groupMorphInProgress ->
        { description =
            { custom = Emptiable.empty
            , inner = groupMorphInProgress.description |> PartsDescription
            }
        , toNarrow =
            \broad_ ->
                broad_
                    |> groupMorphInProgress.toNarrow
                    |> Result.mapError GroupError
        , toBroad = groupMorphInProgress.toBroad
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
        (beforeBeforeNarrow -> Result (ErrorWithDeadEnd deadEnd) beforeToNarrow)
        (beforeToBroad -> broad)
    ->
        (MorphIndependently
            (beforeToNarrow -> Result (ErrorWithDeadEnd deadEnd) narrow)
            (beforeBeforeBroaden -> beforeToBroad)
         ->
            MorphIndependently
                (beforeBeforeNarrow -> Result (ErrorWithDeadEnd deadEnd) narrow)
                (beforeBeforeBroaden -> broad)
        )
over morphBroad =
    \narrowMorph ->
        { description =
            { inner =
                ChainDescription
                    { broad = morphBroad |> description
                    , narrow = narrowMorph |> description
                    }
            , custom = Emptiable.empty
            }
        , toBroad =
            \beforeToBroad ->
                beforeToBroad
                    |> toBroad narrowMorph
                    |> toBroad morphBroad
        , toNarrow =
            \beforeToNarrow ->
                beforeToNarrow
                    |> toNarrow morphBroad
                    |> Result.mapError (\error -> ChainError { place = InChainBroad, error = error })
                    |> Result.andThen
                        (\beforeNarrowNarrow ->
                            beforeNarrowNarrow
                                |> toNarrow narrowMorph
                                |> Result.mapError (\error -> ChainError { place = InChainNarrow, error = error })
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
        { toNarrow =
            \beforeMap ->
                beforeMap |> toBroad translate_ |> Ok
        , toBroad = mapTo translate_
        , description =
            { inner = InverseDescription (translate_ |> description)
            , custom = Emptiable.empty
            }
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

            UntilError untilError ->
                { elementCount = untilError.elementCount
                , startDownInBroadList = untilError.startDownInBroadList
                , breakError =
                    case untilError.breakError of
                        UntilEndError untilEndError ->
                            untilEndError |> deadEndMap deadEndChange |> UntilEndError

                        UntilCommitError untilCommitError ->
                            untilCommitError |> deadEndMap deadEndChange |> UntilCommitError
                , elementError = untilError.elementError |> deadEndMap deadEndChange
                }
                    |> UntilError

            SequenceError inSequence ->
                SequenceError
                    { place = inSequence.place
                    , startDownInBroadList = inSequence.startDownInBroadList
                    , error = inSequence.error |> deadEndMap deadEndChange
                    }

            ChainError inChain ->
                ChainError
                    { place = inChain.place
                    , error = inChain.error |> deadEndMap deadEndChange
                    }

            GroupError parts_ ->
                parts_
                    |> Stack.map
                        (\_ partError ->
                            { index = partError.index
                            , error = partError.error |> deadEndMap deadEndChange
                            }
                        )
                    |> GroupError

            ElementsError inElementsError ->
                inElementsError
                    |> Stack.map
                        (\_ elementError ->
                            { location = elementError.location
                            , error = elementError.error |> deadEndMap deadEndChange
                            }
                        )
                    |> ElementsError

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


{-| An [`Error`](#ErrorWithDeadEnd) where running into a dead end is impossible.

Because each kind of error needs at least one dead end, tho, no such error can be created.
Therefore, you can treat it as _any_ value.

Under the hood, only [`Basics.never`](https://dark.elm.dmy.fr/packages/elm/core/latest/Basics#never)
is used so it's completely safe to use.

-}
deadEndNever : ErrorWithDeadEnd Never -> any_
deadEndNever =
    \error ->
        case error of
            DeadEnd deadEnd ->
                deadEnd |> never

            UntilError untilError ->
                untilError.elementError |> deadEndNever

            SequenceError inSequence ->
                inSequence.error |> deadEndNever

            ChainError inChain ->
                inChain.error |> deadEndNever

            ElementsError inElementsError ->
                inElementsError |> Stack.top |> .error |> deadEndNever

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
            (beforeToNarrow -> Result error narrowed)
            toBroad
    ->
        MorphIndependently
            (beforeToNarrow -> Result errorMapped narrowed)
            toBroad
narrowErrorMap errorChange =
    \morph ->
        { description = morph |> description
        , toBroad = toBroad morph
        , toNarrow =
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
    , toNarrow =
        \broad_ ->
            broad_
                |> structureMap (mapTo elementTranslate)
                |> Ok
    , toBroad =
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
        Morph.named "point"
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
type alias MorphRowIndependently beforeToBroad narrow broadElement =
    MorphIndependently
        (Emptiable (Stacked broadElement) Possibly
         ->
            Result
                Error
                { narrow : narrow
                , broad : Emptiable (Stacked broadElement) Possibly
                }
        )
        (beforeToBroad
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
        , toNarrow =
            \broad_ ->
                case broad_ of
                    Emptiable.Empty _ ->
                        "end of input" |> DeadEnd |> Err

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
                                error |> Err
        , toBroad =
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
    { description = { inner = SucceedDescription, custom = Emptiable.empty }
    , toNarrow =
        \broad_ ->
            { narrow = narrowConstant
            , broad = broad_
            }
                |> Ok
    , toBroad =
        \_ -> Rope.empty
    }


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
        { description =
            { custom = Emptiable.empty
            , inner =
                SequenceDescription
                    { early = groupMorphRowSoFar |> description
                    , late = nextMorphRow |> description
                    }
            }
        , toNarrow =
            \broad_ ->
                case broad_ |> toNarrow groupMorphRowSoFar of
                    Err earlyError ->
                        SequenceError
                            { place = InSequenceEarly
                            , error = earlyError
                            , startDownInBroadList = broad_ |> Stack.length
                            }
                            |> Err

                    Ok result ->
                        case result.broad |> toNarrow nextMorphRow of
                            Ok nextParsed ->
                                { narrow = result.narrow |> partChange nextParsed.narrow
                                , broad = nextParsed.broad
                                }
                                    |> Ok

                            Err lateError ->
                                SequenceError
                                    { place = InSequenceLate
                                    , error = lateError
                                    , startDownInBroadList = result.broad |> Stack.length
                                    }
                                    |> Err
        , toBroad =
            \groupNarrow ->
                groupNarrow
                    |> partAccess
                    |> toBroad nextMorphRow
                    |> Rope.appendTo
                        (groupNarrow |> toBroad groupMorphRowSoFar)
        }


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
    MorphRowIndependently beforeToBroad beforeToNarrow broadElement
    ->
        (MorphIndependently
            (beforeToNarrow -> Result Error narrow)
            (beforeBeforeBroaden -> beforeToBroad)
         -> MorphRowIndependently beforeBeforeBroaden narrow broadElement
        )
overRow morphRowBeforeMorph =
    \narrowMorph ->
        { description =
            { custom = Emptiable.empty
            , inner =
                ChainDescription
                    { broad = morphRowBeforeMorph |> description
                    , narrow = narrowMorph |> description
                    }
            }
        , toNarrow =
            \broad_ ->
                case broad_ |> toNarrow morphRowBeforeMorph of
                    Err beforeError ->
                        ChainError { place = InChainBroad, error = beforeError } |> Err

                    Ok beforeToNarrow ->
                        case beforeToNarrow.narrow |> toNarrow narrowMorph of
                            Err error ->
                                ChainError { place = InChainNarrow, error = error } |> Err

                            Ok ok ->
                                { narrow = ok
                                , broad = beforeToNarrow.broad
                                }
                                    |> Ok
        , toBroad =
            \beforeToNarrow ->
                beforeToNarrow
                    |> toBroad narrowMorph
                    |> toBroad morphRowBeforeMorph
        }



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
    , element : MorphRow goOnElement broadElement
    }
    -> MorphRow (Emptiable (Stacked goOnElement) Possibly) broadElement
before untilStep =
    until
        { commit =
            translate .before
                (\before_ -> { before = before_, end = () })
        , end = untilStep.end
        , element = untilStep.element
        }


untilAsFold :
    { commit :
        Morph
            commitResult
            { end : endElement
            , before : Emptiable folded Possibly
            }
    , element :
        Emptiable folded Possibly -> MorphRow element broadElement
    , end : MorphRow endElement broadElement
    , fold : Translate folded ( element, Emptiable folded Possibly )
    }
    -> MorphRow commitResult broadElement
untilAsFold config =
    { description =
        { inner =
            UntilDescription
                { commit = config.commit |> description
                , element = config.element Emptiable.empty |> description
                , end = config.commit |> description
                }
        , custom = Emptiable.empty
        }
    , toBroad =
        let
            step :
                { previous : Emptiable folded Possibly
                , rest : Emptiable folded Possibly
                }
                -> Rope broadElement
            step state =
                case state.rest of
                    Emptiable.Empty _ ->
                        Rope.empty

                    Emptiable.Filled folded ->
                        let
                            ( element, rest ) =
                                folded |> toBroad config.fold

                            previousWithElement =
                                ( element, state.previous ) |> mapTo config.fold
                        in
                        step { previous = previousWithElement |> Emptiable.filled, rest = rest }
                            |> Rope.prependTo
                                (element
                                    |> toBroad
                                        (config.element
                                            (previousWithElement |> Emptiable.filled)
                                        )
                                )
        in
        \beforeToBroad ->
            let
                uncommitted =
                    beforeToBroad |> toBroad config.commit
            in
            step { previous = Emptiable.empty, rest = uncommitted.before }
                |> Rope.prependTo
                    (uncommitted.end
                        |> toBroad config.end
                    )
    , toNarrow =
        let
            stepFrom :
                Emptiable folded Possibly
                ->
                    (Emptiable (Stacked broadElement) Possibly
                     ->
                        Result
                            (UntilError Error)
                            { broad : Emptiable (Stacked broadElement) Possibly
                            , narrow : commitResult
                            }
                    )
            stepFrom state =
                \beforeToNarrow ->
                    let
                        continue element =
                            let
                                foldedWithStepped =
                                    case state of
                                        Emptiable.Empty _ ->
                                            ( element.narrow, Emptiable.empty ) |> mapTo config.fold

                                        Emptiable.Filled folded ->
                                            ( element.narrow, folded |> Emptiable.filled ) |> mapTo config.fold
                            in
                            case element.broad |> stepFrom (foldedWithStepped |> Emptiable.filled) of
                                Err error ->
                                    { error | elementCount = error.elementCount + 1 }
                                        |> Err

                                Ok result ->
                                    result |> Ok
                    in
                    case beforeToNarrow |> toNarrow config.end of
                        Err endError ->
                            case beforeToNarrow |> toNarrow (config.element state) of
                                Err elementError ->
                                    { elementCount = 0
                                    , elementError = elementError
                                    , breakError = UntilEndError endError
                                    , startDownInBroadList = beforeToNarrow |> Stack.length
                                    }
                                        |> Err

                                Ok element ->
                                    continue element

                        Ok endElement ->
                            case
                                { end = endElement.narrow, before = state }
                                    |> toNarrow config.commit
                            of
                                Ok committed ->
                                    { narrow = committed, broad = endElement.broad } |> Ok

                                Err commitError ->
                                    case beforeToNarrow |> toNarrow (config.element state) of
                                        Err elementError ->
                                            { elementCount = 0
                                            , elementError = elementError
                                            , breakError = UntilCommitError commitError
                                            , startDownInBroadList = beforeToNarrow |> Stack.length
                                            }
                                                |> Err

                                        Ok element ->
                                            continue element
        in
        \beforeToNarrow ->
            beforeToNarrow
                |> stepFrom Emptiable.empty
                |> Result.mapError UntilError
    }


{-| How could one define `atMost 3 elements`, `until "Decoder" ...`, ...?

    decoderNameSubject : MorphRow String Char
    decoderNameSubject =
        Stack.string
            |> Morph.over
                (MorphRow.until
                    { end =
                        Morph.succeed ()
                            |> match (String.Morph.only "Decoder")
                            |> match Morph.end
                    , element = Morph.keep |> Morph.one
                    , commit =
                        translate .before
                            (\before -> { before = before, end = () })
                    }
                )

↑ can be simplified with [`before`](#before)

_Any kind of structure check that if it fails should proceed to go on parsing `element`s
must be in `commit`_

-}
until :
    { commit :
        Morph
            commitResult
            { end : endElement
            , before : Emptiable (Stacked element) Possibly
            }
    , end : MorphRow endElement broadElement
    , element : MorphRow element broadElement
    }
    -> MorphRow commitResult broadElement
until config =
    untilAsFold
        { commit =
            config.commit
                |> over
                    (translate
                        (\beforeAndEnd ->
                            { beforeAndEnd | before = beforeAndEnd.before |> Stack.reverse }
                        )
                        identity
                    )
        , element = \_ -> config.element
        , end = config.end
        , fold =
            translate (\( top, below ) -> below |> Stack.onTopLay top |> Emptiable.fill)
                (\stacked ->
                    let
                        stackFilled =
                            stacked |> Emptiable.filled
                    in
                    ( stackFilled |> Stack.top, stackFilled |> Stack.removeTop )
                )
        }


{-| Keep on parsing `element`s until you encounter an `end` element that doesn't fail `commit`ting.
This behavior is just like [`until`](#until).

In addition, [`untilFold`](#untilFold) carries accumulated "status" information
to the next element morph where you can decide how to proceed.

If that sounds complicated, then you don't need it.
Sadly some formats like midi want to save space by making you remember stuff about past events.

The fact that a morph for this case exist is pretty neat.
But because a few functions are involved,
its [description](#description) can be less nice than you're used to from other structures.
Maybe add some more context via [`Morph.named`](#named) to the whole thing and or the `commit` morph :)

If you want to use this but feel like you need
the accumulated state in `commit` and or `end`, please open an issue :).
Implementing it is straightforward but it would clutter the API a bit.

-}
untilFold :
    { commit :
        Morph
            commitResult
            { end : endElement
            , before : Emptiable (Stacked element) Possibly
            }
    , end : MorphRow endElement broadElement
    , element : folded -> MorphRow element broadElement
    , initial : folded
    , fold : element -> (folded -> folded)
    }
    -> MorphRow commitResult broadElement
untilFold config =
    untilAsFold
        { commit =
            config.commit
                |> over
                    (translate
                        (\beforeAndEnd ->
                            { end = beforeAndEnd.end
                            , before = beforeAndEnd.before |> Emptiable.map .stack |> Stack.reverse
                            }
                        )
                        (\beforeAndEnd ->
                            { before =
                                beforeAndEnd.before
                                    |> Emptiable.map (\before_ -> { stack = before_, folded = config.initial })
                            , end = beforeAndEnd.end
                            }
                        )
                    )
        , element =
            \state ->
                case state of
                    Emptiable.Empty _ ->
                        config.initial |> config.element

                    Emptiable.Filled folded ->
                        folded.folded |> config.element
        , end = config.end
        , fold =
            translate
                (\( top, state ) ->
                    case state of
                        Emptiable.Empty _ ->
                            { folded = config.initial
                            , stack = Stack.one top |> Emptiable.fill
                            }

                        Emptiable.Filled folded ->
                            { folded = folded.folded |> config.fold top
                            , stack = folded.stack |> Emptiable.filled |> Stack.onTopLay top |> Emptiable.fill
                            }
                )
                (\state ->
                    let
                        stackFilled =
                            state.stack |> Emptiable.filled
                    in
                    ( stackFilled |> Stack.top
                    , stackFilled
                        |> Stack.removeTop
                        |> Emptiable.map
                            (\stacked ->
                                { folded = state.folded |> config.fold (stackFilled |> Stack.top)
                                , stack = stacked
                                }
                            )
                    )
                )
        }


{-| Keep going until an element fails, just like [`atLeast n0`](ArraySized-Morph#atLeast).
In addition, [`whilePossibleFold`](#whilePossibleFold) carries accumulated status information
to the next element morph where you can decide how to proceed.

If that sounds complicated, then you don't need it.
Sadly some formats like midi want to save space by making you remember stuff about past events.

The fact that a morph for this case exist is pretty neat.
But because a few functions are involved,
its [description](#description) can be less nice than you're used to from other structures.
Maybe add some more context via [`Morph.named`](#named) :)

-}
whilePossibleFold :
    { element : folded -> MorphRow element broadElement
    , fold : element -> (folded -> folded)
    , initial : folded
    }
    -> MorphRow (Emptiable (Stacked element) Possibly) broadElement
whilePossibleFold config =
    translate
        (\state ->
            state
                |> Emptiable.mapFlat
                    (\folded ->
                        folded
                            |> .stack
                            |> Emptiable.emptyAdapt (\_ -> Possible)
                            |> Stack.reverse
                    )
        )
        (\stack ->
            stack
                |> Emptiable.map
                    (\stacked ->
                        { stack = stacked |> Emptiable.filled
                        , status = config.initial
                        }
                    )
        )
        |> overRow
            (whilePossibleAsFold
                { element =
                    \state ->
                        state
                            |> Emptiable.map .status
                            |> Emptiable.fillElseOnEmpty (\_ -> config.initial)
                            |> config.element
                , fold =
                    translate
                        (\( top, below ) ->
                            case below of
                                Emptiable.Empty _ ->
                                    { status = config.initial |> config.fold top
                                    , stack = top |> Stack.one
                                    }

                                Emptiable.Filled belowFolded ->
                                    { status = belowFolded.status |> config.fold top
                                    , stack = belowFolded.stack |> Stack.onTopLay top
                                    }
                        )
                        (\state ->
                            ( state.stack |> Stack.top
                            , case state.stack |> Stack.removeTop of
                                Emptiable.Empty _ ->
                                    Emptiable.empty

                                Emptiable.Filled belowStacked ->
                                    { status = state.status |> config.fold (state.stack |> Stack.top)
                                    , stack = belowStacked |> Emptiable.filled
                                    }
                                        |> Emptiable.filled
                            )
                        )
                }
            )


whilePossibleAsFold :
    { element : Emptiable folded Possibly -> MorphRow element broadElement
    , fold : Translate folded ( element, Emptiable folded Possibly )
    }
    -> MorphRow (Emptiable folded Possibly) broadElement
whilePossibleAsFold config =
    { description =
        { inner =
            WhilePossibleDescription
                (config.element Emptiable.empty |> description)
        , custom = Emptiable.empty
        }
    , toBroad =
        let
            step :
                { previous : Emptiable folded Possibly
                , rest : Emptiable folded Possibly
                }
                -> Rope broadElement
            step state =
                case state.rest of
                    Emptiable.Empty _ ->
                        Rope.empty

                    Emptiable.Filled folded ->
                        let
                            ( element, rest ) =
                                folded |> toBroad config.fold

                            previousWithElement =
                                ( element, state.previous ) |> mapTo config.fold
                        in
                        step { previous = previousWithElement |> Emptiable.filled, rest = rest }
                            |> Rope.prependTo
                                (element
                                    |> toBroad
                                        (config.element
                                            (previousWithElement |> Emptiable.filled)
                                        )
                                )
        in
        \beforeToBroad ->
            step { previous = Emptiable.empty, rest = beforeToBroad }
    , toNarrow =
        let
            stepFrom :
                Emptiable folded Possibly
                ->
                    (Emptiable (Stacked broadElement) Possibly
                     ->
                        { broad : Emptiable (Stacked broadElement) Possibly
                        , narrow : Emptiable folded Possibly
                        }
                    )
            stepFrom previousElement =
                \beforeToNarrow ->
                    case beforeToNarrow |> toNarrow (config.element previousElement) of
                        Err _ ->
                            { broad = beforeToNarrow, narrow = Emptiable.empty }

                        Ok stepped ->
                            let
                                foldedWithStepped =
                                    case previousElement of
                                        Emptiable.Empty _ ->
                                            ( stepped.narrow, Emptiable.empty ) |> mapTo config.fold

                                        Emptiable.Filled folded ->
                                            ( stepped.narrow, folded |> Emptiable.filled ) |> mapTo config.fold
                            in
                            stepped.broad |> stepFrom (foldedWithStepped |> Emptiable.filled)
        in
        \beforeToNarrow ->
            beforeToNarrow
                |> stepFrom Emptiable.empty
                |> Ok
    }



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
    , toNarrow =
        \broad_ ->
            case broad_ of
                Emptiable.Empty _ ->
                    { narrow = ()
                    , broad = Emptiable.empty
                    }
                        |> Ok

                Emptiable.Filled _ ->
                    "remaining input" |> DeadEnd |> Err
    , toBroad =
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
        , toNarrow =
            \broadElements ->
                broadElements
                    |> toNarrow morphRow
                    |> Result.andThen
                        (\result ->
                            result.broad
                                |> toNarrow end
                                |> Result.map (\_ -> result.narrow)
                        )
        , toBroad =
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
        , toNarrow :
            choiceBeforeNarrow
            ->
                Result
                    (-- tries
                     Emptiable (Stacked error) noTryPossiblyOrNever
                    )
                    choiceNarrow
        , toBroad : choiceBroaden
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
        Morph.named "blank"
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
            |> Morph.choiceFinish

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
    , toNarrow =
        \_ ->
            Emptiable.empty |> Err
    , toBroad = choiceBroadenDiscriminatedByPossibility
    }


{-| Offer multiple [`Morph`](Morph#Morph) possibilities based on a given list of elements.
Within the possibilities tried earlier and later,
the one labelled `broad` will be preferred when calling [`Morph.toBroad`](#toBroad)

Usually, you'll be better off with a [`Morph.choice`](#choice)
for an explicit tagged union `type`
because you'll have the option to preserve what was [narrowed](Morph#toNarrow).
(Remember: you can always discard that info and set a preferred option with [`Morph.broad`](Morph#broad))

Use [`choiceEquivalent`](#choiceEquivalent) if you have a "dynamic" list of aliases/morphs to treat equally.
An example is defined variable names

    -- use it with plain Morphs
    Morph.choiceEquivalent Char.Morph.only
        { tryEarly = []
        , broad = '∨'
        , tryLate = [ '⋁', '|', 'ᚖ' ]
        }

    -- use it with MorphRows
    Morph.choiceEquivalent String.Morph.only
        { tryEarly = []
        , broad = "∨"
        , alternatives = [ "||", "|", "or" ]
        }

    Morph.choiceEquivalent String.Morph.only
        { tryEarly = []
        , broad = = "±"
        , tryLate = [ "pm", "plusminus" ]
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
            (beforeToNarrow
             -> Result (ErrorWithDeadEnd deadEnd) narrow
            )
            broaden
    )
    ->
        { tryEarly : List broadPossibility
        , broad : broadPossibility
        , tryLate : List broadPossibility
        }
    ->
        MorphIndependently
            (beforeToNarrow
             -> Result (ErrorWithDeadEnd deadEnd) narrow
            )
            broaden
choiceEquivalent traversePossibility possibilities =
    let
        possibilitiesStack : Emptiable (Stacked broadPossibility) never_
        possibilitiesStack =
            Stack.fromList possibilities.tryEarly
                |> Stack.attachAdapt Up
                    (Stack.topBelow possibilities.broad possibilities.tryLate)
    in
    { description =
        { custom = Emptiable.empty
        , inner =
            possibilitiesStack
                |> Stack.map (\_ possibility -> possibility |> traversePossibility |> description)
                |> ChoiceDescription
        }
    , toNarrow =
        \beforeToNarrow ->
            beforeToNarrow
                |> choiceEquivalentToNarrow traversePossibility possibilitiesStack
    , toBroad =
        (traversePossibility possibilities.broad).toBroad
    }


choiceEquivalentToNarrow :
    (element
     ->
        MorphIndependently
            (beforeToNarrow
             -> Result (ErrorWithDeadEnd deadEnd) narrow
            )
            broaden_
    )
    -> Emptiable (Stacked element) Never
    ->
        (beforeToNarrow
         -> Result (ErrorWithDeadEnd deadEnd) narrow
        )
choiceEquivalentToNarrow traverseTry possibilities =
    \beforeToNarrow ->
        possibilities
            |> Stack.foldFromOne
                (\top ->
                    beforeToNarrow
                        |> toNarrow (traverseTry top)
                        |> Result.mapError Stack.one
                )
                Up
                (\elementForMorph resultSoFar ->
                    resultSoFar
                        |> recoverTry
                            (\errorsSoFar ->
                                beforeToNarrow
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
        , toNarrow : narrow
        , toBroad : broaden
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
        , toNarrow =
            \broadValue ->
                broadValue
                    |> choiceMorphSoFar.toNarrow
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
        , toBroad =
            choiceMorphSoFar.toBroad
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
    , toNarrow = narrowByPossibility
    , toBroad = broadenByPossibility
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
        , toNarrow =
            choiceMorphSoFar.toNarrow
                (\broad_ ->
                    case broad_ |> toNarrow possibilityMorph of
                        Ok possibility ->
                            possibility |> possibilityToChoice |> Ok

                        Err error ->
                            { index = choiceMorphSoFar.description |> Stack.length, error = error } |> Err
                )
        , toBroad =
            choiceMorphSoFar.toBroad
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
        (beforeToNarrow
         -> Result { index : Int, error : ErrorWithDeadEnd deadEnd } narrow
        )
        broaden
    ->
        MorphIndependently
            (beforeToNarrow
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
        , toNarrow =
            \beforeToNarrow ->
                beforeToNarrow
                    |> choiceMorphComplete.toNarrow
                    |> Result.mapError VariantError
        , toBroad = choiceMorphComplete.toBroad
        }



-- MorphRow


{-| Possibly incomplete [`MorphRow`](#MorphRow) to and from a Morph.choice.
See [`Morph.choice`](Morph#choice), [`Morph.tryRow`](#try), [`Morph.choiceFinish`](#choiceFinish)
-}
type alias ChoiceMorphRowEmptiable noTryPossiblyOrNever choiceNarrow choiceBroaden broadElement =
    { description :
        Emptiable (Stacked Description) noTryPossiblyOrNever
    , toNarrow :
        Emptiable (Stacked broadElement) Possibly
        ->
            Result
                (-- tries
                 Emptiable (Stacked Error) noTryPossiblyOrNever
                )
                { narrow : choiceNarrow
                , broad : Emptiable (Stacked broadElement) Possibly
                }
    , toBroad : choiceBroaden
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
            |> Morph.choiceFinish

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
        , toNarrow =
            \choiceBroad ->
                choiceBroad
                    |> choiceMorphSoFar.toNarrow
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
        , toBroad =
            choiceMorphSoFar.toBroad
                (toBroad possibilityMorph)
        }


{-| Always the last step of a [`Morph.choice`](Morph#choice) `|>` [`Morph.try`](Morph#try) or `|>` [`Morph.tryRow`](#tryRow) builder.
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
            { custom = Emptiable.empty
            , inner =
                choiceMorphComplete.description |> ChoiceDescription
            }
        , toNarrow =
            \beforeToNarrow ->
                beforeToNarrow
                    |> choiceMorphComplete.toNarrow
                    |> Result.mapError ChoiceError
        , toBroad =
            choiceMorphComplete.toBroad
        }
