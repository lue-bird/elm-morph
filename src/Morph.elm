module Morph exposing
    ( Morph, OneToOne, MorphOrError, MorphIndependently
    , toBroadOnly
    , custom, only, validate
    , oneToOne, broad, toggle, keep, oneToOneOn
    , recursive
    , end, one, succeed, grab, match
    , named
    , invert
    , deadEndMap
    , deadEndNever, narrowErrorMap
    , Error, ErrorWithDeadEnd(..), PartsError, SequenceError, SequencePlace(..), ChainError, ChainPlace(..), UntilError, CountAndExactlyElementSequenceError(..)
    , description
    , Description(..), ChainDescription, SequenceDescription, UntilDescription
    , descriptionToTree, DescriptionKind(..)
    , descriptionAndErrorToTree, DescriptionOrErrorKind(..)
    , treeToLines
    , toBroad, toNarrow, mapTo
    , over, overRow
    , PartsMorphEmptiable
    , parts, part, partsFinish
    , tryTopToBottom
    , VariantsMorphEmptiable, variants, variant, variantsFinish
    , choice
    , ChoiceMorphEmptiable, try, choiceFinish
    , ChoiceMorphRowEmptiable, rowTry
    , MorphRow, MorphRowIndependently, rowFinish
    , whilePossible
    , untilNext, broadEnd, untilLast
    , whilePossibleFold, untilNextFold, untilLastFold
    )

{-| Call it Codec, ParserPrinter, TransformReversible, ...
We call it

@docs Morph, OneToOne, MorphOrError, MorphIndependently


## create

@docs toBroadOnly
@docs custom, only, validate
@docs oneToOne, broad, toggle, keep, oneToOneOn

@docs recursive


### create row

@docs end, one, succeed, grab, match


## alter

@docs named
@docs invert
@docs deadEndMap
@docs deadEndNever, narrowErrorMap


## error

@docs Error, ErrorWithDeadEnd, PartsError, SequenceError, SequencePlace, ChainError, ChainPlace, UntilError, CountAndExactlyElementSequenceError


## describe

@docs description
@docs Description, ChainDescription, SequenceDescription, UntilDescription


## error and description visualization

@docs descriptionToTree, DescriptionKind
@docs descriptionAndErrorToTree, DescriptionOrErrorKind

You'll usually want to present this tree as an expandable view.
In [examples](https://github.com/lue-bird/elm-morph/tree/master/example), we have a minimal custom implementation of that.

I'm sure you'd want to present it differently in your app (css support, better accessibility, more consistent style, more features etc.).
Since a tree-view isn't the focus of this package, no half-hearted component like that is exposed.
Frankly I also can't recommend any existing package for this either; existing solutions to me seemed lacking.

Maybe you've found or built some nice tree-view? Please share it :)

@docs treeToLines


## scan

@docs toBroad, toNarrow, mapTo


## chain

@docs over, overRow

If you're missing something more "I need a length andThen I can construct this many elements"-like,
try [`ArraySized.Morph.exactlyWith`](ArraySized-Morph#exactlyWith).


## group

@docs PartsMorphEmptiable
@docs parts, part, partsFinish


## choice [`Morph`](Morph#Morph)

[`Morph`](#Morph) a union `type`

@docs tryTopToBottom


### morph by variant

@docs VariantsMorphEmptiable, variants, variant, variantsFinish
@docs choice
@docs ChoiceMorphEmptiable, try, choiceFinish


## choice [`MorphRow`](#MorphRow)

@docs ChoiceMorphRowEmptiable, rowTry


## row

@docs MorphRow, MorphRowIndependently, rowFinish


## sequence

  - optional → [`Maybe.Morph.row`](Maybe-Morph#row)
  - [`atLeast`](ArraySized-Morph#atLeast)
  - [`exactly`](ArraySized-Morph#exactly)
  - between → [`ArraySized.Morph.in_`](ArraySized-Morph#exactly)

@docs whilePossible
@docs untilNext, broadEnd, untilLast
@docs whilePossibleFold, untilNextFold, untilLastFold


### oh look! other projects do similar things

  - haskell: [`invertible-syntax`](https://hackage.haskell.org/package/invertible-syntax),
    [`partial-isomorphisms`](https://hackage.haskell.org/package/partial-isomorphisms)
  - python: [`construct`](https://construct.readthedocs.io/en/latest/intro.html) from and to broad bits
  - kotlin: [`searles/parsing`](https://github.com/searles/parsing)
  - js: [`nearley.js`](https://nearley.js.org/)
  - prolog: [`phrase/2`](https://www.swi-prolog.org/pldoc/doc_for?object=phrase/2) is somewhat similar for text
  - custom: [FliPpr](https://link.springer.com/article/10.1007/s00354-018-0033-7)
  - parse-build an enum over a String: [`jmpavlick/bimap`](https://dark.elm.dmy.fr/packages/jmpavlick/bimap/latest/), [`toastal/select-prism`](https://package.elm-lang.org/packages/toastal/select-prism/latest/), [`Herteby/enum`](https://package.elm-lang.org/packages/Herteby/enum/latest), [`genthaler/elm-enum`](https://package.elm-lang.org/packages/genthaler/elm-enum/latest/), [`the-sett/elm-refine` `Enum`](https://package.elm-lang.org/packages/the-sett/elm-refine/latest/Enum)
  - equivalent to [`Morph.OneToOne`](#OneToOne): [`arturopala/elm-monocle` `Monocle.Iso`](https://package.elm-lang.org/packages/arturopala/elm-monocle/latest/Monocle-Iso), [`Heimdell/elm-optics` `Optics.Core.Iso`](https://package.elm-lang.org/packages/Heimdell/elm-optics/latest/Optics-Core#Iso), [`erlandsona/elm-accessors` `Accessors.Iso`](https://dark.elm.dmy.fr/packages/erlandsona/elm-accessors/latest/Accessors#Iso), [`fujiy/elm-json-convert` `Json.Convert.Iso`](https://package.elm-lang.org/packages/fujiy/elm-json-convert/latest/Json-Convert#Iso)
  - I expect there to be lots more, please send some (PR, issue etc)!

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
import Util exposing (recoverTry, stackInit, stackLast)



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


{-| Conversion functions from a more general → broad to
a more specific → narrow format and back.

Each type `Morph narrow broad`,
say for example `Morph Email String`, can convert


### `toBroad : narrow -> broad`

  - example: `Email -> String`
  - going from a specific type to a general one
  - always successful
  - can loose information on its way


### `toNarrow : broad -> Result error narrow`

  - example: `String -> Result Morph.Error Email`
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
            including for other developers,
            "allowing for improved reasoning across the codebase"
  - 🎙️ [podcast "Parse, don't validate"](https://elm-radio.com/episode/parse-dont-validate/)


## limits

A Morph can only fail one way
but what if some parts of the broad format are actually more narrow?


### solution: require the narrow structure to have equally narrow parts

Example: Your narrow elm value has a `Float` but the broad JSON's number can't have Nan or infinity.

What `MorphValue` has settled on is the following:

JSON uses the [`Decimal`](Decimal#Decimal) type which doesn't have exception states.

Now the user has a choice:

  - use [`Decimal`](Decimal#Decimal) instead of `Float` in your code, too, if you don't expect exceptions

  - explicitly encode the exception as well

        Float.Morph.decimalOrException
            |> Morph.over Decimal.Morph.orExceptionValue

This is pretty idealistic but if you can do something like this, give it a shot.


### solution: 2 separate morphs

Example: translating one programming language to another,
where both can represent stuff the other can't.
This can fail in both directions.

I haven't done something like that but both directions to a subset combine well

    LanguageABSubset.a : Morph LanguageABSubset LanguageA
    -- LanguageABSubset -> LanguageA will always work

    -- and

    LanguageABSubset.b : Morph LanguageABSubset LanguageB
    -- LanguageABSubset -> LanguageB will always work

    -- combine

    --: LanguageA -> Result Morph.Error LanguageB
    Morph.toNarrow LanguageABSubset.a >> Result.map (Morph.toBroad LanguageABSubset.b)

    --: LanguageB -> Result Morph.Error LanguageA
    Morph.toNarrow LanguageABSubset.b >> Result.map (Morph.toBroad LanguageABSubset.a)


### Why do most primitives here not allow custom error types?

They [could](#MorphOrError) but what is the problem you are trying to solve?

Errors with more narrow structural information are mostly useful for recovery based on what went wrong.

In that case you probably want

    Morph.OneToOne (Result YourRecoverable YourNarrow) YourBroad

So always having an extra type variable just adds complexity to the types.

-}
type alias Morph narrow broad =
    MorphOrError narrow broad Error


{-| Sometimes, you'll see the most general version of [`Morph`](#Morph):

    : MorphIndependently narrow broaden

where

  - [`toNarrow`](#toNarrow) result types can't necessarily be used as input for [`toBroad`](#toBroad)
  - [`toBroad`](#toBroad) result types can't necessarily be used as input for [`toNarrow`](#toNarrow)

For example:

  - [`MorphValue`](Value-Morph#MorphValue): [`toBroad`](#toBroad) returns a value where we know
    **both index and name** for each field/variant,
    whereas [`toNarrow`](#toNarrow) allows **either index or name** for each field/variant.
    This allows us to choose whether we want a [`descriptive`](Value-Morph#descriptive) or [`compact`](Value-Morph#compact)
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

    type alias OneToOne mapped unmapped =
        MorphOrError mapped unmapped (ErrorWithDeadEnd Never)

-}
type alias MorphOrError narrow broad error =
    MorphIndependently
        (broad -> Result error narrow)
        (narrow -> broad)


{-| Describing what the Morph [narrows to](#toNarrow) and [broadens from](#toBroad)
in a neatly structured way.

Why do some variants have type aliases? → To concisely annotate them
in the implementation of for example [`descriptionAndErrorToTree`](#descriptionAndErrorToTree).

-}
type
    Description
    -- OneToOne
    = InverseDescription Description
    | CustomDescription
      -- MorphRow
    | SucceedDescription
    | EndDescription
    | WhilePossibleDescription Description
    | UntilNextDescription UntilDescription
    | UntilLastDescription UntilDescription
    | SequenceDescription SequenceDescription
      -- others
    | NamedDescription { name : String, description : Description }
    | OnlyDescription String
    | RecursiveDescription String (() -> Description)
    | ChainDescription ChainDescription
      -- group morph
    | ElementsDescription Description
    | PartsDescription (Emptiable (Stacked { tag : String, value : Description }) Never)
      -- choice morph
    | ChoiceDescription (Emptiable (Stacked Description) Never)
    | VariantsDescription (Emptiable (Stacked { tag : String, value : Description }) Never)


{-| [`untilNext`](#untilNext) and [`untilNextFold`](#untilNextFold)-specific [`Description`](#Description)
-}
type alias UntilDescription =
    RecordWithoutConstructorFunction
        { end : Description
        , element : Description
        }


{-| [Description](#Description) specific to
[`MorphRow`](#MorphRow)s following one after the other
like with [`|> grab`](#grab), [`|> match`](#match) etc.
-}
type alias SequenceDescription =
    RecordWithoutConstructorFunction
        { early : Description
        , late : Description
        }


{-| [Description](#Description) specific to
[`narrow |> Morph.overRow broad`](#overRow) and [`narrow |> Morph.over broad`](#over)
-}
type alias ChainDescription =
    RecordWithoutConstructorFunction
        { broad : Description
        , narrow : Description
        }


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


isDescriptive : Description -> Bool
isDescriptive =
    \description_ ->
        case description_ of
            NamedDescription _ ->
                True

            CustomDescription ->
                False

            EndDescription ->
                True

            SucceedDescription ->
                False

            OnlyDescription _ ->
                True

            RecursiveDescription _ _ ->
                True

            InverseDescription inverseDescription ->
                inverseDescription |> isDescriptive

            WhilePossibleDescription _ ->
                True

            UntilNextDescription _ ->
                True

            UntilLastDescription _ ->
                True

            ChainDescription elements ->
                [ elements.broad, elements.narrow ] |> List.any isDescriptive

            SequenceDescription elements ->
                [ elements.early, elements.late ] |> List.any isDescriptive

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


collapseChainDescription : ChainDescription -> List Description
collapseChainDescription chainDescription =
    [ chainDescription.narrow, chainDescription.broad ]
        |> List.concatMap
            (\chainSideDescription ->
                if isDescriptive chainSideDescription then
                    case chainSideDescription of
                        NamedDescription namedDescription ->
                            [ NamedDescription namedDescription ]

                        ChainDescription lateSequenceDescription ->
                            collapseChainDescription lateSequenceDescription

                        lastDescription ->
                            [ lastDescription ]

                else
                    []
            )


collapseSequenceDescription : SequenceDescription -> List Description
collapseSequenceDescription sequenceDescription =
    [ sequenceDescription.early, sequenceDescription.late ]
        |> List.concatMap
            (\sequenceSideDescription ->
                if isDescriptive sequenceSideDescription then
                    case sequenceSideDescription of
                        NamedDescription namedDescription ->
                            [ NamedDescription namedDescription ]

                        SequenceDescription lateSequenceDescription ->
                            collapseSequenceDescription lateSequenceDescription

                        lastDescription ->
                            [ lastDescription ]

                else
                    []
            )


{-| Create a tree from the structured [`Description`](#Description)
-}
descriptionToTree : Description -> Tree { kind : DescriptionKind, text : String }
descriptionToTree description_ =
    case description_ of
        CustomDescription ->
            Tree.singleton
                { kind = DescriptionStructureKind
                , text = "(custom)"
                }

        EndDescription ->
            Tree.singleton { kind = DescriptionStructureKind, text = "end" }

        InverseDescription inverseDescription ->
            Tree.tree { kind = DescriptionStructureKind, text = "inverse" }
                [ descriptionToTree inverseDescription ]

        SucceedDescription ->
            Tree.singleton
                { kind = DescriptionStructureKind
                , text = "(always succeeds)"
                }

        OnlyDescription onlyDescription ->
            Tree.singleton
                { kind = DescriptionStructureKind
                , text = "only " ++ onlyDescription
                }

        RecursiveDescription recursiveStructureName _ ->
            Tree.singleton
                { kind = DescriptionStructureKind
                , text = "recursive: " ++ recursiveStructureName
                }

        WhilePossibleDescription elementDescription ->
            Tree.tree
                { kind = DescriptionStructureKind
                , text = "while possible"
                }
                [ descriptionToTree elementDescription ]

        UntilNextDescription untilDescription ->
            Tree.tree { kind = DescriptionStructureKind, text = "until next" }
                [ Tree.tree { kind = DescriptionStructureKind, text = "element" }
                    [ descriptionToTree untilDescription.element ]
                , Tree.tree { kind = DescriptionStructureKind, text = "end" }
                    [ descriptionToTree untilDescription.end ]
                ]

        UntilLastDescription untilDescription ->
            Tree.tree { kind = DescriptionStructureKind, text = "until last" }
                [ Tree.tree { kind = DescriptionStructureKind, text = "element" }
                    [ descriptionToTree untilDescription.element ]
                , Tree.tree { kind = DescriptionStructureKind, text = "end" }
                    [ descriptionToTree untilDescription.end ]
                ]

        SequenceDescription elementDescriptions ->
            elementDescriptions
                |> collapseSequenceDescription
                |> List.map descriptionToTree
                |> groupDescriptionTreesAs "sequence"

        ChainDescription elementDescriptions ->
            elementDescriptions
                |> collapseChainDescription
                |> List.map descriptionToTree
                |> groupDescriptionTreesAs "chained"

        ChoiceDescription possibilities ->
            Tree.tree { kind = DescriptionStructureKind, text = "either" }
                (possibilities
                    |> Stack.toList
                    |> List.map descriptionToTree
                )

        ElementsDescription elementDescription ->
            Tree.tree { kind = DescriptionStructureKind, text = "elements" }
                [ elementDescription |> descriptionToTree ]

        PartsDescription partsDescription ->
            Tree.tree { kind = DescriptionStructureKind, text = "both" }
                (partsDescription
                    |> Stack.toList
                    |> List.map
                        (\partDescription ->
                            Tree.tree
                                { kind = DescriptionStructureKind
                                , text = partDescription.tag
                                }
                                [ partDescription.value |> descriptionToTree ]
                        )
                )

        VariantsDescription variantsDescription ->
            Tree.tree { kind = DescriptionStructureKind, text = "variants" }
                (variantsDescription
                    |> Stack.toList
                    |> List.map
                        (\variantDescription ->
                            Tree.tree
                                { kind = DescriptionStructureKind
                                , text = variantDescription.tag
                                }
                                [ variantDescription.value |> descriptionToTree ]
                        )
                )

        NamedDescription namedDescription ->
            Tree.tree { kind = DescriptionNameKind, text = namedDescription.name }
                (if isDescriptive namedDescription.description then
                    [ descriptionToTree namedDescription.description ]

                 else
                    []
                )


groupDescriptionTreesAs : String -> List (Tree { kind : DescriptionKind, text : String }) -> Tree { kind : DescriptionKind, text : String }
groupDescriptionTreesAs structureName =
    \informativeElements ->
        case informativeElements of
            [] ->
                Tree.singleton
                    { kind = DescriptionStructureKind
                    , text = "(empty " ++ structureName ++ ")"
                    }

            onlyElement :: [] ->
                onlyElement

            element0 :: element1 :: elements2Up ->
                Tree.tree { kind = DescriptionStructureKind, text = structureName }
                    (element0 :: element1 :: elements2Up)


{-| What does the label in an [error and description tree](#descriptionAndErrorToTree) describe?
Is it from an error, a [structure description or custom description](#DescriptionKind)?
-}
type DescriptionOrErrorKind
    = DescriptionKind DescriptionKind
    | ErrorKind


{-| What does the label in a [description tree](#descriptionToTree) describe?
Is it a description from some internal control structure
or a custom description from [`Morph.named`](#named)?
-}
type DescriptionKind
    = DescriptionNameKind
    | DescriptionStructureKind


errorToLabelTree : Error -> Tree { text : String, kind : DescriptionOrErrorKind }
errorToLabelTree =
    \error ->
        error
            |> errorToTree
            |> Tree.map (\labelString -> { kind = ErrorKind, text = labelString })


startDownMessage : { startDownInBroadList : Int } -> String
startDownMessage =
    \rowError ->
        [ "starting at ", rowError.startDownInBroadList |> String.fromInt, " from last" ] |> String.concat


errorToTree : Error -> Tree String
errorToTree =
    \error ->
        case error of
            DeadEnd deadEnd ->
                Tree.singleton ("but I found " ++ deadEnd)

            UntilError untilError ->
                Tree.tree "until"
                    ((untilError.startsDownInBroadList
                        |> Stack.removeTop
                        |> Stack.toList
                        |> List.reverse
                        |> List.map
                            (\startDown ->
                                Tree.tree "successful element"
                                    [ { startDownInBroadList = startDown } |> startDownMessage |> Tree.singleton ]
                            )
                     )
                        ++ [ Tree.tree "end"
                                [ untilError.endError |> errorToTree
                                , { startDownInBroadList =
                                        untilError.startsDownInBroadList |> Stack.top
                                  }
                                    |> startDownMessage
                                    |> Tree.singleton
                                ]
                           ]
                    )

            SequenceError sequenceError ->
                Tree.tree ([ sequenceError.place |> inSequencePlaceToString, " in sequence" ] |> String.concat)
                    [ sequenceError.error |> errorToTree ]

            ChainError chainError ->
                Tree.tree ([ chainError.place |> inChainPlaceToString, " in chain" ] |> String.concat)
                    [ chainError.error |> errorToTree ]

            ElementsError elementErrors ->
                Tree.tree "elements"
                    (elementErrors
                        |> Stack.toList
                        |> List.map
                            (\elementError ->
                                Tree.tree elementError.location
                                    [ elementError.error |> errorToTree ]
                            )
                    )

            CountAndExactlyElementSequenceError countAndExactlyElementSequenceError ->
                case countAndExactlyElementSequenceError of
                    CountError countError ->
                        Tree.tree "count" [ countError |> errorToTree ]

                    ExactlyCountElementSequenceError exactlyCountElementSequenceError ->
                        Tree.tree "repeating element sequence"
                            ((exactlyCountElementSequenceError.startsDownInBroadList
                                |> Stack.removeTop
                                |> Stack.toList
                                |> List.reverse
                                |> List.map
                                    (\startDown ->
                                        Tree.tree "successful element"
                                            [ { startDownInBroadList = startDown } |> startDownMessage |> Tree.singleton ]
                                    )
                             )
                                ++ [ Tree.tree
                                        ([ "failed element" ] |> String.concat)
                                        [ { startDownInBroadList =
                                                exactlyCountElementSequenceError.startsDownInBroadList |> Stack.top
                                          }
                                            |> startDownMessage
                                            |> Tree.singleton
                                        , exactlyCountElementSequenceError.error |> errorToTree
                                        ]
                                   ]
                            )

            PartsError partErrors ->
                Tree.tree "both"
                    (partErrors
                        |> Stack.toList
                        |> List.map
                            (\elementError ->
                                Tree.tree ([ "index ", elementError.index |> String.fromInt ] |> String.concat)
                                    [ elementError.error |> errorToTree ]
                            )
                    )

            VariantError variantError ->
                Tree.tree ([ "variant, index ", variantError.index |> String.fromInt ] |> String.concat)
                    [ variantError.error |> errorToTree ]

            ChoiceError possibilityErrors ->
                Tree.tree "choice"
                    (possibilityErrors
                        |> Stack.toList
                        |> List.map errorToTree
                    )


inChainPlaceToString : ChainPlace -> String
inChainPlaceToString =
    \inChainPlace ->
        case inChainPlace of
            ChainPlaceBroad ->
                "broad"

            ChainPlaceNarrow ->
                "narrow"


inSequencePlaceToString : SequencePlace -> String
inSequencePlaceToString =
    \inChainPlace ->
        case inChainPlace of
            SequencePlaceEarly ->
                "early"

            SequencePlaceLate ->
                "late"


descriptionToLabelTree : Description -> Tree { text : String, kind : DescriptionOrErrorKind }
descriptionToLabelTree =
    \description_ ->
        description_
            |> descriptionToTree
            |> Tree.map
                (\labelString ->
                    { kind = labelString.kind |> DescriptionKind, text = labelString.text }
                )


unexpectedErrorToTree : Error -> Description -> Tree { kind : DescriptionOrErrorKind, text : String }
unexpectedErrorToTree unexpectedError description_ =
    description_
        |> descriptionToLabelTree
        |> Tree.prependChild
            (case unexpectedError of
                DeadEnd deadEndMessage ->
                    Tree.singleton { kind = ErrorKind, text = deadEndMessage }

                nonDeadEnd ->
                    Tree.tree { kind = ErrorKind, text = "unexpected error kind" }
                        [ nonDeadEnd |> errorToLabelTree ]
            )


{-| Create a tree describing a given [`Error`](#Error) embedded in a given [`Description`](#Description).
-}
descriptionAndErrorToTree : Description -> Error -> Tree { text : String, kind : DescriptionOrErrorKind }
descriptionAndErrorToTree description_ =
    case description_ of
        CustomDescription ->
            \error ->
                error |> errorToLabelTree

        EndDescription ->
            \error ->
                Tree.tree { kind = DescriptionStructureKind |> DescriptionKind, text = "end" }
                    [ error |> errorToLabelTree ]

        InverseDescription inverseDescription ->
            \error ->
                Tree.tree { kind = DescriptionStructureKind |> DescriptionKind, text = "inverse" }
                    [ descriptionAndErrorToTree inverseDescription error ]

        SucceedDescription ->
            \_ ->
                Tree.singleton
                    { kind = DescriptionStructureKind |> DescriptionKind
                    , text = "(always succeeds)"
                    }

        OnlyDescription onlyDescription ->
            \error ->
                Tree.tree
                    { kind = DescriptionStructureKind |> DescriptionKind
                    , text = "only " ++ onlyDescription
                    }
                    [ error |> errorToLabelTree ]

        RecursiveDescription _ lazyDescription ->
            \error ->
                descriptionAndErrorToTree
                    (lazyDescription ()
                        |> descriptionCustomNameAlter (\s -> "recursive: " ++ s)
                    )
                    error

        WhilePossibleDescription elementDescription ->
            \_ ->
                Tree.tree
                    { kind = DescriptionStructureKind |> DescriptionKind
                    , text = "while possible"
                    }
                    [ descriptionToLabelTree elementDescription ]

        UntilNextDescription untilDescription ->
            \error ->
                case error of
                    UntilError untilError ->
                        Tree.tree { kind = DescriptionStructureKind |> DescriptionKind, text = "until next" }
                            (Tree.tree
                                { kind = DescriptionStructureKind |> DescriptionKind, text = "end" }
                                [ descriptionAndErrorToTree untilDescription.end untilError.endError
                                ]
                                :: (untilError.startsDownInBroadList
                                        |> Stack.removeTop
                                        |> Stack.toList
                                        |> List.reverse
                                        |> List.map
                                            (\startDown ->
                                                Tree.tree { kind = DescriptionStructureKind |> DescriptionKind, text = "element" }
                                                    [ startDownLabel { startDownInBroadList = startDown }
                                                    , untilDescription.element |> descriptionToLabelTree
                                                    ]
                                            )
                                   )
                                ++ [ Tree.tree { kind = DescriptionStructureKind |> DescriptionKind, text = "element" }
                                        [ descriptionAndErrorToTree untilDescription.element untilError.elementError
                                        , startDownLabel { startDownInBroadList = untilError.startsDownInBroadList |> Stack.top }
                                        ]
                                   ]
                            )

                    unexpectedError ->
                        unexpectedErrorToTree unexpectedError (UntilNextDescription untilDescription)

        UntilLastDescription untilDescription ->
            \error ->
                case error of
                    UntilError untilError ->
                        Tree.tree { kind = DescriptionStructureKind |> DescriptionKind, text = "until last" }
                            (Tree.tree
                                { kind = DescriptionStructureKind |> DescriptionKind, text = "end" }
                                [ descriptionAndErrorToTree untilDescription.end untilError.endError
                                ]
                                :: (untilError.startsDownInBroadList
                                        |> stackInit
                                        |> Stack.toList
                                        |> List.map
                                            (\startDown ->
                                                Tree.tree { kind = DescriptionStructureKind |> DescriptionKind, text = "element" }
                                                    [ startDownLabel { startDownInBroadList = startDown }
                                                    , untilDescription.element |> descriptionToLabelTree
                                                    ]
                                            )
                                   )
                                ++ [ Tree.tree { kind = DescriptionStructureKind |> DescriptionKind, text = "element" }
                                        [ descriptionAndErrorToTree untilDescription.element untilError.elementError
                                        , startDownLabel { startDownInBroadList = untilError.startsDownInBroadList |> stackLast }
                                        ]
                                   ]
                            )

                    unexpectedError ->
                        unexpectedErrorToTree unexpectedError (UntilNextDescription untilDescription)

        SequenceDescription elementDescriptions ->
            \error ->
                case error of
                    SequenceError inSequenceError ->
                        collapseSequenceDescriptionAndError elementDescriptions inSequenceError
                            |> groupTreesAs "sequence"

                    unexpectedError ->
                        unexpectedErrorToTree unexpectedError (SequenceDescription elementDescriptions)

        ChainDescription elementDescriptions ->
            \error ->
                case error of
                    ChainError chainError ->
                        collapseChainDescriptionAndError elementDescriptions chainError
                            |> groupTreesAs "chained"

                    unexpectedError ->
                        unexpectedErrorToTree unexpectedError (ChainDescription elementDescriptions)

        ChoiceDescription possibilities ->
            \error ->
                -- TODO check who got the furthest and show that first
                case error of
                    ChoiceError tryErrors ->
                        Tree.tree { kind = DescriptionStructureKind |> DescriptionKind, text = "either" }
                            (List.map2
                                (\elementDescription elementError ->
                                    descriptionAndErrorToTree elementDescription elementError
                                )
                                (possibilities |> Stack.toList)
                                (tryErrors |> Stack.toList)
                                -- TODO reversing sometimes seems not correct (for example Maybe.Morph.row)
                                |> List.reverse
                            )

                    unexpectedError ->
                        unexpectedErrorToTree unexpectedError (ChoiceDescription possibilities)

        ElementsDescription elementDescription ->
            \error ->
                case error of
                    ElementsError inCollectionError ->
                        Tree.tree { kind = DescriptionStructureKind |> DescriptionKind, text = "elements" }
                            (inCollectionError
                                |> Stack.toList
                                |> List.concatMap
                                    (\elementError ->
                                        [ descriptionAndErrorToTree elementDescription elementError.error
                                        , Tree.singleton { kind = ErrorKind, text = "at " ++ elementError.location }
                                        ]
                                    )
                            )

                    unexpectedError ->
                        unexpectedErrorToTree unexpectedError (ElementsDescription elementDescription)

        PartsDescription partsDescription ->
            \error ->
                case error of
                    PartsError partsError ->
                        Tree.tree { kind = DescriptionStructureKind |> DescriptionKind, text = "both" }
                            (partsDescription
                                |> Stack.toList
                                |> List.indexedMap
                                    (\index partDescription ->
                                        Tree.tree
                                            { kind = DescriptionStructureKind |> DescriptionKind
                                            , text = partDescription.tag
                                            }
                                            [ case partsError |> Stack.toList |> List.filter (\partError -> partError.index == index) of
                                                [] ->
                                                    descriptionToLabelTree partDescription.value

                                                partError :: _ ->
                                                    descriptionAndErrorToTree partDescription.value partError.error
                                            ]
                                    )
                            )

                    unexpectedError ->
                        unexpectedErrorToTree unexpectedError (PartsDescription partsDescription)

        VariantsDescription variantsDescription ->
            \error ->
                case error of
                    VariantError variantError ->
                        Tree.tree { kind = DescriptionStructureKind |> DescriptionKind, text = "variants" }
                            (variantsDescription
                                |> Stack.toList
                                |> List.indexedMap
                                    (\index variantDescription ->
                                        Tree.tree
                                            { kind = DescriptionStructureKind |> DescriptionKind
                                            , text = variantDescription.tag
                                            }
                                            [ if variantError.index == index then
                                                descriptionAndErrorToTree variantDescription.value variantError.error

                                              else
                                                descriptionToLabelTree variantDescription.value
                                            ]
                                    )
                            )

                    unexpectedError ->
                        unexpectedErrorToTree unexpectedError (VariantsDescription variantsDescription)

        NamedDescription namedDescription ->
            \error ->
                Tree.tree
                    { kind = DescriptionStructureKind |> DescriptionKind
                    , text = namedDescription.name
                    }
                    [ descriptionAndErrorToTree namedDescription.description error ]


collapseSequenceDescriptionAndError :
    SequenceDescription
    -> SequenceError Error
    -> List (Tree { text : String, kind : DescriptionOrErrorKind })
collapseSequenceDescriptionAndError sequenceDescription sequenceError =
    case sequenceError.place of
        SequencePlaceEarly ->
            let
                earlyCollapsed : List (Tree { text : String, kind : DescriptionOrErrorKind })
                earlyCollapsed =
                    case sequenceDescription.early of
                        NamedDescription namedDescription ->
                            [ descriptionAndErrorToTree (NamedDescription namedDescription) sequenceError.error ]

                        SequenceDescription earlySequenceDescription ->
                            case sequenceError.error of
                                SequenceError earlySequenceError ->
                                    collapseSequenceDescriptionAndError
                                        earlySequenceDescription
                                        earlySequenceError

                                unexpectedError ->
                                    [ unexpectedErrorToTree unexpectedError (SequenceDescription earlySequenceDescription) ]

                        earlyNonSequenceDescription ->
                            [ descriptionAndErrorToTree earlyNonSequenceDescription sequenceError.error ]

                lateCollapsed : List (Tree { text : String, kind : DescriptionOrErrorKind })
                lateCollapsed =
                    case sequenceDescription.late of
                        SequenceDescription lateSequenceDescription ->
                            collapseSequenceDescription lateSequenceDescription
                                |> List.map descriptionToLabelTree

                        sequenceLateNonSequence ->
                            if isDescriptive sequenceLateNonSequence then
                                [ descriptionToLabelTree sequenceLateNonSequence ]

                            else
                                []
            in
            earlyCollapsed ++ lateCollapsed

        SequencePlaceLate ->
            let
                earlyCollapsed : List (Tree { text : String, kind : DescriptionOrErrorKind })
                earlyCollapsed =
                    case sequenceDescription.early of
                        SequenceDescription earlySequenceDescription ->
                            collapseSequenceDescription earlySequenceDescription
                                |> List.map descriptionToLabelTree

                        sequenceEarlyNonSequence ->
                            if isDescriptive sequenceEarlyNonSequence then
                                [ descriptionToLabelTree sequenceEarlyNonSequence ]

                            else
                                []

                lateCollapsed : List (Tree { text : String, kind : DescriptionOrErrorKind })
                lateCollapsed =
                    case sequenceDescription.late of
                        NamedDescription namedDescription ->
                            [ descriptionAndErrorToTree (NamedDescription namedDescription) sequenceError.error ]

                        SequenceDescription lateSequenceDescription ->
                            case sequenceError.error of
                                SequenceError lateSequenceError ->
                                    collapseSequenceDescriptionAndError
                                        lateSequenceDescription
                                        lateSequenceError

                                unexpectedError ->
                                    [ unexpectedErrorToTree unexpectedError (SequenceDescription lateSequenceDescription) ]

                        lateNonSequenceDescription ->
                            [ descriptionAndErrorToTree lateNonSequenceDescription sequenceError.error ]
            in
            earlyCollapsed ++ lateCollapsed


collapseChainDescriptionAndError :
    ChainDescription
    -> ChainError Error
    -> List (Tree { text : String, kind : DescriptionOrErrorKind })
collapseChainDescriptionAndError chainDescription chainError =
    case chainError.place of
        ChainPlaceNarrow ->
            let
                narrowCollapsed : List (Tree { text : String, kind : DescriptionOrErrorKind })
                narrowCollapsed =
                    case chainDescription.narrow of
                        NamedDescription namedDescription ->
                            [ descriptionAndErrorToTree (NamedDescription namedDescription) chainError.error ]

                        ChainDescription narrowChainDescription ->
                            case chainError.error of
                                ChainError narrowChainError ->
                                    collapseChainDescriptionAndError
                                        narrowChainDescription
                                        narrowChainError

                                unexpectedError ->
                                    [ unexpectedErrorToTree unexpectedError (ChainDescription narrowChainDescription) ]

                        narrowNonChainDescription ->
                            [ descriptionAndErrorToTree narrowNonChainDescription chainError.error ]

                broadCollapsed : List (Tree { text : String, kind : DescriptionOrErrorKind })
                broadCollapsed =
                    case chainDescription.broad of
                        ChainDescription broadChainDescription ->
                            collapseChainDescription broadChainDescription
                                |> List.map descriptionToLabelTree

                        chainDescriptionBroadNonChain ->
                            if isDescriptive chainDescriptionBroadNonChain then
                                [ descriptionToLabelTree chainDescriptionBroadNonChain ]

                            else
                                []
            in
            narrowCollapsed ++ broadCollapsed

        ChainPlaceBroad ->
            let
                narrowCollapsed : List (Tree { text : String, kind : DescriptionOrErrorKind })
                narrowCollapsed =
                    case chainDescription.narrow of
                        ChainDescription narrowChainDescription ->
                            collapseChainDescription narrowChainDescription
                                |> List.map descriptionToLabelTree

                        chainDescriptionNarrowNonChain ->
                            if isDescriptive chainDescriptionNarrowNonChain then
                                [ descriptionToLabelTree chainDescriptionNarrowNonChain ]

                            else
                                []

                broadCollapsed : List (Tree { text : String, kind : DescriptionOrErrorKind })
                broadCollapsed =
                    case chainDescription.broad of
                        NamedDescription namedDescription ->
                            [ descriptionAndErrorToTree (NamedDescription namedDescription) chainError.error ]

                        ChainDescription broadChainDescription ->
                            case chainError.error of
                                ChainError broadChainError ->
                                    collapseChainDescriptionAndError
                                        broadChainDescription
                                        broadChainError

                                unexpectedError ->
                                    [ unexpectedErrorToTree unexpectedError (ChainDescription broadChainDescription) ]

                        broadNonChainDescription ->
                            [ descriptionAndErrorToTree broadNonChainDescription chainError.error ]
            in
            narrowCollapsed ++ broadCollapsed


startDownLabel : { startDownInBroadList : Int } -> Tree { kind : DescriptionOrErrorKind, text : String }
startDownLabel =
    \rowError ->
        Tree.singleton
            { kind = ErrorKind
            , text = rowError |> startDownMessage
            }


groupTreesAs : String -> List (Tree { text : String, kind : DescriptionOrErrorKind }) -> Tree { text : String, kind : DescriptionOrErrorKind }
groupTreesAs structureName =
    \informativeElements ->
        case informativeElements of
            [] ->
                Tree.singleton
                    { kind = DescriptionStructureKind |> DescriptionKind
                    , text = "(empty " ++ structureName ++ ")"
                    }

            onlyElement :: [] ->
                onlyElement

            element0 :: element1 :: elements2Up ->
                Tree.tree { kind = DescriptionStructureKind |> DescriptionKind, text = structureName }
                    (element0 :: element1 :: elements2Up)


descriptionCustomNameAlter : (String -> String) -> (Description -> Description)
descriptionCustomNameAlter nameAlter =
    \description_ ->
        case description_ of
            NamedDescription namedDescription ->
                { namedDescription | name = namedDescription.name |> nameAlter }
                    |> NamedDescription

            notNamedDescription ->
                notNamedDescription


{-| Where [narrowing](#toNarrow) has failed.

`String` is not enough for display?
→ use [`MorphOrError`](#MorphOrError) [`ErrorWithDeadEnd`](#ErrorWithDeadEnd) doing [`deadEndMap`](#deadEndMap)
on [`Morph`](#Morph) that are returned

Have trouble doing so because some API is too strict on errors? → issue

-}
type alias Error =
    ErrorWithDeadEnd String


{-| [`Error`](#Error) with a custom value on `DeadEnd`

    type alias OneToOne mapped unmapped =
        MorphOrError mapped unmapped (ErrorWithDeadEnd Never)

`deadEnd` could also be formatted text for display.
For that, use [`MorphOrError`](#MorphOrError) [`ErrorWithDeadEnd`](#ErrorWithDeadEnd) doing [`deadEndMap`](#deadEndMap)
on [`Morph`](#Morph) that are returned.

Have trouble doing so because some API is too strict on errors? → issue

Why do some variants have type aliases? → To concisely annotate them
in the implementation of for example [`descriptionAndErrorToTree`](#descriptionAndErrorToTree).

-}
type ErrorWithDeadEnd deadEnd
    = DeadEnd deadEnd
    | UntilError (UntilError (ErrorWithDeadEnd deadEnd))
    | SequenceError (SequenceError (ErrorWithDeadEnd deadEnd))
    | ChainError (ChainError (ErrorWithDeadEnd deadEnd))
    | ElementsError
        (Emptiable
            (Stacked { location : String, error : ErrorWithDeadEnd deadEnd })
            Never
        )
    | CountAndExactlyElementSequenceError (CountAndExactlyElementSequenceError (ErrorWithDeadEnd deadEnd))
    | PartsError (PartsError (ErrorWithDeadEnd deadEnd))
    | VariantError { index : Int, error : ErrorWithDeadEnd deadEnd }
    | ChoiceError (Emptiable (Stacked (ErrorWithDeadEnd deadEnd)) Never)


{-| An error when using [`ArraySized.Morph.exactlyWith`](ArraySized-Morph#exactlyWith)
-}
type CountAndExactlyElementSequenceError error
    = CountError error
    | ExactlyCountElementSequenceError
        { error : error
        , startsDownInBroadList : Emptiable (Stacked Int) Never
        }


{-| [Error](#Error) specific to
[`MorphRow`](#MorphRow)s following one after the other
like with [`|> grab`](#grab), [`|> match`](#match) etc.

Since this is a sequence, failure can happen [at the first section or after that](#SequencePlace)

-}
type alias SequenceError error =
    RecordWithoutConstructorFunction
        { place : SequencePlace
        , error : error
        , startDownInBroadList : Int
        }


{-| At the first section (early) or after that (late) in the sequence?
-}
type SequencePlace
    = SequencePlaceEarly
    | SequencePlaceLate


{-| [Error](#Error) specific to
[`narrow |> Morph.overRow broad`](#overRow) and [`narrow |> Morph.over broad`](#over)

Since this is a sequence, failure can happen [at the broader transformation or the narrower transformation](#ChainPlace)

-}
type alias ChainError error =
    RecordWithoutConstructorFunction
        { place : ChainPlace
        , error : error
        }


{-| The more narrow or broad morph in an `narrow |> Morph.over... broad` chain?
-}
type ChainPlace
    = ChainPlaceBroad
    | ChainPlaceNarrow


{-| [`Error`](#Error) specific to
[`untilNext`](#untilNext), [`untilLast`](#untilLast), [`untilNextFold`](#untilNextFold), [`untilLastFold`](#untilLastFold)
-}
type alias UntilError partError =
    RecordWithoutConstructorFunction
        { endError : partError
        , elementError : partError
        , startsDownInBroadList : Emptiable (Stacked Int) Never
        }


{-| A group's part [`Error`](#Error)s, each with their part index
-}
type alias PartsError partError =
    Emptiable
        (Stacked { index : Int, error : partError })
        Never


{-| Describe what you want to narrow to.
This will make errors and descriptions easier to understand.

A good rule of thumb is to at least add a [`Morph.named`](#named) to every morph _declaration_.
More `named` = •ᴗ•.

    import Morph exposing (MorphRow, match, grab)
    import List.Morph
    import String.Morph
    import Int.Morph
    import Integer.Morph
    -- from lue-bird/elm-no-record-type-alias-constructor-function
    import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)

    type alias Point =
        -- makes `Point` function unavailable:
        RecordWithoutConstructorFunction
            { x : Int
            , y : Int
            }

    -- we can use `expect` to have more context when an error happens
    point : MorphRow Point Char
    point =
        Morph.named "point"
            (Morph.succeed (\x y -> { x = x, y = y })
                |> match (String.Morph.only "(")
                |> grab .x (Int.Morph.integer |> Morph.overRow Integer.Morph.chars)
                |> match (String.Morph.only ",")
                |> grab .y (Int.Morph.integer |> Morph.overRow Integer.Morph.chars)
                |> match (String.Morph.only ")")
            )

    "(12,34)"
        |> Morph.toNarrow
            (point
                |> Morph.rowFinish
                |> Morph.over List.Morph.string
            )
    --> Ok { x = 12, y = 34 }

    -- we can get the error context stack as well as where they started matching
    "(a,b)"
        |> Morph.toNarrow
            (point
                |> Morph.rowFinish
                |> Morph.over List.Morph.string
            )
        |> Result.toMaybe
    --> Nothing

Especially for [`oneToOne`](#oneToOne) etc,
adding a description doesn't really add value
as users often don't need to know that you for example converted a [stack to a list](List-Morph#stack)

-}
named :
    String
    ->
        (MorphIndependently narrow broaden
         -> MorphIndependently narrow broaden
        )
named narrowExpectationCustomDescription morphToDescribe =
    { morphToDescribe
        | description =
            NamedDescription
                { description = morphToDescribe.description
                , name = narrowExpectationCustomDescription
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

    import List.Morph

    "3456" |> Morph.mapTo List.Morph.string
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


{-| [`Morph`](#Morph) between representations
that have the same structural information
and can be mapped 1:1 into each other.
[narrowing](#mapTo) can `Never` fail

Examples:

  - some [`Morph`](#Morph) needs a different narrow type

        Morph.oneToOne Set.toList Set.fromList
            |> Morph.over
                (Value.list elementMorph)

      - [`List.Morph.array`](List-Morph#array), [`Array.Morph.list`](Array-Morph#list)
      - [`String.Morph.stack`](String-Morph#stack), [`Stack.Morph.string`](Stack-Morph#string)
      - ...

  - strip unnecessary information
    ~`{ end : (), before :`~`List element`~`}`~

        Morph.oneToOne .before
            (\before_ -> { before = before_, end = () })

Only use [`Morph.OneToOne`](#OneToOne) to annotate arguments. For results,

    MorphOrError (List Char) String error_

allows it to be mixed with other [`Morph`](#Morph)s that can actually fail.

Since both type arguments of `OneToOne` are equally narrow/broad,
choosing one as the `mapped` and one as the `unmapped` is rather arbitrary.

That's the reason we usually expose 2 versions: `A.Morph.b` & `A.Morph.toB`.

**!** Note that information _can_ get lost on the way:

    dictFromListMorph =
        Morph.oneToOne Dict.fromList Dict.toList

Still, it's a `OneToOne` because there's no narrowing necessary to translate one state to the other

-}
type alias OneToOne mapped unmapped =
    MorphOrError mapped unmapped (ErrorWithDeadEnd Never)


{-| Switch between 2 opposite representations. Examples:

    toggle List.reverse

    toggle not

    toggle negate

    toggle (\n -> n ^ -1)

    toggle Linear.opposite

If you want to allow both directions to [`MorphIndependently`](#MorphIndependently),
opt for `oneToOne v v` instead of `toggle v`!

-}
toggle :
    (subject -> subject)
    ->
        MorphIndependently
            (subject -> Result error_ subject)
            (subject -> subject)
toggle changeToOpposite =
    oneToOne changeToOpposite changeToOpposite


{-| A [`Morph`](#Morph) that doesn't transform anything.
Any possible input stays, remains the same. A no-op.

Same as writing:

  - [`toBroad`](#toBroad)`identity`
  - [`oneToOne`](#oneToOne)`identity identity`
  - [`toggle`](#toggle)`identity` when broad and narrow types match
  - [`validate`](#validate)`Ok`
  - `custom ... { toBroad = identity, toNarrow = Ok }`

-}
keep :
    MorphIndependently
        (a -> Result error_ a)
        (b -> b)
keep =
    oneToOne identity identity


{-| Create a [`Morph.OneToOne`](#OneToOne)

    String.Morph.toList : MorphOrError (List Char) String error_
    String.Morph.toList =
        Morph.oneToOne String.toList String.fromList

See the type's documentation for more detail

-}
oneToOne :
    (beforeMap -> mapped)
    -> (beforeUnmap -> unmapped)
    ->
        MorphIndependently
            (beforeMap -> Result error_ mapped)
            (beforeUnmap -> unmapped)
oneToOne map unmap =
    { description = CustomDescription
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
    oneToOne identity broadenFromNarrow


{-| [`Morph`](#Morph) that when calling [`toBroad`](Morph#toBroad) always returns a given constant.

For any more complex [`toBroad`](#toBroad) process, use [`oneToOne`](#oneToOne)

-}
broad :
    broadConstant
    ->
        MorphIndependently
            (beforeNarrow_ -> Result error_ ())
            (() -> broadConstant)
broad broadConstantSeed =
    oneToOne (\_ -> ()) (\() -> broadConstantSeed)


{-| Match only the specific given broad element.

Make helpers for each type of constant for convenience

    Char.Morph.only broadCharConstant =
        Morph.only String.fromChar broadCharConstant

-}
only :
    (broadConstant -> String)
    -> broadConstant
    -> Morph () broadConstant
only broadElementToString specificValidBroadElement =
    { description = OnlyDescription (specificValidBroadElement |> broadElementToString)
    , toNarrow =
        \broadValue ->
            if broadValue == specificValidBroadElement then
                () |> Ok

            else
                (broadValue |> broadElementToString)
                    |> DeadEnd
                    |> Err
    , toBroad = \() -> specificValidBroadElement
    }


{-| Create a custom morph for a value by explicitly specifying

  - a `String` description
  - `toNarrow`: a transformation that can fail with any error consistent with your other errors (so most likely a string)
  - `toBroad`: a transformation that can build the parsed value back to what a value that can be parsed

A common use-case is narrowing to one of multiple variants.
Example: only succeed with an int-like expression:

    intLike : Morph Int Number
    intLike =
        Morph.custom "int-like"
            { toBroad = IntValue
            , toNarrow =
                \value ->
                    case value of
                        IntValue intValue ->
                            intValue |> Ok

                        HexValue hexIntValue ->
                            hexIntValue |> Ok

                        FloatValue _ ->
                            "float" |> Err
            }

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
    named descriptionCustom
        { description = CustomDescription
        , toNarrow =
            \beforeToNarrow ->
                beforeToNarrow
                    |> morphTransformations.toNarrow
                    |> Result.mapError DeadEnd
        , toBroad = morphTransformations.toBroad
        }



--


{-| Define a [`Morph`](#Morph) recursively

    import Morph exposing (grab, match)
    import Int.Morph
    import Integer.Morph
    import String.Morph
    import ArraySized.Morph exposing (atLeast)
    import ArraySized
    import List.Morph
    import N exposing (n1)

    type IntList
        = End
        | Next { head : Int, tail : IntList }

    intList : MorphRow IntList Char
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
                    |> Morph.rowTry (\() -> End) (String.Morph.only "[]")
                    |> Morph.rowTry Next
                        (Morph.succeed (\h t -> { head = h, tail = t })
                            |> grab .head (Int.Morph.integer |> Morph.overRow Integer.Morph.chars)
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
                            |> grab .tail (innerIntList ())
                        )
                    |> Morph.choiceFinish
            )

    "[]"
        |> Morph.toNarrow
            (intList |> Morph.rowFinish |> Morph.over List.Morph.string)
    --> Ok End

    "1 :: []"
        |> Morph.toNarrow
            (intList |> Morph.rowFinish |> Morph.over List.Morph.string)
    --> Ok (Next { head = 1, tail = End })

    "1 :: 2 :: []"
        |> Morph.toNarrow
            (intList |> Morph.rowFinish |> Morph.over List.Morph.string)
    --> Ok (Next { head = 1, tail = Next { head = 2, tail = End } })

Without `recursive`, you would get an error like:

>     The `intList` definition is causing a very tricky infinite loop.
>
>     The `intList` value depends on itself

Read more about why this limitation exists
in [compiler hint "bad recursion"](https://github.com/elm/compiler/blob/master/hints/bad-recursion.md#tricky-recursion)
up until the end

Note: in this example you can also simply use [`Morph.broadEnd`](#broadEnd) `|> over` [`Morph.untilNext`](#untilNext)

More notes:

  - This would compile:

        intList : () -> MorphRow IntList
        intList =
            Morph.choice ...
                |> Morph.rowTry (\() -> End) ...
                |> Morph.rowTry Next
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

    (In principle, this would be possible btw by introducing `LazyDescription (() -> Description)`
    but it's ugly when displaying because we don't have a structure name for the recursion)

-}
recursive :
    String
    ->
        ((() -> MorphIndependently (beforeToNarrow -> narrow) (beforeToBroad -> broad))
         -> MorphIndependently (beforeToNarrow -> narrow) (beforeToBroad -> broad)
        )
    -> MorphIndependently (beforeToNarrow -> narrow) (beforeToBroad -> broad)
recursive structureName morphLazy =
    let
        morphRecursive : () -> MorphIndependently (beforeToNarrow -> narrow) (beforeToBroad -> broad)
        morphRecursive () =
            morphLazy (\() -> lazy structureName morphRecursive)
    in
    named structureName (morphRecursive ())


lazy :
    String
    -> (() -> MorphIndependently (beforeToNarrow -> narrow) (beforeToBroad -> broad))
    -> MorphIndependently (beforeToNarrow -> narrow) (beforeToBroad -> broad)
lazy structureName morphLazy =
    { description = RecursiveDescription structureName (\unit -> morphLazy unit |> description)
    , toNarrow = \beforeToNarrow -> beforeToNarrow |> toNarrow (morphLazy ())
    , toBroad = \beforeToBroad -> beforeToBroad |> toBroad (morphLazy ())
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

    import Int.Morph
    import Integer.Morph
    import List.Morph
    import Morph

    ( "4", "5" )
        |> Morph.toNarrow
            (Morph.parts
                ( \x y -> { x = x, y = y }
                , \x y -> ( x, y )
                )
                |> Morph.part "x"
                    ( .x, Tuple.first )
                    (Int.Morph.integer
                        |> Morph.overRow Integer.Morph.chars
                        |> Morph.rowFinish
                        |> Morph.over List.Morph.string
                    )
                |> Morph.part "y"
                    ( .y, Tuple.second )
                    (Int.Morph.integer
                        |> Morph.overRow Integer.Morph.chars
                        |> Morph.rowFinish
                        |> Morph.over List.Morph.string
                    )
                |> Morph.partsFinish
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
                    (PartsError partError)
                    (partNarrow -> groupNarrowFurther)
            )
            (groupNarrow -> (partBroad -> groupBroadenFurther))
         ->
            PartsMorphEmptiable
                noPartNever_
                (groupBroad
                 ->
                    Result
                        (PartsError partError)
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
                (PartsError (ErrorWithDeadEnd deadEnd))
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
        { description = groupMorphInProgress.description |> PartsDescription
        , toNarrow =
            \broad_ ->
                broad_
                    |> groupMorphInProgress.toNarrow
                    |> Result.mapError PartsError
        , toBroad = groupMorphInProgress.toBroad
        }


{-| Go over an additional step of [`Morph`](#Morph) on its broad type

Chaining

  - `<<` on the broad side
  - `<< Result.andThen` on the narrow side

This can be used to, for example

  - [`Morph.OneToOne`](#OneToOne) what was [narrowed](#toNarrow)
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
            ChainDescription
                { broad = morphBroad |> description
                , narrow = narrowMorph |> description
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
                    |> Result.mapError (\error -> ChainError { place = ChainPlaceBroad, error = error })
                    |> Result.andThen
                        (\beforeNarrowNarrow ->
                            beforeNarrowNarrow
                                |> toNarrow narrowMorph
                                |> Result.mapError (\error -> ChainError { place = ChainPlaceNarrow, error = error })
                        )
        }


{-| `OneToOne a <-> b`
by swapping the functions [`map`](#mapTo) <-> [`unmap`](#toBroad).

    import Morph
    import String.Morph

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
        Morph.oneToOne Stack.toTopBelow Stack.fromTopBelow

[`toBroad`](#toBroad) `...` is equivalent to `mapTo (... |> invert)`.

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
            InverseDescription (translate_ |> description)
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
                { startsDownInBroadList = untilError.startsDownInBroadList
                , endError =
                    untilError.endError |> deadEndMap deadEndChange
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

            PartsError parts_ ->
                parts_
                    |> Stack.map
                        (\_ partError ->
                            { index = partError.index
                            , error = partError.error |> deadEndMap deadEndChange
                            }
                        )
                    |> PartsError

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

            CountAndExactlyElementSequenceError countAndElementsError ->
                (case countAndElementsError of
                    CountError countError ->
                        countError |> deadEndMap deadEndChange |> CountError

                    ExactlyCountElementSequenceError elementSequenceError ->
                        { startsDownInBroadList = elementSequenceError.startsDownInBroadList
                        , error = elementSequenceError.error |> deadEndMap deadEndChange
                        }
                            |> ExactlyCountElementSequenceError
                )
                    |> CountAndExactlyElementSequenceError


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

            PartsError parts_ ->
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

            CountAndExactlyElementSequenceError countAndElementsError ->
                case countAndElementsError of
                    CountError countError ->
                        countError |> deadEndNever

                    ExactlyCountElementSequenceError sequenceError ->
                        sequenceError.error |> deadEndNever


{-| Change the potential [`Error`](#Error). This is usually used with either

  - [`deadEndNever : ErrorWithDeadEnd Never -> any_`](#deadEndNever)
      - allows you to for example annotate a
        [`MorphOrError narrow broad (ErrorWithDeadEnd never_)`](#MorphOrError)
        as
        `MorphOrError narrow broad never_`
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
        oneToOneOn ( List.map, List.map ) elementTranslate

-}
oneToOneOn :
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
oneToOneOn ( structureMap, structureUnmap ) elementTranslate =
    { description = CustomDescription
    , toNarrow =
        \broad_ ->
            broad_
                |> structureMap (mapTo elementTranslate)
                |> Ok
    , toBroad =
        structureUnmap (toBroad elementTranslate)
    }



-- row


{-| Parser-printer:

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

    import Morph exposing (MorphRow, atLeast, match, Morph.succeed, grab)
    import Char.Morph as Char
    import String.Morph as Text exposing (number)
    -- from lue-bird/elm-no-record-type-alias-constructor-function
    import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)

    type alias Point =
        -- makes `Point` constructor function unavailable
        RecordWithoutConstructorFunction
            { x : Float
            , y : Float
            }

    -- successful parsing looks like
    "(2.71, 3.14)"
        |> Morph.toNarrow
            (point
                |> Morph.rowFinish
                |> Morph.over List.Morph.sting
            )
    --> Ok { x = 2.71, y = 3.14 }

    -- building always works
    { x = 2.71, y = 3.14 }
        |> Morph.toBroad
            (point
                |> Morph.rowFinish
                |> Morph.over List.Morph.sting
            )
    --> "( 2.71, 3.14 )"

    point : MorphRow Point Char
    point =
        Morph.succeed (\x y -> { x = x, y = y })
            |> match (String.Morph.only "(")
            |> match
                (broad [ () ]
                    |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                )
            |> grab .x Decimal.Morph.chars
            |> match
                (broad []
                    |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                )
            |> match (String.Morph.only ",")
            |> match
                (broad [ () ]
                    |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                )
            |> grab .y Decimal.Morph.chars
            |> match
                (broad [ () ]
                    |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                )
            |> match (String.Morph.only ")")

    "(2.71, x)"
        |> Morph.toNarrow
            (point
                |> Morph.rowFinish
                |> Morph.over List.Morph.sting
            )
        |> Result.toMaybe
    --> Nothing

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

  - 👍 errors will always show all options and why they failed,
    showing those that came the furthest first

  - 👎 performs worse as there are more [possibilities](Morph#try) to parse to know it failed

-}
type alias MorphRow narrow broadElement =
    MorphIndependently
        (List broadElement
         ->
            Result
                Error
                { narrow : narrow
                , broad : List broadElement
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
type alias MorphRowIndependently narrow beforeToBroad broadElement =
    MorphIndependently
        (List broadElement
         ->
            Result
                Error
                { narrow : narrow
                , broad : List broadElement
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
    import List.Morph
    import String.Morph

    -- can match any character
    "a"
        |> Morph.toNarrow
            (Morph.keep |> Morph.one |> Morph.rowFinish |> Morph.over List.Morph.string)
    --> Ok 'a'

    "#"
        |> Morph.toNarrow
            (Morph.keep |> Morph.one |> Morph.rowFinish |> Morph.over List.Morph.string)
    --> Ok '#'

    -- only fails if we run out of inputs
    ""
        |> Morph.toNarrow
            (Morph.keep |> Morph.one |> Morph.rowFinish |> Morph.over List.Morph.string)
        |> Result.toMaybe
    --> Nothing

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
                    [] ->
                        "end of input" |> DeadEnd |> Err

                    nextBroadElement :: afterNextBroadElement ->
                        nextBroadElement
                            |> toNarrow morph
                            |> Result.map
                                (\narrowNarrow ->
                                    { narrow = narrowNarrow
                                    , broad = afterNextBroadElement
                                    }
                                )
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

    import Morph
    import String.Morph
    import Integer.Morph
    import Int.Morph
    import List.Morph
    -- from lue-bird/elm-no-record-type-alias-constructor-function
    import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)

    type alias Point =
        -- makes `Point` function unavailable:
        RecordWithoutConstructorFunction
            { x : Int
            , y : Int
            }

    point : MorphRow Point Char
    point =
        Morph.succeed (\x y -> { x = x, y = y })
            |> grab .x (Int.Morph.integer |> Morph.overRow Integer.Morph.chars)
            |> match (String.Morph.only ",")
            |> grab .y (Int.Morph.integer |> Morph.overRow Integer.Morph.chars)

    "12,34"
        |> Morph.toNarrow
            (point |> Morph.rowFinish |> Morph.over List.Morph.string)
    --> Ok { x = 12, y = 34 }


### example: infix-separated elements

    Morph.succeed Stack.topBelow
        |> grab Stack.top element
        |> grab (Stack.removeTop >> Stack.toList)
            (Morph.whilePossible
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
    -> MorphRowIndependently narrowConstant beforeBroaden_ broadElement_
succeed narrowConstant =
    { description = SucceedDescription
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
    (groupNarrow -> partNextBeforeToBroad)
    -> (partNextNarrow -> (groupNarrowConstruct -> groupNarrowConstructChanged))
    -> MorphRowIndependently partNextNarrow partNextBeforeToBroad broadElement
    ->
        (MorphRowIndependently
            groupNarrowConstruct
            groupNarrow
            broadElement
         ->
            MorphRowIndependently
                groupNarrowConstructChanged
                groupNarrow
                broadElement
        )
next partAccess partChange nextMorphRow =
    \groupMorphRowSoFar ->
        { description =
            SequenceDescription
                { early = groupMorphRowSoFar |> description
                , late = nextMorphRow |> description
                }
        , toNarrow =
            \broad_ ->
                case broad_ |> toNarrow groupMorphRowSoFar of
                    Err earlyError ->
                        SequenceError
                            { place = SequencePlaceEarly
                            , error = earlyError
                            , startDownInBroadList = broad_ |> List.length
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
                                    { place = SequencePlaceLate
                                    , error = lateError
                                    , startDownInBroadList = result.broad |> List.length
                                    }
                                    |> Err
        , toBroad =
            \groupNarrow ->
                groupNarrow
                    |> toBroad groupMorphRowSoFar
                    |> Rope.prependTo
                        (groupNarrow
                            |> partAccess
                            |> toBroad nextMorphRow
                        )
        }


{-| Take what we get from [converting](#MorphRow) the next section
and channel it back up to the [`Morph.succeed`](Morph#succeed) grouping
-}
grab :
    (groupNarrow -> partNextBeforeToBroad)
    -> MorphRowIndependently partNextNarrow partNextBeforeToBroad broadElement
    ->
        (MorphRowIndependently
            (partNextNarrow -> groupNarrowFurther)
            groupNarrow
            broadElement
         ->
            MorphRowIndependently
                groupNarrowFurther
                groupNarrow
                broadElement
        )
grab partAccess grabbedNextMorphRow =
    \groupMorphRowSoFar ->
        groupMorphRowSoFar |> next partAccess (|>) grabbedNextMorphRow


{-| Require values to be present next to continue but ignore the result.
On the parsing side, this is often called "skip" or "drop", `elm/parser` uses `|.`

    import String.Morph
    import List.Morph
    import Morph exposing (match, grab)
    import AToZ.Morph
    import AToZ exposing (AToZ(..))

    -- parse a simple email, but we're only interested in the username
    "user@example.com"
        |> Morph.toNarrow
            (Morph.succeed (\userName -> { username = userName })
                |> grab .username
                    (Morph.whilePossible (AToZ.Morph.lowerChar |> Morph.one))
                |> match (String.Morph.only "@")
                |> match
                    (broad [ E, X, A, M, P, L, E ]
                        |> Morph.overRow
                            (Morph.whilePossible (AToZ.Morph.lowerChar |> Morph.one))
                    )
                |> match (String.Morph.only ".com")
                |> Morph.rowFinish
                |> Morph.over List.Morph.string
            )
    --> Ok { username = [ U, S, E, R ] }

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


{-| Chain to an even more broad type.

[`Morph.overRow`](#overRow)
is nice to translate the narrow result your [`MorphRow`](#MorphRow) produces:

    import Morph
    import String.Morph
    import Integer.Morph
    import Int.Morph
    import List.Morph

    "12"
        |> Morph.toNarrow
            (Int.Morph.integer
                |> Morph.overRow Integer.Morph.chars
                |> Morph.rowFinish
                |> Morph.over List.Morph.string
            )
    --> Ok 12

Try to keep [`Morph.overRow`](#overRow) filters/validations to a minimum to get

  - a better error description out of the box
  - a more descriptive and correct type
  - building invalid values becomes impossible
  - a morph that actually, constructively describes the format

```
import Morph
import N exposing (n0, n9)
import String.Morph
import List.Morph

"2-2"
    |> Morph.toNarrow
        -- two digits should be at most 9
        -- and we're only interested in the sum
        (N.Morph.in_ ( n0, n9 )
            |> Morph.overRow
                (Morph.succeed (\a b -> a |> N.add b)
                    |> Morph.grab (\_ -> n0 |> N.maxTo n9) (N.Morph.inChar ( n0, n9 ))
                    |> Morph.match (String.Morph.only "-")
                    |> Morph.grab identity (N.Morph.inChar ( n0, n9 ))
                )
            |> Morph.rowFinish
            |> Morph.over List.Morph.string
        )
--> Ok (n4 |> N.minTo n0 |> N.maxTo n9)
```

If this example seems really obscure then I can only agree with you.
Have a better example showing a valid use-case? → PR

-}
overRow :
    MorphRowIndependently beforeToNarrow beforeToBroad broadElement
    ->
        (MorphIndependently
            (beforeToNarrow -> Result Error narrow)
            (beforeBeforeBroaden -> beforeToBroad)
         -> MorphRowIndependently narrow beforeBeforeBroaden broadElement
        )
overRow morphRowBeforeMorph =
    \narrowMorph ->
        { description =
            ChainDescription
                { broad = morphRowBeforeMorph |> description
                , narrow = narrowMorph |> description
                }
        , toNarrow =
            \broad_ ->
                case broad_ |> toNarrow morphRowBeforeMorph of
                    Err beforeError ->
                        ChainError { place = ChainPlaceBroad, error = beforeError } |> Err

                    Ok beforeToNarrow ->
                        case beforeToNarrow.narrow |> toNarrow narrowMorph of
                            Err error ->
                                ChainError { place = ChainPlaceNarrow, error = error } |> Err

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

    decoderNameSubject : MorphRow String Char
    decoderNameSubject =
        String.Morph.list
            |> Morph.over Morph.broadEnd
            |> Morph.overRow
                (Morph.until
                    { end = String.Morph.only "Decoder"
                    , element = Morph.keep |> Morph.one
                    }
                )

You might think: Why not use

    decoderNameSubject : MorphRow (List Char) Char
    decoderNameSubject =
        Morph.whilePossible (Morph.keep |> Morph.one)
            |> Morph.match (String.Morph.only "Decoder")

Problem is: This will never succeed.
`whilePossible (Morph.keep |> Morph.one)` always goes on.
We never reach the necessary [`match`](#match).

-}
broadEnd : Morph (List beforeEndElement) { end : (), beforeEnd : List beforeEndElement }
broadEnd =
    oneToOne .beforeEnd
        (\before_ -> { beforeEnd = before_, end = () })


{-| See if we have an `end`, if not, morph an `element`. Repeat.

An example: going through all declarations, which one is a decoder and for what?

    import String.Morph
    import List.Morph

    "userDecoder"
        |> Morph.toNarrow
            (decoderNameSubject
                |> Morph.rowFinish
                |> Morph.over List.Morph.string
            )
    --> Ok "user"

    decoderNameSubject : MorphRow String Char
    decoderNameSubject =
        String.Morph.list
            |> Morph.over Morph.broadEnd
            |> Morph.overRow
                (Morph.untilNext
                    { end =
                        Morph.succeed ()
                            |> match (String.Morph.only "Decoder")
                            |> match Morph.end
                    , element = Morph.keep |> Morph.one
                    }
                )

See [`broadEnd`](#broadEnd).

Notice the [`Morph.end`](#end) which makes "userDecoders" fail.

**If you still have input after the end element in `untilNext`, use [`untilLast`](#untilLast)**

If you need to carry information to the next element (which is super rare), try [`untilNextFold`](#untilNextFold)

-}
untilNext :
    { end : MorphRow endElement broadElement
    , element : MorphRow element broadElement
    }
    ->
        MorphRow
            { end : endElement
            , beforeEnd : List element
            }
            broadElement
untilNext config =
    untilNextFold
        { element = \() -> config.element
        , end = config.end
        , initial = ()
        , fold = \_ () -> ()
        }


{-| Keep on parsing `element`s until you encounter an `end` element.
This behavior is just like [`untilNext`](#untilNext).

In addition, [`untilNextFold`](#untilNextFold) carries accumulated "status" information
to the next element morph where you can decide how to proceed.

If that sounds complicated, then you don't need it.
Sadly some formats like midi want to save space by making you remember stuff about past events.

The fact that a morph for this case exist is pretty neat.
But because a few functions are involved,
its [description](#description) can be less nice than you're used to from other structures.
Maybe add some more context via [`Morph.named`](#named)

-}
untilNextFold :
    { end : MorphRow endElement broadElement
    , element : folded -> MorphRow element broadElement
    , initial : folded
    , fold : element -> (folded -> folded)
    }
    ->
        MorphRow
            { end : endElement
            , beforeEnd : List element
            }
            broadElement
untilNextFold config =
    let
        commit =
            oneToOne
                (\beforeAndEnd ->
                    { end = beforeAndEnd.end
                    , beforeEnd = beforeAndEnd.beforeEnd |> Stack.toList |> List.reverse
                    }
                )
                (\beforeAndEnd ->
                    { end = beforeAndEnd.end
                    , beforeEnd = beforeAndEnd.beforeEnd |> Stack.fromList
                    }
                )
                |> over
                    (oneToOne
                        (\beforeAndEnd ->
                            { end = beforeAndEnd.end
                            , beforeEnd =
                                beforeAndEnd.beforeEnd |> Emptiable.map .stack
                            }
                        )
                        (\beforeAndEnd ->
                            { beforeEnd =
                                beforeAndEnd.beforeEnd
                                    |> Emptiable.map (\before_ -> { stack = before_, folded = config.initial })
                            , end = beforeAndEnd.end
                            }
                        )
                    )

        element =
            \state ->
                case state of
                    Emptiable.Empty _ ->
                        config.initial |> config.element

                    Emptiable.Filled folded ->
                        folded.folded |> config.element

        fold =
            oneToOne
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
    in
    { description =
        UntilNextDescription
            { element = config.element config.initial |> description
            , end = config.end |> description
            }
    , toBroad =
        let
            step :
                { previous : Emptiable { folded : folded, stack : Stacked element } Possibly
                , rest : Emptiable { folded : folded, stack : Stacked element } Possibly
                }
                -> Rope broadElement
            step state =
                case state.rest of
                    Emptiable.Empty _ ->
                        Rope.empty

                    Emptiable.Filled folded ->
                        let
                            ( elementBroad, rest ) =
                                folded |> toBroad fold

                            previousWithElement =
                                ( elementBroad, state.previous ) |> mapTo fold
                        in
                        step { previous = previousWithElement |> Emptiable.filled, rest = rest }
                            |> Rope.prependTo
                                (elementBroad
                                    |> toBroad
                                        (element
                                            (previousWithElement |> Emptiable.filled)
                                        )
                                )
        in
        \beforeToBroad ->
            let
                uncommitted =
                    beforeToBroad |> toBroad commit
            in
            step { previous = Emptiable.empty, rest = uncommitted.beforeEnd }
                |> Rope.prependTo
                    (uncommitted.end
                        |> toBroad config.end
                    )
    , toNarrow =
        let
            stepFrom :
                Emptiable { folded : folded, stack : Stacked element } Possibly
                ->
                    (List broadElement
                     ->
                        Result
                            (UntilError Error)
                            { broad : List broadElement
                            , narrow : { beforeEnd : List element, end : endElement }
                            }
                    )
            stepFrom state =
                \beforeToNarrow ->
                    let
                        continue :
                            { narrow : element, broad : List broadElement }
                            -> Result (UntilError Error) { broad : List broadElement, narrow : { beforeEnd : List element, end : endElement } }
                        continue elementParsed =
                            let
                                foldedWithStepped =
                                    case state of
                                        Emptiable.Empty _ ->
                                            ( elementParsed.narrow, Emptiable.empty ) |> mapTo fold

                                        Emptiable.Filled folded ->
                                            ( elementParsed.narrow, folded |> Emptiable.filled ) |> mapTo fold
                            in
                            case elementParsed.broad |> stepFrom (foldedWithStepped |> Emptiable.filled) of
                                Err error ->
                                    { error
                                        | startsDownInBroadList =
                                            error.startsDownInBroadList
                                                |> Stack.onTopLay (elementParsed.broad |> List.length)
                                    }
                                        |> Err

                                Ok result ->
                                    result |> Ok
                    in
                    case beforeToNarrow |> toNarrow config.end of
                        Err endError ->
                            case beforeToNarrow |> toNarrow (element state) of
                                Err elementError ->
                                    { elementError = elementError
                                    , endError = endError
                                    , startsDownInBroadList = beforeToNarrow |> List.length |> Stack.one
                                    }
                                        |> Err

                                Ok elementParsed ->
                                    continue elementParsed

                        Ok endElement ->
                            { narrow =
                                { end = endElement.narrow, beforeEnd = state }
                                    |> mapTo commit
                            , broad = endElement.broad
                            }
                                |> Ok
        in
        \beforeToNarrow ->
            beforeToNarrow
                |> stepFrom Emptiable.empty
                |> Result.mapError
                    (\error ->
                        UntilError
                            { error
                                | startsDownInBroadList =
                                    error.startsDownInBroadList
                                        |> Stack.reverse
                            }
                    )
    }


{-| Keep on parsing `element`s until you encounter an `end` with no `element`s after it.

    import String.Morph
    import List.Morph
    import AToZ.Morph
    import AToZ exposing (..)

    "listDecoder userDecoder"
        |> Morph.toNarrow
            (Morph.succeed (\called arg -> { called = called, arg = arg })
                |> Morph.grab .called decoderNameSubject
                |> Morph.match (String.Morph.only " ")
                |> Morph.grab .arg decoderNameSubject
                |> Morph.rowFinish
                |> Morph.over List.Morph.string
            )
    --> Ok
    -->     { called = [ L, I, S, T ] |> List.map (\l -> { case_ = AToZ.CaseLower, letter = l })
    -->     , arg = [ U, S, E, R ] |> List.map (\l -> { case_ = AToZ.CaseLower, letter = l })
    -->     }

    decoderNameSubject : MorphRow (List { case_ : AToZ.Case, letter : AToZ }) Char
    decoderNameSubject =
        Morph.broadEnd
            |> Morph.overRow
                (Morph.untilLast
                    { end = String.Morph.only "Decoder"
                    , element = AToZ.Morph.char |> Morph.one
                    }
                )

See [`broadEnd`](#broadEnd).

Fun fact: This use-case was the original motivation for creating `elm-morph`.

If you just want to repeat elements until the next `end` element regardless of whether there are
more elements after it, use [`untilNext`](#untilNext).

If you need to carry information to the next element (which is super rare), try [`untilLastFold`](#untilLastFold)

-}
untilLast :
    { element : MorphRow element broadElement
    , end : MorphRow end broadElement
    }
    -> MorphRow { end : end, beforeEnd : List element } broadElement
untilLast config =
    untilLastFold
        { element = \() -> config.element
        , end = config.end
        , initial = ()
        , fold = \_ () -> ()
        }


{-| Just like [`untilLast`](#untilLast):
Keep on parsing `element`s until you encounter an `end` with no `element`s after it.

In addition, [`untilLastFold`](#untilLastFold) carries accumulated "status" information
to the next element morph where you can decide how to proceed.

If that sounds complicated, then you don't need it.
Sadly some formats like midi want to save space by making you remember stuff about past events.

The fact that a morph for this case exist is pretty neat.
But because a few functions are involved,
its [description](#description) can be less nice than you're used to from other structures.
Maybe add some more context via [`Morph.named`](#named)

-}
untilLastFold :
    { element : folded -> MorphRow element broadElement
    , end : MorphRow end broadElement
    , initial : folded
    , fold : element -> (folded -> folded)
    }
    -> MorphRow { end : end, beforeEnd : List element } broadElement
untilLastFold config =
    { description =
        UntilLastDescription
            { end = config.end |> description
            , element = config.element config.initial |> description
            }
    , toNarrow =
        let
            step foldedSoFar beforeToNarrow =
                case beforeToNarrow |> toNarrow (config.element foldedSoFar) of
                    Ok element ->
                        case element.broad |> step (foldedSoFar |> config.fold element.narrow) of
                            Ok tail ->
                                Ok
                                    { narrow =
                                        { end = tail.narrow.end
                                        , beforeEnd = element.narrow :: tail.narrow.beforeEnd
                                        }
                                    , broad = tail.broad
                                    }

                            Err tailError ->
                                case beforeToNarrow |> toNarrow config.end of
                                    Ok endParsed ->
                                        Ok
                                            { broad = endParsed.broad
                                            , narrow =
                                                { end = endParsed.narrow, beforeEnd = [] }
                                            }

                                    Err endError ->
                                        Err
                                            { elementError = tailError.elementError
                                            , endError = endError
                                            , startsDownInBroadList =
                                                tailError.startsDownInBroadList
                                                    |> Stack.onTopLay (beforeToNarrow |> List.length)
                                            }

                    Err elementError ->
                        case beforeToNarrow |> toNarrow config.end of
                            Ok endParsed ->
                                Ok
                                    { broad = endParsed.broad
                                    , narrow =
                                        { end = endParsed.narrow, beforeEnd = [] }
                                    }

                            Err endError ->
                                Err
                                    { elementError = elementError
                                    , endError = endError
                                    , startsDownInBroadList =
                                        beforeToNarrow
                                            |> List.length
                                            |> Stack.one
                                    }
        in
        \initialBeforeToBroad ->
            initialBeforeToBroad
                |> step config.initial
                |> Result.mapError UntilError
    , toBroad =
        \beforeAndEnd ->
            beforeAndEnd.beforeEnd
                |> List.foldl
                    (\element soFar ->
                        { folded = soFar.folded |> config.fold element
                        , rope =
                            soFar.rope
                                |> Rope.prependTo
                                    (element |> toBroad (config.element soFar.folded))
                        }
                    )
                    { folded = config.initial, rope = Rope.empty }
                |> .rope
                |> Rope.prependTo (beforeAndEnd.end |> toBroad config.end)
    }


{-| Keep going until an element fails, just like [`atLeast n0`](ArraySized-Morph#atLeast).
If you want a to morph a `List` instead of an `ArraySized ... (Min (On N0))`, you might as well use [`whilePossible`](#whilePossible)

    ArraySized.Morph.toList
        |> Morph.overRow (ArraySized.Morph.atLeast n0)

If you need to carry information to the next element (which is super rare), try [`whilePossibleFold`](#whilePossibleFold)

-}
whilePossible :
    MorphRowIndependently elementNarrow elementBeforeToBroad broadElement
    -> MorphRowIndependently (List elementNarrow) (List elementBeforeToBroad) broadElement
whilePossible element =
    { description = WhilePossibleDescription (element |> description)
    , toBroad =
        \beforeToBroad ->
            beforeToBroad
                |> List.map (toBroad element)
                |> Rope.fromList
                |> Rope.concat
    , toNarrow =
        let
            step :
                List broadElement
                ->
                    { broad : List broadElement
                    , narrow : List elementNarrow
                    }
            step beforeToNarrow =
                case beforeToNarrow |> toNarrow element of
                    Err _ ->
                        { broad = beforeToNarrow, narrow = [] }

                    Ok stepped ->
                        let
                            after : { broad : List broadElement, narrow : List elementNarrow }
                            after =
                                stepped.broad |> step
                        in
                        { narrow = stepped.narrow :: after.narrow
                        , broad = after.broad
                        }
        in
        \beforeToNarrow ->
            beforeToNarrow |> step |> Ok
    }


{-| Keep going until an element fails, just like [`whilePossible`](#whilePossible)/[`atLeast n0`](ArraySized-Morph#atLeast).
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
    -> MorphRow (List element) broadElement
whilePossibleFold config =
    oneToOne
        (\state ->
            state
                |> Emptiable.mapFlat
                    (\folded ->
                        folded
                            |> .stack
                            |> Emptiable.emptyAdapt (\_ -> Possible)
                            |> Stack.reverse
                    )
                |> Stack.toList
        )
        (\list ->
            list
                |> Stack.fromList
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
                    oneToOne
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
    , fold : OneToOne folded ( element, Emptiable folded Possibly )
    }
    -> MorphRow (Emptiable folded Possibly) broadElement
whilePossibleAsFold config =
    { description =
        WhilePossibleDescription
            (config.element Emptiable.empty |> description)
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
                    (List broadElement
                     ->
                        { broad : List broadElement
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


{-| Only succeeds when there are no remaining broad input elements afterwards.

This is not required for [`Morph.toNarrow`](#toNarrow) to succeed.

The only use I can think of is when checking for a line ending:

    type LineEnding
        = LineBreak
        | EndOfFile

    lineEnding : MorphRow () Char
    lineEnding =
        -- always prefer a line-break before the end of the file
        Morph.broad LineBreak
            |> Morph.overRow
                (Morph.choice
                    (\lineBreak endOfFile lineEndingChoice ->
                        case lineEndingChoice of
                            LineBreak ->
                                lineBreak ()

                            EndOfFile ->
                                endOfFile ()
                    )
                    |> Morph.rowTry (\() -> LineBreak) (String.Morph.only "\n")
                    |> Morph.rowTry (\() -> EndOfFile) Morph.end
                )

-}
end : MorphRow () broadElement_
end =
    { description = EndDescription
    , toNarrow =
        \broad_ ->
            case broad_ of
                [] ->
                    { narrow = ()
                    , broad = []
                    }
                        |> Ok

                _ :: _ ->
                    "remaining input" |> DeadEnd |> Err
    , toBroad =
        \() -> Rope.empty
    }


{-| Final step before running a [`MorphRow`](#MorphRow),
transforming it into a [`Morph`](#Morph) on the full stack of input elements.

    fromString =
        Morph.toNarrow
            (Point.morphChars
                |> Morph.rowFinish
                |> Morph.over List.Morph.string
            )

Once you've called `|> rowFinish` there is no (performant) way to convert it back to a [`MorphRow`](#MorphRow),
so delay it for as long as you can.

-}
rowFinish :
    MorphRow narrow broadElement
    -> Morph narrow (List broadElement)
rowFinish =
    \morphRow ->
        let
            morphRowThenEnd : MorphRow narrow broadElement
            morphRowThenEnd =
                succeed (\before_ -> before_) |> grab (\before_ -> before_) morphRow |> match end
        in
        { description = morphRowThenEnd |> description
        , toNarrow =
            \broadElements ->
                broadElements
                    |> toNarrow morphRowThenEnd
                    |> Result.map .narrow
        , toBroad =
            \narrow -> narrow |> toBroad morphRowThenEnd |> Rope.toList
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
            |> Morph.rowTry Return
                (returnChar |> Morph.one)
            |> Morph.rowTry (\() -> InputEnd)
                Morph.end
            |> Morph.choiceFinish

    -- match a blank
    "\n\t "
        |> Morph.toNarrow
            (Morph.whilePossible blank
                |> Morph.rowFinish
                |> Morph.over List.Morph.string
            )
    --> Ok [ Return NewLine, Tab, Space ]

    -- anything else makes it fail
    'a' |> Morph.toNarrow blank |> Result.toMaybe
    --> Nothing

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

Use [`tryTopToBottom`](#tryTopToBottom) if you have a "dynamic" list of equal type morphs.
An example is defined variable names

    import Stack
    import String.Morph

    Morph.oneToOne .info (\info -> { tag = "±", info = info })
        |> Morph.over
            (Morph.tryTopToBottom String.Morph.only
                (Stack.topBelow "±" [ "pm", "plusminus" ])
            )

That looks really cursed. Let me try to explain:

Let's call these stacked elements like `"pm"` and `"plusminus"` "tags".
They are used to build a morph for each tag, tried in the order in the stack,
here `String.Morph.only`.

Let's call what you morph to `info`, in this case it's `()`
because `String.Morph.only : MorphRow () Char` only has one valid value.

Now how can the printer choose which of the tags should be used?
Someone needs to tell it.
The most generic way to do so is by setting the printer as

    toBroad : { tag, info } -> broad

because if we have the `tag`, we know which morph's printer to use!

The other direction, narrowing, is similar.

Say you wanted a morph with a dynamic stack of letters.

    Morph.tryTopToBottom Char.Morph.only yourLetters

if we just return what the tried morph with a matching tag returns, we'd have

    toNarrow : beforeToNarrow -> Result ... ()

That does not seem very useful, we want to know what possibility worked as well!

    toNarrow : beforeToNarrow -> Result ... { info = (), tag = Char }

Now all that's left to do is wire everything
so that the narrow thing doesn't show the empty info:

    Morph.oneToOne .tag (\tag -> { tag = tag, info = () })
        |> Morph.over
            (Morph.tryTopToBottom Char.Morph.only yourLetters)

This is not exactly pretty but it's versatile at the very least.
We can provide default tags (see example at the top) and
we can set the tag as the narrow value when the `info = ()`.

I welcome every question on github or @lue on slack.

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
tryTopToBottom :
    (tag
     ->
        MorphIndependently
            (beforeToNarrow
             -> Result (ErrorWithDeadEnd deadEnd) possibilityInfoNarrow
            )
            (possibilityInfoBeforeToBroad -> broad)
    )
    -> Emptiable (Stacked tag) Never
    ->
        MorphIndependently
            (beforeToNarrow
             -> Result (ErrorWithDeadEnd deadEnd) { tag : tag, info : possibilityInfoNarrow }
            )
            ({ tag : tag, info : possibilityInfoBeforeToBroad } -> broad)
tryTopToBottom traversePossibility tags =
    { description =
        tags
            |> Stack.map (\_ tag -> tag |> traversePossibility |> description)
            |> ChoiceDescription
    , toNarrow =
        \beforeToNarrow ->
            beforeToNarrow
                |> tryTopToBottomToNarrow
                    (\tag onTagBeforeToNarrow ->
                        onTagBeforeToNarrow
                            |> toNarrow (traversePossibility tag)
                            |> Result.map (\info -> { tag = tag, info = info })
                    )
                    tags
    , toBroad =
        \possibilityBeforeToBroad ->
            let
                possibilityBroad =
                    possibilityBeforeToBroad

                possibility =
                    possibilityBroad |> .tag |> traversePossibility
            in
            possibilityBroad |> .info |> possibility.toBroad
    }


tryTopToBottomToNarrow :
    (tag
     -> beforeToNarrow
     -> Result (ErrorWithDeadEnd deadEnd) narrow
    )
    -> Emptiable (Stacked tag) Never
    ->
        (beforeToNarrow
         -> Result (ErrorWithDeadEnd deadEnd) narrow
        )
tryTopToBottomToNarrow traverseTry possibilities =
    \beforeToNarrow ->
        possibilities
            |> Stack.foldFromOne
                (\top ->
                    beforeToNarrow
                        |> traverseTry top
                        |> Result.mapError Stack.one
                )
                Up
                (\elementForMorph resultSoFar ->
                    resultSoFar
                        |> recoverTry
                            (\errorsSoFar ->
                                beforeToNarrow
                                    |> traverseTry elementForMorph
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

    import Char.Morph
    import AToZ.Morph
    import Morph
    import AToZ exposing (AToZ)

    type UnderscoreOrLetter
        = Underscore
        | Letter AToZ

    underscoreOrLetter : Morph UnderscoreOrLetter Char
    underscoreOrLetter =
        Morph.choice
            (\underscore letter underscoreOrLetterChoice ->
                case underscoreOrLetterChoice of
                    Underscore ->
                        underscore ()
                    Letter aToZ ->
                        letter aToZ
            )
            |> Morph.try (\() -> Underscore) (Char.Morph.only '_')
            |> Morph.try Letter AToZ.Morph.lowerChar
            |> Morph.choiceFinish

    -- try the first possibility
    '_' |> Morph.toNarrow underscoreOrLetter
    --> Ok Underscore

    -- if it fails, try the next
    'a' |> Morph.toNarrow underscoreOrLetter
    --> Ok (Letter AToZ.A)

    -- if none work, we get the error from all possible steps
    '1'
        |> Morph.toNarrow underscoreOrLetter
        |> Result.toMaybe
    --> Nothing

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
concluding the printer with [`Morph.choiceFinish`](#choiceFinish)

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
a simple [`oneToOne`](Morph#oneToOne) also does the job

    signInternal : MorphOrError Sign Sign.Internal.Sign error_
    signInternal =
        Morph.oneToOne
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
            choiceMorphComplete.description
                |> VariantsDescription
        , toNarrow =
            \beforeToNarrow ->
                beforeToNarrow
                    |> choiceMorphComplete.toNarrow
                    |> Result.mapError VariantError
        , toBroad = choiceMorphComplete.toBroad
        }



-- MorphRow


{-| Possibly incomplete [`MorphRow`](#MorphRow) for a choice/variant type/custom type.
See [`Morph.choice`](Morph#choice), [`Morph.rowTry`](#try), [`Morph.choiceFinish`](#choiceFinish)
-}
type alias ChoiceMorphRowEmptiable noTryPossiblyOrNever choiceNarrow choiceBroaden broadElement =
    RecordWithoutConstructorFunction
        { description :
            Emptiable (Stacked Description) noTryPossiblyOrNever
        , toNarrow :
            List broadElement
            ->
                Result
                    (-- tries
                     Emptiable (Stacked Error) noTryPossiblyOrNever
                    )
                    { narrow : choiceNarrow
                    , broad : List broadElement
                    }
        , toBroad : choiceBroaden
        }


{-| If the previous [`possibility`](#rowTry) fails
try this [`MorphRow`](#MorphRow).

> ℹ️ Equivalent regular expression: `|`


### example: fallback step if the previous step fails

    import Morph
    import N.Morph
    import ArraySized.Morph exposing (atLeast)
    import ArraySized exposing (ArraySized)
    import N exposing (n0, n1, n2, n3, n9, N, Min, In, On, N1, N0, N9)
    import AToZ exposing (AToZ(..))
    import AToZ.Morph
    import List.Morph

    type AlphaNum
        = Digits (ArraySized (N (In (On N0) (On N9))) (Min (On N1)))
        | Letters (ArraySized AToZ (Min (On N1)))

    alphaNum : MorphRow AlphaNum Char
    alphaNum =
        Morph.choice
            (\letter digit alphaNumChoice ->
                case alphaNumChoice of
                    Digits int ->
                        digit int
                    Letters char ->
                        letter char
            )
            |> Morph.rowTry Letters
                (atLeast n1 (AToZ.Morph.lowerChar |> Morph.one))
            |> Morph.rowTry Digits
                (atLeast n1 (N.Morph.inChar ( n0, n9 ) |> Morph.one))
            |> Morph.choiceFinish

    -- try letters, or else give me some digits
    "abc"
        |> Morph.toNarrow
            (alphaNum |> Morph.rowFinish |> Morph.over List.Morph.string)
    --→ Ok (Letters (ArraySized.l3 A B C |> ArraySized.minTo n1))

    -- we didn't get letters, but we still got digits
    "123" |> Morph.toNarrow (alphaNum |> Morph.rowFinish |> Morph.over List.Morph.string)
    --→ Ok
    --→     (Digits
    --→         (ArraySized.l3 (n1 |> N.minTo n0 |> N.maxTo n9) (n2 |> N.minTo n0 |> N.maxTo n9) (n3 |> N.minTo n0 |> N.maxTo n9) |> ArraySized.minTo n1)
    --→     )

    -- but if we still fail, records what failed for each possibility in the error
    "_"
        |> Morph.toNarrow (alphaNum |> Morph.rowFinish |> Morph.over List.Morph.string)
        |> Result.toMaybe
    --> Nothing


## anti-example: using `MorphRow` for what could be a `Morph` of a single element

    import Morph
    import AToZ exposing (AToZ(..))
    import Char.Morph
    import String.Morph

    type UnderscoreOrLetter
        = Underscore
        | Letter AToZ

    underscoreOrLetter : MorphRow UnderscoreOrLetter Char
    underscoreOrLetter =
        Morph.choice
            (\underscoreVariant letterVariant underscoreOrLetterNarrow ->
                case underscoreOrLetterNarrow of
                    Underscore ->
                        underscoreVariant ()
                    Letter letter ->
                        letterVariant letter
            )
            |> Morph.rowTry (\() -> Underscore) (String.Morph.only "_")
            |> Morph.rowTry Letter
                (AToZ.Morph.broadCase AToZ.CaseLower
                    |> Morph.over AToZ.Morph.char
                    |> Morph.one
                )
            |> Morph.choiceFinish

    -- try the first possibility
    "_" |> Morph.toNarrow (underscoreOrLetter |> Morph.rowFinish |> Morph.over List.Morph.string)
    --> Ok Underscore

    -- if it fails, try the next
    "a" |> Morph.toNarrow (underscoreOrLetter |> Morph.rowFinish |> Morph.over List.Morph.string)
    --> Ok (Letter A)

    -- if none work, records what failed for each possibility in the error
    "1"
        |> Morph.toNarrow (underscoreOrLetter |> Morph.rowFinish |> Morph.over List.Morph.string)
        |> Result.toMaybe
    --> Nothing

better would be

    underscoreOrLetterBetter : Morph UnderscoreOrLetter Char
    underscoreOrLetterBetter =
        Morph.choice
            (\underscoreVariant letterVariant underscoreOrLetterNarrow ->
                case underscoreOrLetterNarrow of
                    Underscore ->
                        underscoreVariant ()
                    Letter letter ->
                        letterVariant letter
            )
            |> Morph.try (\() -> Underscore) (Char.Morph.only '_')
            |> Morph.try Letter
                (AToZ.Morph.broadCase AToZ.CaseLower
                    |> Morph.over AToZ.Morph.char
                )
            |> Morph.choiceFinish

    underscoreOrLetter |> Morph.one

-}
rowTry :
    (possibilityNarrow -> choiceNarrow)
    -> MorphRowIndependently possibilityNarrow possibilityBeforeToBroad broadElement
    ->
        (ChoiceMorphRowEmptiable
            noTryPossiblyOrNever_
            choiceNarrow
            ((possibilityBeforeToBroad -> Rope broadElement)
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
rowTry possibilityToChoice possibilityMorph =
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


{-| Always the last step of a [`Morph.choice`](Morph#choice) `|>` [`Morph.try`](Morph#try) or `|>` [`Morph.rowTry`](#rowTry) builder.
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
            choiceMorphComplete.description |> ChoiceDescription
        , toNarrow =
            \beforeToNarrow ->
                beforeToNarrow
                    |> choiceMorphComplete.toNarrow
                    |> Result.mapError ChoiceError
        , toBroad =
            choiceMorphComplete.toBroad
        }
