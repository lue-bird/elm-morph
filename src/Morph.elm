module Morph exposing
    ( Morph
    , Expected(..), expectation, errorExpectationMap
    , expectationMap
    , Expectation, ExpectationWith(..), Error, ErrorWith, expect
    , validate, specific
    , broaden, narrow
    , Translate
    , translate, broadenFrom, toggle, remain
    , map, unmap
    , lazy, over
    , reverse
    , Tagged(..), TagOrValue(..)
    , group, part, GroupMorphInProgress
    , ChoiceMorphInProgress
    , choice, possibility, choiceFinish
    )

{-| Call it Codec, Conversion, PrismReversible,

@docs Morph


## fallible

@docs Expected, expectation, errorExpectationMap
@docs expectationMap
@docs Expectation, ExpectationWith, Error, ErrorWith, expect


### fallible create

@docs validate, specific


### fallible scan

@docs broaden, narrow


## translating

@docs Translate


### [`Translate`](Morph#Translate) create

@docs translate, broadenFrom, toggle, remain


### [`Translate`](Morph#Translate) scan

@docs map, unmap


## transform

@docs lazy, over


### [`Translate`](Morph#Translate) transform

@docs reverse


## step

@docs Tagged, TagOrValue


### groups

@docs group, part, GroupMorphInProgress


### choices

@docs ChoiceMorphInProgress
@docs choice, possibility, choiceFinish

-}

import Hand exposing (Empty, Hand)
import Possibly exposing (Possibly)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Stack exposing (Stacked)


{-| Morph functions to a more general format and back.

👀 type `Morph narrow broad error`:

  - example: `Morph Email String (StackFilled DeadEnd)`

  - `broaden : narrow -> broad`
      - going from a specific type to a general one
      - the result can't always be turned back successfully
      - can loose information on its way
      - example: `Email -> String`

  - `narrow : broad -> Result error narrow`
      - going from a general type to a specific one
      - the result can always be turned back successfully
      - can gain information on its way
      - example: `String -> Result (MorphRow.Error Char Email) Email`
      - the above is exactly what parser looks like!

It's this abstract. No use-case implied.
Composition, group, choice steps etc. can be defined just as well.

-}
type alias Morph narrow broad error =
    GroupMorphInProgress (narrow -> broad) narrow broad error



--


{-| An expectation that hasn't been met.
-}
type Expected expectation
    = Expected expectation


{-| The expectation that hasn't been met.

    Morph.Expected 3
        |> Morph.errorExpectation
    --> 3

-}
expectation : Expected expectation -> expectation
expectation =
    \(Expected expectation_) -> expectation_


{-| Change the expectation that hasn't been met.
-}
errorExpectationMap :
    (expectation -> expectationMapped)
    -> Expected expectation
    -> Expected expectationMapped
errorExpectationMap expectationChange =
    \(Expected expectation_) ->
        Expected (expectation_ |> expectationChange)


{-| Take what the `narrow` function [`Expected`](#Expected) and adapt it.
-}
expectationMap :
    (expectation -> expectationMapped)
    -> Morph specific general (Expected expectation)
    -> Morph specific general (Expected expectationMapped)
expectationMap expectationChange =
    \morph ->
        { broaden = morph |> broaden
        , narrow =
            \general ->
                general
                    |> (morph |> narrow)
                    |> Result.mapError
                        (errorExpectationMap expectationChange)
        }



--


{-| The function that turns `narrow` into `broad`.
-}
broaden :
    Morph narrow broad error_
    -> (narrow -> broad)
broaden =
    \morph -> morph.broaden


{-| The function that turns `broad` into `narrow` or an `error`.
-}
narrow :
    Morph specific general error
    -> (general -> Result error specific)
narrow =
    \morph -> morph.narrow


{-| Convert values of the arbitrarily chosen types `unmapped -> mapped`.

    "3456" |> map stringToList --> [ '3', '4', '5', '6' ]

-}
map : Translate unmapped mapped -> (unmapped -> mapped)
map translate_ =
    \unmapped ->
        unmapped |> broaden translate_


{-| [`reverse`](#reverse) `|> map` is equivalent.
-}
unmap : Translate unmapped mapped -> (mapped -> unmapped)
unmap translate_ =
    \mapped ->
        case mapped |> narrow translate_ of
            Ok mappedNarrow ->
                mappedNarrow

            Err error ->
                error |> never



--


{-| Filter specific values.

In general, try to narrow down the type when limiting values:
["Parse, don't validate"](https://elm-radio.com/episode/parse-dont-validate/).
That's a core idea in elm. You'll find lots of legendary resources surrounding this topic.

-}
validate :
    (value -> Result error value)
    -> Morph value value error
validate narrowConvert =
    { narrow = narrowConvert
    , broaden = identity
    }



--


{-| What went wrong.
-}
type alias Expectation atom description =
    ExpectationWith {} atom description


{-| Generic version of [`Expectation`](#Expectation) where each inner [error](#ErrorWith) has extra fields.
-}
type ExpectationWith location atom description
    = NoFail
    | MoreInput
    | NoMoreInput
    | Specific atom
    | OneIn (Hand (Stacked (ErrorWith location atom description)) Possibly Empty)


{-| [What went wrong](#Expectation), where it went wrong and maybe a custom description.
-}
type alias Error atom description =
    ErrorWith {} atom description


{-| Generic version of [`Error`](#Error) which can have extra fields.
-}
type alias ErrorWith location atom description =
    RecordWithoutConstructorFunction
        { location
            | expected : ExpectationWith location atom description
            , description : Hand (Stacked description) Possibly Empty
        }


{-| Describe the context to improve error messages.

TODO example

-}
expect :
    description
    ->
        (Morph narrow broad (ErrorWith location atom description)
         -> Morph narrow broad (ErrorWith location atom description)
        )
expect expectationCustomDescription morphToDescribe =
    { morphToDescribe
        | narrow =
            morphToDescribe.narrow
                >> Result.mapError
                    (\error ->
                        { error
                            | description =
                                error.description
                                    |> Stack.onTopLay
                                        expectationCustomDescription
                        }
                    )
    }



--


{-| [`map`](#map) & .
Limits consumed arguments to [`Morph`](#Morph)s that can `Never` fail to [`unmap`](#unmap), for example

    stringToListTranslate =
        Morph.translate String.toList String.fromList

Don't use [`Translate`](Morph#Translate) to annotate created [`translate`](#translate)s!

    Morph (List Char) String error_

Allows it to be used in more general [`Morph`](#Morph) chains where the target value can be an `error_`.

Both type arguments are really equal in "rank",
so choosing one as the `mapped` and one as the `unmapped` is rather arbitrary.

That's the reason it's a good idea to always expose 2 versions: `aToB` & `bToA`.

**!** Information can get lost on the way:

    dictToListMorph :
        Translate
            (Dict comparableKey value)
            (List ( comparableKey, value ))
    dictToListMorph =
        Morph.translate Dict.toList Dict.fromList

Still, there's no parsing to translate one state to the other.

-}
type alias Translate unmapped mapped =
    Morph unmapped mapped Never


{-| Switch between 2 opposite representations.

    toggle List.reverse

    toggle Linear.opposite

    toggle not

-}
toggle : (value -> value) -> Morph value value error_
toggle changeToOpposite =
    translate changeToOpposite changeToOpposite


{-| A [`Morph`](#Morph) that doesn't transform anything.

Same as writing:

  - [`toggle`](#toggle) `identity`
  - [`translate`](#translate) `identity identity`
  - [`validate`](#validate) `Ok`
  - `{ broaden = identity, narrow = Ok }`

-}
remain : Morph value value error_
remain =
    toggle identity


{-| Mutual `Morph` = [`Translate`](#Translate)
between representations
that have the same structural information
and can be mapped 1:1 into each other.

    stringToListMorph : Morph (List Char) String error_
    stringToListMorph =
        Morph.translate String.toList String.fromList

Examples:

  - [`Morph.Text.toList`](Morph-Text#toList), [`Morph.Text.fromList`](Morph-Text#fromList)
  - [`Array.Morph.toList`](Array-Morph#toList), [`Array.Morph.fromList`](Array-Morph#fromList)

-}
translate :
    (unmapped -> mapped)
    -> (mapped -> unmapped)
    -> Morph unmapped mapped error_
translate mapTo unmapFrom =
    { broaden = mapTo
    , narrow = \mapped -> mapped |> unmapFrom |> Ok
    }


{-| [`Morph`](#Morph) that always [`broaden`](#broaden)s to a given constant.

For any more complex [`broaden`](#broaden)ing process, use [`translate`](#translate)

-}
broadenFrom : broadConstant -> Morph () broadConstant error_
broadenFrom narrowConstant =
    translate (\() -> narrowConstant) (\_ -> ())


{-| Match only a specific broad input.
-}
specific :
    broadConstant
    -> Morph () broadConstant (Error broadConstant description_)
specific broadConstant =
    { narrow =
        \broad ->
            if broad == broadConstant then
                () |> Ok

            else
                { expected = Specific broadConstant
                , description = Hand.empty
                }
                    |> Err
    , broaden =
        \() -> broadConstant
    }



--


{-| A missed expectation on either its tag or its value.
-}
type TagOrValue tagExpectation valueExpectation
    = Tag tagExpectation
    | Value valueExpectation


{-| tag-value pair like a field or a variant.
-}
type Tagged tag value
    = Tagged tag value


{-| The most general version of [`Morph`](#Morph) where

  - [`narrow`](#narrow)ed value types can't necessarily be [`broaden`](#broaden)ed
  - [`broaden`](#broaden)ed value types can't necessarily be [`narrow`](#narrow)ed

This general form is helpful to describe a step in building an incomplete [`Morph`](#Morph).

-}
type alias GroupMorphInProgress broadenFromNarrow narrowFromBroad broad error =
    RecordWithoutConstructorFunction
        { narrow : broad -> Result error narrowFromBroad
        , broaden : broadenFromNarrow
        }


{-| Describe the [`Morph`](#Morph) of the next part in a [`group`](#group).

    group
        ( \nameFirst nameLast email ->
            { nameFirst = nameFirst, nameLast = nameLast, email = email }
        , \nameFirst nameLast email ->
            Dict.empty
                |> Dict.insert "nameFirst" nameFirst
                |> Dict.insert "nameLast" nameLast
                |> Dict.insert "email" email
        )
        |> part ( .nameFirst, Dict.get "nameFirst" ) remain
        |> part ( .nameLast, Dict.get "nameLast" ) remain
        |> part ( .email, Dict.get "email" ) emailMorph

    ( "4", "5" )
        |> narrow
            (group
                (\x y -> { x = x, y = y })
                (\x y -> ( x, y ))
                |> partOnOk ( .x, Tuple.first >> Ok )
                    (Integer.Morph.toInt |> MorphRow.over Integer.Morph.fromText)
                |> partOnOk ( .y, Tuple.second >> Ok )
                    (Integer.Morph.toInt |> MorphRow.over Integer.Morph.fromText)
            )
    --> Ok { x = 4, y = 5 }

-}
part :
    ( groupNarrow -> partNarrow
    , groupBroad -> Result partError partBroad
    )
    -> Morph partNarrow partBroad partError
    ->
        (GroupMorphInProgress
            (groupNarrow -> (partBroad -> groupBroadenFurther))
            (partNarrow -> groupNarrowFurther)
            groupBroad
            partError
         ->
            GroupMorphInProgress
                (groupNarrow -> groupBroadenFurther)
                groupNarrowFurther
                groupBroad
                partError
        )
part ( narrowPartAccess, broadPartAccess ) partMorph =
    \groupMorphSoFar ->
        { narrow =
            groupMorphSoFar.narrow
                |> narrowPart broadPartAccess (narrow partMorph)
        , broaden =
            groupMorphSoFar.broaden
                |> broadenPart narrowPartAccess (broaden partMorph)
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
    (groupBroad -> Result partError partBroad)
    -> (partBroad -> Result partError partNarrow)
    ->
        ((groupBroad -> Result partError (partNarrow -> groupNarrowFurther))
         -> (groupBroad -> Result partError groupNarrowFurther)
        )
narrowPart broadPartAccess narrowPartMorph =
    \groupMorphSoFarNarrow ->
        \groupBroad ->
            groupBroad
                |> groupMorphSoFarNarrow
                |> Result.andThen
                    (\partNarrowEat ->
                        let
                            narrowPartOrError : Result partError partNarrow
                            narrowPartOrError =
                                groupBroad
                                    |> broadPartAccess
                                    |> Result.andThen narrowPartMorph
                        in
                        narrowPartOrError
                            |> Result.map partNarrowEat
                    )


{-| Possibly incomplete [`Morph`](#Morph) to and from a choice.
See [`choice`](#choice), [`possibility`](#possibility), [`choiceFinish`](#choiceFinish)
-}
type alias ChoiceMorphInProgress choiceNarrow choiceBroad choiceBroaden possibilityExpectation =
    { narrow :
        choiceBroad
        ->
            Result
                (Expected
                    { possibilities :
                        Hand (Stacked possibilityExpectation) Possibly Empty
                    }
                )
                choiceNarrow
    , broaden : choiceBroaden
    }


{-| If the previous [`possibility`](#possibility) fails
try this [`Morph`](#Morph).

> ℹ️ Equivalent regular expression: `|`

    import MorphRow exposing (atom)
    import Morph.CharRow as Char
    import MorphRow.Error

    type UnderscoreOrLetter
        = Underscore
        | Letter Char

    underscoreOrLetter : MorphRow Char UnderscoreOrLetter
    underscoreOrLetter =
        choice
            (\underscore letter first ->
                case first of
                    Underscore ->
                        underscore ()

                    Letter char ->
                        letter char
            )
            |> possibility (\() -> Underscore) (atom '_')
            |> possibility Letter Char.letter
            |> choiceFinish

    -- try the first possibility
    "_"
        |> Text.narrowWith underscoreOrLetter
    --> Ok Underscore

    -- if it fails, try the next
    "a"
        |> Text.narrowWith underscoreOrLetter
    --> Ok 'a'

    -- if none work, we get the error from all possible steps
    "1"
        |> Text.narrowWith (onFailDown [ atom '_', Char.letter ])
        |> Result.mapError MorphRow.Error.textMessage
    --> Err "1:1: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '1'."

-}
possibility :
    (possibilityNarrow -> narrowChoice)
    ->
        Morph
            possibilityNarrow
            possibilityBroad
            possibilityError
    ->
        (ChoiceMorphInProgress
            narrowChoice
            possibilityBroad
            ((possibilityNarrow -> possibilityBroad) -> choiceBroadenFurther)
            possibilityError
         ->
            ChoiceMorphInProgress
                narrowChoice
                possibilityBroad
                choiceBroadenFurther
                possibilityError
        )
possibility possibilityToChoice possibilityMorph =
    \choiceMorphSoFar ->
        { narrow =
            \broad ->
                case broad |> choiceMorphSoFar.narrow of
                    Ok possibilityNarrowed ->
                        possibilityNarrowed |> Ok

                    Err (Expected soFarExpectation) ->
                        case broad |> narrow possibilityMorph of
                            Ok possibilityNarrow ->
                                possibilityNarrow
                                    |> possibilityToChoice
                                    |> Ok

                            Err possibilityExpectation ->
                                Expected
                                    { possibilities =
                                        soFarExpectation.possibilities
                                            |> Stack.onTopLay possibilityExpectation
                                    }
                                    |> Err
        , broaden =
            choiceMorphSoFar.broaden
                (broaden possibilityMorph)
        }


{-| Assemble a combined whole from its [parts](#part).
-}
group :
    narrowAssemble
    -> broadAssemble
    ->
        GroupMorphInProgress
            (groupNarrow_ -> broadAssemble)
            narrowAssemble
            broad_
            error_
group narrowAssemble broadAssemble =
    { narrow = \_ -> narrowAssemble |> Ok
    , broaden = \_ -> broadAssemble
    }


{-| Discriminate into [possibilities](#possibility).
-}
choice :
    choiceBroadenByPossibility
    ->
        ChoiceMorphInProgress
            choiceNarrow_
            choiceBroad_
            choiceBroadenByPossibility
            possibilityExpectation_
choice choiceBroadenDiscriminatedByPossibility =
    { narrow =
        \_ ->
            Expected { possibilities = Hand.empty } |> Err
    , broaden = choiceBroadenDiscriminatedByPossibility
    }


{-| Conclude a [`Morph.choice`](Morph#choice) |> [`Morph.possibility`](Morph#possibility) chain.
-}
choiceFinish :
    ChoiceMorphInProgress
        narrowUnion
        possibilityBroad
        (narrowUnion -> possibilityBroad)
        (Error atom possibilityExpectationCustom)
    ->
        Morph
            narrowUnion
            possibilityBroad
            (Error atom possibilityExpectationCustom)
choiceFinish =
    \choiceMorphComplete ->
        { narrow =
            narrow choiceMorphComplete
                >> Result.mapError
                    (\(Expected expectation_) ->
                        { expected = OneIn expectation_.possibilities
                        , description = Hand.empty
                        }
                    )
        , broaden =
            \narrowUnion ->
                narrowUnion |> broaden choiceMorphComplete
        }



--


{-| [morphs](#Morph) _lazily_.
This allows to create self-referential parsers for recursive definitions.

    import MorphRow exposing (grab, skip, atom)
    import Integer.Morph
    import Morph.Text

    type LazyList
        = End
        | Next ( Int, LazyList )

    lazyList : MorphRow LazyList
    lazyList =
        choice
            (\endVariant nextVariant lazyListNarrow ->
                case lazyListNarrow of
                    End ->
                        endVariant ()
                    Next next ->
                        nextVariant next
            )
            |> Morph.possibility (\() -> End)
                (Morph.Text.specific "[]")
            |> Morph.possibility Next
                (succeed Tuple.pair
                    |> grab
                        (Integer.Morph.toInt
                            |> MorphRow.over Integer.Morph.text
                        )
                    |> skip
                        (broadenFrom [ Morph.Char.Space ]
                            |> MorphRow.over
                                (atLeast 1 (Morph.Char.blank |> atom))
                        )
                    |> skip (Morph.Text.specific "::")
                    |> skip
                        (broadenFrom [ Morph.Char.Space ]
                            |> MorphRow.over
                                (atLeast 1 (Morph.Char.blank |> atom))
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
    (() -> Morph specific general error)
    -> Morph specific general error
lazy morphLazy =
    { narrow =
        \broad ->
            broad |> narrow (morphLazy ())
    , broaden =
        \narrow_ ->
            narrow_ |> broaden (morphLazy ())
    }


{-| Go over an additional step of [`Morph`](#Morph) on a broader type.

    Morph.translate Set.toList Set.fromList
        |> Morph.over
            (Value.list elementMorph)

You might recognize similarities to the concept of `andThen`.

-}
over :
    Morph narrow broad error
    -> Morph narrowNarrow narrow error
    -> Morph narrowNarrow broad error
over morphNarrowBroad =
    \morph ->
        { broaden =
            morph.broaden
                >> broaden morphNarrowBroad
        , narrow =
            morphNarrowBroad.narrow
                >> Result.andThen (narrow morph)
        }


{-| Reverse the `Translate a <-> b`
by swapping the functions [`narrow`](#narrow) <-> [`broaden`](#broaden).

    [ 'O', 'h', 'a', 'y', 'o' ]
        |> (Morph.stringToList
            |> Morph.reverse
            |> Morph.map
           )
    --> "Ohayo"

[`unmap`](#unmap) is equivalent to `|> reverse |> map`.

-}
reverse :
    Translate unmapped mapped
    -> Morph mapped unmapped error_
reverse =
    \translate_ ->
        { narrow =
            \unmapped ->
                unmapped |> map translate_ |> Ok
        , broaden = unmap translate_
        }
