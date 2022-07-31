module Morph exposing
    ( Morph, MorphInProgress
    , Expectation, ExpectationWith(..), Error, ErrorWith, failure, expect
    , validate, specific
    , broaden, narrow
    , Translate
    , translate, broadenFrom, toggle, remain
    , map, unmap
    , lazy, over
    , reverse
    , Tagged(..), TagOrValue(..)
    , group, part
    , ChoiceMorphInProgress
    , choice, possibility, choiceFinish
    )

{-| Call it Codec, Conversion, PrismReversible,

@docs Morph, MorphInProgress


## fallible

@docs Expected, expectation, errorExpectationMap
@docs expectationMap
@docs Expectation, ExpectationWith, Error, ErrorWith, failure, expect


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

@docs group, part


### choices

@docs ChoiceMorphInProgress
@docs choice, possibility, choiceFinish

-}

import Emptiable exposing (Emptiable)
import Possibly exposing (Possibly)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Stack exposing (Stacked)


{-| Conversion functions to a more general format and back.

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
    MorphInProgress
        { narrow : broad -> Result error narrow
        , broaden : narrow -> broad
        }


{-| Sometimes, you'll see the most general version of [`Morph`](#Morph):

    : MorphInProgress
    :     { narrow : narrow
    :     , broaden : broaden
    :     }

where

  - [`narrow`](#narrow)ed value types can't necessarily be [`broaden`](#broaden)ed
  - [`broaden`](#broaden)ed value types can't necessarily be [`narrow`](#narrow)ed

This general form is helpful to describe a step in building an incomplete [`Morph`](#Morph).

-}
type alias MorphInProgress narrowAndBroaden =
    narrowAndBroaden



--


{-| What went wrong.
-}
type alias Expectation specific =
    ExpectationWith () specific


{-| Generic version of [`Expectation`](#Expectation) where each inner [error](#ErrorWith) has extra fields.
-}
type ExpectationWith location specific
    = NoFail
    | OneOf (Emptiable (Stacked (ErrorWith location specific)) Possibly)
    | Specific specific
      -- | LocatedAt location
      -- row-specific
    | MoreInput
    | NoMoreInput


{-| [What went wrong](#Expectation), where it went wrong and maybe custom descriptions.
-}
type alias Error specific =
    ErrorWith () specific


{-| Generic version of [`Error`](#Error) which can have extra fields.
-}
type alias ErrorWith location specific =
    RecordWithoutConstructorFunction
        { expected : ExpectationWith location specific
        , description : Emptiable (Stacked String) Possibly
        , location : location
        }


specificMap :
    (specific -> specificMapped)
    ->
        (Morph narrow broad (ErrorWith location specific)
         -> Morph narrow broad (ErrorWith location specificMapped)
        )
specificMap specificChange =
    \morph ->
        { narrow =
            .narrow morph
                >> Result.mapError
                    (errorSpecificMap specificChange)
        , broaden = .broaden morph
        }


errorSpecificMap :
    (specific -> specificMapped)
    ->
        (ErrorWith location specific
         -> ErrorWith location specificMapped
        )
errorSpecificMap specificChange =
    \error ->
        { expected =
            error.expected
                |> expectationSpecificMap specificChange
        , description = error.description
        , location = error.location
        }


expectationSpecificMap :
    (specific -> specificMapped)
    ->
        (ExpectationWith location specific
         -> ExpectationWith location specificMapped
        )
expectationSpecificMap specificChange =
    \expectation_ ->
        case expectation_ of
            NoFail ->
                NoFail

            OneOf possibilities ->
                possibilities
                    |> Stack.map
                        (\_ -> errorSpecificMap specificChange)
                    |> OneOf

            Specific specific_ ->
                Specific (specific_ |> specificChange)

            MoreInput ->
                MoreInput

            NoMoreInput ->
                NoMoreInput


{-| Always `Err`. Make sure to [`expect`](#expect) something.

TODO: remove? add description?

-}
failure : Result (Error specific_) narrow_
failure =
    { expected = NoFail
    , description = Emptiable.empty
    , location = ()
    }
        |> Err


{-| Describe the context to improve error messages.

TODO example

-}
expect :
    String
    ->
        (Morph narrow broad (ErrorWith location specific)
         -> Morph narrow broad (ErrorWith location specific)
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


{-| The function that turns `narrow` into `broad`.
-}
broaden :
    MorphInProgress { narrow : narrow, broaden : broaden }
    -> broaden
broaden =
    .broaden


{-| The function that turns `broad` into `narrow` or an `error`.
-}
narrow :
    MorphInProgress { narrow : narrow, broaden : broaden_ }
    -> narrow
narrow =
    .narrow


{-| Convert values of the arbitrarily chosen types `unmapped -> mapped`.

    "3456" |> (Morph.Text.toList |> Morph.map)
    --> [ '3', '4', '5', '6' ]

-}
map :
    MorphInProgress
        { narrow : unmapped -> Result Never mapped
        , broaden : broaden_
        }
    -> (unmapped -> mapped)
map translate_ =
    \unmapped ->
        case unmapped |> narrow translate_ of
            Ok mappedNarrow ->
                mappedNarrow

            Err error ->
                error |> never


{-| Equivalent to [`Morph.reverse`](#reverse) `|> Morph.map`
-}
unmap :
    MorphInProgress
        { narrow : narrow_
        , broaden : unmap
        }
    -> unmap
unmap translate_ =
    broaden translate_



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
printable : Morph LocalSymbolPrintable Char (Morph.Error Char expectationCustom_)
printable =
    choice
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
        |> Morph.possibility (\() -> ExclamationMark) (Morph.specific '!')
        |> Morph.possibility (\() -> NumberSign) (Morph.specific '#')
        |> Morph.possibility (\() -> DollarSign) (Morph.specific '$')
        |> Morph.possibility (\() -> PercentSign) (Morph.specific '%')
        |> Morph.possibility (\() -> Ampersand) (Morph.specific '&')
        |> Morph.possibility (\() -> Asterisk) (Morph.specific '*')
        |> Morph.possibility (\() -> LowLine) (Morph.specific '_')
        |> Morph.possibility (\() -> HyphenMinus) (Morph.specific '-')
        |> Morph.choiceFinish
```

-}
validate :
    (narrow -> Result error narrow)
    ->
        MorphInProgress
            { narrow : narrow -> Result error narrow
            , broaden : broad -> broad
            }
validate narrowConvert =
    { narrow = narrowConvert
    , broaden = identity
    }


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

    dictFromListMorph =
        Morph.translate Dict.fromList Dict.toList

Still, there's no parsing to translate one state to the other.

-}
type alias Translate mapped unmapped =
    Morph mapped unmapped Never


{-| Switch between 2 opposite representations. Examples:

    toggle List.reverse

    toggle not

    toggle negate

    toggle (\n -> n ^ -1)

    toggle Linear.opposite

-}
toggle : (value -> value) -> Morph value value error_
toggle changeToOpposite =
    translate changeToOpposite changeToOpposite


{-| A [`Morph`](#Morph) that doesn't transform anything.

Same as writing:

  - [`validate`](#validate) `Ok`
  - [`translate`](#translate) `identity identity`
  - `{ broaden = identity, narrow = Ok }`
  - [`toggle`](#toggle) `identity` when broad and narrow types match

-}
remain :
    MorphInProgress
        { narrow : narrow -> Result error_ narrow
        , broaden : broad -> broad
        }
remain =
    translate identity identity


{-| Mutual `Morph` = [`Translate`](#Translate)
between representations
that have the same structural information
and can be mapped 1:1 into each other.

    stringToListMorph : Morph (List Char) String error_
    stringToListMorph =
        Morph.translate String.toList String.fromList

Examples:

  - [`Text.Morph.toList`](Text-Morph#toList), [`Text.Morph.fromList`](Text-Morph#fromList)
  - [`Array.Morph.toList`](Array-Morph#toList), [`Array.Morph.fromList`](Array-Morph#fromList)
  - [`Stack.Morph.toList`](Stack-Morph#toList), [`Stack.Morph.fromList`](Stack-Morph#fromList)
  - [`Stack.Morph.toText`](Stack-Morph#toText), [`Stack.Morph.fromText`](Stack-Morph#fromText)

-}
translate :
    (beforeMap -> mapped)
    -> (beforeUnmapped -> unmapped)
    ->
        MorphInProgress
            { narrow : beforeMap -> Result error_ mapped
            , broaden : beforeUnmapped -> unmapped
            }
translate mapTo unmapFrom =
    { narrow = mapTo >> Ok
    , broaden = unmapFrom
    }


{-| [`Morph`](#Morph) that always [`broaden`](#broaden)s to a given constant.

For any more complex [`broaden`](#broaden)ing process, use [`translate`](#translate)

-}
broadenFrom : broadConstant -> Morph () broadConstant error_
broadenFrom narrowConstant =
    translate (\_ -> ()) (\() -> narrowConstant)


{-| Match only the specific given broad input
-}
specific :
    broadConstant
    -> Morph () broadConstant (Error broadConstant)
specific broadConstant =
    { narrow =
        \broad ->
            if broad == broadConstant then
                () |> Ok

            else
                { expected = Specific broadConstant
                , description = Emptiable.empty
                , location = ()
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
        (MorphInProgress
            { narrow :
                groupBroad
                -> Result partError (partNarrow -> groupNarrowFurther)
            , broaden : groupNarrow -> (partBroad -> groupBroadenFurther)
            }
         ->
            MorphInProgress
                { narrow : groupBroad -> Result partError groupNarrowFurther
                , broaden : groupNarrow -> groupBroadenFurther
                }
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
                { possibilities :
                    Emptiable (Stacked possibilityExpectation) Possibly
                }
                choiceNarrow
    , broaden : choiceBroaden
    }


{-| If the previous [`possibility`](#possibility) fails
try this [`Morph`](#Morph).

> ℹ️ Equivalent regular expression: `|`

    import MorphRow exposing (atom)
    import Morph.Char as Char
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
            |> possibility (\() -> Underscore) (one '_')
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
        |> Text.narrowWith (onFailDown [ one '_', Char.letter ])
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

                    Err soFarExpectation ->
                        case broad |> narrow possibilityMorph of
                            Ok possibilityNarrow ->
                                possibilityNarrow
                                    |> possibilityToChoice
                                    |> Ok

                            Err possibilityExpectation ->
                                { possibilities =
                                    soFarExpectation.possibilities
                                        |> Stack.onTopLay possibilityExpectation
                                }
                                    |> Err
        , broaden =
            choiceMorphSoFar.broaden
                (broaden possibilityMorph)
        }


{-| Assemble a combined whole from its [parts](#part)
-}
group :
    narrowAssemble
    -> broadAssemble
    ->
        MorphInProgress
            { narrow : broad_ -> Result error_ narrowAssemble
            , broaden : groupNarrow_ -> broadAssemble
            }
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
            { possibilities = Emptiable.empty } |> Err
    , broaden = choiceBroadenDiscriminatedByPossibility
    }


{-| Conclude a [`Morph.choice`](Morph#choice) |> [`Morph.possibility`](Morph#possibility) chain.
-}
choiceFinish :
    ChoiceMorphInProgress
        narrowUnion
        possibilityBroad
        (narrowUnion -> possibilityBroad)
        (Error specific)
    ->
        Morph
            narrowUnion
            possibilityBroad
            (Error specific)
choiceFinish =
    \choiceMorphComplete ->
        { narrow =
            narrow choiceMorphComplete
                >> Result.mapError
                    (\expectation_ ->
                        { expected = OneOf expectation_.possibilities
                        , description = Emptiable.empty
                        , location = ()
                        }
                    )
        , broaden =
            \narrowUnion ->
                narrowUnion |> broaden choiceMorphComplete
        }



--


{-| [morphs](#Morph) _lazily_.
This allows to create self-referential parsers for recursive definitions.

    import MorphRow exposing (grab, skip, one)
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
                                (atLeast 1 (Morph.Char.blank |> one))
                        )
                    |> skip (Morph.Text.specific "::")
                    |> skip
                        (broadenFrom [ Morph.Char.Space ]
                            |> MorphRow.over
                                (atLeast 1 (Morph.Char.blank |> one))
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


{-| `Translate a <-> b`
by swapping the functions [`map`](#map) <-> [`unmap`](#unmap).

    [ 'O', 'h', 'a', 'y', 'o' ]
        |> (Text.Morph.toList
                |> Morph.reverse
                |> Morph.map
           )
    --> "Ohayo"

[`unmap`](#unmap) is equivalent to `|> reverse |> map`.

-}
reverse :
    Translate mapped unmapped
    -> Morph unmapped mapped error_
reverse =
    \translate_ ->
        { narrow =
            \unmapped ->
                unmapped |> unmap translate_ |> Ok
        , broaden = map translate_
        }
