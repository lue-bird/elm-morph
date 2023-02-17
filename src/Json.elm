module Json exposing
    ( Json, Atom(..), Composed(..)
    , value
    , tagMap, tagTranslate
    , JsValueMagic
    , jsValueMagic, jsValueMagicDecoder
    )

{-| JSON

@docs Json, Atom, Composed


## morph

@docs value


## tag

@docs tagMap, tagTranslate


## js value magic

@docs JsValueMagic
@docs jsValueMagic, jsValueMagicDecoder

-}

import Array
import Choice
import Decimal exposing (Decimal)
import Decimal.Internal
import Dict exposing (Dict)
import Emptiable exposing (Emptiable)
import FloatExplicit exposing (FloatExplicit)
import Group
import Json.Decode
import Json.Encode
import Linear exposing (Direction(..))
import Morph exposing (Morph, MorphIndependently, MorphOrError, Translate, translate)
import Possibly exposing (Possibly(..))
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Sign exposing (Sign)
import Sign.Internal
import Stack exposing (Stacked)
import Value exposing (AtomOrComposed(..))


{-| A value from the javascript side:
from a [`port`](https://guide.elm-lang.org/interop/ports.html),
on `init`,
from [`elm/http`](https://package.elm-lang.org/packages/elm/http/latest), ...

Compiler magic. Not `case`able. Elm crashes on `==`.
Can include functions, proxies, getters, bigInts, anything

and.. of course this can be abused to break elm's promises 🙈

> examples
>
>   - [randomness without `Cmd` ellie](https://ellie-app.com/hpXzJxh4HRda1)
>   - web-audio examples
>       - [`WebAudio.Context.currentTime`](https://package.elm-lang.org/packages/pd-andy/elm-web-audio/latest/WebAudio-Context#currentTime)
>       - [`WebAudio.Context.AudioContext`](https://package.elm-lang.org/packages/pd-andy/elm-web-audio/latest/WebAudio-Context#AudioContext)
>   - [`getBoundingClientRect`](https://github.com/funk-team/funkLang/blob/master/src/domMonkeyPatches.js#L44)
>   - [listening to events outside a given element](https://github.com/funk-team/funkLang/blob/master/src/domMonkeyPatches/eventsOutside.js#L21)

-}
type alias JsValueMagic =
    Json.Encode.Value


{-| A valid JSON value. `case`able. Elm doesn't crash on `==`.
Can't contain any [spooky impure stuff](#JsValueMagic)
-}
type alias Json tag =
    AtomOrComposed Atom (Composed tag)


{-| json atom. null, bool, number, string
-}
type Atom
    = Null ()
    | Bool Bool
    | Number Decimal
    | String String


{-| json structure. record/object/dict or array
-}
type Composed tag
    = Array (Array.Array (Json tag))
    | Object (Emptiable (Stacked (Tagged tag)) Possibly)


type alias Tagged tag =
    RecordWithoutConstructorFunction
        { tag : tag, value : Json tag }


{-| [Morph](#Morph) to valid [`Json` value](#Json) format from [`JsValueMagic`](#JsValueMagic)

About json numbers...

  - json numbers don't strictly adhere to a `Float`
    as defined in the [IEEE 754 standard][ieee]
    which is hardcoded into almost all CPUs.
    This standard allows `Infinity` and `NaN` which the [json.org spec][json] does not include.
  - [`elm/json` silently encodes both as `null`](https://github.com/elm/json/blob/0206c00884af953f2cba8823fee111ee71a0330e/src/Json/Encode.elm#L106).
    This behavior matches `JSON.stringify` behavior in plain JS
  - our json representation doesn't have this footgun since it uses [`Decimal`](Decimal#Decimal)
  - elm `Decoder`s/`Encoder`s can only handle `Float` range which dictates the range we can use for [`Decimal`](Decimal#Decimal)s

[ieee]: https://en.wikipedia.org/wiki/IEEE_754
[json]: https://www.json.org/

-}
jsValueMagic : Morph (Json String) JsValueMagic
jsValueMagic =
    Morph.to "JSON"
        { description = { custom = Emptiable.empty, inner = Emptiable.empty }
        , narrow =
            \jsValueMagicBeforeNarrow ->
                jsValueMagicBeforeNarrow
                    |> Json.Decode.decodeValue jsValueMagicDecoder
                    |> Result.mapError decodeErrorToMorph
        , broaden = jsValueMagicEncode ()
        }


{-| Should be redundant if `anyDecoder` catches all cases
-}
decodeErrorToMorph : Json.Decode.Error -> Morph.Error
decodeErrorToMorph =
    \decodeError ->
        case decodeError of
            Json.Decode.Field fieldName error ->
                [ "field `"
                , fieldName
                , "`:\n"
                , error
                    |> decodeErrorToMorph
                    |> Morph.errorToLines
                    |> Stack.fold Up (\line soFar -> soFar ++ "\n" ++ line)
                , "\n\n"
                , "`Morph` can't turn this into a more composedd error"
                , " because it refers to field errors by their location in the dict/record/object."
                , "\n"
                , "When decoding elm-internal json however, the error only preserves names."
                ]
                    |> String.concat
                    |> Morph.DeadEnd

            Json.Decode.Index arrayIndex error ->
                { index = arrayIndex
                , error = error |> decodeErrorToMorph
                }
                    |> Stack.one
                    |> Morph.Parts

            Json.Decode.OneOf possibilities ->
                case possibilities |> Stack.fromList of
                    Emptiable.Empty Possible ->
                        "no expected possibilities in Json.Decode.oneOf"
                            |> Morph.DeadEnd

                    Emptiable.Filled stacked ->
                        stacked
                            |> Emptiable.filled
                            |> Stack.map (\_ -> decodeErrorToMorph)
                            |> Morph.Tries

            Json.Decode.Failure custom jsValue ->
                [ custom
                , "\n\n"
                , "    "
                , jsValue
                    |> Json.Encode.encode 4
                    |> String.lines
                    |> String.join "    "
                ]
                    |> String.concat
                    |> Morph.DeadEnd


{-| [Morph](#Morph) to valid [`Json.Any` value](#Any) format from a `String`

[Broadens](Morph#broadenFrom) to a compact `String`.
To adjust format readability → [`stringBroadWith`](#stringBroadWith)

-}
string : Morph (Json String) String
string =
    stringBroadWith { indentation = 0 }


{-| [`Json.string`](#string) [Morph](#Morph) with adjustable readability configuration
-}
stringBroadWith : { indentation : Int } -> Morph (Json String) String
stringBroadWith { indentation } =
    Morph.to "JSON"
        { description = { custom = Emptiable.empty, inner = Emptiable.empty }
        , narrow =
            \jsValueMagicBroad ->
                jsValueMagicBroad
                    |> Json.Decode.decodeString jsValueMagicDecoder
                    |> Result.mapError decodeErrorToMorph
        , broaden =
            \json ->
                json
                    |> jsValueMagicEncode ()
                    |> Json.Encode.encode indentation
        }


jsValueMagicEncode : () -> (Json String -> JsValueMagic)
jsValueMagicEncode () =
    \jsonAny ->
        case jsonAny of
            Atom atom ->
                atom |> atomJsValueMagicEncode

            Composed composed ->
                composed |> composedJsValueMagicEncode ()


atomJsValueMagicEncode : Atom -> JsValueMagic
atomJsValueMagicEncode =
    \atom ->
        case atom of
            Null () ->
                Json.Encode.null

            Bool boolAtom ->
                boolAtom |> Json.Encode.bool

            Number floatAtom ->
                floatAtom
                    |> Morph.broadenFrom
                        (Decimal.floatExplicit |> Morph.over FloatExplicit.float)
                    |> Json.Encode.float

            String stringAtom ->
                stringAtom |> Json.Encode.string


composedJsValueMagicEncode : () -> (Composed String -> JsValueMagic)
composedJsValueMagicEncode () =
    \composedAny ->
        case composedAny of
            Array arrayAny ->
                arrayAny
                    |> Json.Encode.array (jsValueMagicEncode ())

            Object objectAny ->
                objectAny
                    |> Stack.map
                        (\_ field ->
                            ( field.tag
                            , field.value |> jsValueMagicEncode ()
                            )
                        )
                    |> Stack.toList
                    |> Json.Encode.object


{-| Some elm functions,
[for example html events](https://dark.elm.dmy.fr/packages/elm/html/latest/Html-Events#on)
require a `Json.Decode.Decoder`,
which is an opaque type and can't be constructed (for example by from `Json.Decode.Value -> Result Json.Error elm`)

In general, try to use [`Json.jsValueMagic`](#jsValueMagic) instead wherever possible

-}
jsValueMagicDecoder : Json.Decode.Decoder (Json String)
jsValueMagicDecoder =
    Json.Decode.oneOf
        [ Json.Decode.map Atom jsonAtomDecoder
        , Json.Decode.map Composed jsonComposedDecoder
        ]


jsonAtomDecoder : Json.Decode.Decoder Atom
jsonAtomDecoder =
    Json.Decode.oneOf
        [ Null () |> Json.Decode.null
        , Json.Decode.map Bool Json.Decode.bool
        , Json.Decode.andThen
            (\float ->
                case
                    float
                        |> Morph.narrowTo
                            (Decimal.floatExplicit
                                |> Morph.over FloatExplicit.float
                            )
                of
                    Ok decimal ->
                        Number decimal |> Json.Decode.succeed

                    Err exception ->
                        exception
                            |> Morph.errorToLines
                            |> Stack.fold Up (\line soFar -> soFar ++ "\n" ++ line)
                            |> Json.Decode.fail
            )
            Json.Decode.float
        , Json.Decode.map String Json.Decode.string
        ]


jsonComposedDecoder : Json.Decode.Decoder (Composed String)
jsonComposedDecoder =
    Json.Decode.lazy
        (\() ->
            Json.Decode.oneOf
                [ Json.Decode.map Array
                    (Json.Decode.array jsValueMagicDecoder)
                , Json.Decode.map Object
                    (Json.Decode.keyValuePairs jsValueMagicDecoder
                        |> Json.Decode.map
                            (\keyValuePairs ->
                                keyValuePairs
                                    |> List.map
                                        (\( tag, v ) -> { tag = tag, value = v })
                                    |> Stack.fromList
                            )
                    )
                ]
        )


{-| Convert a [representation of an elm value](Value#Value) to a [valid `Json` value](#Json)
-}
value :
    MorphIndependently
        (Value.Value narrowTag
         -> Result error_ (Json narrowTag)
        )
        (Json Value.IndexAndName
         -> Value.Value Value.IndexAndName
        )
value =
    translate fromValueImplementation toValue


toValue : Json Value.IndexAndName -> Value.Value Value.IndexAndName
toValue =
    \json ->
        case json of
            Atom atom ->
                atom |> atomToValue

            Composed composed ->
                composed |> composedToValue |> Composed


atomToValue : Atom -> Value.Value Value.IndexAndName
atomToValue =
    \atom ->
        case atom of
            Null unit ->
                unit |> Value.Unit |> Atom

            Number decimal ->
                decimal |> Morph.broadenFrom decimalInternal |> Value.Number |> Atom

            String string_ ->
                string_ |> Value.String |> Atom

            Bool bool ->
                { value = () |> Value.Unit |> Atom
                , tag =
                    case bool of
                        False ->
                            { index = 0, name = "False" }

                        True ->
                            { index = 1, name = "True" }
                }
                    |> Value.Variant
                    |> Composed


composedToValue :
    Composed Value.IndexAndName
    -> Value.Composed Value.IndexAndName
composedToValue =
    \composed ->
        case composed of
            Array array ->
                array |> Array.map toValue |> Value.Array

            Object object ->
                object
                    |> Stack.map
                        (\_ tagged ->
                            { tag = tagged.tag
                            , value = tagged.value |> toValue
                            }
                        )
                    |> Value.Record


fromValueImplementation : Value.Value tag -> Json tag
fromValueImplementation =
    \json ->
        case json of
            Atom atom ->
                atom |> atomFromValue |> Atom

            Composed composed ->
                composed |> composedFromValue |> Composed


composedFromValue : Value.Composed tag -> Composed tag
composedFromValue =
    \composed ->
        case composed of
            _ ->
                Debug.todo ""


atomFromValue : Value.Atom -> Atom
atomFromValue =
    \atom ->
        case atom of
            Value.Unit () ->
                Null ()

            Value.String stringAtom ->
                stringAtom |> String

            Value.Number decimal ->
                decimal |> Morph.mapTo decimalInternal |> Number



-- tag


{-| [`Translate`](Morph#Translate) [`Json`](#Json) by calling [`tagMap`](#tagMap) in both directions

For [`Value`](#Value), it's

    ...
        |> Morph.over (Json.tagTranslate Value.compact)

    -- or
    ...
        |> Morph.over (Json.tagTranslate Value.descriptive)

-}
tagTranslate :
    MorphIndependently
        (tagBeforeMap -> Result (Morph.ErrorWithDeadEnd Never) tagMapped)
        (tagBeforeUnmap -> tagUnmapped)
    ->
        MorphIndependently
            (Json tagBeforeMap
             -> Result (Morph.ErrorWithDeadEnd never_) (Json tagMapped)
            )
            (Json tagBeforeUnmap -> Json tagUnmapped)
tagTranslate tagTranslate_ =
    translate
        (tagMap (Morph.mapTo tagTranslate_))
        (tagMap (Morph.broadenFrom tagTranslate_))


{-| Reduce the amount of tag information.
Used to make its representation [`compact`] or [`descriptive`](#descriptive)
-}
tagMap : (tag -> tagMapped) -> (Json tag -> Json tagMapped)
tagMap tagChange =
    \json ->
        json |> Value.composedMap (composedTagMap tagChange)


composedTagMap :
    (tag -> tagMapped)
    -> (Composed tag -> Composed tagMapped)
composedTagMap tagChange =
    \composed ->
        case composed of
            Array array ->
                array |> Array.map (tagMap tagChange) |> Array

            Object object ->
                object |> Stack.map (\_ -> taggedTagMap tagChange) |> Object


taggedTagMap : (tag -> tagMapped) -> (Tagged tag -> Tagged tagMapped)
taggedTagMap tagChange =
    \tagged ->
        { tag = tagged.tag |> tagChange
        , value = tagged.value |> tagMap tagChange
        }



-- Decimal


decimalInternal :
    MorphOrError
        Decimal
        Decimal.Internal.Decimal
        (Morph.ErrorWithDeadEnd deadEnd_)
decimalInternal =
    Choice.toFrom
        ( \variantN0 variantSigned decimalInternalBeforeNarrow ->
            case decimalInternalBeforeNarrow of
                Decimal.Internal.N0 ->
                    variantN0 ()

                Decimal.Internal.Signed signedValue ->
                    variantSigned signedValue
        , \variantN0 variantSigned decimal ->
            case decimal of
                Decimal.N0 ->
                    variantN0 ()

                Decimal.Signed signedValue ->
                    variantSigned signedValue
        )
        |> Choice.variant ( \() -> Decimal.N0, \() -> Decimal.Internal.N0 ) Morph.keep
        |> Choice.variant ( Decimal.Signed, Decimal.Internal.Signed ) signedInternal
        |> Choice.finishToFrom


signedInternal :
    MorphOrError
        Decimal.Signed
        Decimal.Internal.Signed
        (Morph.ErrorWithDeadEnd deadEnd_)
signedInternal =
    Group.toFrom
        ( \sign absolutePart -> { sign = sign, absolute = absolutePart }
        , \sign absolutePart -> { sign = sign, absolute = absolutePart }
        )
        |> Group.part ( .sign, .sign ) signInternal
        |> Group.part ( .absolute, .absolute ) absoluteInternal
        |> Group.finish


absoluteInternal : MorphOrError Decimal.Absolute Decimal.Internal.Absolute error_
absoluteInternal =
    Choice.toFrom
        ( \variantFraction variantAtLeast1 decimal ->
            case decimal of
                Decimal.Internal.Fraction fractionValue ->
                    variantFraction fractionValue

                Decimal.Internal.AtLeast1 atLeast1Value ->
                    variantAtLeast1 atLeast1Value
        , \variantFraction variantAtLeast1 decimal ->
            case decimal of
                Decimal.Fraction fractionValue ->
                    variantFraction fractionValue

                Decimal.AtLeast1 atLeast1Value ->
                    variantAtLeast1 atLeast1Value
        )
        |> Choice.variant ( Decimal.Fraction, Decimal.Internal.Fraction ) Morph.keep
        |> Choice.variant ( Decimal.AtLeast1, Decimal.Internal.AtLeast1 ) Morph.keep
        |> Choice.finishToFrom


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
