module Json exposing
    ( Json, Literal(..), Structure(..)
    , value
    , tagMap, tagTranslate
    , JsValueMagic
    , jsValueMagic, jsValueMagicDecoder
    )

{-| JSON

@docs Json, Literal, Structure


## morph

@docs value


## tag

@docs tagMap, tagTranslate


## js value magic

@docs JsValueMagic
@docs jsValueMagic, jsValueMagicDecoder

-}

import Array
import Decimal exposing (Decimal)
import Dict exposing (Dict)
import Emptiable exposing (Emptiable)
import Json.Decode
import Json.Encode
import Morph exposing (Morph, MorphIndependently, Translate, translate)
import Possibly exposing (Possibly(..))
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Stack exposing (Stacked)
import Value exposing (LiteralOrStructure(..))


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


{-| A valid\* JSON value. `case`able. Elm doesn't crash on `==`.
Can't contain any [spooky impure stuff](#JsValueMagic)

---

\*

  - json numbers don't strictly adhere to a `Float`
    as defined in the [IEEE 754 standard][ieee]
    which is hardcoded into almost all CPUs.
    This standard allows `Infinity` and `NaN`
  - elm `Decoder`s/`Encoder`s can only handle `Float`s
  - [The json.org spec][json] does not include `Infinity` and `NaN`
    and [`elm/json` silently encodes both as `null`](https://github.com/elm/json/blob/0206c00884af953f2cba8823fee111ee71a0330e/src/Json/Encode.elm#L106)
  - this behavior matches `JSON.stringify` behavior in plain JS
  - In general, ["[...] JSON is a Minefield 💣"](https://seriot.ch/projects/parsing_json.html)

[ieee]: https://en.wikipedia.org/wiki/IEEE_754
[json]: https://www.json.org/

-}
type alias Json tag =
    LiteralOrStructure Literal (Structure tag)


{-| json literal. null, bool, number, string
-}
type Literal
    = Null ()
    | Bool Bool
    | Number Float
    | String String


{-| json structure. record/object/dict and array
-}
type Structure tag
    = Array (Array.Array (Json tag))
    | Object (Emptiable (Stacked (Value.Tagged tag)) Possibly)


type alias Tagged tag =
    RecordWithoutConstructorFunction
        { tag : tag, value : Json tag }


{-| [Morph](#Morph) to valid [`Json` value](#Json) format from [`JsValueMagic`](#JsValueMagic)
-}
jsValueMagic : Morph (Json String) JsValueMagic
jsValueMagic =
    Morph.to "JSON"
        { narrow =
            Json.Decode.decodeValue jsValueMagicDecoder
                >> Result.mapError decodeErrorToMorph
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
                , "`: "
                , error |> decodeErrorToMorph |> Morph.Error.errorToString
                , "\n\n"
                , "`Morph` can't turn this into a more structured error"
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
                    |> Stack.only
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
                            |> Morph.Possibilities

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
    Morph.to "json"
        { narrow =
            \jsValueMagicBroad ->
                jsValueMagicBroad
                    |> Json.Decode.decodeValue jsValueMagicDecoder
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
            Literal literal ->
                literal |> literalJsValueMagicEncode

            Structure structure ->
                structure |> structureJsValueMagicEncode ()


literalJsValueMagicEncode : Literal -> JsValueMagic
literalJsValueMagicEncode =
    \literal ->
        case literal of
            Null () ->
                Json.Encode.null

            Bool boolLiteral ->
                boolLiteral |> Json.Encode.bool

            Number floatLiteral ->
                floatLiteral |> Json.Encode.float

            String stringLiteral ->
                stringLiteral |> Json.Encode.string


structureJsValueMagicEncode : () -> (Structure String -> JsValueMagic)
structureJsValueMagicEncode () =
    \structureAny ->
        case structureAny of
            Array arrayAny ->
                arrayAny
                    |> Json.Encode.array (jsValueMagicEncode ())

            Object objectAny ->
                objectAny
                    |> Stack.map
                        (\_ field ->
                            ( field.tag.name
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
        [ Json.Decode.map Literal jsonLiteralDecoder
        , Json.Decode.map Structure jsonStructureDecoder
        ]


jsonLiteralDecoder : Json.Decode.Decoder Literal
jsonLiteralDecoder =
    Json.Decode.oneOf
        [ Null () |> Json.Decode.null
        , Json.Decode.map Bool Json.Decode.bool
        , Json.Decode.map Number Json.Decode.float
        , Json.Decode.map String Json.Decode.string
        ]


jsonStructureDecoder : Json.Decode.Decoder (Structure Value.Name)
jsonStructureDecoder =
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


{-| Convert a [representation of an elm value](Value#Value) to a [valid `Json` value](#Json)
-}
value :
    MorphIndependently
        (Value.Value Value.IndexAndName
         -> Result error_ (Json Value.IndexAndName)
        )
        (Json tag -> Value.Value tag)
value =
    translate toValue value


toValue : Json Value.IndexAndName -> Value.Value Value.IndexAndName
toValue =
    \json ->
        case json of
            Literal literal ->
                literal |> literalToValue

            Structure structure ->
                structure |> structureToValue |> Structure


literalToValue : Literal -> Value.Value Value.IndexAndName
literalToValue =
    \literal ->
        case literal of
            Null unit ->
                unit |> Value.Unit |> Literal

            Number float ->
                float |> Value.Float |> Literal

            String string_ ->
                string_ |> Value.String |> Literal

            Bool bool ->
                { value = () |> Value.Unit |> Literal
                , tag =
                    case bool of
                        False ->
                            { index = 0, name = "False" }

                        True ->
                            { index = 1, name = "True" }
                }
                    |> Value.Variant
                    |> Structure


structureToValue : Structure tag -> Value.Structure tag
structureToValue =
    \structure ->
        case structure of
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


value : Value.Value tag -> Json tag
value =
    \json ->
        case json of
            Literal literal ->
                literal |> literalFromValue |> Literal

            Structure structure ->
                structure |> structureFromValue |> Structure


structureFromValue : Value.Structure tag -> Structure tag
structureFromValue =
    \structure ->
        case structure of
            _ ->
                Debug.todo ""


literalFromValue : Value.Literal -> Literal
literalFromValue =
    \literal ->
        case literal of
            Value.Unit () ->
                Null ()

            Value.String stringLiteral ->
                stringLiteral |> String

            Value.Float float ->
                float |> Number



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
        json |> Value.structureMap (structureTagMap tagChange)


structureTagMap :
    (tag -> tagMapped)
    -> (Structure tag -> Structure tagMapped)
structureTagMap tagChange =
    \structure ->
        case structure of
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
