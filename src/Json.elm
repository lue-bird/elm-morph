module Json exposing
    ( JsValueMagic
    , JsonStructurey(..)
    , jsValueMagic, anyDecoder
    , value
    , Any, LiteralAny, Literaly(..), StructureAny(..)
    )

{-| JSON

@docs JsValueMagic
@docs JsonAny, JsonAnyIn, JsonLiteralAny, JsonLiteraly, JsonStructureAny, JsonStructurey


## morph

@docs jsValueMagic, anyDecoder
@docs value

-}

import Array
import Dict exposing (Dict)
import Emptiable
import Json.Decode
import Json.Encode
import Morph exposing (Morph, Translate, translate)
import Morph.Error
import Number exposing (Rational)
import Possibly exposing (Possibly(..))
import Stack
import Value exposing (DescriptiveAny, LiteralOrStructure(..), Tagged(..))


{-| A value from the javascript side:
from a [`port`](https://guide.elm-lang.org/interop/ports.html), on `init`, from [`elm/http`](https://package.elm-lang.org/packages/elm/http/latest), ...
Compiler magic. Not `case`able. Elm crashes on `==`.
Can include functions, proxies, getters, bigInts, anything

and.. of course this can be abused to break elm's promises \*eyes at hayleigh, Jan Wirth, ...\*:

  - [randomness without `Cmd` ellie](https://ellie-app.com/hpXzJxh4HRda1)
  - web-audio examples
      - [`WebAudio.Context.currentTime`](https://package.elm-lang.org/packages/pd-andy/elm-web-audio/latest/WebAudio-Context#currentTime)
      - [`WebAudio.Context.AudioContext`](https://package.elm-lang.org/packages/pd-andy/elm-web-audio/latest/WebAudio-Context#AudioContext)
  - [`getBoundingClientRect`](https://github.com/funk-team/funkLang/blob/master/src/domMonkeyPatches.js#L44)
  - [listening to events outside a given element](https://github.com/funk-team/funkLang/blob/master/src/domMonkeyPatches/eventsOutside.js#L21)

-}
type alias JsValueMagic =
    Json.Encode.Value


{-| A valid JSON value. `case`able. Elm doesn't crash on `==`.
Can't contain any [spooky impure stuff](#JsValueMagic)
-}
type alias Any =
    LiteralOrStructure LiteralAny StructureAny


{-| json literal. null, bool, number, string are supported

Just ignore the `aNumber`. It's prevents introducing a type constraint.

-}
type Literaly null bool aNumber string
    = Null null
    | Bool bool
    | Number aNumber
    | String string


{-| Any json literal. null or bool or number or string are supported
-}
type alias LiteralAny =
    Literaly
        ()
        Bool
        -- json numbers aren't strictly necessary to adhere `Float` range,
        -- but elm `Decoder`s/`Encoder`s can only handle `Float`s
        Float
        String


{-| json structure. record/object/dict or array are supported
-}
type JsonStructurey array dict
    = Array array
    | Dict dict


{-| Any json structure. record/dict or array are supported

`type alias`es can't be recursive in elm
→ structures are wrapped with `StructureAny`

-}
type StructureAny
    = StructureAny
        (JsonStructurey
            (Array.Array Any)
            (List (Value.Tagged String Any))
        )


{-| [Morph](#Morph) to valid [`Json.Any` value](#Any) format from [`JsValueMagic`](#JsValueMagic)
-}
jsValueMagic : Morph Any JsValueMagic
jsValueMagic =
    Morph.to "json"
        { narrow =
            Json.Decode.decodeValue anyDecoder
                >> Result.mapError decodeErrorToMorph
        , broaden = anyEncode ()
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
                , error |> decodeErrorToMorph |> Morph.Error.toString
                , "\n\n"
                , "`Morph` can't turn this into a more structured error"
                , " because it refers to field errors by their location in the dict/record/object."
                , "\n"
                , "When decoding elm-internal json however, the error only preserves names."
                ]
                    |> String.concat
                    |> Morph.DeadEnd

            Json.Decode.Index arrayIndex error ->
                { location = arrayIndex
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

[Broadens](Morph#broadenWith) to a compact `String`.
To adjust format readability → [`stringBroadWith`](#stringBroadWith)

-}
string : Morph Any String
string =
    stringBroadWith { indentation = 0 }


{-| [`Json.string`](#string) [Morph](#Morph) with adjustable readability configuration
-}
stringBroadWith : { indentation : Int } -> Morph Any String
stringBroadWith { indentation } =
    Morph.to "json"
        { narrow =
            Json.Decode.decodeValue anyDecoder
                >> Result.mapError decodeErrorToMorph
        , broaden =
            anyEncode ()
                >> Json.Encode.encode indentation
        }


anyEncode : () -> Any -> JsValueMagic
anyEncode () =
    \jsonAny ->
        case jsonAny of
            Literal literalAny ->
                literalAny |> literalAnyEncode

            Structure structureAny ->
                structureAny |> structureAnyEncode ()


literalAnyEncode : LiteralAny -> Json.Encode.Value
literalAnyEncode =
    \literalAny ->
        case literalAny of
            Null () ->
                Json.Encode.null

            Bool boolLiteral ->
                boolLiteral |> Json.Encode.bool

            Number floatLiteral ->
                floatLiteral |> Json.Encode.float

            String stringLiteral ->
                stringLiteral |> Json.Encode.string


structureAnyEncode : () -> StructureAny -> JsValueMagic
structureAnyEncode () =
    \(StructureAny structureAny) ->
        case structureAny of
            Array arrayAny ->
                arrayAny
                    |> Json.Encode.array (anyEncode ())

            Dict objectAny ->
                objectAny
                    |> List.map
                        (\(Tagged tag fieldValue) ->
                            ( tag, fieldValue |> anyEncode () )
                        )
                    |> Json.Encode.object


{-| Some elm functions, [for example html events](https://dark.elm.dmy.fr/packages/elm/html/latest/Html-Events#on), require a `Json.Decode.Decoder`,
which is an opaque type and can't be constructed (for example by supplying a `Json.Decode.Value -> Result Json.Error elm`).

In general, try to use [`Json.jsValueMagic`](#jsValueMagic) instead wherever possible.

-}
anyDecoder : Json.Decode.Decoder Any
anyDecoder =
    let
        jsonLiteralDecoder : Json.Decode.Decoder LiteralAny
        jsonLiteralDecoder =
            Json.Decode.oneOf
                [ Null () |> Json.Decode.null
                , Json.Decode.map Bool Json.Decode.bool
                , Json.Decode.map
                    (Morph.broadenWith Number.fromFloat >> Number)
                    Json.Decode.float
                , Json.Decode.map String Json.Decode.string
                ]

        jsonStructureDecoder : Json.Decode.Decoder StructureAny
        jsonStructureDecoder =
            Json.Decode.map StructureAny
                (Json.Decode.oneOf
                    [ Json.Decode.map Array
                        (Json.Decode.array
                            (Json.Decode.lazy (\() -> anyDecoder))
                        )
                    , Json.Decode.map
                        (List.map (\( tag, v ) -> Value.Tagged tag v)
                            >> Dict
                        )
                        (Json.Decode.keyValuePairs
                            (Json.Decode.lazy (\() -> anyDecoder))
                        )
                    ]
                )
    in
    Json.Decode.oneOf
        [ Json.Decode.map Literal jsonLiteralDecoder
        , Json.Decode.map Structure jsonStructureDecoder
        ]


{-| This is the trickiest part of the whole package:
Converting between a valid [`JsonAny`](#JsonAny) value and your preferred elm representation.

Like with `encode`-`decode` pairs, there are a few things to consider:

These are the default morphs:

  - `Nully ()` <-> `Unity ()`
  - `Objecty ([ ( "Posix", Floaty ) ] |> Dict.fromList)`
    <-> `Posixy` (considering dat iso format js uses)
  - `Objecty ([ ( "Int", Floaty ) ] |> Dict.fromList)`
    <-> `Inty`
  - `Objecty ([ ( "Char", Stringy ) ] |> Dict.fromList)`
    <-> `Chary`

-}
value : Translate DescriptiveAny Any
value =
    translate
        jsonAnyToValueAny
        valueAnyToJsonAny


valueAnyToJsonAny : DescriptiveAny -> Any
valueAnyToJsonAny =
    \valueAny ->
        case valueAny of
            Literal literalAny ->
                case literalAny of
                    Value.Unit () ->
                        Null () |> Literal

                    Value.Char char ->
                        (char |> String.fromChar)
                            |> String
                            |> Literal
                            |> Tagged "char"
                            |> List.singleton
                            |> Dict
                            |> StructureAny
                            |> Structure

                    Value.Int int ->
                        (int |> toFloat |> Number.fromFloat)
                            |> Number
                            |> Literal
                            |> Tagged "int"
                            |> List.singleton
                            |> Dict
                            |> StructureAny
                            |> Structure

                    _ ->
                        Debug.todo ""

            Structure (Value.StructureAny structureAny) ->
                case structureAny of
                    _ ->
                        Debug.todo ""


jsonAnyToValueAny : Any -> DescriptiveAny
jsonAnyToValueAny =
    \jsonAny ->
        Debug.todo ""
