module Json exposing
    ( JsValueMagic
    , JsonAny, JsonAnyIn(..), JsonLiteralAny, JsonLiteraly(..), JsonStructureAny, JsonStructurey(..)
    , jsValueMagic, decoder
    , value
    )

{-| JSON

@docs JsValueMagic
@docs JsonAny, JsonAnyIn, JsonLiteralAny, JsonLiteraly, JsonStructureAny, JsonStructurey


## morph

@docs jsValueMagic, decoder
@docs value

-}

import Array
import Dict exposing (Dict)
import Json.Decode
import Json.Encode
import Morph exposing (Morph, Translate, translate)
import Value exposing (ValueAny, Valuey(..))


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
Can't contain any [spooky impure stuff](#JsValueMagic).
-}
type alias JsonAny =
    Valuey JsonLiteralAny JsonStructureAny


{-| `type alias`es can't be recursive in elm
→ structure's inner json parts are wrapped with `Json.AnyIn`.
-}
type JsonAnyIn
    = AnyIn JsonAny


{-| json literal. null or bool or number or string are supported
-}
type JsonLiteraly nully booly floaty stringy
    = Nully nully
    | Booly booly
    | Floaty floaty
    | Stringy stringy


{-| Any json literal. null or bool or number or string are supported
-}
type alias JsonLiteralAny =
    JsonLiteraly () Bool Float String


{-| json structure. record/dict or array are supported
-}
type JsonStructurey arrayy objecty
    = Arrayy arrayy
    | Objecty objecty


{-| Any json structure. record/dict or array are supported
-}
type alias JsonStructureAny =
    JsonStructurey (Array.Array JsonAnyIn) (Dict String JsonAnyIn)


{-| Morph between valid [`JsonAny`](#JsonAny) format and [`JsValueMagic`](#JsValueMagic)
-}
jsValueMagic :
    Morph
        JsonAny
        JsValueMagic
        (Morph.Expected { validJsonFormat : String })
jsValueMagic =
    { narrow =
        \jsonMagicBroad ->
            jsonMagicBroad
                |> Json.Decode.decodeValue decoder
                |> Result.mapError
                    (\errorWhenJsonFormatInvalid ->
                        Morph.Expected
                            { validJsonFormat =
                                errorWhenJsonFormatInvalid
                                    |> Json.Decode.errorToString
                            }
                    )
    , broaden =
        let
            jsonAnyEncode : () -> JsonAny -> JsValueMagic
            jsonAnyEncode () =
                \jsonAny ->
                    case jsonAny of
                        Literaly literalAny ->
                            literalAny |> literalAnyEncode

                        Structurey structureAny ->
                            structureAny |> structureAnyEncode ()

            step : () -> JsonAnyIn -> JsValueMagic
            step () =
                \(AnyIn jsonAny) ->
                    jsonAny |> jsonAnyEncode ()

            literalAnyEncode : JsonLiteralAny -> Json.Encode.Value
            literalAnyEncode =
                \literalAny ->
                    case literalAny of
                        Nully () ->
                            Json.Encode.null

                        Booly boolLiteral ->
                            boolLiteral |> Json.Encode.bool

                        Floaty floatLiteral ->
                            floatLiteral |> Json.Encode.float

                        Stringy stringLiteral ->
                            stringLiteral |> Json.Encode.string

            structureAnyEncode : () -> JsonStructureAny -> JsValueMagic
            structureAnyEncode () =
                \structureAny ->
                    case structureAny of
                        Arrayy arrayAny ->
                            arrayAny
                                |> Json.Encode.array (step ())

                        Objecty objectAny ->
                            objectAny
                                |> Dict.map (\_ -> step ())
                                |> Dict.toList
                                |> Json.Encode.object
        in
        jsonAnyEncode ()
    }


{-| Some elm functions, [for example html events](https://dark.elm.dmy.fr/packages/elm/html/latest/Html-Events#on), require a `Json.Decode.Decoder`,
which is an opaque type and can't be constructed (for example by supplying a `Json.Decode.Value -> Result Json.Error elm`).

In general, try to use [`Json.jsValueMagic`](#jsValueMagic) wherever possible instead.

-}
decoder : Json.Decode.Decoder JsonAny
decoder =
    let
        jsonLiteralDecoder : Json.Decode.Decoder JsonLiteralAny
        jsonLiteralDecoder =
            Json.Decode.oneOf
                [ Nully () |> Json.Decode.null
                , Json.Decode.map Booly Json.Decode.bool
                , Json.Decode.map Floaty Json.Decode.float
                , Json.Decode.map Stringy Json.Decode.string
                ]

        jsonStructureDecoder : Json.Decode.Decoder JsonStructureAny
        jsonStructureDecoder =
            Json.Decode.oneOf
                [ Json.Decode.map Arrayy
                    (Json.Decode.array inDecoder)
                , Json.Decode.map Objecty
                    (Json.Decode.dict inDecoder)
                ]

        inDecoder : Json.Decode.Decoder JsonAnyIn
        inDecoder =
            (\() -> decoder |> Json.Decode.map AnyIn)
                |> Json.Decode.lazy
    in
    Json.Decode.oneOf
        [ Json.Decode.map Literaly jsonLiteralDecoder
        , Json.Decode.map Structurey jsonStructureDecoder
        ]


{-| This is the trickiest part of the whole package:
Converting between a valid [`JsonAny`](#JsonAny) value and your preferred elm representation.

Like with `encode`-`decode` pairs, there are a few things to consider:

These are the default morphs:

  - `Nully ()` <-> `Unity ()`
  - `Objecty ([ ( "tag", "elm/time:Time.Posix" ), ( "value", Floaty ) ] |> Dict.fromList)`
    <-> `Posixy` (considering dat iso format js uses)
  - `Objecty ([ ( "tag", "elm/core:Basics.Int" ), ( "value", Floaty ) ] |> Dict.fromList)`
    <-> `Inty`
  - `Objecty ([ ( "tag", "elm/core:Basics.Char" ), ( "value", Stringy ) ] |> Dict.fromList)`
    <-> `Chary`

TODO: add options (Maybe as customizable `Morph Origin Json.Encode.Value ...`):

  - `user/repository/Module.Variant` tag
  - `Module.Variant` tag
  - `Variant` tag
  - `aIndex` variant (/field) tag

-}
value : Translate JsonAny ValueAny
value =
    translate
        (Debug.todo "")
        (Debug.todo "")
