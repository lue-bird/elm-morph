module Json.Morph exposing
    ( compact, value
    , string, stringBroadWith
    , JsValueMagic, jsValueMagic
    )

{-| [Morph](Morph#Morph) [JSON](Json)

@docs compact, value
@docs string, stringBroadWith
@docs JsValueMagic, jsValueMagic

-}

import Decimal exposing (Decimal)
import Decimal.Morph
import Emptiable
import Json exposing (Json)
import Json.Decode
import Json.Encode
import Morph exposing (Morph, MorphIndependently, MorphOrError)
import Possibly exposing (Possibly(..))
import Result.Morph
import Stack
import Tree
import Value exposing (AtomOrComposed(..))


{-| A value from the javascript side:
from a [`port`](https://guide.elm-lang.org/interop/ports.html),
on `init`,
from [`elm/http`](https://package.elm-lang.org/packages/elm/http/latest), ...

Compiler magic. Not `case`able. Elm crashes on `==`.
Can include functions, proxies, getters, bigInts, anything

and.. of course this can be abused to break elm's promises 🙈, see for example
[randomness without `Cmd` ellie](https://ellie-app.com/hpXzJxh4HRda1)

-}
type alias JsValueMagic =
    Json.Encode.Value


{-| Should be redundant if `anyDecoder` catches all cases
-}
decodeErrorToMorph : Json.Decode.Error -> Morph.Error
decodeErrorToMorph =
    \decodeError ->
        case decodeError of
            Json.Decode.Field fieldName error ->
                Morph.ElementsError ({ location = fieldName, error = error |> decodeErrorToMorph } |> Stack.one)

            Json.Decode.Index arrayIndex error ->
                { index = arrayIndex
                , error = error |> decodeErrorToMorph
                }
                    |> Stack.one
                    |> Morph.PartsError

            Json.Decode.OneOf possibilities ->
                case possibilities |> Stack.fromList of
                    Emptiable.Empty Possible ->
                        "missing expected possibilities in Json.Decode.oneOf"
                            |> Morph.DeadEnd

                    Emptiable.Filled stacked ->
                        stacked
                            |> Emptiable.filled
                            |> Stack.map (\_ -> decodeErrorToMorph)
                            |> Morph.ChoiceError

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


jsValueMagicEncode : () -> (Json -> JsValueMagic)
jsValueMagicEncode () =
    \jsonAny ->
        case jsonAny of
            Atom atom ->
                atom |> atomJsValueMagicEncode

            Composed composed ->
                composed |> composedJsValueMagicEncode ()


decimalFloatMorph : Morph Decimal Float
decimalFloatMorph =
    Result.Morph.toOk
        |> Morph.over Decimal.Morph.orExceptionFloat
        |> Morph.errorMap (Morph.deadEndMap Decimal.exceptionToString)


atomJsValueMagicEncode : Json.Atom -> JsValueMagic
atomJsValueMagicEncode =
    \atom ->
        case atom of
            Json.Null () ->
                Json.Encode.null

            Json.Bool boolAtom ->
                boolAtom |> Json.Encode.bool

            Json.Number floatAtom ->
                floatAtom
                    |> Morph.toBroad decimalFloatMorph
                    |> Json.Encode.float

            Json.String stringAtom ->
                stringAtom |> Json.Encode.string


{-| Some elm functions,
[for example html events](https://dark.elm.dmy.fr/packages/elm/html/latest/Html-Events#on)
require a `Json.Decode.Decoder`,
which is an opaque type and can't be constructed (for example by from `Json.Decode.Value -> Result Json.Error elm`)

In general, try to use [`Json.jsValueMagic`](#jsValueMagic) instead wherever possible

-}
jsValueMagicDecoder : Json.Decode.Decoder Json
jsValueMagicDecoder =
    Json.Decode.oneOf
        [ Json.Decode.map Atom jsonAtomDecoder
        , Json.Decode.map Composed jsonComposedDecoder
        ]


jsonAtomDecoder : Json.Decode.Decoder Json.Atom
jsonAtomDecoder =
    Json.Decode.oneOf
        [ Json.Null () |> Json.Decode.null
        , Json.Decode.map Json.Bool Json.Decode.bool
        , Json.Decode.andThen
            (\float ->
                case float |> Morph.toNarrow decimalFloatMorph of
                    Ok decimal ->
                        Json.Number decimal |> Json.Decode.succeed

                    Err exception ->
                        Morph.descriptionAndErrorToTree (decimalFloatMorph |> Morph.description) exception
                            |> Tree.map .text
                            |> Morph.treeToLines
                            |> String.join "\n"
                            |> Json.Decode.fail
            )
            Json.Decode.float
        , Json.Decode.map Json.String Json.Decode.string
        ]


{-| [Morph](Morph#Morph) to valid [`Json` value](Json#Json) format from [`JsValueMagic`](#JsValueMagic)

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
jsValueMagic : Morph Json JsValueMagic
jsValueMagic =
    Morph.named "JSON"
        { description = Morph.CustomDescription
        , toNarrow =
            \jsValueMagicBeforeNarrow ->
                jsValueMagicBeforeNarrow
                    |> Json.Decode.decodeValue jsValueMagicDecoder
                    |> Result.mapError decodeErrorToMorph
        , toBroad = jsValueMagicEncode ()
        }


{-| [Morph](Morph#Morph) from a `String`

[Broadens](Morph#toBroad) to a compact `String`.
To adjust format readability → [`stringBroadWith`](#stringBroadWith)

-}
string : Morph Json String
string =
    stringBroadWith { indentation = 0 }


{-| [`Json.string`](#string) [Morph](Morph#Morph) with adjustable indentation.

This is a bit clunky since `stringBroadWith` is using [`Json.Encode.encode`](https://dark.elm.dmy.fr/packages/elm/json/latest/Json-Encode#encode).
I bet we could make this cleaner with a custom [`MorphRow`](Morph#MorphRow)
and a nice pretty-printable broad representation like [`the-sett/elm-pretty-printer`](https://github.com/the-sett/elm-pretty-printer/blob/3.0.0/src/Internals.elm#L4).

-}
stringBroadWith : { indentation : Int } -> Morph Json String
stringBroadWith { indentation } =
    Morph.named "JSON"
        { description = Morph.CustomDescription
        , toNarrow =
            \jsValueMagicBroad ->
                jsValueMagicBroad
                    |> Json.Decode.decodeString jsValueMagicDecoder
                    |> Result.mapError decodeErrorToMorph
        , toBroad =
            \json ->
                json
                    |> jsValueMagicEncode ()
                    |> Json.Encode.encode indentation
        }


composedJsValueMagicEncode : () -> (Json.Composed -> JsValueMagic)
composedJsValueMagicEncode () =
    \composedAny ->
        case composedAny of
            Json.Array arrayAny ->
                arrayAny
                    |> Json.Encode.array (jsValueMagicEncode ())

            Json.Object objectAny ->
                objectAny
                    |> List.map
                        (\field ->
                            ( field.tag
                            , field.value |> jsValueMagicEncode ()
                            )
                        )
                    |> Json.Encode.object


jsonComposedDecoder : Json.Decode.Decoder Json.Composed
jsonComposedDecoder =
    Json.Decode.lazy
        (\() ->
            Json.Decode.oneOf
                [ Json.Decode.map Json.Array
                    (Json.Decode.array jsValueMagicDecoder)
                , Json.Decode.map Json.Object
                    (Json.Decode.keyValuePairs jsValueMagicDecoder
                        |> Json.Decode.map
                            (\keyValuePairs ->
                                keyValuePairs
                                    |> List.map
                                        (\( tag, v ) -> { tag = tag, value = v })
                            )
                    )
                ]
        )


{-| [`Morph.OneToOne`](Morph#OneToOne) from a [generic representation of an elm value](Value#Value)

[Inverse](Morph#invert) of [`Value.Morph.json`](Value-Morph#json)

-}
value : MorphOrError Json (Value.Value String) error_
value =
    Morph.oneToOne Json.fromValue Json.toValue


{-| With compact indexes. Use in combination with [`Value.Morph.eachTag`](Value-Morph#eachTag)
just before chaining to [`Value.Morph.json`](Value-Morph#json)

  - part tag = [`part` `MorphValue`](Value-Morph#part) index index in the builder
  - variant tag = [`variant` `MorphValue`](Value-Morph#variant) index in the builder
  - →
      - unreadable to humans
      - only readable by other tools if they know the variant and field order
      - not easily debuggable
      - shuffling [`Value.Morph.part`](Value-Morph#part) order → breaking change
      - renaming [`Value.Morph.part`](Value-Morph#part)s → no change

See also [`Value.Morph.descriptive`](Value-Morph#descriptive)

-}
compact :
    MorphIndependently
        (String -> Result error_ Value.IndexOrName)
        (Value.IndexAndName -> String)
compact =
    Morph.oneToOne
        (\tag ->
            case tag |> String.uncons of
                Just ( 'a', tagAfterA ) ->
                    case tagAfterA |> String.toInt of
                        Nothing ->
                            String.cons 'a' tagAfterA |> Value.Name

                        Just index ->
                            index |> Value.Index

                Just ( firstCharNotA, tagAfterFirstChar ) ->
                    String.cons firstCharNotA tagAfterFirstChar |> Value.Name

                Nothing ->
                    "" |> Value.Name
        )
        (\tag -> "a" ++ (tag.index |> String.fromInt))
