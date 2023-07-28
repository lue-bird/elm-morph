module Json.Morph exposing
    ( value
    , string, stringBroadWith
    , eachTag
    , JsValueMagic, jsValueMagic
    )

{-| [Morph](Morph#Morph) [JSON](Json)


## morph

@docs value
@docs string, stringBroadWith
@docs eachTag
@docs JsValueMagic, jsValueMagic

-}

import Array
import Decimal exposing (Decimal)
import Decimal.Morph
import Emptiable
import Json exposing (Json)
import Json.Decode
import Json.Encode
import Morph exposing (Morph, MorphIndependently)
import Possibly exposing (Possibly(..))
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


jsValueMagicEncode : () -> (Json String -> JsValueMagic)
jsValueMagicEncode () =
    \jsonAny ->
        case jsonAny of
            Atom atom ->
                atom |> atomJsValueMagicEncode

            Composed composed ->
                composed |> composedJsValueMagicEncode ()


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
                    |> Morph.toBroad
                        (Decimal.Morph.orException |> Morph.over Decimal.Morph.orExceptionFloat)
                    |> Json.Encode.float

            Json.String stringAtom ->
                stringAtom |> Json.Encode.string


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


decimalFloatMorph : Morph Decimal Float
decimalFloatMorph =
    Decimal.Morph.orException
        |> Morph.over Decimal.Morph.orExceptionFloat


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
jsValueMagic : Morph (Json String) JsValueMagic
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


{-| [Morph](Morph#Morph) to valid [`Json` value](Json#Json) format from a `String`

[Broadens](Morph#toBroad) to a compact `String`.
To adjust format readability → [`stringBroadWith`](#stringBroadWith)

-}
string : Morph (Json String) String
string =
    stringBroadWith { indentation = 0 }


{-| [`Json.string`](#string) [Morph](Morph#Morph) with adjustable indentation.

This is a bit clunky since `stringBroadWith` is using [`Json.Encode.encode`](https://dark.elm.dmy.fr/packages/elm/json/latest/Json-Encode#encode).
I bet we could make this cleaner with a custom [`MorphRow`](Morph#MorphRow)
and a nice pretty-printable broad representation like [`the-sett/elm-pretty-printer`](https://github.com/the-sett/elm-pretty-printer/blob/3.0.0/src/Internals.elm#L4).

-}
stringBroadWith : { indentation : Int } -> Morph (Json String) String
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


composedJsValueMagicEncode : () -> (Json.Composed String -> JsValueMagic)
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


jsonComposedDecoder : Json.Decode.Decoder (Json.Composed String)
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


toValue : Json Value.IndexAndName -> Value.Value Value.IndexAndName
toValue =
    \json ->
        case json of
            Atom atom ->
                atom |> atomToValue

            Composed composed ->
                composed |> composedToValue |> Composed


atomToValue : Json.Atom -> Value.Value Value.IndexAndName
atomToValue =
    \atom ->
        case atom of
            Json.Null unit ->
                unit |> Value.Unit |> Atom

            Json.Number decimal ->
                decimal |> Value.Number |> Atom

            Json.String string_ ->
                string_ |> Value.String |> Atom

            Json.Bool bool ->
                { value = () |> Value.Unit |> Atom
                , tag =
                    if bool then
                        { index = 0, name = "False" }

                    else
                        { index = 1, name = "True" }
                }
                    |> Value.Variant
                    |> Composed


fromValueImplementation : Value.Value tag -> Json tag
fromValueImplementation =
    \json ->
        case json of
            Atom atom ->
                atom |> atomFromValue |> Atom

            Composed composed ->
                composed |> composedFromValue |> Composed



-- tag


atomFromValue : Value.Atom -> Json.Atom
atomFromValue =
    \atom ->
        case atom of
            Value.Unit () ->
                Json.Null ()

            Value.String stringAtom ->
                stringAtom |> Json.String

            Value.Number decimal ->
                decimal |> Json.Number


{-| Convert a [representation of an elm value](Value#Value) to a [valid `Json` value](Json#Json)
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
    Morph.oneToOne fromValueImplementation toValue


composedToValue :
    Json.Composed Value.IndexAndName
    -> Value.Composed Value.IndexAndName
composedToValue =
    \composed ->
        case composed of
            Json.Array array ->
                array |> Array.toList |> List.map toValue |> Value.List

            Json.Object object ->
                object
                    |> List.map
                        (\tagged ->
                            { tag = tagged.tag
                            , value = tagged.value |> toValue
                            }
                        )
                    |> Value.Record


composedFromValue : Value.Composed tag -> Json.Composed tag
composedFromValue =
    \composed ->
        case composed of
            Value.List list ->
                list |> List.map fromValueImplementation |> Array.fromList |> Json.Array

            Value.Record record ->
                record
                    |> List.map
                        (\field ->
                            { tag = field.tag
                            , value = field.value |> fromValueImplementation
                            }
                        )
                    |> Json.Object

            Value.Variant variant ->
                { tag = variant.tag, value = variant.value |> fromValueImplementation }
                    |> List.singleton
                    |> Json.Object


{-| [`Morph.OneToOne`](Morph#OneToOne) for [`Json`](Json#Json)
where [`Json.tagMap`](Json#tagMap) is called in both directions

    ...
        |> Morph.over (Json.eachTag Value.compact)

    -- or
    ...
        |> Morph.over (Json.eachTag Value.descriptive)

Links: [`Value.compact`](Value-Morph#compact), [`Value.descriptive`](Value-Morph#descriptive)

-}
eachTag :
    MorphIndependently
        (tagBeforeMap -> Result (Morph.ErrorWithDeadEnd Never) tagMapped)
        (tagBeforeUnmap -> tagUnmapped)
    ->
        MorphIndependently
            (Json tagBeforeMap
             -> Result (Morph.ErrorWithDeadEnd never_) (Json tagMapped)
            )
            (Json tagBeforeUnmap -> Json tagUnmapped)
eachTag tagTranslate_ =
    Morph.oneToOneOn ( Json.tagMap, Json.tagMap ) tagTranslate_
