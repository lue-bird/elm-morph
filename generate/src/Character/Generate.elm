module Character.Generate exposing (main)

{-| Helps you generate the source code of the unicode `module Character`

Run `elm reactor` in this directory to preview & download.

Links

  - [`the-sett/elm-syntax-dsl`](https://package.elm-lang.org/packages/the-sett/elm-syntax-dsl/latest/)
  - [`elm/http`](https://dark.elm.dmy.fr/packages/elm/http/latest/)
  - [`BrianHicks/elm-csv`](https://dark.elm.dmy.fr/packages/BrianHicks/elm-csv/latest/Csv-Decode)
  - auto-generate similar to <https://dark.elm.dmy.fr/packages/miniBill/elm-unicode/latest/Unicode#Category>

-}

import Browser
import Csv.Decode
import Dict exposing (Dict)
import Dict.Extra as Dict
import Element as Ui
import Element.Background as UiBg
import Element.Border as UiBorder
import Element.Font as UiFont
import Element.Input as UiInput
import Elm.CodeGen as Generation exposing (applyBinOp, code, construct, fqVal, importStmt, markdown, piper, tuple, typeVar, typed, val)
import Elm.CodeGen.Extra exposing (..)
import File.Download
import Hex
import Html exposing (Html)
import Http
import Parser exposing ((|.), (|=), Parser)
import Set exposing (Set)
import String.Extra as String
import Task
import Time
import Ui.Extra as Ui exposing (edges)
import Unicode
import UnicodeCsv
import Zip


main : Program () State Event
main =
    Browser.element
        { init = \() -> init
        , update = \event _ -> reactTo event
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type alias State =
    { moduleFolding : Folding }


type alias UnicodeData =
    Dict
        -- category
        String
        (Dict
            -- block
            String
            (List { name : String, char : Char })
        )


unicodeNameToElmName : String -> String
unicodeNameToElmName =
    \unicodeName ->
        (case unicodeName |> String.leftOf "(" of
            "" ->
                unicodeName

            leftOfClose ->
                leftOfClose
        )
            |> String.toLower
            |> String.classify


unicodeCharacterDescriptionToElmName : String -> String
unicodeCharacterDescriptionToElmName =
    \unicodeCharacterDescription ->
        unicodeCharacterDescription
            |> String.replace "," ""
            |> String.replace "-" ""
            |> String.replace "quote" "Quote"
            |> String.replace " " ""


characterCsvDecode :
    Csv.Decode.Decoder
        { category : String
        , name : String
        , char : Char
        }
characterCsvDecode =
    Csv.Decode.into
        (\category name nameAlternative char ->
            { category = category
            , name =
                (case nameAlternative of
                    "" ->
                        name

                    nameAlternativeExisting ->
                        nameAlternativeExisting
                )
                    |> unicodeNameToElmName
            , char = char
            }
        )
        |> Csv.Decode.pipeline (Csv.Decode.column 2 Csv.Decode.string)
        |> Csv.Decode.pipeline (Csv.Decode.column 1 Csv.Decode.string)
        |> Csv.Decode.pipeline (Csv.Decode.column 10 Csv.Decode.string)
        |> Csv.Decode.pipeline
            (Csv.Decode.column 0 (csvDecodeParse charHexParser "char code in hex format"))


csvDecodeParse : Parser narrow -> String -> Csv.Decode.Decoder narrow
csvDecodeParse parser description =
    Csv.Decode.string
        |> Csv.Decode.andThen
            (\charHexString ->
                case charHexString |> Parser.run parser of
                    Ok char ->
                        char |> Csv.Decode.succeed

                    Err _ ->
                        Csv.Decode.fail ("expected " ++ description)
            )


blockCsvDecode :
    Csv.Decode.Decoder
        { name : String, charFirst : Char, charLast : Char }
blockCsvDecode =
    Csv.Decode.into
        (\blockName charRange ->
            { name =
                blockName |> unicodeNameToElmName
            , charFirst = charRange.start
            , charLast = charRange.end
            }
        )
        |> Csv.Decode.pipeline (Csv.Decode.column 1 Csv.Decode.string)
        |> Csv.Decode.pipeline
            (Csv.Decode.column 0
                (Csv.Decode.string
                    |> Csv.Decode.andThen
                        (\charHexString ->
                            case charHexString |> Parser.run (rangeParser charHexParser) of
                                Ok char ->
                                    char |> Csv.Decode.succeed

                                Err _ ->
                                    Csv.Decode.fail "expected range of 2 char codes in hex format: first..last"
                        )
                )
            )


type Folding
    = Shown
    | Folded



--


unicodeDataParseResult : Result UnicodeCsvDecodeErrors UnicodeData
unicodeDataParseResult =
    case
        ( UnicodeCsv.text
            |> Csv.Decode.decodeCustom
                { fieldSeparator = ';' }
                Csv.Decode.NoFieldNames
                characterCsvDecode
        , UnicodeCsv.blocks
            |> Csv.Decode.decodeCustom
                { fieldSeparator = ';' }
                Csv.Decode.NoFieldNames
                blockCsvDecode
        )
    of
        ( Ok decoded, Ok blocksDecoded ) ->
            decoded
                |> Dict.groupBy .category
                |> Dict.map
                    (\_ ->
                        List.map
                            (\character ->
                                { name = character.name, char = character.char }
                            )
                            >> Dict.groupBy
                                (\character ->
                                    let
                                        charCode =
                                            character.char |> Char.toCode
                                    in
                                    blocksDecoded
                                        |> List.foldl
                                            (\block soFar ->
                                                case soFar of
                                                    Just matchingBlock ->
                                                        matchingBlock |> Just

                                                    Nothing ->
                                                        if
                                                            charCode
                                                                >= (block |> .charFirst |> Char.toCode)
                                                                && (charCode
                                                                        <= (block |> .charLast |> Char.toCode)
                                                                   )
                                                        then
                                                            block.name |> Just

                                                        else
                                                            Nothing
                                            )
                                            Nothing
                                        |> Maybe.withDefault "NO_BLOCK"
                                )
                    )
                |> Ok

        ( Err decodeError, Ok _ ) ->
            { characters = decodeError |> Err, blocks = Ok () } |> Err

        ( Err decodeError, Err blocksDecodeError ) ->
            { characters = decodeError |> Err, blocks = blocksDecodeError |> Err } |> Err

        ( Ok _, Err decodeError ) ->
            { characters = Ok (), blocks = decodeError |> Err } |> Err


init : ( State, Cmd Event )
init =
    ( { moduleFolding = Folded }
    , Cmd.none
    )


type Event
    = ModulesDownloadClicked UnicodeData State
    | ModulesDownloadClickedAtTime ( Time.Zone, Time.Posix ) UnicodeData State
    | ModuleUnfoldClicked GeneratedModuleKind State


type GeneratedModuleKind
    = ModuleUnicode


reactTo : Event -> ( State, Cmd Event )
reactTo msg =
    case msg of
        ModulesDownloadClicked modules state ->
            ( state
            , Task.perform
                (\time -> state |> ModulesDownloadClickedAtTime time modules)
                (Time.here
                    |> Task.andThen
                        (\here ->
                            Time.now
                                |> Task.map (\now -> ( here, now ))
                        )
                )
            )

        ModulesDownloadClickedAtTime time modules state ->
            ( state
            , File.Download.bytes
                "elm-unicode.zip"
                "application/zip"
                (let
                    toZipEntry moduleFile =
                        zipEntryFromModule time moduleFile
                 in
                 Zip.fromEntries
                    [ toZipEntry (moduleCharacter modules)
                    ]
                    |> Zip.toBytes
                )
            )

        ModuleUnfoldClicked moduleKind state ->
            ( case moduleKind of
                ModuleUnicode ->
                    { state
                        | moduleFolding =
                            state.moduleFolding |> foldingToggle
                    }
            , Cmd.none
            )


foldingToggle : Folding -> Folding
foldingToggle visibility =
    case visibility of
        Shown ->
            Folded

        Folded ->
            Shown



-- tags
--


moduleCharacter :
    UnicodeData
    -> List (Module String)
moduleCharacter =
    \characterByCategory ->
        { name = [ "Character" ]
        , roleInPackage =
            PackageExposedModule
                { moduleComment =
                    \declarations ->
                        [ markdown "unicode `Char`s to [\"parse, don't validate\"](https://elm-radio.com/episode/parse-dont-validate/)"
                        , declarations |> docTagsFrom "Choice"
                        ]
                }
        , imports = []
        , declarations =
            characterByCategory
                |> Dict.remove "Ll"
                |> Dict.toList
                |> List.concatMap
                    (\( categoryName, blocks ) ->
                        let
                            categoryDescription =
                                case categoryName |> Unicode.categoryFromString of
                                    Nothing ->
                                        categoryName

                                    Just category ->
                                        category |> Unicode.categoryToDescription |> unicodeCharacterDescriptionToElmName
                        in
                        blocks
                            |> Dict.toList
                            |> List.map
                                (\( blockName, characters ) ->
                                    packageExposedTypeDecl Choice
                                        OpenType
                                        [ markdown
                                            ([ "Unicode characters in the block "
                                             , blockName
                                             , " in the category ["
                                             , categoryDescription
                                             , " ("
                                             , categoryName
                                             , ")](https://www.compart.com/en/unicode/category/"
                                             , categoryName
                                             , ")"
                                             ]
                                                |> String.concat
                                            )
                                        ]
                                        blockName
                                        []
                                        (characters
                                            |> List.map
                                                (\character ->
                                                    ( case categoryName of
                                                        "Lu" ->
                                                            [ "-- "
                                                            , character.name
                                                            , "\n      "
                                                            , character.char |> String.fromChar
                                                            ]
                                                                |> String.concat

                                                        _ ->
                                                            [ "-- '"
                                                            , character.char |> String.fromChar
                                                            , "'\n      "
                                                            , character.name
                                                            ]
                                                                |> String.concat
                                                    , []
                                                    )
                                                )
                                        )
                                )
                            |> (::)
                                (packageExposedTypeDecl Choice
                                    OpenType
                                    [ markdown
                                        ([ "Unicode characters in the category ["
                                         , categoryName
                                         , "](https://www.compart.com/en/unicode/category/"
                                         , categoryName
                                         , ")"
                                         ]
                                            |> String.concat
                                        )
                                    ]
                                    categoryDescription
                                    []
                                    (blocks
                                        |> Dict.keys
                                        |> List.map
                                            (\block -> ( block, [ Generation.typed block [] ] ))
                                    )
                                )
                    )
                |> (::)
                    (packageExposedTypeDecl Choice
                        OpenType
                        [ markdown "Unicode characters in any category"
                        ]
                        "Character"
                        []
                        (characterByCategory
                            |> Dict.keys
                            |> List.map
                                (\categoryShort ->
                                    let
                                        categoryDescription =
                                            case categoryShort |> Unicode.categoryFromString of
                                                Nothing ->
                                                    categoryShort

                                                Just category ->
                                                    category |> Unicode.categoryToDescription |> unicodeCharacterDescriptionToElmName
                                    in
                                    ( categoryDescription
                                    , [ Generation.typed categoryDescription [] ]
                                    )
                                )
                        )
                    )
        }



--


button :
    event
    -> List (Ui.Attribute event)
    -> Ui.Element event
    -> Ui.Element event
button onPress attributes label =
    UiInput.button
        ([ UiBorder.color interactiveColor
         , UiBorder.widthEach { edges | bottom = 2 }
         ]
            ++ attributes
        )
        { onPress = Just onPress, label = label }


unicodeLoadedUi : UnicodeData -> State -> Ui.Element Event
unicodeLoadedUi unicodeData state =
    [ Ui.text "elm unicode"
        |> Ui.el
            [ UiFont.size 40
            , UiFont.family [ UiFont.monospace ]
            ]
    , button (state |> ModulesDownloadClicked unicodeData)
        [ Ui.paddingXY 0 16
        ]
        (Ui.text "⬇ download")
    , ((Ui.text "📂 preview modules"
            |> Ui.el [ Ui.paddingXY 0 6 ]
       )
        :: (let
                switchButton text switch =
                    button switch
                        [ Ui.padding 12
                        , Ui.width Ui.fill
                        ]
                        (Ui.text text
                            |> Ui.el
                                [ UiFont.family [ UiFont.monospace ] ]
                        )
                        |> Ui.el
                            [ Ui.paddingXY 0 4
                            , Ui.moveUp 6
                            ]
            in
            [ { kind = ModuleUnicode
              , name = "unicode"
              , folding = state.moduleFolding
              , module_ = unicodeData |> moduleCharacter
              }
            ]
                |> List.map
                    (\{ kind, name, module_, folding } ->
                        case folding of
                            Shown ->
                                [ Ui.el
                                    [ Ui.width (Ui.px 1)
                                    , UiBg.color interactiveColor
                                    , Ui.height Ui.fill
                                    ]
                                    Ui.none
                                , [ switchButton ("⌃ " ++ name) (state |> ModuleUnfoldClicked kind)
                                  , module_
                                        |> Ui.module_
                                        |> Ui.el [ Ui.moveRight 5 ]
                                  ]
                                    |> Ui.column []
                                ]
                                    |> Ui.row
                                        [ Ui.height Ui.fill ]

                            Folded ->
                                switchButton ("⌄ " ++ name) (state |> ModuleUnfoldClicked kind)
                    )
           )
      )
        |> Ui.column
            [ Ui.width Ui.fill
            ]
    ]
        |> Ui.column
            [ Ui.paddingXY 40 60
            , Ui.spacing 46
            , Ui.width Ui.fill
            , Ui.height Ui.fill
            , UiBg.color (Ui.rgb255 35 36 31)
            , UiFont.color (Ui.rgb 1 1 1)
            ]


type alias UnicodeCsvDecodeErrors =
    { characters : Result Csv.Decode.Error ()
    , blocks : Result Csv.Decode.Error ()
    }


csvDecodeErrorUi :
    UnicodeCsvDecodeErrors
    -> Ui.Element event_
csvDecodeErrorUi unicodeCsvDecodeErrors =
    Ui.paragraph []
        [ Ui.text
            ([ "Failed loading\n"
             , case unicodeCsvDecodeErrors.characters of
                Ok () ->
                    ""

                Err error ->
                    [ "- unicode from https://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt due to an unexpected csv format: "
                    , error |> Csv.Decode.errorToString
                    ]
                        |> String.concat
             , case unicodeCsvDecodeErrors.blocks of
                Ok () ->
                    ""

                Err error ->
                    [ "- unicode blocks due to an unexpected csv format: "
                    , error |> Csv.Decode.errorToString
                    ]
                        |> String.concat
             ]
                |> String.concat
            )
        ]


view : State -> Html Event
view =
    \state ->
        let
            ui =
                case unicodeDataParseResult of
                    Err csvDecodeError ->
                        csvDecodeError |> csvDecodeErrorUi

                    Ok loaded ->
                        state |> unicodeLoadedUi loaded
        in
        ui
            |> Ui.layoutWith
                { options =
                    [ Ui.focusStyle
                        { borderColor = Ui.rgba 0 1 1 0.4 |> Just
                        , backgroundColor = Nothing
                        , shadow = Nothing
                        }
                    ]
                }
                []


interactiveColor : Ui.Color
interactiveColor =
    Ui.rgb 1 0.4 0



-- hex


rangeParser : Parser end -> Parser { start : end, end : end }
rangeParser endParser =
    Parser.succeed (\start end -> { start = start, end = end })
        |= endParser
        |. Parser.symbol ".."
        |= endParser


charHexParser : Parser Char
charHexParser =
    Parser.chompWhile charIsHex
        |> Parser.getChompedString
        |> Parser.andThen
            (\hexString ->
                case hexString |> String.toLower |> Hex.fromString of
                    Err error ->
                        error |> Parser.problem

                    Ok hex ->
                        hex |> Char.fromCode |> Parser.succeed
            )


charIsHex : Char -> Bool
charIsHex =
    \char ->
        case char |> Char.toUpper of
            '0' ->
                True

            '1' ->
                True

            '2' ->
                True

            '3' ->
                True

            '4' ->
                True

            '5' ->
                True

            '6' ->
                True

            '7' ->
                True

            '8' ->
                True

            '9' ->
                True

            'A' ->
                True

            'B' ->
                True

            'C' ->
                True

            'D' ->
                True

            'E' ->
                True

            'F' ->
                True

            _ ->
                False
