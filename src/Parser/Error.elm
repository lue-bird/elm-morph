module Parser.Error exposing
    ( message, textMessage, dump, dumpCodeSnippet
    , TextLocation
    )

{-| Error reporting

@docs message, textMessage, dump, dumpCodeSnippet


## text location

@docs TextLocation

-}

import Parser exposing (Error)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)


{-| Position withing multiple lines.
-}
type alias TextLocation =
    RecordWithoutConstructorFunction
        { line : Int
        , column : Int
        }


{-| Creates an error message from an `Error` data.

    import Parser exposing (parse)
    import Char.Parser exposing (letter)

    -- Getting a digit instead of a letter.
    parse "123" letter
        |> Result.mapError message
    --> Err "1:1: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '1'."

    -- Running out of input characters.
    parse "" letter
        |> Result.mapError message
    --> Err "1:0: I was expecting a letter [a-zA-Z]. I reached the end of the input."

-}
textMessage : Parser.Error Char -> String
textMessage =
    message String.fromChar


message : (atom -> String) -> Error atom -> String
message inputSingleToString =
    \error ->
        [ error.location |> String.fromInt -- |> locationToString
        , ": I was expecting "
        , error.expected |> expectedToString inputSingleToString
        , ". "
        , case error.stuckOn of
            Just character ->
                [ "I got stuck when I got "
                , character |> inputSingleToString
                , "."
                ]
                    |> String.concat

            Nothing ->
                "I reached the end of the input"
        ]
            |> String.concat


expectedToString : (atom -> String) -> Parser.Expected atom -> String
expectedToString inputSingleToString =
    \expected ->
        case expected of
            Parser.ExpectedSpecifically inputSpecific ->
                inputSpecific |> inputSingleToString

            Parser.ExpectedEnd ->
                "the end of the input, but there is still some remaining input"

            Parser.ExpectedCustom custom ->
                custom


locationToString : TextLocation -> String
locationToString =
    \location ->
        [ location.line |> String.fromInt
        , ":"
        , location.column |> String.fromInt
        ]
            |> String.concat


{-| Dumps the error into a human-readable format.

    import Parser exposing (Parser, drop, into, parse, succeed, atLeast, atom, take)
    import Char.Parser as Char
    import Text.Parser as Text

    atLeast 0 Char.blank
        |> andThen (\_ -> Text.number)
        |> parse "  abc  "
        |> Result.mapError (dump "filename.txt")
    --> Err
    -->     [ "[ERROR] filename.txt:1:3: I was expecting a digit [0-9]. I got stuck when I got the character 'a'."
    -->     , ""
    -->     , "1|  abc  "
    -->     , "    ^"
    -->     ]


    type alias Point =
        { x : Float
        , y : Float
        }

    point : Parser Point
    point =
        into "Point"
            (succeed Point
                |> drop (atom '(')
                |> take Text.number
                |> drop (atom ',')
                |> take Text.number
                |> drop (atom ')')
            )

    atLeast 0 Char.blank
        |> andThen (\_ -> point)
        |> parse "  (12,)  "
        |> Result.mapError (dump "filename.txt")
    --> Err
    -->     [ "[ERROR] filename.txt:1:7: I was expecting a digit [0-9]. I got stuck when I got the character ')'."
    -->     , "  in Point at line 1:3"
    -->     , ""
    -->     , "1|  (12,)  "
    -->     , "    ~~~~^"
    -->     ]

    type alias Line =
        { p1 : Point
        , p2 : Point
        }

    line : Parser Line
    line =
        into "Line"
            (succeed (\p1 p2 -> { p1 = p1, p2 = p2 })
                |> drop (atom '[')
                |> take point
                |> drop (atom ',')
                |> take point
                |> drop (atom ']')
            )

    succeed (\line -> line)
        |> drop (atLeast 0 Char.blank)
        |> take line
        |> parse "  [(12,34),(56,)]  "
        |> Result.mapError (dump "filename.txt")
    --> Err
    -->     [ "[ERROR] filename.txt:1:16: I was expecting a digit [0-9]. I got stuck when I got the character ')'."
    -->     , "  in Point at line 1:12"
    -->     , "  in Line at line 1:3"
    -->     , ""
    -->     , "1|  [(12,34),(56,)]  "
    -->     , "             ~~~~^"
    -->     ]

-}
dump :
    { source : String
    , error : Parser.Error Char
    }
    -> List String
dump { source, error } =
    let
        location =
            error.location
                |> locationIn source
    in
    [ [ [ "[ERROR] ", source, ":", error |> textMessage ]
            |> String.concat
      , ""
      ]
    , error.context
        |> List.map
            (\context ->
                [ "  in "
                , context.name
                , " at line "
                , location |> locationToString
                ]
                    |> String.concat
            )
    , dumpCodeSnippet { error = error, source = source }
    ]
        |> List.concat


locationIn : String -> Int -> { line : Int, column : Int }
locationIn source =
    \offset ->
        source
            |> String.lines
            |> List.foldl
                (\line locationOrSoFar ->
                    case locationOrSoFar of
                        Ok location ->
                            location |> Ok

                        Err soFar ->
                            let
                                nextOffset =
                                    soFar.offset + (line |> String.length)
                            in
                            if nextOffset > offset then
                                { column = soFar.column, line = offset - nextOffset } |> Ok

                            else
                                Err { column = soFar.column + 1, offset = nextOffset }
                )
                (Err { column = 1, offset = 1 })
            |> -- meh
               Result.withDefault { line = 0, column = 0 }


{-| Dumps a snippet of the input text that caused the parser to fail.

    import Parser exposing (Parser, drop, into, parse, succeed, atLeast, take, atom)
    import Char.Parser exposing (blank)
    import Text.Parser exposing (number)

    type alias Point =
        { x : Float
        , y : Float
        }

    point : Parser Point
    point =
        into "Point"
            (succeed (\x y -> { x = x, y = y })
                |> drop (atom '(')
                |> drop (atLeast 0 blank)
                |> take number
                |> drop (atLeast 0 blank)
                |> drop (atom ',')
                |> drop (atLeast 0 blank)
                |> take number
                |> drop (atLeast 0 blank)
                |> drop (atom ')')
            )

    atLeast 0 blank
        |> andThen (\_ -> point)
        |> parse "  (12,)  "
        |> Result.mapError dumpCodeSnippet
    --> Err
    -->     [ "1|  (12,)  "
    -->     , "    ~~~~^"
    -->     ]

    atLeast 0 blank
        |> andThen (\_ -> point)
        |> parse
            (String.join "\n"
                [ "  "
                , "  (  "
                , "  12  "
                , "  ,  "
                , "  )  "
                , "  "
                ]
            )
        |> Result.mapError dumpCodeSnippet
    --> Err
    -->     [ "2|  (  "
    -->     , "3|  12  "
    -->     , "4|  ,  "
    -->     , "5|  )  "
    -->     , " +~~^"
    -->     ]

-}
dumpCodeSnippet :
    { error : Parser.Error Char
    , source : String
    }
    -> List String
dumpCodeSnippet { error, source } =
    let
        errorLocation =
            error.location |> locationIn source

        rangeStart =
            case error.context of
                [] ->
                    errorLocation

                context :: _ ->
                    context.location
                        |> locationIn source

        lineNumberWidth =
            max
                (rangeStart.line |> String.fromInt |> String.length)
                (errorLocation.line |> String.fromInt |> String.length)

        sourceSnippet =
            source
                |> String.lines
                |> List.drop (rangeStart.line - 1)
                |> List.take (errorLocation.line - rangeStart.line + 1)
                |> List.indexedMap
                    (\i ln ->
                        [ String.padLeft lineNumberWidth
                            ' '
                            (String.fromInt (rangeStart.line + i))
                        , "|"
                        , ln
                        ]
                            |> String.concat
                    )

        underline =
            [ String.repeat lineNumberWidth " "
            , case error.context of
                [] ->
                    String.repeat errorLocation.column " "

                _ :: _ ->
                    if rangeStart.line == errorLocation.line then
                        [ String.repeat rangeStart.column " "
                        , String.repeat (errorLocation.column - rangeStart.column) "~"
                        ]
                            |> String.concat

                    else
                        [ "+"
                        , String.repeat (errorLocation.column - 1) "~"
                        ]
                            |> String.concat
            , "^"
            ]
                |> String.concat
    in
    sourceSnippet ++ [ underline ]
