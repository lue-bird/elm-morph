module Parser.Error exposing
    ( describe
    , fromLastToFromFirstIn, fromLastToFromFirstInLine
    , TextLocation, fromLastToFromFirstInLines
    )

{-| Error reporting

@docs describe


## offset

@docs fromLastToFromFirstIn, fromLastToFromFirstInLine
@docs TextLocation, fromLastToFromFirstInLines

-}

import Parser
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Stack


{-| Position withing multiple lines.
-}
type alias TextLocation =
    RecordWithoutConstructorFunction
        { line : Int
        , column : Int
        }


{-| Create an [`Error`](Parser#Error) message.

    import Parser
    import Text.Parser as Text
    import Char.Parser exposing (letter)

    -- Getting a digit instead of a letter.
    "123"
        |> Text.narrowWith letter
        |> Result.mapError
            (expectationMissMessage { source = "123" })
    --> Err "1:1: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '1'."

    -- Running out of input characters.
    ""
        |> Text.narrowWith  letter
        |> Result.mapError
            (expectationMissMessage { source = "" })
    --> Err "1:0: I was expecting a letter [a-zA-Z]. I reached the end of the input."

-}
expectationMissMessage :
    { source : String }
    -> Parser.ExpectationMiss Char
    -> String
expectationMissMessage { source } =
    \error ->
        case error.stuckAtFromLast of
            0 ->
                [ "I was expecting "
                , error.expected |> expectedToString { source = source }
                , " but I reached the end of the input"
                ]
                    |> String.concat

            stuckAtFromLast ->
                let
                    stuckAt =
                        stuckAtFromLast |> fromLastToFromFirstInLine source
                in
                [ "At "
                , stuckAt |> fromLastToFromFirstInLines source |> locationToString
                , " I was expecting "
                , error.expected |> expectedToString { source = source }
                , ". I got stuck on "
                , source |> String.slice stuckAt (stuckAt + 1)
                , "."
                ]
                    |> String.concat


expectedToString : { source : String } -> Parser.Expected Char -> String
expectedToString source =
    \expected ->
        case expected of
            Parser.ExpectedSpecifically atomSpecific ->
                atomSpecific |> String.fromChar

            Parser.ExpectedCustom custom ->
                custom

            Parser.Expected1In possibilities ->
                [ "one of these possibilities:\n"
                , possibilities
                    |> List.map (snippet source)
                    |> List.map
                        (\possibility ->
                            "\n  - "
                                ++ (possibility
                                        |> String.join "    "
                                   )
                        )
                    |> String.concat
                ]
                    |> String.concat


{-| How far parsing got from the beginning of an input source.

  - 0 for before the first input atom
  - 1 for the first input atom
  - input-length for the last input atom

Use [`fromLastToFromFirstInLine`](#fromLastToFromFirstInLine) for `String` inputs.

-}
fromLastToFromFirstIn : List atom_ -> Int -> Int
fromLastToFromFirstIn inputSource =
    \fromLast ->
        1
            + ((inputSource |> List.length) - 1)
            - fromLast


fromLastToFromFirstInLines : String -> Int -> TextLocation
fromLastToFromFirstInLines source =
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


{-| How far parsing got from the beginning of an input source `String`.

Use [`fromLastToFromFirstIn`](#fromLastToFromFirstIn) for any `List` inputs.

-}
fromLastToFromFirstInLine : String -> Int -> Int
fromLastToFromFirstInLine inputSource =
    fromLastToFromFirstIn (inputSource |> String.toList)


{-| Present the `TextLocation` as `"line:column"`, for example

    { line = 3, column = 10 } |> locationToString
    --> "line 3)10"

-}
locationToString : TextLocation -> String
locationToString =
    \location ->
        [ "line "
        , location.line |> String.fromInt
        , ")"
        , location.column |> String.fromInt
        ]
            |> String.concat


{-| Dumps the error into a human-readable format.

    import Parser exposing (Parser, take, drop, into, succeed, atLeast, atom, take)
    import Char.Parser as Char
    import Text.Parser as Text

    "  abc  "
        |> Text.narrowWith
            (succeed (\number -> number)
                |> take (atLeast 0 Char.blank)
                |> drop Text.number
            )
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

    "  (12,)  "
        |> Text.narrowWith
            (succeed (\point -> point)
                |> drop (atLeast 0 Char.blank)
                |> take point
            )
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

    "  [(12,34),(56,)]  "
        |> Text.narrowWith
            (succeed (\line -> line)
                |> drop (atLeast 0 Char.blank)
                |> take line
            )
        |> Result.mapError (dump { source = "  [(12,34),(56,)]  " })
    --> Err
    -->     [ "I was expecting a digit [0-9]. I got stuck when I got the character ')'."
    -->     , "  in Point at line 1:12"
    -->     , "  in Line at line 1:3"
    -->     , ""
    -->     , "1|  [(12,34),(56,)]  "
    -->     , "             ~~~~^"
    -->     ]

-}
describe :
    { source : String }
    -> Parser.Error Char narrow_
    -> { message : String, details : List String }
describe { source } =
    \error ->
        case error of
            Parser.ExpectationMiss expectationMiss ->
                { message = expectationMiss |> expectationMissMessage { source = source }
                , details =
                    [ expectationMiss.context
                        |> List.map
                            (\context ->
                                [ "  in "
                                , context.description
                                , " starting at "
                                , (source |> String.length)
                                    - context.fromLast
                                    |> fromLastToFromFirstInLines source
                                    |> locationToString
                                ]
                                    |> String.concat
                            )
                    , expectationMiss |> snippet { source = source }
                    ]
                        |> List.concat
                }

            Parser.InputRemaining remaining ->
                { message =
                    [ "I was done parsing when I unexpectedly found remaining input: \""
                    , remaining.input |> Stack.toList |> String.fromList
                    , "\""
                    ]
                        |> String.concat
                , details = []
                }


{-| Dumps a snippet of the input text that caused the parser to fail.

    import Parser exposing (Parser, drop, into, succeed, atLeast, take, atom)
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

    "  (12,)  "
        |> Text.narrowWith
            (succeed (\point -> point)
                |> drop (atLeast 0 Char.blank)
                |> take point
            )
        |> Result.mapError dumpCodeSnippet
    --> Err
    -->     [ "1|  (12,)  "
    -->     , "    ~~~~^"
    -->     ]

    String.join "\n"
        [ "  "
        , "  (  "
        , "  12  "
        , "  ,  "
        , "  )  "
        , "  "
        ]
        |> Text.narrowWith
            (succeed (\point -> point)
                |> drop (atLeast 0 Char.blank)
                |> take point
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
snippet :
    { source : String }
    -> Parser.ExpectationMiss Char
    -> List String
snippet { source } =
    \error ->
        let
            errorOffset =
                error.stuckAtFromLast |> fromLastToFromFirstInLine source

            errorLocation =
                errorOffset |> fromLastToFromFirstInLines source

            rangeStart =
                case error.context of
                    [] ->
                        errorLocation

                    context :: _ ->
                        (errorOffset - context.fromLast)
                            |> fromLastToFromFirstInLines source

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
