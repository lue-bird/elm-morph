module ConversionStep.Error exposing
    ( describe
    , TextLocation, downToUpIn, downToUpInLine, downToUpInLines
    )

{-| Error reporting

@docs describe


## offset

@docs TextLocation, downToUpIn, downToUpInLine, downToUpInLines

-}

import ConversionStep
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Stack


{-| Position withing multiple lines.
-}
type alias TextLocation =
    RecordWithoutConstructorFunction
        { line : Int
        , column : Int
        }


{-| How far parsing got from the beginning of an input source.

  - 0 for before the first input atom
  - 1 for the first input atom
  - input-length for the last input atom

Use [`downToUpInLine`](#downToUpInLine) for `String` inputs.

-}
downToUpIn : List atom_ -> Int -> Int
downToUpIn inputSource =
    \fromLast ->
        1
            + ((inputSource |> List.length) - 1)
            - fromLast


downToUpInLines : String -> Int -> TextLocation
downToUpInLines source =
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

Use [`downToUpIn`](#downToUpIn) for any `List` inputs.

-}
downToUpInLine : String -> Int -> Int
downToUpInLine inputSource =
    downToUpIn (inputSource |> String.toList)


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

    import ConversionStep exposing (ConversionStep, take, drop, into, succeed, atLeast, atom, take)
    import Char.ConversionStep as Char
    import Text.ConversionStep as Text

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

    point : ConversionStep Point
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

    line : ConversionStep Line
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

    import ConversionStep exposing (ConversionStep, drop, into, succeed, atLeast, take, atom)
    import Char.ConversionStep exposing (blank)
    import Text.ConversionStep exposing (number)

    type alias Point =
        { x : Float
        , y : Float
        }

    point : ConversionStep Point
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
describe :
    { source : String }
    -> ConversionStep.Error Char narrow_
    -> List String
describe { source } =
    \error ->
        case error of
            ConversionStep.InputRemaining remaining ->
                [ [ [ remaining.input
                        |> Stack.length
                        |> downToUpInLines source
                        |> locationToString
                    , ": I was done parsing when I unexpectedly found remaining input:\n"
                    ]
                        |> String.concat
                  ]
                , ((remaining.input
                        |> Stack.toList
                        |> String.fromList
                        |> String.left 100
                   )
                    ++ "...\""
                  )
                    |> String.lines
                    |> List.map (\line -> "    " ++ line)
                , [ "Correct or remove that part and then try again" ]
                ]
                    |> List.concat

            ConversionStep.ExpectationMiss expectationMiss ->
                [ [ "I was expecting" ]
                , expectationMiss |> expectationMissDescribe { source = source }
                ]
                    |> List.concat


expectationMissDescribe :
    { source : String }
    -> ConversionStep.ExpectationMiss Char
    -> List String
expectationMissDescribe { source } =
    \expectationMiss ->
        case expectationMiss of
            [] ->
                [ "to `expect` something. Tell the parser what it parses!"
                ]

            context0 :: contexts1Up ->
                [ case context0.startingAtDown of
                    0 ->
                        [ context0.expected |> expectedDescribe { source = source }
                        , [ "but I then reached the end of the input" ]
                        ]
                            |> List.concat

                    stuckAtDown ->
                        let
                            stuckAt =
                                stuckAtDown |> downToUpInLine source
                        in
                        [ [ (stuckAt |> downToUpInLines source |> locationToString) ++ ":" ]
                        , context0.expected |> expectedDescribe { source = source }
                        ]
                            |> List.concat
                , (context0 :: contexts1Up)
                    |> List.concatMap
                        (\context ->
                            [ context.expected |> expectedDescribe { source = source }
                            , [ [ "starting at "
                                , (source |> String.length)
                                    - context.startingAtDown
                                    |> downToUpInLines source
                                    |> locationToString
                                ]
                                    |> String.concat
                              ]
                            ]
                                |> List.concat
                        )
                , let
                    errorOffset =
                        context0.startingAtDown |> downToUpInLine source

                    errorLocation =
                        errorOffset |> downToUpInLines source

                    rangeStart =
                        (errorOffset - context0.startingAtDown)
                            |> downToUpInLines source

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
                        , if rangeStart.line == errorLocation.line then
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
                ]
                    |> List.concat


{-| Create an [`Error`](ConversionStep#Error) message.

TODO: update examples

    import ConversionStep
    import Text.ConversionStep as Text
    import Char.ConversionStep exposing (letter)

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
expectedDescribe :
    { source : String }
    -> ConversionStep.Expected Char
    -> List String
expectedDescribe source =
    \expected ->
        case expected of
            ConversionStep.Fail ->
                []

            ConversionStep.ExpectedSpecifically atomSpecific ->
                [ atomSpecific |> String.fromChar ]

            ConversionStep.ExpectedCustom custom ->
                [ custom ]

            ConversionStep.Expected1In possibilities ->
                [ [ "one of these possibilities:" ]
                , possibilities
                    |> List.map (expectationMissDescribe source)
                    |> List.concatMap
                        (\possibilityLines ->
                            case possibilityLines of
                                [] ->
                                    []

                                possibilityLine0 :: possibilityLines1Up ->
                                    ("\n  - " ++ possibilityLine0)
                                        :: (possibilityLines1Up
                                                |> List.map (\possibilityLine -> "    " ++ possibilityLine)
                                           )
                        )
                ]
                    |> List.concat
