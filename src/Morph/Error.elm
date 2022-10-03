module Morph.Error exposing
    ( LinesLocation, downToUpIn, downToUpInLine, downToUpInLines
    , toString
    )

{-| Error reporting

@docs describe


## offset

@docs LinesLocation, downToUpIn, downToUpInLine, downToUpInLines

-}

import Emptiable exposing (Emptiable)
import Morph
import Possibly exposing (Possibly)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Stack exposing (Stacked)


{-| Position withing multiple lines
-}
type alias LinesLocation =
    RecordWithoutConstructorFunction
        { line : Int
        , column : Int
        }


{-| How far parsing got from the beginning of an input source.

  - 0 for before the first input element
  - 1 for the first input element
  - input-length for the last input element

Use [`downToUpInLine`](#downToUpInLine) for `String` inputs.

-}
downToUpIn : Emptiable (Stacked broadElement_) Possibly -> Int -> Int
downToUpIn inputSource =
    \fromLast ->
        (inputSource |> Stack.length)
            - fromLast


{-| How far parsing got from the beginning of input source lines
-}
downToUpInLines : String -> Int -> LinesLocation
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
    downToUpIn (inputSource |> Stack.fromText)


errorToString :
    { source : String }
    ->
        (Morph.Error
         -> List String
        )
errorToString { source } =
    \expectationMiss ->
        [ case expectationMiss.location.startDown of
            0 ->
                [ expectationMiss.expected |> expectedDescribe { source = source }
                , [ "but nothing's left to parse" ]
                ]
                    |> List.concat

            stuckAtDown ->
                let
                    stuckAt =
                        stuckAtDown |> downToUpInLine source
                in
                [ [ (stuckAt |> downToUpInLines source |> locationToString) ++ ":" ]
                , expectationMiss.expected |> expectedDescribe { source = source }
                ]
                    |> List.concat
        , [ expectationMiss.expected |> expectedDescribe { source = source }
          , [ [ "starting at "
              , (source |> String.length)
                    - expectationMiss.location.startDown
                    |> downToUpInLines source
                    |> locationToString
              ]
                |> String.concat
            ]
          ]
            |> List.concat
        , let
            errorOffset =
                expectationMiss.location.startDown |> downToUpInLine source

            errorLocation =
                errorOffset |> downToUpInLines source

            rangeStart =
                (errorOffset - expectationMiss.location.startDown)
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


{-| Present the `TextLocation` as `"line:column"`, for example

    { line = 3, column = 10 } |> locationToString
    --> "line 3)10"

-}
locationToString : LinesLocation -> String
locationToString =
    \location ->
        [ "line "
        , location.line |> String.fromInt
        , ")"
        , location.column |> String.fromInt
        ]
            |> String.concat


{-| Dumps the error into a human-readable format.

    import Morph exposing (MorphRow, take, drop, into, succeed, atLeast, one, take)
    import Char.Morph as Char
    import String.Morph as Text

    "  abc  "
        |> Text.narrowWith
            (succeed (\number -> number)
                |> grab (atLeast n0 Char.Morph.only ' ')
                |> skip Text.number
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

    point : MorphRow Point
    point =
        into "Point"
            (succeed Point
                |> skip (one '(')
                |> grab Text.number
                |> skip (one ',')
                |> grab Text.number
                |> skip (one ')')
            )

    "  (12,)  "
        |> Text.narrowWith
            (succeed (\point -> point)
                |> skip (atLeast n0 Char.Morph.only ' ')
                |> grab point
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

    line : MorphRow Line
    line =
        into "Line"
            (succeed (\p1 p2 -> { p1 = p1, p2 = p2 })
                |> skip (one '[')
                |> grab point
                |> skip (one ',')
                |> grab point
                |> skip (one ']')
            )

    "  [(12,34),(56,)]  "
        |> Text.narrowWith
            (succeed (\line -> line)
                |> skip (atLeast n0 Char.Morph.only ' ')
                |> grab line
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

    import Morph exposing (MorphRow, drop, into, succeed, atLeast, take, one)
    import Char.Morph exposing (blank)
    import String.Morph exposing (number)

    type alias Point =
        { x : Float
        , y : Float
        }

    point : MorphRow Point
    point =
        into "Point"
            (succeed (\x y -> { x = x, y = y })
                |> skip (one '(')
                |> skip (atLeast n0 blank)
                |> grab number
                |> skip (atLeast n0 blank)
                |> skip (one ',')
                |> skip (atLeast n0 blank)
                |> grab number
                |> skip (atLeast n0 blank)
                |> skip (one ')')
            )

    "  (12,)  "
        |> Text.narrowWith
            (succeed (\point -> point)
                |> skip (atLeast n0 Char.Morph.only ' ')
                |> grab point
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
                |> skip (atLeast n0 Char.Morph.only ' ')
                |> grab point
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
toString : Morph.Error -> String
toString =
    \error ->
        [ [ "I was expecting" ]
        , error |> errorToString
        ]
            |> List.concat


{-| Create an [`Error`](MorphRow#Error) message.

TODO: update examples

    import Morph
    import String.Morph as Text
    import Char.Morph exposing (letter)

    -- Getting a digit instead of a letter.
    "123"
        |> Text.narrowWith letter
        |> Result.mapError
            (expectationMissMessage { source = "123" })
    --> Err "1:1: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '1'."

    -- Running out of input characters.
    ""
        |> Text.narrowWith letter
        |> Result.mapError
            (expectationMissMessage { source = "" })
    --> Err "1:0: I was expecting a letter [a-zA-Z]. I reached the end of the input."

-}
expectedDescribe :
    { source : String }
    -> Morph.Error { startDown : Int } Char
    -> List String
expectedDescribe source =
    \expected ->
        case expected of
            Morph.DeadEnd unexpectedDescription ->
                [ "not " ++ unexpectedDescription ]

            Morph.Only specific ->
                [ specific |> String.fromChar ]

            Morph.Possibilities possibilities ->
                [ [ "either" ]
                , possibilities
                    |> Emptiable.emptyAdapt never
                    |> Stack.map
                        (\_ possibilityExpectation ->
                            case possibilityExpectation |> errorToString source of
                                [] ->
                                    Emptiable.empty

                                possibilityLine0 :: possibilityLines1Up ->
                                    Stack.topDown
                                        ("\n  - " ++ possibilityLine0)
                                        (possibilityLines1Up
                                            |> List.map (\possibilityLine -> "    " ++ possibilityLine)
                                        )
                        )
                    |> Stack.flatten
                    |> Stack.toList
                ]
                    |> List.concat
