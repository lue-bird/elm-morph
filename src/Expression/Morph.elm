module Expression.Morph exposing
    ( expression
    , term, prefix, suffix, inBetween, fromLeft, fromRight
    , Operator(..), InfixDirection(..)
    )

{-| [`Morph`](Morph#Morph) an expression with operator precedence

**Should not be exposed!** TODO "figure this out"

Adapted from

  - [`lambda-phi/parser` `Parser.Expression`](https://dark.elm.dmy.fr/packages/lambda-phi/parser/latest/Parser-Expression)
  - [Pratt parsers for operator precedence](https://eli.thegreenplace.net/2010/01/02/top-down-operator-precedence-parsing)

@docs expression


## operator

@docs term, prefix, suffix, inBetween, fromLeft, fromRight


## custom operators

@docs Operator, InfixDirection

-}

import ArraySized.Morph exposing (atLeast)
import Char.Morph
import Morph exposing (Morph, MorphRow, grab, skip, succeed)
import N exposing (n0)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)


{-| Parses an expression using the provided operators with operator precedence.
There can be multiple operators sharing the same precedence like `+` and `-`,
or `*` and `/`.

    import Parser exposing (Parser, andThen, skip, succeed, grab, broadElement)
    import Text.Parser as Text exposing (number)

    factorial : Float -> Float
    factorial n =
        List.product (List.range 1 (floor n) |> List.map toFloat)

    calculate : Parser Char Float
    calculate =
        expression
            [ [ prefix identity (broadElement '+')
              , prefix (\x -> -x) (broadElement '-')
              , suffix factorial (broadElement '!')
              ]
            , [ fromRight (^) (broadElement '^')
              ]
            , [ fromLeft (*) (broadElement '*')
              , fromLeft (/) (broadElement '/')
              ]
            , [ fromLeft (+) (broadElement '+')
              , fromLeft (-) (broadElement '-')
              ]
            , [ inBetween identity (broadElement '(') (broadElement ')')
              , term identity number
              ]
            ]

    -- terms and parentheses
    "1" |> Text.narrowWith calculate   --> Ok 1
    "(1)" |> Text.narrowWith calculate --> Ok 1

    -- prefix operators
    "-1" |> Text.narrowWith calculate  --> Ok -1  --   -1
    "+-1" |> Text.narrowWith calculate --> Ok -1  -- +(-1)
    "-+1" |> Text.narrowWith calculate --> Ok -1  -- -(+1)
    "--1" |> Text.narrowWith calculate --> Ok 1   -- -(-1)

    -- suffix operators
    "5!" |> Text.narrowWith calculate  --> Ok 120  -- 5!
    "3!!" |> Text.narrowWith calculate --> Ok 720  -- (3!)!

    -- left-associative binary operators
    "1+2+3" |> Text.narrowWith calculate   --> Ok 6    -- (1 + 2) + 3
    "1-2-3" |> Text.narrowWith calculate   --> Ok -4   -- (1 - 2) - 3
    "1-(2-3)" |> Text.narrowWith calculate --> Ok 2    -- 1 - (2 - 3)
    "2*3*4" |> Text.narrowWith calculate   --> Ok 24   -- (2 * 3) * 4
    "2/4/5" |> Text.narrowWith calculate   --> Ok 0.1  -- (2 / 4) / 5
    "2/(4/5)" |> Text.narrowWith calculate --> Ok 2.5  -- 2 / (4 / 5)

    -- right-associative binary operators
    "2^3^2" |> Text.narrowWith calculate   --> Ok 512  -- 2 ^ (3 ^ 2)
    "(2^3)^2" |> Text.narrowWith calculate --> Ok 64   -- (2 ^ 3) ^ 2)

    -- operator precedence
    "1+-2" |> Text.narrowWith calculate  --> Ok -1  -- 1 + (-2)
    "1+2*3" |> Text.narrowWith calculate --> Ok 7   -- 1 + (2 * 3)
    "1*2+3" |> Text.narrowWith calculate --> Ok 5   -- (1 * 2) + 3
    "1*2^3" |> Text.narrowWith calculate --> Ok 8   -- 1 * (2 ^ 3)
    "1^2*3" |> Text.narrowWith calculate --> Ok 3   -- (1 ^ 2) * 3

-}
expression : List (List (Operator broadElement narrow)) -> MorphRow narrow broadElement
expression operators =
    let
        operatorsWithPrecedence : List ( Int, Operator broadElement narrow )
        operatorsWithPrecedence =
            operators
                |> List.indexedMap
                    (\i ops ->
                        ( (operators |> List.length) - i, ops )
                    )
                |> List.concatMap
                    (\( precedence, ops ) ->
                        List.map (\op -> ( precedence, op )) ops
                    )

        unaryOps : List (OperatorOn1 broadElement narrow)
        unaryOps =
            List.filterMap
                (\( precedence, operator ) ->
                    case operator of
                        Prefix parse ->
                            Just { parse = parse, precedence = precedence }

                        Infix _ _ ->
                            Nothing
                )
                operatorsWithPrecedence

        binaryOperators : List (OperatorOn2 broadElement narrow)
        binaryOperators =
            List.filterMap
                (\( operatorPrecedence, operator ) ->
                    case operator of
                        Prefix _ ->
                            Nothing

                        Infix FromLeft parse ->
                            { parse = parse
                            , check = \precedence -> precedence < operatorPrecedence
                            , precedence = operatorPrecedence
                            }
                                |> Just

                        Infix FromRight parse ->
                            { parse = parse
                            , check = \precedence -> precedence <= operatorPrecedence
                            , precedence = operatorPrecedence
                            }
                                |> Just
                )
                operatorsWithPrecedence
    in
    evalWithOperators unaryOps binaryOperators 0


{-| Defines an individual term.
This is usually a number, or a variable name,
or anything not containing an operator.

    import Parser exposing (Parser)
    import Text.Parser as Text exposing (number)

    expr : MorphRow Float Char
    expr =
        expression [ [ term Morph.keep number ] ]

    "5" |> Text.narrowWith expr
    --> Ok 5

    -- does not trim spaces beforehand
    " 5" |> Text.narrowWith expr |> Result.toMaybe
    --> Nothing

-}
term :
    Morph a b
    -> MorphRow a broadElement
    -> Operator b broadElement
term eval x =
    Prefix (\_ -> eval |> Morph.overRow x)


{-| Defines a unary prefix operator.

    import Parser exposing (Parser, broadElement)
    import Text.Parser as Text exposing (number)

    expr : Parser Char Float
    expr =
        expression
            [ [ prefix (\x -> -x) (broadElement '-') ]
            , [ term identity number ]
            ]

    "-5" |> Text.narrowWith expr --> Ok -5

    -- does not trim spaces beforehand
    " -5" |> Text.narrowWith expr |> Result.toMaybe
    --> Nothing

    -- but it does afterwards
    "- 5" |> Text.narrowWith expr --> Ok -5

-}
prefix :
    (narrow -> narrow)
    -> MorphRow operator Char
    -> Operator narrow Char
prefix eval operator =
    Prefix
        (\expr ->
            succeed eval
                |> skip operator
                |> skip (atLeast (Char.Morph.only " ") n0)
                |> grab expr
        )


{-| Defines a unary suffix operator.

    import Parser exposing (Parser, broadElement)
    import Text.Parser as Text exposing (number)

    factorial : Float -> Float
    factorial n =
        List.product (List.range 1 (floor n) |> List.map toFloat)

    expr : Parser Char Float
    expr =
        expression
            [ [ suffix factorial (broadElement '!') ]
            , [ term identity number ]
            ]

    "5!" |> Text.narrowWith expr --> Ok 120

    -- trims spaces beforehand
    "5 !" |> Text.narrowWith expr --> Ok 120

-}
suffix :
    (narrow -> narrow)
    -> MorphRow operator Char
    -> Operator narrow Char
suffix eval op =
    Infix FromLeft
        (\_ ->
            succeed eval
                |> skip (atLeast (Char.Morph.only " ") n0)
                |> skip op
        )


{-| Defines an expression surrounded by an `open` and `close` operator.

    import Parser exposing (Parser, broadElement)
    import Text.Parser as Text exposing (number)

    expr : Parser Char Float
    expr =
        expression
            [ [ inBetween identity (broadElement '(') (broadElement ')') ]
            , [ term identity number ]
            ]

    "(5)" |> Text.narrowWith expr --> Ok 5

    "(5" |> Text.narrowWith expr |> Result.toMaybe
    --> Nothing

    "()" |> Text.narrowWith expr |> Result.toMaybe
    --> Nothing

    -- does not trim spaces beforehand
    " (5)" |> Text.narrowWith expr |> Result.toMaybe
    --> Nothing

    -- it does inBetween
    "( 5 )" |> Text.narrowWith expr --> Ok 5

-}
inBetween :
    (narrow -> narrow)
    -> MorphRow open Char
    -> MorphRow close Char
    -> Operator narrow Char
inBetween eval open close =
    Prefix
        (\expr ->
            succeed eval
                |> skip open
                |> skip (atLeast (Char.Morph.only " ") n0)
                |> grab expr
                |> skip (atLeast (Char.Morph.only " ") n0)
                |> skip close
        )


{-| Defines a binary left-associative operator.

    import Parser exposing (Parser, broadElement)
    import Text.Parser as Text exposing (number)

    expr : Parser Char Float
    expr =
        expression
            [ [ fromLeft (+) (broadElement '+')
              , fromLeft (-) (broadElement '-')
              ]
            , [ term identity number ]
            ]

    "1+2" |> Text.narrowWith expr   --> Ok 3
    "1+2+3" |> Text.narrowWith expr --> Ok 6
    "1-2-3" |> Text.narrowWith expr --> Ok -4

    -- does not trim spaces beforehand
    " 1+2" |> Text.narrowWith expr |> Result.toMaybe
    --> Nothing

    -- it does inBetween
    "1 + 2" |> Text.narrowWith expr --> Ok 3

-}
fromLeft :
    (narrow -> (narrow -> narrow))
    -> MorphRow operator Char
    -> Operator narrow Char
fromLeft eval op =
    Infix FromLeft
        (\expr ->
            succeed (\right left -> eval left right)
                |> skip (atLeast (Char.Morph.only " ") n0)
                |> skip op
                |> skip (atLeast (Char.Morph.only " ") n0)
                |> grab expr
        )


{-| Defines a binary right-associative operator.

    import Parser exposing (Parser, broadElement)
    import Text.Parser exposing (number)

    expr : Parser Char Float
    expr =
        expression
            [ [ fromRight (^) (broadElement '^') ]
            , [ term identity number ]
            ]

    "2^3" |> Text.narrowWith expr   --> Ok 8
    "2^3^2" |> Text.narrowWith expr --> Ok 512 -- 2 ^ (3 ^ 2)

    -- does not trim spaces beforehand
    " 2^3" |> Text.narrowWith expr |> Result.toMaybe
    --> Nothing

    -- it does inBetween
    "2 ^ 3" |> Text.narrowWith expr --> Ok 8

-}
fromRight :
    (narrow -> (narrow -> narrow))
    -> MorphRow operator Char
    -> Operator narrow Char
fromRight eval operator =
    Infix FromRight
        (\expr ->
            succeed (\right left -> eval left right)
                |> skip (atLeast (Char.Morph.only " ") n0)
                |> skip operator
                |> skip (atLeast (Char.Morph.only " ") n0)
                |> grab expr
        )


evalWithOperators :
    List (OperatorOn1 narrow broadElement)
    -> List (OperatorOn2 narrow broadElement)
    -> Int
    -> MorphRow narrow broadElement
evalWithOperators unaryOperators operatorsOn2 precedence =
    let
        operationOn1Eval :
            OperatorOn1 narrow broadElement
            -> MorphRow narrow broadElement
        operationOn1Eval operator =
            operator.parse
                (Morph.lazy
                    (\() ->
                        evalWithOperators unaryOperators operatorsOn2 operator.precedence
                    )
                )

        operationOn2Eval :
            OperatorOn2 narrow broadElement
            -> MorphRow (narrow -> narrow) broadElement
        operationOn2Eval operator =
            if operator.check precedence then
                operator.parse
                    (Morph.lazy
                        (\() ->
                            evalWithOperators unaryOperators operatorsOn2 operator.precedence
                        )
                    )

            else
                -- TODO error message?
                Debug.todo ""
    in
    onFailDown
        (List.map operationOn1Eval unaryOperators)
        |> andThen
            (\firstTerm ->
                Parser.loop
                    { initial = { termSoFar = firstTerm, operatorsOn2Left = operatorsOn2 }
                    , step =
                        \{ termSoFar, operatorsOn2Left } ->
                            case operatorsOn2Left of
                                [] ->
                                    succeed (termSoFar |> Parser.Commit)

                                operatorOn2 :: nextOperatorsOn2 ->
                                    (operatorOn2 |> operationOn2Eval)
                                        |> map
                                            (\op ->
                                                { termSoFar = termSoFar |> op
                                                , operatorsOn2Left = nextOperatorsOn2
                                                }
                                                    |> Parser.GoOn
                                            )
                    }
            )



-- operators


{-| Describes an operator as a parser.
They grab a [`Parser`](Parser#Parser) as an input, which is used to parse subexpressions
recursively.

> ℹ️ Both `Prefix` and `Infix*` operators _must_ start by parsing something
> other than a sub-expression, otherwise there will be a stack overflow.

    import Parser exposing (Parser, skip, succeed, grab,single)
    import Text.Parser as Text exposing (number)

    -- Prefix operators simply parse and apply the operation.
    neg : Operator Float
    neg =
        Prefix
            (\expr ->
                succeed (\x -> -x)
                    |> skip (broadElement '-')
                    |> grab expr
            )

    -- Infix operators parse only the right side of the sub-expression.
    -- The Parser returns a function that takes the left side of the
    -- sub-expression as an input, and applies the operation.
    -- This is for a left-associative infix operator.
    add : Operator Float
    add =
        InfixFromLeft
            (\expr ->
                succeed (\right left -> left + right)
                    |> skip (broadElement '+')
                    |> grab expr
            )

    -- You can also define a right-associative infix operator.
    pow : Operator Float
    pow =
        InfixFromLeft
            (\expr ->
                succeed (\right left -> left ^ right)
                    |> skip (broadElement '^')
                    |> grab expr
            )

    -- A term can be achieved by simply parsing a token but not recursing.
    num : Operator Float
    num =
        Prefix (\_ -> number)

    calculate : Parser Char Float
    calculate =
        expression
            [ [ neg ] -- -1
            , [ pow ] -- 1 ^ 2
            , [ add ] -- 1 + 2
            , [ num ] -- 1
            ]

    "1+-2^3" |> Text.narrowWith calculate --> Ok -7 -- 1 + ((-2) ^ 3)

-}
type Operator narrow broadElement
    = Prefix
        (MorphRow narrow broadElement
         -> MorphRow narrow broadElement
        )
    | Infix
        InfixDirection
        (MorphRow narrow broadElement
         -> MorphRow (narrow -> narrow) broadElement
        )


type InfixDirection
    = FromLeft
    | FromRight


type alias OperatorOn1 narrow broadElement =
    RecordWithoutConstructorFunction
        { parse : MorphRow narrow broadElement -> MorphRow narrow broadElement
        , precedence : Int
        }


type alias OperatorOn2 narrow broadElement =
    RecordWithoutConstructorFunction
        { parse :
            MorphRow narrow broadElement
            -> MorphRow (narrow -> narrow) broadElement
        , check : Int -> Bool
        , precedence : Int
        }
