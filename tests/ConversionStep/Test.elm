module ConversionStep.Test exposing (tests)

import Char.Conversion exposing (N0To9)
import Char.ConversionStep as Char
import Conversion exposing (transfer)
import ConversionStep exposing (ConversionStep, LoopStep(..), atLeast, atom, choice, drop, loop, map, possibility, split, succeed, take)
import Expect
import Hand exposing (Empty, Hand)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Stack exposing (Stacked)
import Test exposing (Test, test)
import Text.ConversionStep


tests : Test
tests =
    Test.describe
        "Conversion step"
        [ Test.describe
            "email"
            [ Test.describe
                "valid"
                ([ """simple@example.com"""
                 , """very.common@example.com"""
                 , """other.email-with-hyphen@example.com"""
                 , """fully-qualified-domain@example.com"""
                 , -- one-letter local-part
                   """x@example.com"""
                 , """example-indeed@strange-example.com"""
                 , -- slashes are an allowed printable character
                   """test/test@test.com"""
                 , -- local domain name with no TLD, although ICANN highly discourages dotless email addresses
                   """admin@mailserver1"""
                 , """example@s.example"""
                 , """user-@example.org"""
                 ]
                    |> List.map
                        (\exampleEmail ->
                            test
                                exampleEmail
                                (\() ->
                                    case exampleEmail |> Text.ConversionStep.narrowWith email of
                                        Ok emailParsed ->
                                            emailParsed
                                                |> Text.ConversionStep.build email
                                                |> Expect.equal exampleEmail

                                        Err _ ->
                                            Expect.pass
                                )
                        )
                )
            , Test.describe
                "invalid"
                ([ -- no @ character
                   """Abc.example.com"""
                 , -- only one @ is allowed
                   """A@b@c@example.com"""
                 , -- Underscore is not allowed in domain part
                   """i_like_underscore@but_its_not_allowed_in_this_part.example.com"""
                 , """QA[icon]CHOCOLATE[icon]@test.com"""
                 ]
                    |> List.map
                        (\exampleEmail ->
                            test
                                exampleEmail
                                (\() ->
                                    case exampleEmail |> Text.ConversionStep.narrowWith email of
                                        Ok _ ->
                                            Expect.fail exampleEmail

                                        Err _ ->
                                            Expect.pass
                                )
                        )
                )
            ]
        ]


type alias Email =
    RecordWithoutConstructorFunction
        { local : Local
        , domain : Domain
        }


email : ConversionStep Char Email
email =
    succeed
        (\local_ domain_ ->
            { local = local_
            , domain = domain_
            }
        )
        |> take .local local
        |> drop (atom '@')
        |> take .domain domain



-- local


type alias Local =
    RecordWithoutConstructorFunction
        { dotSeparated :
            Hand (Stacked (List LocalSymbol)) Never Empty
        }


local : ConversionStep Char Local
local =
    succeed (\dotSeparated -> { dotSeparated = dotSeparated })
        |> take .dotSeparated
            (split ( atLeast 1, atom '.' ) (atLeast 1 localSymbol)
                |> map
                    (transfer
                        (Stack.belowTopMap (\_ -> .part))
                        (Stack.belowTopMap
                            (\_ part -> { separator = (), part = part })
                        )
                    )
            )


type LocalSymbol
    = LocalSymbolPrintable LocalSymbolPrintable
    | LocalSymbolAToZ Char.AToZ
    | LocalSymbol0To9


type LocalSymbolPrintable
    = ExclamationMark
    | NumberSign
    | DollarSign
    | PercentSign
    | Ampersand
    | Asterisk
    | LowLine
    | HyphenMinus
    | Tilde
    | VerticalLine
    | PlusSign
    | EqualsSign
    | GraveAccent
    | LeftCurlyBracket
    | RightCurlyBracket


{-| TODO

    lowerAToZ

    digit

-}
localSymbol =
    Debug.todo "local symbol"


localSymbolPrintable =
    choice
        (\exclamationMark numberSign dollarSign percentSign ampersand asterisk lowLine hyphenMinus tilde verticalLine plusSign equalsSign graveAccent leftCurlyBracket rightCurlyBracket printable ->
            case printable of
                ExclamationMark ->
                    exclamationMark ()

                NumberSign ->
                    numberSign ()

                DollarSign ->
                    dollarSign ()

                PercentSign ->
                    percentSign ()

                Ampersand ->
                    ampersand ()

                Asterisk ->
                    asterisk ()

                LowLine ->
                    lowLine ()

                HyphenMinus ->
                    hyphenMinus ()

                Tilde ->
                    tilde ()

                VerticalLine ->
                    verticalLine ()

                PlusSign ->
                    plusSign ()

                EqualsSign ->
                    equalsSign ()

                GraveAccent ->
                    graveAccent ()

                LeftCurlyBracket ->
                    leftCurlyBracket ()

                RightCurlyBracket ->
                    rightCurlyBracket ()
        )
        (possibility (\() -> ExclamationMark) (atom '!')
            >> possibility (\() -> NumberSign) (atom '#')
            >> possibility (\() -> DollarSign) (atom '$')
            >> possibility (\() -> PercentSign) (atom '%')
            >> possibility (\() -> Ampersand) (atom '&')
            >> possibility (\() -> Asterisk) (atom '*')
            >> possibility (\() -> LowLine) (atom '_')
            >> possibility (\() -> HyphenMinus) (atom '-')
            >> possibility (\() -> Tilde) (atom '~')
            >> possibility (\() -> VerticalLine) (atom '|')
            >> possibility (\() -> PlusSign) (atom '+')
            >> possibility (\() -> EqualsSign) (atom '=')
            >> possibility (\() -> GraveAccent) (atom '`')
            >> possibility (\() -> LeftCurlyBracket) (atom '{')
            >> possibility (\() -> RightCurlyBracket) (atom '}')
        )



-- domain


type alias Domain =
    RecordWithoutConstructorFunction
        { first : HostLabel
        , secondUp :
            { hostLabels : List HostLabel
            , topLevel : DomainTopLevel
            }
        }


domain : ConversionStep Char Domain
domain =
    succeed (\first secondUp -> { first = first, secondUp = secondUp })
        |> take .first hostLabel
        |> drop (atom '.')
        |> take .secondUp
            (loop
                { initial = []
                , goOnBroaden = Stack.toList
                , step =
                    \hostLabels ->
                        possibility GoOn
                            (succeed (\label -> Stack.topDown label hostLabels)
                                |> take Stack.top hostLabel
                                |> drop (atom '.')
                            )
                            >> possibility Commit
                                (succeed
                                    (\topLevel_ ->
                                        { topLevel = topLevel_
                                        , hostLabels = hostLabels |> List.reverse
                                        }
                                    )
                                    |> take .topLevel domainTopLevel
                                )
                }
            )


type alias HostLabel =
    RecordWithoutConstructorFunction
        { firstSymbol : HostLabelSideSymbol
        , betweenFirstAndLastSymbols : List HostLabelSymbol
        , lastSymbol : HostLabelSideSymbol
        }


hostLabel : ConversionStep Char HostLabel
hostLabel =
    succeed
        (\firstSymbol betweenFirstAndLastSymbols lastSymbol ->
            { firstSymbol = firstSymbol
            , betweenFirstAndLastSymbols = betweenFirstAndLastSymbols
            , lastSymbol = lastSymbol
            }
        )
        |> take .firstSymbol hostLabelSideSymbol
        |> take .betweenFirstAndLastSymbols (atLeast 0 hostLabelSymbol)
        |> take .lastSymbol hostLabelSideSymbol


type HostLabelSideSymbol
    = HostLabelSideSymbolAToZ { case_ : Char.Case, letter : Char.AToZ }
    | HostLabelSideSymbol0To9 N0To9


hostLabelSideSymbol : ConversionStep Char HostLabelSideSymbol
hostLabelSideSymbol =
    choice
        (\aToZVariant n0To9Variant sideSymbol ->
            case sideSymbol of
                HostLabelSideSymbolAToZ aToZValue ->
                    aToZVariant aToZValue

                HostLabelSideSymbol0To9 n0To9Value ->
                    n0To9Variant n0To9Value
        )
        (possibility HostLabelSideSymbolAToZ Char.aToZ
            >> possibility HostLabelSideSymbol0To9 Char.n0To9
        )


type HostLabelSymbol
    = HostLabelHyphenMinus
    | HostLabelSymbolAToZ { case_ : Char.Case, letter : Char.AToZ }
    | HostLabelSymbol0To9 N0To9


hostLabelSymbol : ConversionStep Char HostLabelSymbol
hostLabelSymbol =
    choice
        (\hyphenMinus aToZVariant n0To9Variant symbol ->
            case symbol of
                HostLabelHyphenMinus ->
                    hyphenMinus ()

                HostLabelSymbolAToZ aToZValue ->
                    aToZVariant aToZValue

                HostLabelSymbol0To9 n0To9Value ->
                    n0To9Variant n0To9Value
        )
        (possibility (\() -> HostLabelHyphenMinus) (atom '-')
            >> possibility HostLabelSymbolAToZ Char.aToZ
            >> possibility HostLabelSymbol0To9 Char.n0To9
        )


type alias DomainTopLevel =
    RecordWithoutConstructorFunction
        { startDigits : List N0To9
        , afterStartDigits : List DomainTopLevelAfterStartDigitsSymbol
        }


domainTopLevel : ConversionStep Char DomainTopLevel
domainTopLevel =
    succeed
        (\startDigits afterStartDigits ->
            { startDigits = startDigits, afterStartDigits = afterStartDigits }
        )
        |> take .startDigits (atLeast 0 Char.n0To9)
        |> -- guarantees it can't be numeric only
           take .afterStartDigits (atLeast 1 domainTopLevelSymbol)


type DomainTopLevelAfterStartDigitsSymbol
    = DomainTopLevelSymbolAToZ { case_ : Char.Case, letter : Char.AToZ }
    | DomainTopLevelSymbol0To9 N0To9


domainTopLevelSymbol : ConversionStep Char DomainTopLevelAfterStartDigitsSymbol
domainTopLevelSymbol =
    choice
        (\aToZVariant n0To9Variant domainTopLevelSymbolUnion ->
            case domainTopLevelSymbolUnion of
                DomainTopLevelSymbolAToZ aToZValue ->
                    aToZVariant aToZValue

                DomainTopLevelSymbol0To9 n0To9Value ->
                    n0To9Variant n0To9Value
        )
        (possibility DomainTopLevelSymbolAToZ Char.aToZ
            >> possibility DomainTopLevelSymbol0To9 Char.n0To9
        )
