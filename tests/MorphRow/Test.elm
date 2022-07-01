module MorphRow.Test exposing (tests)

import Digit.Morph
import Expect
import Hand exposing (Empty, Hand, filled)
import Morph exposing (Morph, broaden, broadenFrom, choice, narrow, translate)
import Morph.Char as Char
import Morph.Text
import MorphRow exposing (LoopStep(..), MorphRow, atLeast, atom, grab, loop, skip, split, succeed)
import Number.Morph exposing (Number)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Stack exposing (Stacked)
import Stack.Morph
import Test exposing (Test, test)


tests : Test
tests =
    Test.describe
        "Morph to row"
        [ pointTest
        , emailTest
        ]



--point


pointTest : Test
pointTest =
    Test.describe "point"
        [ test "narrow"
            (\() ->
                "(3,  -9999.124)"
                    |> narrow
                        (Morph.group
                            (\x y -> { x = x, y = y })
                            (\x y -> { x = x, y = y })
                            |> Morph.part ( .x, .x >> Ok ) Number.Morph.fromFloat
                            |> Morph.part ( .y, .y >> Ok ) Number.Morph.fromFloat
                            |> MorphRow.over point
                            |> Morph.over Stack.Morph.toText
                        )
                    |> Expect.equal (Ok { x = 3.0, y = -9999.124 })
            )
        , test "broaden"
            (\() ->
                { x = 3.0, y = -9999.124 }
                    |> broaden
                        (Morph.group
                            (\x y -> { x = x, y = y })
                            (\x y -> { x = x, y = y })
                            |> Morph.part ( .x, .x >> Ok ) Number.Morph.fromFloat
                            |> Morph.part ( .y, .y >> Ok ) Number.Morph.fromFloat
                            |> MorphRow.over point
                            |> Morph.over Stack.Morph.toText
                        )
                    |> Expect.equal "( 3, -9999.124 )"
            )
        ]


point : MorphRow Char Point String
point =
    succeed (\x y -> { x = x, y = y })
        |> skip (Morph.Text.specific "(")
        |> skip
            (broadenFrom [ Char.Space ]
                |> MorphRow.over (atLeast 0 (Char.blank |> atom))
            )
        |> grab .x Number.Morph.text
        |> skip
            (broadenFrom []
                |> MorphRow.over (atLeast 0 (Char.blank |> atom))
            )
        |> skip (Morph.Text.specific ",")
        |> skip
            (broadenFrom [ Char.Space ]
                |> MorphRow.over (atLeast 0 (Char.blank |> atom))
            )
        |> grab .y Number.Morph.text
        |> skip
            (broadenFrom [ Char.Space ]
                |> MorphRow.over (atLeast 0 (Char.blank |> atom))
            )
        |> skip (Morph.Text.specific ")")


emailTest : Test
emailTest =
    let
        emailToText =
            email |> Morph.over Stack.Morph.toText
    in
    Test.describe
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
                                case exampleEmail |> narrow emailToText of
                                    Ok emailParsed ->
                                        emailParsed
                                            |> broaden emailToText
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
                                case exampleEmail |> narrow emailToText of
                                    Ok _ ->
                                        Expect.fail exampleEmail

                                    Err _ ->
                                        Expect.pass
                            )
                    )
            )
        ]



-- email
-- format as described in https://en.wikipedia.org/wiki/Email_address


email : MorphRow Char Email expectedCustom_
email =
    succeed
        (\local_ domain_ ->
            { local = local_
            , domain = domain_
            }
        )
        |> grab .local local
        |> skip (Morph.Text.specific "@")
        |> grab .domain domain


local : MorphRow Char Local expectedCustom_
local =
    succeed (\dotSeparated -> { dotSeparated = dotSeparated })
        |> grab .dotSeparated
            (Stack.Morph.belowTopEach
                (translate
                    (\part -> { separator = (), part = part })
                    .part
                )
                |> MorphRow.over
                    (split
                        ( atLeast 1, Morph.Text.specific "." )
                        (atLeast 1 (localSymbol |> atom))
                    )
            )


localSymbol : Morph LocalSymbol Char (Morph.Error Char variantExpectation_)
localSymbol =
    choice
        (\printableVariant aToZVariant n0To9Variant localSymbolUnion ->
            case localSymbolUnion of
                LocalSymbolPrintable printableValue ->
                    printableVariant printableValue

                LocalSymbolAToZ aToZValue ->
                    aToZVariant aToZValue

                LocalSymbol0To9 n0To9Value ->
                    n0To9Variant n0To9Value
        )
        |> Morph.possibility LocalSymbolPrintable localSymbolPrintable
        |> Morph.possibility LocalSymbolAToZ
            (translate
                (\letter -> { letter = letter, case_ = Char.CaseLower })
                .letter
                |> Morph.over Char.aToZ
            )
        |> Morph.possibility LocalSymbol0To9 Digit.Morph.n0To9
        |> Morph.choiceFinish



-- local


localSymbolPrintable :
    Morph
        LocalSymbolPrintable
        Char
        (Morph.Error Char variantExpectationDescription_)
localSymbolPrintable =
    choice
        (\exclamationMark numberSign dollarSign percentSign ampersand asterisk lowLine hyphenMinus tilde verticalLine plusSign equalsSign graveAccent leftCurlyBracket rightCurlyBracket localSymbolPrintableNarrow ->
            case localSymbolPrintableNarrow of
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
        |> Morph.possibility (\() -> ExclamationMark) (Morph.specific '!')
        |> Morph.possibility (\() -> NumberSign) (Morph.specific '#')
        |> Morph.possibility (\() -> DollarSign) (Morph.specific '$')
        |> Morph.possibility (\() -> PercentSign) (Morph.specific '%')
        |> Morph.possibility (\() -> Ampersand) (Morph.specific '&')
        |> Morph.possibility (\() -> Asterisk) (Morph.specific '*')
        |> Morph.possibility (\() -> LowLine) (Morph.specific '_')
        |> Morph.possibility (\() -> HyphenMinus) (Morph.specific '-')
        |> Morph.possibility (\() -> Tilde) (Morph.specific '~')
        |> Morph.possibility (\() -> VerticalLine) (Morph.specific '|')
        |> Morph.possibility (\() -> PlusSign) (Morph.specific '+')
        |> Morph.possibility (\() -> EqualsSign) (Morph.specific '=')
        |> Morph.possibility (\() -> GraveAccent) (Morph.specific '`')
        |> Morph.possibility (\() -> LeftCurlyBracket) (Morph.specific '{')
        |> Morph.possibility (\() -> RightCurlyBracket) (Morph.specific '}')
        |> Morph.choiceFinish


domain : MorphRow Char Domain expectedCustom_
domain =
    succeed
        (\first secondUp -> { first = first, secondUp = secondUp })
        |> MorphRow.grab .first hostLabel
        |> MorphRow.skip (Morph.Text.specific ".")
        |> MorphRow.grab .secondUp
            (loop
                { initial = []
                , step =
                    \hostLabels ->
                        choice
                            (\goOn commit loopStep ->
                                case loopStep of
                                    MorphRow.GoOn goOnValue ->
                                        goOn goOnValue

                                    MorphRow.Commit commitValue ->
                                        commit commitValue
                            )
                            |> MorphRow.possibility GoOn
                                (succeed (\label -> Stack.topDown label hostLabels)
                                    |> MorphRow.grab Stack.top hostLabel
                                    |> MorphRow.skip (Morph.Text.specific ".")
                                )
                            |> MorphRow.possibility Commit
                                (translate
                                    .topLevel
                                    (\topLevelNarrow ->
                                        { topLevel = topLevelNarrow
                                        , hostLabels = hostLabels |> List.reverse
                                        }
                                    )
                                    |> MorphRow.over domainTopLevel
                                )
                , goOnBroaden = Stack.toList
                , commitBack =
                    \commitValue ->
                        case commitValue.hostLabels |> Stack.fromList of
                            Hand.Empty _ ->
                                Nothing

                            Hand.Filled stacked ->
                                stacked |> filled |> Stack.reverse |> Just
                , goOnBack =
                    \stack ->
                        case stack |> Stack.topRemove of
                            Hand.Empty _ ->
                                Nothing

                            Hand.Filled downStacked ->
                                downStacked |> filled |> Just
                }
            )


hostLabel : MorphRow Char HostLabel expectedCustom_
hostLabel =
    succeed
        (\firstSymbol betweenFirstAndLastSymbols lastSymbol ->
            { firstSymbol = firstSymbol
            , betweenFirstAndLastSymbols = betweenFirstAndLastSymbols
            , lastSymbol = lastSymbol
            }
        )
        |> grab .firstSymbol
            (hostLabelSideSymbol |> atom)
        |> grab .betweenFirstAndLastSymbols
            (atLeast 0 (hostLabelSymbol |> atom))
        |> grab .lastSymbol
            (hostLabelSideSymbol |> atom)


hostLabelSideSymbol : Morph HostLabelSideSymbol Char (Morph.Error Char variantExpectation_)
hostLabelSideSymbol =
    choice
        (\aToZVariant n0To9Variant sideSymbol ->
            case sideSymbol of
                HostLabelSideSymbolAToZ aToZValue ->
                    aToZVariant aToZValue

                HostLabelSideSymbol0To9 n0To9Value ->
                    n0To9Variant n0To9Value
        )
        |> Morph.possibility HostLabelSideSymbolAToZ Char.aToZ
        |> Morph.possibility HostLabelSideSymbol0To9 Digit.Morph.n0To9
        |> Morph.choiceFinish


hostLabelSymbol : Morph HostLabelSymbol Char (Morph.Error Char variantExpectation_)
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
        |> Morph.possibility (\() -> HostLabelHyphenMinus)
            (Morph.specific '-')
        |> Morph.possibility HostLabelSymbolAToZ Char.aToZ
        |> Morph.possibility HostLabelSymbol0To9 Digit.Morph.n0To9
        |> Morph.choiceFinish


domainTopLevel : MorphRow Char DomainTopLevel expectedCustom_
domainTopLevel =
    succeed
        (\startDigits firstAToZ afterFirstAToZ ->
            { startDigits = startDigits
            , firstAToZ = firstAToZ
            , afterFirstAToZ = afterFirstAToZ
            }
        )
        |> grab .startDigits
            (atLeast 0 (Digit.Morph.n0To9 |> atom))
        |> -- guarantees it can't be numeric only
           grab .firstAToZ
            (Char.aToZ |> atom)
        |> grab .afterFirstAToZ
            (atLeast 0 (domainTopLevelAfterFirstAToZSymbol |> atom))



-- domain


domainTopLevelAfterFirstAToZSymbol :
    Morph
        DomainTopLevelAfterFirstAToZSymbol
        Char
        (Morph.Error Char variantExpectation_)
domainTopLevelAfterFirstAToZSymbol =
    choice
        (\aToZVariant n0To9Variant domainTopLevelSymbolUnion ->
            case domainTopLevelSymbolUnion of
                DomainTopLevelSymbolAToZ aToZValue ->
                    aToZVariant aToZValue

                DomainTopLevelSymbol0To9 n0To9Value ->
                    n0To9Variant n0To9Value
        )
        |> Morph.possibility DomainTopLevelSymbolAToZ Char.aToZ
        |> Morph.possibility DomainTopLevelSymbol0To9 Digit.Morph.n0To9
        |> Morph.choiceFinish


type alias Point =
    RecordWithoutConstructorFunction
        { x : Number, y : Number }


type alias Email =
    RecordWithoutConstructorFunction
        { local : Local
        , domain : Domain
        }


type alias Local =
    RecordWithoutConstructorFunction
        { dotSeparated :
            Hand (Stacked (List LocalSymbol)) Never Empty
        }


type LocalSymbol
    = LocalSymbolPrintable LocalSymbolPrintable
    | LocalSymbolAToZ Char.AToZ
    | LocalSymbol0To9 Digit.Morph.N0To9


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


type alias Domain =
    RecordWithoutConstructorFunction
        { first : HostLabel
        , secondUp :
            { hostLabels : List HostLabel
            , topLevel : DomainTopLevel
            }
        }


type alias HostLabel =
    RecordWithoutConstructorFunction
        { firstSymbol : HostLabelSideSymbol
        , betweenFirstAndLastSymbols : List HostLabelSymbol
        , lastSymbol : HostLabelSideSymbol
        }


type HostLabelSideSymbol
    = HostLabelSideSymbolAToZ { case_ : Char.Case, letter : Char.AToZ }
    | HostLabelSideSymbol0To9 Digit.Morph.N0To9


type HostLabelSymbol
    = HostLabelHyphenMinus
    | HostLabelSymbolAToZ { case_ : Char.Case, letter : Char.AToZ }
    | HostLabelSymbol0To9 Digit.Morph.N0To9


type alias DomainTopLevel =
    RecordWithoutConstructorFunction
        { startDigits : List Digit.Morph.N0To9
        , firstAToZ : { case_ : Char.Case, letter : Char.AToZ }
        , afterFirstAToZ : List DomainTopLevelAfterFirstAToZSymbol
        }


type DomainTopLevelAfterFirstAToZSymbol
    = DomainTopLevelSymbolAToZ { case_ : Char.Case, letter : Char.AToZ }
    | DomainTopLevelSymbol0To9 Digit.Morph.N0To9
