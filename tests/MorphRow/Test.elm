module MorphRow.Test exposing (tests)

import ArraySized exposing (ArraySized)
import ArraySized.Morph
import Digit.Morph
import Emptiable
import Expect
import Linear exposing (DirectionLinear(..))
import Morph exposing (Morph, broaden, broadenFrom, choice, narrow, translate)
import Morph.Char as Char
import Morph.Text
import MorphRow exposing (MorphRow, atLeast, grab, one, separatedBy, skip, succeed)
import N exposing (Fixed, Min, N0, N1, N2, n0, n1)
import Number.Morph exposing (Number)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Stack
import Stack.Morph
import Test exposing (Test, test)


tests : Test
tests =
    Test.describe
        "Morph to row"
        [ pointTest
        , emailTest
        ]



-- point


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
                            |> Morph.part ( .x, .x ) Number.Morph.fromFloat
                            |> Morph.part ( .y, .y ) Number.Morph.fromFloat
                            |> MorphRow.over point
                            |> MorphRow.finish
                            |> Morph.over Stack.Morph.fromText
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
                            |> Morph.part ( .x, .x ) Number.Morph.fromFloat
                            |> Morph.part ( .y, .y ) Number.Morph.fromFloat
                            |> MorphRow.over point
                            |> MorphRow.finish
                            |> Morph.over Stack.Morph.fromText
                        )
                    |> Expect.equal "( 3, -9999.124 )"
            )
        ]


point : MorphRow Char Point
point =
    succeed (\x y -> { x = x, y = y })
        |> skip (Morph.Text.specific "(")
        |> skip
            (broadenFrom (ArraySized.l1 Char.Space |> ArraySized.min n0 |> ArraySized.maxNo)
                |> MorphRow.over (atLeast n0 (Char.blank |> one))
            )
        |> grab .x Number.Morph.text
        |> skip
            (broadenFrom (ArraySized.empty |> ArraySized.maxNo)
                |> MorphRow.over (atLeast n0 (Char.blank |> one))
            )
        |> skip (Morph.Text.specific ",")
        |> skip
            (broadenFrom (ArraySized.l1 Char.Space |> ArraySized.min n0 |> ArraySized.maxNo)
                |> MorphRow.over (atLeast n0 (Char.blank |> one))
            )
        |> grab .y Number.Morph.text
        |> skip
            (broadenFrom (ArraySized.l1 Char.Space |> ArraySized.min n0 |> ArraySized.maxNo)
                |> MorphRow.over (atLeast n0 (Char.blank |> one))
            )
        |> skip (Morph.Text.specific ")")


emailTest : Test
emailTest =
    let
        emailToText =
            email |> MorphRow.finish |> Morph.over Stack.Morph.fromText
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


email : MorphRow Char Email
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


local : MorphRow Char Local
local =
    translate (\dotSeparated -> { dotSeparated = dotSeparated })
        .dotSeparated
        |> Morph.over
            (translate
                (\{ first, afterFirst } ->
                    ArraySized.l1 first
                        |> ArraySized.minGlue Up
                            (afterFirst |> ArraySized.min n1)
                )
                (\arr ->
                    { first = arr |> ArraySized.element ( Up, n0 )
                    , afterFirst = arr |> ArraySized.minElementRemove ( Up, n0 )
                    }
                )
                |> Morph.over
                    (Morph.group
                        (\first afterFirst -> { first = first, afterFirst = afterFirst })
                        (\first afterFirst -> { first = first, afterFirst = afterFirst })
                        |> Morph.part ( .first, .first ) Morph.remain
                        |> Morph.part ( .afterFirst, .afterFirst )
                            (ArraySized.Morph.elementEach
                                (translate .part
                                    (\part -> { separator = (), part = part })
                                )
                            )
                    )
            )
        |> MorphRow.over
            (separatedBy
                ( atLeast n1, Morph.Text.specific "." )
                (atLeast n1 (localSymbol |> one))
            )


localSymbol : Morph LocalSymbol Char (Morph.Error Char)
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
            (translate .letter
                (\letter -> { letter = letter, case_ = Char.CaseLower })
                |> Morph.over Char.aToZ
            )
        |> Morph.possibility LocalSymbol0To9 Digit.Morph.n0To9
        |> Morph.choiceFinish



-- local


localSymbolPrintable :
    Morph
        LocalSymbolPrintable
        Char
        (Morph.Error Char)
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


domain : MorphRow Char Domain
domain =
    succeed
        (\first hostLabels topLevel ->
            { first = first, hostLabels = hostLabels, topLevel = topLevel }
        )
        |> MorphRow.grab .first hostLabel
        |> MorphRow.skip (Morph.Text.specific ".")
        |> MorphRow.grab .hostLabels
            (atLeast n0
                (succeed (\label -> label)
                    |> MorphRow.grab (\label -> label) hostLabel
                    |> MorphRow.skip (Morph.Text.specific ".")
                )
            )
        |> MorphRow.grab .topLevel domainTopLevel


hostLabel : MorphRow Char HostLabel
hostLabel =
    succeed
        (\firstSymbol betweenFirstAndLastSymbols lastSymbol ->
            { firstSymbol = firstSymbol
            , betweenFirstAndLastSymbols = betweenFirstAndLastSymbols
            , lastSymbol = lastSymbol
            }
        )
        |> grab .firstSymbol
            (hostLabelSideSymbol |> one)
        |> grab .betweenFirstAndLastSymbols
            (atLeast n0 (hostLabelSymbol |> one))
        |> grab .lastSymbol
            (hostLabelSideSymbol |> one)


hostLabelSideSymbol : Morph HostLabelSideSymbol Char (Morph.Error Char)
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


hostLabelSymbol : Morph HostLabelSymbol Char (Morph.Error Char)
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


domainTopLevel : MorphRow Char DomainTopLevel
domainTopLevel =
    succeed
        (\startDigits firstAToZ afterFirstAToZ ->
            { startDigits = startDigits
            , firstAToZ = firstAToZ
            , afterFirstAToZ = afterFirstAToZ
            }
        )
        |> grab .startDigits
            (atLeast n0 (Digit.Morph.n0To9 |> one))
        |> -- guarantees it can't be numeric only
           grab .firstAToZ
            (Char.aToZ |> one)
        |> grab .afterFirstAToZ
            (atLeast n0 (domainTopLevelAfterFirstAToZSymbol |> one))



-- domain


domainTopLevelAfterFirstAToZSymbol :
    Morph
        DomainTopLevelAfterFirstAToZSymbol
        Char
        (Morph.Error Char)
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
            ArraySized
                (Min (Fixed N2))
                (ArraySized (Min (Fixed N1)) LocalSymbol)
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
        , hostLabels : ArraySized (Min (Fixed N0)) HostLabel
        , topLevel : DomainTopLevel
        }


type alias HostLabel =
    RecordWithoutConstructorFunction
        { firstSymbol : HostLabelSideSymbol
        , betweenFirstAndLastSymbols : ArraySized (Min (Fixed N0)) HostLabelSymbol
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
        { startDigits : ArraySized (Min (Fixed N0)) Digit.Morph.N0To9
        , firstAToZ : { case_ : Char.Case, letter : Char.AToZ }
        , afterFirstAToZ : ArraySized (Min (Fixed N0)) DomainTopLevelAfterFirstAToZSymbol
        }


type DomainTopLevelAfterFirstAToZSymbol
    = DomainTopLevelSymbolAToZ { case_ : Char.Case, letter : Char.AToZ }
    | DomainTopLevelSymbol0To9 Digit.Morph.N0To9
