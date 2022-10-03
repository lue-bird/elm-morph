module Morph.Test exposing (tests)

import AToZ exposing (AToZ)
import Array
import ArraySized exposing (ArraySized)
import ArraySized.Morph
import Char.Morph
import Decimal exposing (Decimal)
import Expect
import Fuzz
import Linear exposing (Direction(..))
import Morph exposing (Morph, MorphRow, MorphRowIndependently, atLeast, broad, broadenWith, choice, grab, narrowWith, one, skip, succeed, translate)
import N exposing (Fixed, In, InFixed, Min, N, N0, N1, N2, N9, n0, n1, n9)
import N.Morph
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Stack.Morph
import String.Morph
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
                    |> narrowWith
                        (Morph.group
                            ( \x y -> { x = x, y = y }
                            , \x y -> { x = x, y = y }
                            )
                            |> Morph.part ( .x, .x ) Decimal.float
                            |> Morph.part ( .y, .y ) Decimal.float
                            |> Morph.overRow point
                            |> Morph.rowFinish
                            |> Morph.over Stack.Morph.string
                        )
                    |> Expect.equal (Ok { x = 3.0, y = -9999.124 })
            )
        , test "broaden"
            (\() ->
                { x = 3.0, y = -9999.124 }
                    |> broadenWith
                        (Morph.group
                            ( \x y -> { x = x, y = y }
                            , \x y -> { x = x, y = y }
                            )
                            |> Morph.part ( .x, .x ) Decimal.float
                            |> Morph.part ( .y, .y ) Decimal.float
                            |> Morph.overRow point
                            |> Morph.rowFinish
                            |> Morph.over Stack.Morph.string
                        )
                    |> Expect.equal "( 3, -9999.124 )"
            )
        ]


point : MorphRow Char Point
point =
    succeed (\x y -> { x = x, y = y })
        |> skip (String.Morph.only "(")
        |> skip
            (broad (ArraySized.l1 ())
                |> Morph.overRow (atLeast n0 (String.Morph.only " "))
            )
        |> grab .x Decimal.rowChar
        |> skip
            (broad ArraySized.empty
                |> Morph.overRow (atLeast n0 (String.Morph.only " "))
            )
        |> skip (String.Morph.only ",")
        |> skip
            (broad (ArraySized.l1 ())
                |> Morph.overRow (atLeast n0 (String.Morph.only " "))
            )
        |> grab .y Decimal.rowChar
        |> skip
            (broad (ArraySized.l1 ())
                |> Morph.overRow (atLeast n0 (String.Morph.only " "))
            )
        |> skip (String.Morph.only ")")


emailTest : Test
emailTest =
    let
        emailToText =
            email |> Morph.rowFinish |> Morph.over Stack.Morph.string
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
                                case exampleEmail |> narrowWith emailToText of
                                    Ok emailParsed ->
                                        emailParsed
                                            |> broadenWith emailToText
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
                                case exampleEmail |> narrowWith emailToText of
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
        |> skip (String.Morph.only "@")
        |> grab .domain domain


local : MorphRow Char Local
local =
    succeed
        (\first afterFirst ->
            ArraySized.l1 first
                |> ArraySized.glueMin Up
                    (afterFirst |> ArraySized.minTo n1)
        )
        |> grab (ArraySized.element ( Up, n0 )) localPart
        |> grab (ArraySized.elementRemoveMin ( Up, n0 ))
            (atLeast n1
                (succeed (\part -> part)
                    |> skip (String.Morph.only ".")
                    |> grab (\part -> part) localPart
                )
            )


localPart :
    MorphRowIndependently
        Char
        (ArraySized (In (Fixed N1) max_) LocalSymbol)
        LocalPart
localPart =
    atLeast n1 (localSymbol |> one)


localSymbol : Morph LocalSymbol Char
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
        |> Morph.try LocalSymbolPrintable localSymbolPrintable
        |> Morph.try LocalSymbolAToZ
            (translate .letter
                (\letter -> { letter = letter, case_ = AToZ.CaseLower })
                |> Morph.over AToZ.char
            )
        |> Morph.try LocalSymbol0To9 (N.Morph.charIn ( n0, n9 ))
        |> Morph.choiceFinish



-- local


localSymbolPrintable : Morph LocalSymbolPrintable Char
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
        |> Morph.try (\() -> ExclamationMark) (Char.Morph.only '!')
        |> Morph.try (\() -> DecimalSign) (Char.Morph.only '#')
        |> Morph.try (\() -> DollarSign) (Char.Morph.only '$')
        |> Morph.try (\() -> PercentSign) (Char.Morph.only '%')
        |> Morph.try (\() -> Ampersand) (Char.Morph.only '&')
        |> Morph.try (\() -> Asterisk) (Char.Morph.only '*')
        |> Morph.try (\() -> LowLine) (Char.Morph.only '_')
        |> Morph.try (\() -> HyphenMinus) (Char.Morph.only '-')
        |> Morph.try (\() -> Tilde) (Char.Morph.only '~')
        |> Morph.try (\() -> VerticalLine) (Char.Morph.only '|')
        |> Morph.try (\() -> PlusSign) (Char.Morph.only '+')
        |> Morph.try (\() -> EqualsSign) (Char.Morph.only '=')
        |> Morph.try (\() -> GraveAccent) (Char.Morph.only '`')
        |> Morph.try (\() -> LeftCurlyBracket) (Char.Morph.only '{')
        |> Morph.try (\() -> RightCurlyBracket) (Char.Morph.only '}')
        |> Morph.choiceFinish


domain : MorphRow Char Domain
domain =
    succeed
        (\first hostLabels topLevel ->
            { first = first, hostLabels = hostLabels, topLevel = topLevel }
        )
        |> Morph.grab .first hostLabel
        |> Morph.skip (String.Morph.only ".")
        |> Morph.grab .hostLabels
            (atLeast n0
                (succeed (\label -> label)
                    |> Morph.grab (\label -> label) hostLabel
                    |> Morph.skip (String.Morph.only ".")
                )
            )
        |> Morph.grab .topLevel domainTopLevel


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


hostLabelSideSymbol : Morph HostLabelSideSymbol Char
hostLabelSideSymbol =
    choice
        (\aToZVariant n0To9Variant sideSymbol ->
            case sideSymbol of
                HostLabelSideSymbolAToZ aToZValue ->
                    aToZVariant aToZValue

                HostLabelSideSymbol0To9 n0To9Value ->
                    n0To9Variant n0To9Value
        )
        |> Morph.try HostLabelSideSymbolAToZ AToZ.char
        |> Morph.try HostLabelSideSymbol0To9 (N.Morph.charIn ( n0, n9 ))
        |> Morph.choiceFinish


hostLabelSymbol : Morph HostLabelSymbol Char
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
        |> Morph.try (\() -> HostLabelHyphenMinus)
            (Char.Morph.only '-')
        |> Morph.try HostLabelSymbolAToZ AToZ.char
        |> Morph.try HostLabelSymbol0To9 (N.Morph.charIn ( n0, n9 ))
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
            (atLeast n0 (N.Morph.charIn ( n0, n9 ) |> one))
        |> -- guarantees it can't be numeric only
           grab .firstAToZ
            (AToZ.char |> one)
        |> grab .afterFirstAToZ
            (atLeast n0 (domainTopLevelAfterFirstAToZSymbol |> one))



-- domain


domainTopLevelAfterFirstAToZSymbol : Morph DomainTopLevelAfterFirstAToZSymbol Char
domainTopLevelAfterFirstAToZSymbol =
    choice
        (\aToZVariant n0To9Variant domainTopLevelSymbolUnion ->
            case domainTopLevelSymbolUnion of
                DomainTopLevelSymbolAToZ aToZValue ->
                    aToZVariant aToZValue

                DomainTopLevelSymbol0To9 n0To9Value ->
                    n0To9Variant n0To9Value
        )
        |> Morph.try DomainTopLevelSymbolAToZ AToZ.char
        |> Morph.try DomainTopLevelSymbol0To9 (N.Morph.charIn ( n0, n9 ))
        |> Morph.choiceFinish


type alias Point =
    RecordWithoutConstructorFunction
        { x : Decimal, y : Decimal }


type alias Email =
    RecordWithoutConstructorFunction
        { local : Local
        , domain : Domain
        }


type alias Local =
    ArraySized (Min (Fixed N2)) LocalPart


type alias LocalPart =
    ArraySized (Min (Fixed N1)) LocalSymbol


type LocalSymbol
    = LocalSymbolPrintable LocalSymbolPrintable
    | LocalSymbolAToZ AToZ
    | LocalSymbol0To9 (N (InFixed N0 N9))


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
    = HostLabelSideSymbolAToZ { case_ : AToZ.Case, letter : AToZ }
    | HostLabelSideSymbol0To9 (N (InFixed N0 N9))


type HostLabelSymbol
    = HostLabelHyphenMinus
    | HostLabelSymbolAToZ { case_ : AToZ.Case, letter : AToZ }
    | HostLabelSymbol0To9 (N (InFixed N0 N9))


{-| <https://data.iana.org/TLD/tlds-alpha-by-domain.txt>
-}
type alias DomainTopLevel =
    RecordWithoutConstructorFunction
        { startDigits : ArraySized (Min (Fixed N0)) (N (InFixed N0 N9))
        , firstAToZ : { case_ : AToZ.Case, letter : AToZ }
        , afterFirstAToZ : ArraySized (Min (Fixed N0)) DomainTopLevelAfterFirstAToZSymbol
        }


type DomainTopLevelAfterFirstAToZSymbol
    = DomainTopLevelSymbolAToZ { case_ : AToZ.Case, letter : AToZ }
    | DomainTopLevelSymbol0To9 (N (InFixed N0 N9))
