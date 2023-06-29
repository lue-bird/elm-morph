module Email exposing (Email, chars)

import AToZ exposing (AToZ)
import AToZ.Morph
import ArraySized exposing (ArraySized)
import ArraySized.Morph exposing (atLeast)
import Char.Morph
import Linear exposing (Direction(..))
import Morph exposing (Morph, MorphRow, MorphRowIndependently, grab, match, one, oneToOne, whilePossible)
import N exposing (In, Min, N, N0, N1, N2, N9, On, n0, n1, n9)
import N.Morph
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import String.Morph



-- format as described in https://en.wikipedia.org/wiki/Email_address


chars : MorphRow Email Char
chars =
    Morph.named "email"
        (Morph.succeed
            (\local_ domain_ ->
                { local = local_
                , domain = domain_
                }
            )
            |> grab .local local
            |> match (String.Morph.only "@")
            |> grab .domain domain
        )


local : MorphRow Local Char
local =
    Morph.named "local"
        (Morph.succeed
            (\first afterFirst ->
                ArraySized.one first
                    |> ArraySized.attachMin Up
                        (afterFirst |> ArraySized.minTo n1)
            )
            |> grab (ArraySized.element ( Up, n1 )) localPart
            |> grab (ArraySized.removeMin ( Up, n1 ))
                (atLeast n1
                    (Morph.succeed (\part -> part)
                        |> match (String.Morph.only ".")
                        |> grab (\part -> part) localPart
                    )
                )
        )


localPart :
    MorphRowIndependently
        (ArraySized LocalSymbol (In (On N1) max_))
        LocalPart
        Char
localPart =
    atLeast n1 (localSymbol |> one)


localSymbol : Morph LocalSymbol Char
localSymbol =
    Morph.named "local symbol"
        (Morph.choice
            (\printableVariant aToZVariant n0To9Variant localSymbolUnion ->
                case localSymbolUnion of
                    LocalSymbolPrintable printableValue ->
                        printableVariant printableValue

                    LocalSymbolAToZ aToZValue ->
                        aToZVariant aToZValue

                    LocalSymbol0To9 n0To9Value ->
                        n0To9Variant n0To9Value
            )
            |> Morph.try LocalSymbolPrintable
                localSymbolPrintable
            |> Morph.try LocalSymbolAToZ
                (oneToOne .letter
                    (\letter -> { letter = letter, case_ = AToZ.CaseLower })
                    |> Morph.over AToZ.Morph.char
                )
            |> Morph.try LocalSymbol0To9
                (N.Morph.in_ ( n0, n9 )
                    |> Morph.over N.Morph.char
                )
            |> Morph.choiceFinish
        )



-- local


localSymbolPrintable : Morph LocalSymbolPrintable Char
localSymbolPrintable =
    Morph.named "local symbol printable"
        (Morph.choice
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
            |> Morph.try (\() -> NumberSign) (Char.Morph.only '#')
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
        )


domain : MorphRow Domain Char
domain =
    Morph.named "domain"
        (Morph.succeed
            (\first hostLabels topLevel ->
                { first = first, hostLabels = hostLabels, topLevel = topLevel }
            )
            |> Morph.grab .first hostLabel
            |> Morph.grab .hostLabels
                (whilePossible
                    (Morph.succeed (\label -> label)
                        |> Morph.match (String.Morph.only ".")
                        |> Morph.grab (\label -> label) hostLabel
                    )
                )
            |> Morph.match (String.Morph.only ".")
            |> Morph.grab .topLevel domainTopLevel
        )


hostLabel : MorphRow HostLabel Char
hostLabel =
    Morph.named "host label"
        (Morph.succeed
            (\firstSymbol betweenFirstAndLastSymbols lastSymbol ->
                { firstSymbol = firstSymbol
                , betweenFirstAndLastSymbols = betweenFirstAndLastSymbols
                , lastSymbol = lastSymbol
                }
            )
            |> grab .firstSymbol
                (hostLabelSideSymbol |> one)
            |> grab .betweenFirstAndLastSymbols
                (whilePossible (hostLabelSymbol |> one))
            |> grab .lastSymbol
                (hostLabelSideSymbol |> one)
        )


hostLabelSideSymbol : Morph HostLabelSideSymbol Char
hostLabelSideSymbol =
    Morph.choice
        (\aToZVariant n0To9Variant sideSymbol ->
            case sideSymbol of
                HostLabelSideSymbolAToZ aToZValue ->
                    aToZVariant aToZValue

                HostLabelSideSymbol0To9 n0To9Value ->
                    n0To9Variant n0To9Value
        )
        |> Morph.try HostLabelSideSymbolAToZ
            AToZ.Morph.char
        |> Morph.try HostLabelSideSymbol0To9
            (N.Morph.in_ ( n0, n9 )
                |> Morph.over N.Morph.char
            )
        |> Morph.choiceFinish


hostLabelSymbol : Morph HostLabelSymbol Char
hostLabelSymbol =
    Morph.choice
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
        |> Morph.try HostLabelSymbolAToZ
            AToZ.Morph.char
        |> Morph.try HostLabelSymbol0To9
            (N.Morph.in_ ( n0, n9 )
                |> Morph.over N.Morph.char
            )
        |> Morph.choiceFinish


domainTopLevel : MorphRow DomainTopLevel Char
domainTopLevel =
    Morph.named "domain top-level"
        (Morph.succeed
            (\startDigits firstAToZ afterFirstAToZ ->
                { startDigits = startDigits
                , firstAToZ = firstAToZ
                , afterFirstAToZ = afterFirstAToZ
                }
            )
            |> grab .startDigits
                (whilePossible (N.Morph.char |> one))
            |> -- guarantees it can't be numeric only
               grab .firstAToZ
                (AToZ.Morph.char |> one)
            |> grab .afterFirstAToZ
                (whilePossible (domainTopLevelAfterFirstAToZSymbol |> one))
        )



-- domain


domainTopLevelAfterFirstAToZSymbol : Morph DomainTopLevelAfterFirstAToZSymbol Char
domainTopLevelAfterFirstAToZSymbol =
    Morph.choice
        (\aToZVariant n0To9Variant domainTopLevelSymbolUnion ->
            case domainTopLevelSymbolUnion of
                DomainTopLevelSymbolAToZ aToZValue ->
                    aToZVariant aToZValue

                DomainTopLevelSymbol0To9 n0To9Value ->
                    n0To9Variant n0To9Value
        )
        |> Morph.try DomainTopLevelSymbolAToZ
            AToZ.Morph.char
        |> Morph.try DomainTopLevelSymbol0To9
            (N.Morph.in_ ( n0, n9 )
                |> Morph.over N.Morph.char
            )
        |> Morph.choiceFinish


type alias Email =
    RecordWithoutConstructorFunction
        { local : Local
        , domain : Domain
        }


type alias Local =
    ArraySized LocalPart (Min (On N2))


type alias LocalPart =
    ArraySized LocalSymbol (Min (On N1))


type LocalSymbol
    = LocalSymbolPrintable LocalSymbolPrintable
    | LocalSymbolAToZ AToZ
    | LocalSymbol0To9 (N (In (On N0) (On N9)))


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
        , hostLabels : List HostLabel
        , topLevel : DomainTopLevel
        }


type alias HostLabel =
    RecordWithoutConstructorFunction
        { firstSymbol : HostLabelSideSymbol
        , betweenFirstAndLastSymbols : List HostLabelSymbol
        , lastSymbol : HostLabelSideSymbol
        }


type HostLabelSideSymbol
    = HostLabelSideSymbolAToZ { case_ : AToZ.Case, letter : AToZ }
    | HostLabelSideSymbol0To9 (N (In (On N0) (On N9)))


type HostLabelSymbol
    = HostLabelHyphenMinus
    | HostLabelSymbolAToZ { case_ : AToZ.Case, letter : AToZ }
    | HostLabelSymbol0To9 (N (In (On N0) (On N9)))


{-| <https://data.iana.org/TLD/tlds-alpha-by-domain.txt>
-}
type alias DomainTopLevel =
    RecordWithoutConstructorFunction
        { startDigits : List (N (In (On N0) (On N9)))
        , firstAToZ : { case_ : AToZ.Case, letter : AToZ }
        , afterFirstAToZ : List DomainTopLevelAfterFirstAToZSymbol
        }


type DomainTopLevelAfterFirstAToZSymbol
    = DomainTopLevelSymbolAToZ { case_ : AToZ.Case, letter : AToZ }
    | DomainTopLevelSymbol0To9 (N (In (On N0) (On N9)))
