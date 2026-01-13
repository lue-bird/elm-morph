module Email exposing (Email, chars)

import AToZ exposing (AToZ)
import AToZ.Morph
import Char.Morph
import Linear exposing (Direction(..))
import Morph exposing (Morph, MorphRow, grab, match, whilePossible)
import N1To9 exposing (N1To9)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import String.Morph



-- format as described in https://en.wikipedia.org/wiki/Email_address


chars : MorphRow Email Char
chars =
    Morph.named "email"
        (Morph.narrow
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
        (Morph.narrow
            (\part0 part1 part2Up ->
                { part0 = part0, part1 = part1, part2Up = part2Up }
            )
            |> grab .part0 localPart
            |> grab .part1 localPart
            |> grab .part2Up
                (whilePossible
                    (Morph.narrow (\part -> part)
                        |> match (String.Morph.only ".")
                        |> grab (\part -> part) localPart
                    )
                )
        )


localPart : MorphRow LocalPart Char
localPart =
    Morph.named "local part"
        (Morph.narrow
            (\symbol0 symbol1Up -> { symbol0 = symbol0, symbol1Up = symbol1Up })
            |> grab .symbol0 (Morph.one localSymbol)
            |> grab .symbol1Up (whilePossible (Morph.one localSymbol))
        )


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
            |> Morph.try LocalSymbolPrintable localSymbolPrintable
            |> Morph.try LocalSymbolAToZ
                (AToZ.Morph.broadCase AToZ.CaseLower
                    |> Morph.over AToZ.Morph.char
                )
            |> Morph.try LocalSymbol0To9 N1To9.morphChar
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
        (Morph.narrow
            (\first hostLabels topLevel ->
                { first = first, hostLabels = hostLabels, topLevel = topLevel }
            )
            |> Morph.grab .first hostLabel
            |> Morph.match (String.Morph.only ".")
            |> Morph.grab .hostLabels
                (whilePossible
                    (Morph.narrow (\label -> label)
                        |> Morph.grab (\label -> label) hostLabel
                        |> Morph.match (String.Morph.only ".")
                    )
                )
            |> Morph.grab .topLevel domainTopLevel
        )


hostLabelSideableSymbol : Morph HostLabelSideableSymbol Char
hostLabelSideableSymbol =
    Morph.choice
        (\aToZVariant n0To9Variant sideSymbol ->
            case sideSymbol of
                HostLabelSideSymbolAToZ aToZValue ->
                    aToZVariant aToZValue

                HostLabelSideSymbol0To9 n0To9Value ->
                    n0To9Variant n0To9Value
        )
        |> Morph.try HostLabelSideSymbolAToZ
            (AToZ.Morph.broadCase AToZ.CaseLower
                |> Morph.over AToZ.Morph.char
            )
        |> Morph.try HostLabelSideSymbol0To9 N1To9.morphChar
        |> Morph.choiceFinish


hostLabel : MorphRow HostLabel Char
hostLabel =
    Morph.named "host label"
        (Morph.narrow
            (\firstSymbol afterFirstSymbol ->
                { firstSymbol = firstSymbol
                , afterFirstSymbol = afterFirstSymbol
                }
            )
            |> grab .firstSymbol
                (hostLabelSideableSymbol |> Morph.one)
            |> grab .afterFirstSymbol
                (whilePossible hostLabelSectionAfterFirst)
        )


hostLabelSectionAfterFirst : MorphRow HostLabelSection Char
hostLabelSectionAfterFirst =
    Morph.choice
        (\hyphenMinus sideableVariant symbol ->
            case symbol of
                HostLabelStartingWithHyphenMinus nextValue ->
                    hyphenMinus nextValue

                HostLabelSideableSymbol sideableValue ->
                    sideableVariant sideableValue
        )
        |> Morph.rowTry HostLabelStartingWithHyphenMinus
            (Morph.narrow
                (\hyphenMinusCount next ->
                    { hyphenMinusCount = hyphenMinusCount, next = next }
                )
                |> Morph.grab .hyphenMinusCount
                    (Morph.oneToOne List.length (\l -> List.repeat l ())
                        |> Morph.overRow
                            (Morph.whilePossible (String.Morph.only "-"))
                    )
                |> Morph.grab .next (hostLabelSideableSymbol |> Morph.one)
            )
        |> Morph.rowTry HostLabelSideableSymbol
            (hostLabelSideableSymbol |> Morph.one)
        |> Morph.choiceFinish


domainTopLevel : MorphRow DomainTopLevel Char
domainTopLevel =
    Morph.named "domain top-level"
        (Morph.narrow
            (\startDigits firstAToZ afterFirstAToZ ->
                { startDigits = startDigits
                , firstAToZ = firstAToZ
                , afterFirstAToZ = afterFirstAToZ
                }
            )
            |> grab .startDigits
                (whilePossible (N1To9.morphChar |> Morph.one))
            |> -- guarantees it can't be numeric only
               grab .firstAToZ
                (AToZ.Morph.broadCase AToZ.CaseLower
                    |> Morph.over AToZ.Morph.char
                    |> Morph.one
                )
            |> grab .afterFirstAToZ
                (whilePossible (domainTopLevelAfterFirstAToZSymbol |> Morph.one))
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
            (AToZ.Morph.broadCase AToZ.CaseLower
                |> Morph.over AToZ.Morph.char
            )
        |> Morph.try DomainTopLevelSymbol0To9
            N1To9.morphChar
        |> Morph.choiceFinish


type alias Email =
    RecordWithoutConstructorFunction
        { local : Local
        , domain : Domain
        }


type alias Local =
    { part0 : LocalPart, part1 : LocalPart, part2Up : List LocalPart }


type alias LocalPart =
    { symbol0 : LocalSymbol, symbol1Up : List LocalSymbol }


type LocalSymbol
    = LocalSymbolPrintable LocalSymbolPrintable
    | LocalSymbolAToZ AToZ
    | LocalSymbol0To9 N1To9


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
        { firstSymbol : HostLabelSideableSymbol
        , afterFirstSymbol : List HostLabelSection
        }


type HostLabelSideableSymbol
    = HostLabelSideSymbolAToZ AToZ
    | HostLabelSideSymbol0To9 N1To9


type HostLabelSection
    = HostLabelStartingWithHyphenMinus { hyphenMinusCount : Int, next : HostLabelSideableSymbol }
    | HostLabelSideableSymbol HostLabelSideableSymbol


{-| <https://data.iana.org/TLD/tlds-alpha-by-domain.txt>
-}
type alias DomainTopLevel =
    RecordWithoutConstructorFunction
        { startDigits : List N1To9
        , firstAToZ : AToZ
        , afterFirstAToZ : List DomainTopLevelAfterFirstAToZSymbol
        }


type DomainTopLevelAfterFirstAToZSymbol
    = DomainTopLevelSymbolAToZ AToZ
    | DomainTopLevelSymbol0To9 N1To9
