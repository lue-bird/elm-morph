module N.Morph exposing
    ( in_
    , natural, bit, inChar
    )

{-| [`Morph`](Morph#Morph) for a [natural number of type `N`](https://dark.elm.dmy.fr/packages/lue-bird/elm-bounded-nat/latest/)


## alter

@docs in_


## transform

@docs natural, bit, inChar

-}

import Bit exposing (Bit)
import Char.Morph.Internal
import Emptiable exposing (Emptiable)
import Morph exposing (MorphIndependently)
import N exposing (Add1, In, Min, N, N1, N9, On, To, Up, Up0, Up1, n0, n9)
import Natural exposing (Natural)
import Stack exposing (Stacked)


rangeDescription : ( N minRange_, N maxRange_ ) -> String
rangeDescription =
    \( min, max ) ->
        [ min |> N.toInt |> String.fromInt
        , "|..|"
        , max |> N.toInt |> String.fromInt
        ]
            |> String.concat


{-| [`Morph`](Morph#Morph) the `N` to a more narrow range
-}
in_ :
    ( N (In lowerLimitMin (Up lowerLimitMinX_ To (Add1 lowerLimitMinPlusX_)))
    , N (In (Up upperLimitMinX_ To upperLimitMinPlusX_) upperLimitMax)
    )
    ->
        MorphIndependently
            (N (In narrowMin_ narrowMax_)
             ->
                Result
                    Morph.Error
                    (N (In lowerLimitMin upperLimitMax))
            )
            (N broadRange -> N broadRange)
in_ ( lowerLimit, upperLimit ) =
    Morph.custom (( lowerLimit, upperLimit ) |> rangeDescription)
        { toNarrow =
            \n ->
                n
                    |> N.isIn ( lowerLimit, upperLimit )
                    |> Result.mapError
                        (\notInRange ->
                            case notInRange of
                                N.Below below ->
                                    [ "≤ ", (below |> N.toInt) - 1 |> String.fromInt ]
                                        |> String.concat

                                N.Above above ->
                                    [ "≥ ", (above |> N.toInt) + 1 |> String.fromInt ]
                                        |> String.concat
                        )
        , toBroad = identity
        }


{-| [`Morph`](Morph#Morph) from a [`Natural`](Natural#Natural).

[Inverse](Morph#invert) of [`Natural.Morph.n`](Natural-Morph#n)

-}
natural : MorphIndependently (Natural -> Result error_ (N (Min (Up0 minX_)))) (N range_ -> Natural)
natural =
    Morph.oneToOne Natural.toN Natural.fromN


{-| `Bit.O` ↔ `n0`, `Bit.I` ↔ `n1`.

[Inverse](Morph#invert) of [`Bit.Morph.n`](Bit-Morph#n)

-}
bit :
    MorphIndependently
        (Bit -> Result error_ (N (In (Up0 minX_) (Up1 maxX_))))
        (N (In min_ (Up maxTo1_ To N1)) -> Bit)
bit =
    Morph.oneToOne Bit.toN Bit.fromN


{-| [`Morph`](Morph#Morph) a digit in a given range

    import Morph
    import N exposing (n1, n2, n7, n9)

    '5'
        |> Morph.toNarrow (N.Morph.inChar ( n1, n7 ))
        |> Result.map N.toInt
    --> Ok 5

    '1'
        |> Morph.toNarrow (N.Morph.inChar ( n2, n9 ))
        |> Result.toMaybe
    --> Nothing

-}
inChar :
    ( N (In lowerLimitMin (Up lowerLimitMaxToUpperLimitMin_ To upperLimitMin))
    , N (In (On upperLimitMin) upperLimitMax)
    )
    ->
        MorphIndependently
            (Char
             ->
                Result
                    Morph.Error
                    (N (In lowerLimitMin upperLimitMax))
            )
            (N (In narrowMin_ (Up narrowMaxTo9_ To N9)) -> Char)
inChar ( lowerLimit, upperLimit ) =
    Morph.named (( lowerLimit, upperLimit ) |> rangeDescription)
        (Morph.oneToOne .tag
            (\n -> { tag = n |> N.toIn ( lowerLimit, upperLimit ), info = () })
            |> Morph.over
                (Morph.tryTopToBottom
                    (\n ->
                        Char.Morph.Internal.only
                            (n |> N.toIn ( n0, n9 ) |> digitToChar)
                    )
                    (stackRange ( lowerLimit, upperLimit ))
                )
        )


stackRange :
    ( N (In lowerLimitMin (Up lowerLimitMaxToUpperLimitMin_ To upperLimitMin))
    , N (In (On upperLimitMin) upperLimitMax)
    )
    -> Emptiable (Stacked (N (In lowerLimitMin upperLimitMax))) never_
stackRange ( start, end ) =
    -- ↓ I was very lazy. Better implement it natively
    Stack.topBelow (start |> N.toIn ( start, end ))
        (List.range ((start |> N.toInt) + 1) (end |> N.toInt)
            |> List.map (N.intToIn ( start, end ))
        )


digitToChar : N (In narrowMin_ (Up narrowMaxTo9_ To N9)) -> Char
digitToChar =
    \n ->
        case n |> N.toInt of
            0 ->
                '0'

            1 ->
                '1'

            2 ->
                '2'

            3 ->
                '3'

            4 ->
                '4'

            5 ->
                '5'

            6 ->
                '6'

            7 ->
                '7'

            8 ->
                '8'

            -- 9
            _ ->
                '9'
