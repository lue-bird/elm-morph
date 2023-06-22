module N.Morph exposing
    ( in_
    , natural, toNatural, char
    )

{-| [`Morph`](Morph#Morph) a [natural number of type `N`](https://dark.elm.dmy.fr/packages/lue-bird/elm-bounded-nat/latest/)


## alter

@docs in_


## transform

@docs natural, toNatural, char

-}

import ArraySized
import Bit
import BitArray
import BitArray.Extra
import Char.Morph.Internal
import Linear exposing (Direction(..))
import Morph exposing (MorphIndependently)
import N exposing (Add1, In, Min, N, N9, To, Up, Up0, Up9, n0, n1, n2, n3, n4, n5, n6, n7, n8, n9)
import N.Local exposing (n32)
import Natural exposing (Natural)


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
                                    [ "<= ", (below |> N.toInt) - 1 |> String.fromInt ]
                                        |> String.concat

                                N.Above above ->
                                    [ ">= ", (above |> N.toInt) + 1 |> String.fromInt ]
                                        |> String.concat
                        )
        , toBroad = identity
        }


rangeDescription : ( N minRange_, N maxRange_ ) -> String
rangeDescription =
    \( min, max ) ->
        [ min |> N.toInt |> String.fromInt
        , "|..|"
        , max |> N.toInt |> String.fromInt
        ]
            |> String.concat


{-| [`Morph`](Morph#Morph) from a [`Natural`](Natural#Natural)
-}
natural : MorphIndependently (N range_ -> Result error_ Natural) (Natural -> N (Min (Up0 minX_)))
natural =
    Morph.oneToOne toNaturalImplementation fromNatural


toNaturalImplementation : N range_ -> Natural
toNaturalImplementation =
    \n_ ->
        case
            n_
                |> BitArray.fromN n32
                |> BitArray.Extra.unpad
                |> ArraySized.hasAtLeast n1
        of
            Err _ ->
                Natural.N0

            Ok atLeast1 ->
                Natural.AtLeast1
                    { bitsAfterI =
                        atLeast1
                            |> ArraySized.removeMin ( Up, n1 )
                            |> ArraySized.minToNumber
                            |> ArraySized.maxToInfinity
                    }


fromNatural : Natural -> N (Min (Up0 minX_))
fromNatural =
    \naturalNarrow ->
        case naturalNarrow of
            Natural.N0 ->
                n0 |> N.maxToInfinity

            Natural.AtLeast1 atLeast1 ->
                atLeast1
                    |> .bitsAfterI
                    |> ArraySized.minToOn
                    |> ArraySized.insertMin ( Up, N.n1 ) Bit.I
                    |> BitArray.toN


{-| [`Morph`](Morph#Morph) to a [`Natural`](Natural#Natural)
-}
toNatural : MorphIndependently (Natural -> Result error_ (N (Min (Up0 minX_)))) (N range_ -> Natural)
toNatural =
    Morph.invert natural


{-| [`Morph`](Morph#Morph) a digit in a given range

You can require a maximum >= 10.
In that case, the [narrowed](Morph#toNarrow) `N` will also have a maximum >= 10
even though every possible `Char` can only show a digit <= 9

-}
char :
    MorphIndependently
        (Char
         ->
            Result
                Morph.Error
                (N (In (Up0 broadMinX_) (Up9 broadMax_)))
        )
        (N (In narrowMin_ (Up narrowMaxTo9_ To N9)) -> Char)
char =
    Morph.named "digit"
        (Morph.choice
            (\v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 n ->
                case n |> N.toInt of
                    0 ->
                        v0 ()

                    1 ->
                        v1 ()

                    2 ->
                        v2 ()

                    3 ->
                        v3 ()

                    4 ->
                        v4 ()

                    5 ->
                        v5 ()

                    6 ->
                        v6 ()

                    7 ->
                        v7 ()

                    8 ->
                        v8 ()

                    -- 9
                    _ ->
                        v9 ()
            )
            |> Morph.try (\() -> n0 |> withDigitRange) (Char.Morph.Internal.only '0')
            |> Morph.try (\() -> n1 |> withDigitRange) (Char.Morph.Internal.only '1')
            |> Morph.try (\() -> n2 |> withDigitRange) (Char.Morph.Internal.only '2')
            |> Morph.try (\() -> n3 |> withDigitRange) (Char.Morph.Internal.only '3')
            |> Morph.try (\() -> n4 |> withDigitRange) (Char.Morph.Internal.only '4')
            |> Morph.try (\() -> n5 |> withDigitRange) (Char.Morph.Internal.only '5')
            |> Morph.try (\() -> n6 |> withDigitRange) (Char.Morph.Internal.only '6')
            |> Morph.try (\() -> n7 |> withDigitRange) (Char.Morph.Internal.only '7')
            |> Morph.try (\() -> n8 |> withDigitRange) (Char.Morph.Internal.only '8')
            |> Morph.try (\() -> n9 |> withDigitRange) (Char.Morph.Internal.only '9')
            |> Morph.choiceFinish
        )


withDigitRange :
    N (In (N.On minX_) (Up max_ To N9))
    -> N (In (Up0 resultMinX_) (Up9 resultMaxX_))
withDigitRange digit =
    digit |> N.minTo n0 |> N.maxTo n9
