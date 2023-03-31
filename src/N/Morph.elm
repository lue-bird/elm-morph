module N.Morph exposing
    ( in_
    , inOn, inNumber
    , int, char
    )

{-| [`Morph`](Morph#Morph) an [`N`](https://dark.elm.dmy.fr/packages/lue-bird/elm-bounded-nat/latest/)


## alter

@docs in_
@docs inOn, inNumber


## transform

@docs int, char

-}

import Morph exposing (MorphIndependently, translate)
import N exposing (Add1, In, Min, N, N9, On, To, Up, Up0, Up9, n0, n1, n2, n3, n4, n5, n6, n7, n8, n9)


{-| [`Morph`](Morph#Morph) an `Int` to an `N`,
[erroring](Morph#Error) on negative numbers
-}
int :
    MorphIndependently
        (Int
         -> Result Morph.Error (N (Min (Up0 narrowX_)))
        )
        (N broadRange_ -> Int)
int =
    Morph.value ">= 0"
        { narrow =
            \narrowInt ->
                narrowInt
                    |> N.intIsAtLeast n0
                    |> Result.mapError
                        (\intNegative ->
                            [ intNegative |> String.fromInt, " is <= -1 " ] |> String.concat
                        )
        , broaden = N.toInt
        }


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
    Morph.value (( lowerLimit, upperLimit ) |> rangeDescription)
        { narrow =
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
        , broaden = identity
        }


rangeDescription : ( N minRange_, N maxRange_ ) -> String
rangeDescription =
    \( min, max ) ->
        [ min |> N.toInt |> String.fromInt
        , "|..|"
        , max |> N.toInt |> String.fromInt
        ]
            |> String.concat


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
    Morph.value "digit"
        { narrow =
            \charBroad ->
                case charBroad of
                    '0' ->
                        n0 |> N.maxTo n9 |> Ok

                    '1' ->
                        n1 |> N.minTo n0 |> N.maxTo n9 |> Ok

                    '2' ->
                        n2 |> N.minTo n0 |> N.maxTo n9 |> Ok

                    '3' ->
                        n3 |> N.minTo n0 |> N.maxTo n9 |> Ok

                    '4' ->
                        n4 |> N.minTo n0 |> N.maxTo n9 |> Ok

                    '5' ->
                        n5 |> N.minTo n0 |> N.maxTo n9 |> Ok

                    '6' ->
                        n6 |> N.minTo n0 |> N.maxTo n9 |> Ok

                    '7' ->
                        n7 |> N.minTo n0 |> N.maxTo n9 |> Ok

                    '8' ->
                        n8 |> N.minTo n0 |> N.maxTo n9 |> Ok

                    '9' ->
                        n9 |> N.minTo n0 |> Ok

                    charExceptDigit ->
                        charExceptDigit |> String.fromChar |> Err
        , broaden =
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
        }


{-| [`Morph`](Morph#Morph) from an `N` with an equatable range `In (On ...) (On ...)`
to an `In ... ...` to make it equatable
-}
inOn :
    MorphIndependently
        (N (In (On narrowMin) (On narrowMax))
         -> Result error_ (N (In narrowMin narrowMax))
        )
        (N (In broadMin broadMax)
         -> N (In (On broadMin) (On broadMax))
        )
inOn =
    translate N.inToNumber N.inToOn


{-| [`Morph`](Morph#Morph) from an `N` with a range `In`
to an `In (On ...) (On ...)` to operate on it
-}
inNumber :
    MorphIndependently
        (N (In narrowMin narrowMax)
         -> Result error_ (N (In (On narrowMin) (On narrowMax)))
        )
        (N (In (On broadMin) (On broadMax))
         -> N (In broadMin broadMax)
        )
inNumber =
    translate N.inToOn N.inToNumber
