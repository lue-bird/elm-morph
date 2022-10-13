module N.Morph exposing
    ( in_
    , value, toValue
    , int, char
    )

{-| [`Morph`](Morph#Morph) an [`N`](https://dark.elm.dmy.fr/packages/lue-bird/elm-bounded-nat/latest/)


## alter

@docs in_
@docs value, toValue


## transform

@docs int, char

-}

import Morph exposing (Morph, MorphIndependently, translate)
import N exposing (Add1, Fixed, In, InFixed, InValue, Min, N, N9, To, Up, Up0, Up9, n0, n1, n2, n3, n4, n5, n6, n7, n8, n9)


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
    ( N (In lowerLimitMin (Up lowerLimitMinX To (Add1 lowerLimitMinPlusX)))
    , N (In (Up upperLimitMinX To upperLimitMinPlusX) upperLimitMax)
    )
    ->
        MorphIndependently
            (N (In narrowMin narrowMax)
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


{-| [`Morph`](Morph#Morph) a digit in a given range

You can require a maximum >= 10.
In that case, the [narrowed](Morph#narrowTo) `N` will also have a maximum >= 10
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


{-| [`Morph`](Morph#Morph) from an `N` with an equatable range `InValue`
to an `InFixed` to operate on it
-}
value :
    MorphIndependently
        (N (InValue narrowMin narrowMax)
         -> Result error_ (N (InFixed narrowMin narrowMax))
        )
        (N (InFixed broadMin broadMax)
         -> N (InValue broadMin broadMax)
        )
value =
    translate N.fromValue N.toValue


{-| [`Morph`](Morph#Morph) from an `N` with a range `InFixed`
to an `InValue` to make it equatable
-}
toValue :
    MorphIndependently
        (N (InFixed narrowMin narrowMax)
         -> Result error_ (N (InValue narrowMin narrowMax))
        )
        (N (InValue broadMin broadMax)
         -> N (InFixed broadMin broadMax)
        )
toValue =
    translate N.toValue N.fromValue
