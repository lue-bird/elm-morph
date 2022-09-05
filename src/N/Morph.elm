module N.Morph exposing
    ( intIn, charIn
    , fromValue, toValue
    )

{-| [`Morph`](Morph#Morph) to `N`

@docs intIn, charIn


## equatable value

@docs fromValue, toValue

-}

import Morph exposing (Morph, MorphIndependently, translate)
import N exposing (Fixed, In, InFixed, InValue, N, To, Up)


intIn :
    ( N (In (Up minX To minPlusX) (Up lowerLimitMaxToUpperLimitMin_ To minMax))
    , N (In (Fixed minMax) (Up upperLimitMaxX To maxPlusX))
    )
    ->
        -- TODO: morph independently
        Morph
            (N
                (In
                    (Up minX To minPlusX)
                    (Up upperLimitMaxX To maxPlusX)
                )
            )
            Int
intIn ( min, max ) =
    Morph.value (( min, max ) |> rangeDescription)
        { narrow =
            N.intIsIn ( min, max )
                >> Result.mapError
                    (\error ->
                        case error of
                            N.Below _ ->
                                [ "below ", min |> N.toInt |> String.fromInt ] |> String.concat

                            N.Above _ ->
                                [ "above ", max |> N.toInt |> String.fromInt ] |> String.concat
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


charIn :
    ( N (In min (Up lowerLimitMaxToUpperLimitMin_ To minMax))
    , N (In (Fixed minMax) (Up upperLimitMaxX To upperLimitMaxPlusX))
    )
    -> Morph (N (In min (Up upperLimitMaxX To upperLimitMaxPlusX))) Char
charIn ( min, max ) =
    Morph.value (( min, max ) |> rangeDescription)
        { narrow =
            Debug.todo ""
        , broaden = Debug.todo ""
        }


fromValue :
    MorphIndependently
        (N (InValue narrowMin narrowMax)
         -> Result error_ (N (InFixed narrowMin narrowMax))
        )
        (N (InFixed broadMin broadMax)
         -> N (InValue broadMin broadMax)
        )
fromValue =
    translate N.fromValue N.toValue


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
