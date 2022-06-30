module Sign exposing (ofNumber, number)

{-|

@docs ofNumber, number

-}

import Sign.Morph exposing (Sign(..))


number : Sign -> (number -> number)
number signToAdapt =
    case signToAdapt of
        Positive ->
            abs

        Negative ->
            abs >> negate


ofNumber : number -> Maybe { sign : Sign, absolute : number }
ofNumber =
    \numberSigned ->
        case compare numberSigned 0 of
            EQ ->
                Nothing

            LT ->
                { sign = Negative, absolute = numberSigned |> abs }
                    |> Just

            GT ->
                { sign = Positive, absolute = numberSigned }
                    |> Just
