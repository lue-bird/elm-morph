module Integer.Morph.Internal exposing (intAbsoluteTo0To9s, n0To9sToInt)

import Digit.Morph
import Hand exposing (Empty, Hand)
import Morph
import Possibly exposing (Possibly(..))
import Stack exposing (Stacked)


n0To9sToInt :
    Hand (Stacked Digit.Morph.N0To9) Never Empty
    -> Int
n0To9sToInt =
    \integerDigits ->
        integerDigits
            |> Stack.map
                (\_ -> Morph.broaden Digit.Morph.n0To9ToInt)
            |> Stack.reverse
            |> Stack.map
                (\decimal digit ->
                    digit * 10 ^ decimal.index
                )
            |> Stack.sum


intAbsoluteTo0To9s : Int -> Hand (Stacked Digit.Morph.N0To9) never_ Empty
intAbsoluteTo0To9s =
    \intAbsolute ->
        let
            smallestDecimal =
                intAbsolute |> remainderBy 10

            withoutDecimal =
                intAbsolute // 10
        in
        Stack.onTopLay
            (case smallestDecimal |> Morph.narrow Digit.Morph.n0To9ToInt of
                Err _ ->
                    -- remainderBy 10 is never >= 10
                    Digit.Morph.N0

                Ok decimal0To9 ->
                    decimal0To9
            )
            (case withoutDecimal of
                0 ->
                    Hand.empty

                otherDecimalInt ->
                    otherDecimalInt |> intAbsoluteTo0To9s |> Hand.emptyAdapt (\_ -> Possible)
            )
