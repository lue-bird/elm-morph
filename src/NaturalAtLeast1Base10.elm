module NaturalAtLeast1Base10 exposing
    ( NaturalAtLeast1Base10
    , fromIntPositive
    , fromBase2, toBase2
    , chars
    )

{-| **Should not be exposed**

@docs NaturalAtLeast1Base10


## int positive

@docs fromIntPositive


## base 2

@docs fromBase2, toBase2


## morph

@docs chars

-}

import Array exposing (Array)
import Array.Linear
import ArraySized
import ArraySized.Morph
import Bit exposing (Bit)
import Linear
import List.Linear
import Morph exposing (MorphRow, grab, one)
import N exposing (Add1, In, N, N0, N1, N9, On, Up0, Up9, n0, n1, n10, n2, n9)
import N.Morph
import Number exposing (NaturalAtLeast1)


type alias NaturalAtLeast1Base10 =
    { first : N (In N1 N9)
    , afterFirst : Array (N (In N0 N9))
    }


digitToNumber :
    ( N (In (On newMin) (N.Up newMinMaxToMin_ N.To min))
    , N (In (On newMaxMin) (On newMax))
    )
    -> N (In (On min) (N.Up maxToNewMaxMin_ N.To newMaxMin))
    -> N (In newMin newMax)
digitToNumber ( low, high ) =
    \digit1To9 ->
        digit1To9 |> N.minTo low |> N.maxTo high |> N.inToNumber


digitToUp :
    ( N (In newMin (N.Up newMinMaxToMin_ N.To min)), N (In (On max) newMax) )
    -> (N (In min max) -> N (In newMin newMax))
digitToUp ( low, high ) =
    \digit1To9 ->
        digit1To9 |> N.inToOn |> N.minTo low |> N.maxTo high


toBase2 : NaturalAtLeast1Base10 -> NaturalAtLeast1
toBase2 =
    \naturalAtLeast1Base10 ->
        let
            base2Digits =
                (naturalAtLeast1Base10.first |> N.minToOn |> N.minTo n0 |> N.minToNumber)
                    :: (naturalAtLeast1Base10.afterFirst |> Array.toList)
                    |> digitsToBase2
        in
        base2Digits
            |> base2DigitsUnpad
            -- not possible because the base10 number is >= 1
            |> Maybe.withDefault
                -- 1
                { bitsAfterI =
                    ArraySized.empty |> ArraySized.maxToInfinity |> ArraySized.minToNumber
                }


digitsToBase2 : List (N (In N0 N9)) -> List Bit
digitsToBase2 =
    \digits ->
        let
            digitsDivisionBy2 : { divided : List (N (In N0 N9)), remainder : N (In (On N0) (On N1)) }
            digitsDivisionBy2 =
                digits |> digitsDivideBy2
        in
        (digitsDivisionBy2.remainder |> Bit.fromN)
            :: (digitsDivisionBy2.divided |> digitsToBase2)


digitsDivideBy2 :
    List (N (In N0 N9))
    ->
        { divided : List (N (In N0 N9))
        , remainder : N (In (On N0) (On N1))
        }
digitsDivideBy2 =
    \digits ->
        let
            divisionResult =
                digits
                    |> List.Linear.mapFoldFrom (n0 |> N.maxTo n1)
                        Linear.Up
                        (\step ->
                            let
                                intermediate =
                                    step.folded |> N.multiplyBy n10 |> N.addMin (step.element |> digitToUp ( n0, n9 ))
                            in
                            { element =
                                intermediate
                                    |> N.divideBy n2
                                    -- we can safely clamp to 0..9 because 19/2 is still below 10
                                    |> N.toIn ( n0, n9 )
                                    |> digitToNumber ( n0, n9 )
                            , folded = intermediate |> N.remainderBy n2
                            }
                        )
        in
        { remainder = divisionResult.folded
        , divided = divisionResult.mapped
        }


base2DigitsUnpad : List Bit -> Maybe NaturalAtLeast1
base2DigitsUnpad =
    \base2Digits ->
        case base2Digits of
            [] ->
                Nothing

            first :: afterFirst ->
                case first of
                    Bit.O ->
                        afterFirst |> base2DigitsUnpad

                    Bit.I ->
                        Just { bitsAfterI = afterFirst |> ArraySized.fromList |> ArraySized.minToNumber }


add :
    NaturalAtLeast1Base10
    ->
        (NaturalAtLeast1Base10
         -> NaturalAtLeast1Base10
        )
add toAdd =
    \naturalAtLeast1Base10 ->
        let
            firstDigitSum =
                digitsAfterFirstSum.overflow
                    |> N.add (naturalAtLeast1Base10.first |> digitToUp ( n1, n9 ))
                    |> N.add (toAdd.first |> digitToUp ( n1, n9 ))

            digitsAfterFirstSum :
                { inRange : Array (N (In N0 N9))
                , overflow : N (In (On N0) (On N1))
                }
            digitsAfterFirstSum =
                naturalAtLeast1Base10.afterFirst |> addDigits toAdd.afterFirst
        in
        case firstDigitSum |> N.isAtLeast n10 of
            Ok firstDigitSumAtLeast10 ->
                { first = n1 |> N.minTo n1 |> N.maxTo n9 |> N.inToNumber
                , afterFirst =
                    Array.Linear.insert ( Linear.Up, 0 )
                        (\() -> firstDigitSumAtLeast10 |> N.remainderBy n10 |> N.inToNumber)
                        digitsAfterFirstSum.inRange
                }

            Err firstDigitSumAtMost9 ->
                { first = firstDigitSumAtMost9 |> N.minTo n1 |> N.inToNumber
                , afterFirst = digitsAfterFirstSum.inRange
                }


addDigits :
    Array (N (In N0 N9))
    ->
        (Array (N (In N0 N9))
         ->
            { inRange : Array (N (In N0 N9))
            , overflow : N (In (On N0) (On N1))
            }
        )
addDigits toAdd =
    \digits ->
        let
            lengthMaximum : Int
            lengthMaximum =
                Basics.max
                    (digits |> Array.length)
                    (toAdd |> Array.length)

            addResult : { mapped : Array (N (In N0 N9)), folded : N (In (On N0) (On N1)) }
            addResult =
                Array.initialize lengthMaximum identity
                    |> Array.Linear.mapFoldFrom (n0 |> N.maxTo n1)
                        Linear.Down
                        (\step ->
                            let
                                digit : Array (N (In N0 N9)) -> N (In (Up0 minX_) (Up9 maxX_))
                                digit =
                                    \digitArray ->
                                        case digitArray |> Array.Linear.element ( Linear.Up, step.element ) of
                                            Nothing ->
                                                n0 |> N.maxTo n9

                                            Just digitFound ->
                                                digitFound |> digitToUp ( n0, n9 )
                            in
                            case step.folded |> N.add (digits |> digit) |> N.add (toAdd |> digit) |> N.isAtLeast n10 of
                                Ok digitSumAtLeast10 ->
                                    { element = digitSumAtLeast10 |> N.remainderBy n10 |> N.inToNumber
                                    , folded = n1 |> N.minTo n0
                                    }

                                Err digitSumAtMost9 ->
                                    { element = digitSumAtMost9 |> N.inToNumber, folded = n0 |> N.maxTo n1 }
                        )
        in
        { inRange = addResult.mapped
        , overflow = addResult.folded
        }


fromBase2 : NaturalAtLeast1 -> NaturalAtLeast1Base10
fromBase2 =
    \digits ->
        digits.bitsAfterI
            |> ArraySized.foldFrom
                (fromDigit n1)
                Linear.Up
                (\bit soFar ->
                    case bit of
                        Bit.O ->
                            soFar |> multiplyBy2

                        Bit.I ->
                            soFar |> multiplyBy2 |> add (fromDigit n1)
                )


fromDigit : N (In (On (Add1 maxX_)) (N.Up maxTo9_ N.To N9)) -> { first : N (In N1 N9), afterFirst : Array (N (In N0 N9)) }
fromDigit digit =
    { first = digit |> digitToNumber ( n1, n9 )
    , afterFirst = Array.empty
    }


multiplyBy2 : NaturalAtLeast1Base10 -> NaturalAtLeast1Base10
multiplyBy2 =
    \naturalAtLeast1Base10 ->
        naturalAtLeast1Base10 |> add naturalAtLeast1Base10


fromIntPositive : Int -> NaturalAtLeast1Base10
fromIntPositive =
    \int ->
        let
            highest10Exponent : Int
            highest10Exponent =
                logBase 10 (int |> Basics.toFloat) |> floor
        in
        { first = int |> digitFor10Exponent highest10Exponent |> N.toIn ( n1, n9 ) |> N.inToNumber
        , afterFirst =
            Array.initialize highest10Exponent identity
                |> Array.map
                    (\n10Exponent ->
                        int |> digitFor10Exponent n10Exponent |> N.inToNumber
                    )
        }


digitFor10Exponent : Int -> (Int -> N (In (Up0 minX_) (Up9 maxX_)))
digitFor10Exponent n10Exponent =
    \int ->
        (int // (10 ^ n10Exponent))
            |> N.intModBy n10


toInt : NaturalAtLeast1Base10 -> Int
toInt =
    \naturalAtLeast1Base10 ->
        Array.Linear.insert ( Linear.Up, 0 )
            (\() -> naturalAtLeast1Base10.first |> N.inToOn |> N.minTo n0 |> N.inToNumber)
            naturalAtLeast1Base10.afterFirst
            |> arrayToIntPositive


arrayToIntPositive : Array (N range_) -> Int
arrayToIntPositive =
    \digits ->
        let
            lastIndex =
                (digits |> Array.length) - 1
        in
        digits
            |> Array.indexedMap
                (\index digit ->
                    (digit |> N.toInt) * (10 ^ (lastIndex - index))
                )
            |> Array.foldl (+) 0


chars : MorphRow NaturalAtLeast1Base10 Char
chars =
    Morph.to "natural >= 1"
        (Morph.succeed (\first afterFirst -> { first = first, afterFirst = afterFirst })
            |> grab .first
                (N.Morph.inOn
                    |> Morph.over (N.Morph.in_ ( n1, n9 ))
                    |> Morph.over N.Morph.char
                    |> one
                )
            |> grab .afterFirst
                (ArraySized.Morph.toArray
                    |> Morph.overRow
                        (ArraySized.Morph.atLeast n0
                            (N.Morph.inOn
                                |> Morph.over (N.Morph.in_ ( n0, n9 ))
                                |> Morph.over N.Morph.char
                                |> one
                            )
                        )
                )
        )
