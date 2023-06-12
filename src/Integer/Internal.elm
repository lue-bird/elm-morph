module Integer.Internal exposing (fromInt, toInt)

import ArraySized
import BitArray
import BitArray.Extra
import Integer exposing (Integer)
import Linear exposing (Direction(..))
import N exposing (n0, n1)
import N.Local exposing (n32)
import NaturalAtLeast1.Internal
import Sign exposing (Sign(..))


fromInt : Int -> Integer
fromInt =
    \intBroad ->
        case
            intBroad
                |> abs
                |> N.intToAtLeast n0
                |> BitArray.fromN n32
                |> BitArray.Extra.unpad
                |> ArraySized.hasAtLeast n1
        of
            Err _ ->
                Integer.N0

            Ok absoluteAtLeast1 ->
                Integer.Signed
                    { sign =
                        if intBroad >= 0 then
                            Positive

                        else
                            Negative
                    , absolute =
                        { bitsAfterI =
                            absoluteAtLeast1
                                |> ArraySized.removeMin ( Up, n1 )
                                |> ArraySized.minToNumber
                                |> ArraySized.maxToInfinity
                        }
                    }


toInt : Integer -> Int
toInt =
    \integerNarrow ->
        case integerNarrow of
            Integer.N0 ->
                0

            Integer.Signed signedValue ->
                signedValue.absolute
                    |> NaturalAtLeast1.Internal.toN
                    |> N.toInt
                    |> signPrependToNumber signedValue.sign


{-|

  - `Negative` means negate
  - `Positive` means keep the current sign

-}
signPrependToNumber : Sign -> (number -> number)
signPrependToNumber sign =
    case sign of
        Negative ->
            Basics.negate

        Positive ->
            identity
