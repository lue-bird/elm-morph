module Integer.Internal exposing (fromInt, toInt)

import ArraySized
import Bits
import Integer exposing (Integer(..))
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
                |> Bits.fromN
                |> Bits.unpad
                |> ArraySized.maxTo n32
                |> ArraySized.hasAtLeast n1
        of
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
                                |> ArraySized.removeMin ( Up, n0 )
                                |> ArraySized.minToNumber
                                |> ArraySized.maxToInfinity
                        }
                    }

            Err _ ->
                Integer.N0


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
