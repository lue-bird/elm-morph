module Integer exposing
    ( Integer(..), Signed
    , fromInt
    , absolute, negate
    , toInt
    )

{-| Arbitrary-precision whole number

@docs Integer, Signed


## create

@docs fromInt


## alter

@docs absolute, negate


## transform

@docs toInt

-}

import ArraySized
import BitArray
import BitArray.Extra
import Linear exposing (Direction(..))
import N exposing (n0, n1)
import N.Local exposing (n32)
import Natural exposing (Natural)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Sign exposing (Sign)


{-| Arbitrary-precision integer, constructable from bits
-}
type Integer
    = N0
    | Signed Signed


{-| Arbitrary-precision signed [`Integer`](#Integer), constructable from a [`Sign`](Sign#Sign)
and [`Natural.AtLeast1` bits](Natural#AtLeast1)
-}
type alias Signed =
    RecordWithoutConstructorFunction
        { sign : Sign
        , absolute : Natural.AtLeast1
        }


{-| Flip its [`Sign`](Sign#Sign)
-}
negate : Integer -> Integer
negate =
    \integer ->
        case integer of
            N0 ->
                N0

            Signed signed ->
                Signed { signed | sign = signed.sign |> Sign.opposite }


{-| Remove its [`Sign`](Sign#Sign)
-}
absolute : Integer -> Natural
absolute =
    \integer ->
        case integer of
            N0 ->
                Natural.N0

            Signed signed ->
                Natural.AtLeast1 signed.absolute



{-
   add : Integer -> (Integer -> Integer)
   add toAdd =
       \integer ->
           case ( integer, toAdd ) of
               ( Integer.N0, result ) ->
                   result

               ( Integer.Signed integerSigned, Integer.N0 ) ->
                   Integer.Signed integerSigned

               ( Integer.Signed integerSigned, Integer.Signed toAddSigned ) ->
                   integerSigned |> signedAdd toAddSigned


   signedAdd : Integer.Signed -> (Integer.Signed -> Integer)
   signedAdd toAdd =
       \signed ->
           case ( signed.sign, toAdd.sign ) of
               ( Positive, Positive ) ->
                   Integer.Signed { sign = Positive, absolute = signed.absolute |> Natural.AtLeast1.add toAdd.absolute }

               ( Negative, Negative ) ->
                   Integer.Signed { sign = Negative, absolute = signed.absolute |> Natural.AtLeast1.add toAdd.absolute }

               ( Negative, Positive ) ->
                   signed.absolute |> Natural.AtLeast1.subtract toAdd.absolute |> negate

               ( Positive, Negative ) ->
                   signed.absolute |> Natural.AtLeast1.subtract toAdd.absolute
-}


{-| Convert from an `Int`
-}
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
                N0

            Ok absoluteAtLeast1 ->
                Signed
                    { sign =
                        if intBroad >= 0 then
                            Sign.Positive

                        else
                            Sign.Negative
                    , absolute =
                        { bitsAfterI =
                            absoluteAtLeast1
                                |> ArraySized.removeMin ( Up, n1 )
                                |> ArraySized.toList
                        }
                    }


{-| Convert to an `Int`

Keep in mind that this can overflow
since `Int` is fixed in bit size while [`Integer`](Integer#Integer) is not.

-}
toInt : Integer -> Int
toInt =
    \integerNarrow ->
        case integerNarrow of
            N0 ->
                0

            Signed signedValue ->
                signedValue.absolute
                    |> Natural.AtLeast1
                    |> Natural.toN
                    |> N.toInt
                    |> signPrependToNumber signedValue.sign


{-|

  - `Negative` means negate
  - `Positive` means keep the current sign

-}
signPrependToNumber : Sign -> (number -> number)
signPrependToNumber sign =
    case sign of
        Sign.Negative ->
            Basics.negate

        Sign.Positive ->
            identity
