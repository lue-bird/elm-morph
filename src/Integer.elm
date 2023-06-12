module Integer exposing
    ( Integer(..), Signed
    , absolute, negate
    )

{-| Arbitrary-precision whole number

@docs Integer, Signed


## alter

@docs absolute, negate

-}

import Natural exposing (Natural)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Sign exposing (Sign)


{-| Arbitrary-precision `Int`, constructable from bits
-}
type Integer
    = N0
    | Signed Signed


{-| Arbitrary-precision signed [`Integer`](#Integer), constructable from bits
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
