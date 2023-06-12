module BitArray.Extra exposing (add, reverseEndian, unpad)

import ArraySized exposing (ArraySized)
import Bit exposing (Bit)
import BitArray
import Emptiable
import Linear exposing (Direction(..))
import N exposing (In, N, To, Up, Up0, n8)


{-| Remove `Bit.O` padding on the left until the first `Bit.I`.
-}
unpad :
    ArraySized Bit (In min_ max)
    -> ArraySized Bit (In (Up0 minX_) max)
unpad =
    \arraySized ->
        arraySized
            |> ArraySized.mapFoldFrom
                { foundI = False }
                Linear.Up
                (\state ->
                    if state.folded.foundI then
                        { folded = { foundI = True }, element = state.element |> Emptiable.filled }

                    else
                        case state.element of
                            Bit.I ->
                                { folded = { foundI = True }, element = state.element |> Emptiable.filled }

                            Bit.O ->
                                { folded = { foundI = False }, element = Emptiable.empty }
                )
            |> .mapped
            |> ArraySized.minToOn
            |> ArraySized.fills


{-| Addition that works for natural and 2's complement numbers
-}
add :
    ArraySized Bit (In (Up minX To minPlusX) max)
    ->
        (ArraySized Bit (In (Up minX To minPlusX) max)
         ->
            { inRange : ArraySized Bit (In (Up minX To minPlusX) max)
            , overflow : Bit
            }
        )
add toAdd =
    \bits ->
        let
            lengthMaximum : N (In (Up minX To minPlusX) max)
            lengthMaximum =
                N.greater
                    (bits |> ArraySized.length)
                    (toAdd |> ArraySized.length)

            addResult =
                bits
                    |> ArraySized.toSize Down lengthMaximum (\_ -> Bit.O)
                    |> ArraySized.and
                        (toAdd
                            |> ArraySized.toSize Down lengthMaximum (\_ -> Bit.O)
                        )
                    |> ArraySized.mapFoldFrom Bit.O
                        Down
                        (\step ->
                            case ( step.element, step.folded ) of
                                ( ( Bit.O, Bit.O ), overflowSoFar ) ->
                                    { element = overflowSoFar, folded = Bit.O }

                                ( ( Bit.I, Bit.I ), overflowSoFar ) ->
                                    { element = overflowSoFar, folded = Bit.I }

                                ( ( Bit.I, Bit.O ), Bit.O ) ->
                                    { element = Bit.I, folded = Bit.O }

                                ( ( Bit.O, Bit.I ), Bit.O ) ->
                                    { element = Bit.I, folded = Bit.O }

                                ( ( Bit.I, Bit.O ), Bit.I ) ->
                                    { element = Bit.O, folded = Bit.I }

                                ( ( Bit.O, Bit.I ), Bit.I ) ->
                                    { element = Bit.O, folded = Bit.I }
                        )
        in
        { inRange = addResult.mapped, overflow = addResult.folded }


{-| Big endianness ←→ little endianness.
See [`Bytes.Endianness`](https://dark.elm.dmy.fr/packages/elm/bytes/latest/Bytes#Endianness)
-}
reverseEndian :
    ArraySized Bit (In (Up minX To minPlusX) (Up maxX To maxPlusX))
    -> ArraySized Bit (In (Up minX To minPlusX) (Up maxX To maxPlusX))
reverseEndian =
    \bitArray ->
        bitArray
            |> BitArray.toChunksOf n8
            |> ArraySized.reverse
            |> ArraySized.foldFrom
                (ArraySized.empty |> ArraySized.maxToInfinity)
                Up
                (\next soFar ->
                    soFar
                        |> ArraySized.attachMin Up (next |> ArraySized.minTo0)
                        |> ArraySized.minTo0
                )
            -- unpad
            |> ArraySized.toSize Down (bitArray |> ArraySized.length) (\_ -> Bit.O)
