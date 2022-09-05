module Bit exposing
    ( Bit(..)
    , arrayToBytes
    , toN
    )

{-| bit and bit sequence

@docs Bit


## alter

@docs arrayToBytes


## transform

@docs toN

-}

import ArraySized exposing (ArraySized)
import Linear exposing (DirectionLinear(..))
import List.Linear
import N exposing (Add1, Add8, In, Min, N, N0able, To, Up, n0, n1, n2, n8)
import Possibly exposing (Possibly)


{-| One of 2 states.

  - 1, on: `I`
  - 0, off: `O`

-}
type Bit
    = I
    | O


{-| Convert `O` to zero, `I` to one. `N (In N0 (N1Plus a))` means that the result will be between 0 & 1.

    toInt bit =
        bit |> Bit.toN |> val

`val` is from [`Typed`](https://package.elm-lang.org/packages/lue-bird/elm-typed-value/latest/Typed).

-}
toN : Bit -> N (In (Up minX To minX) (Up maxX To (Add1 maxX)))
toN =
    \bit ->
        case bit of
            O ->
                n0 |> N.max n1

            I ->
                n1 |> N.min n0


{-| Type for the [exact natural number](#N0able) `53`
-}
type alias N53 =
    N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able Never Possibly) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never


{-| Group an `ArraySized` of `Bit`s into `ArraySized`s of size 8.

    ArraySized.l1 O |> Bit.toBytes
    --> ArraySized.l1 (ArraySized.l8 O O O O O O O O)

    ArraySized.l3 I I I
        |> InArraySized.append n8 (ArraySized.l8 O I I I O I O O)
        |> InArraySized.append n8 (ArraySized.l8 O I I I O I O O)
        |> InArraySized.append n8 (ArraySized.l8 O I I I O I O O)
        |> Bit.toBytes
    --> (ArraySized.l8 O O O O O I I I
    -->     |> ArraySized.push (ArraySized.l8 O I I I O I O O)
    -->     |> ArraySized.push (ArraySized.l8 O I I I O I O O)
    -->     |> ArraySized.push (ArraySized.l8 O I I I O I O O)
    --> )

`O`s at the start are kept.

-}
arrayToBytes :
    ArraySized (In min_ (Up maxX To maxPlusX)) Bit
    ->
        ArraySized
            (In (Up minX To minX) (Up maxX To (Add1 maxPlusX)))
            (ArraySized
                (In
                    (Up chunkMinX To (Add8 chunkMinX))
                    (Up chunkMaxX To (Add8 chunkMaxX))
                )
                Bit
            )
arrayToBytes =
    \arraySized ->
        let
            chunked =
                arraySized
                    |> ArraySized.toChunksOf Down n8
        in
        chunked.chunks
            |> ArraySized.map (ArraySized.min n8)
            |> (case chunked.remainder |> ArraySized.hasAtLeast n1 of
                    Ok remainder ->
                        ArraySized.push
                            (remainder
                                |> ArraySized.glue Down
                                    (ArraySized.repeat O n8)
                                |> ArraySized.take ( Down, n8, { atLeast = n8 } )
                            )
                            >> ArraySized.minDown n1

                    Err _ ->
                        ArraySized.maxUp n1
               )
