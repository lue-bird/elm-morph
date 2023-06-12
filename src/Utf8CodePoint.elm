module Utf8CodePoint exposing (Utf8CodePoint(..), bits)

import ArraySized exposing (ArraySized)
import ArraySized.Morph
import Bit exposing (Bit)
import Bit.Morph
import Morph exposing (MorphRow)
import N exposing (Exactly, N3, N4, N5, N6, N7, On, n3, n4, n5, n6, n7)


type Utf8CodePoint
    = OneByte (ArraySized Bit (Exactly (On N7)))
    | TwoBytes
        { first : ArraySized Bit (Exactly (On N5))
        , second : ArraySized Bit (Exactly (On N6))
        }
    | ThreeBytes
        { first : ArraySized Bit (Exactly (On N4))
        , second : ArraySized Bit (Exactly (On N6))
        , third : ArraySized Bit (Exactly (On N6))
        }
    | FourBytes
        { first : ArraySized Bit (Exactly (On N3))
        , second : ArraySized Bit (Exactly (On N6))
        , third : ArraySized Bit (Exactly (On N6))
        , fourth : ArraySized Bit (Exactly (On N6))
        }


{-| Should match <https://en.wikipedia.org/wiki/UTF-8#Encoding>
-}
bits : MorphRow Utf8CodePoint Bit
bits =
    Morph.choice
        (\oneByte twoBytes threeBytes fourBytes uft8CodePointChoice ->
            case uft8CodePointChoice of
                OneByte oneByteValue ->
                    oneByte oneByteValue

                TwoBytes twoBytesValue ->
                    twoBytes twoBytesValue

                ThreeBytes threeBytesValue ->
                    threeBytes threeBytesValue

                FourBytes fourBytesValue ->
                    fourBytes fourBytesValue
        )
        |> Morph.tryRow OneByte
            (Morph.succeed identity
                |> Morph.match (Bit.Morph.only Bit.O |> Morph.one)
                |> Morph.grab identity (ArraySized.Morph.exactly n7 (Morph.keep |> Morph.one))
            )
        |> Morph.tryRow TwoBytes
            (Morph.succeed (\f s -> { first = f, second = s })
                |> Morph.match (Bit.Morph.only Bit.I |> Morph.one)
                |> Morph.match (Bit.Morph.only Bit.I |> Morph.one)
                |> Morph.match (Bit.Morph.only Bit.O |> Morph.one)
                |> Morph.grab .first (ArraySized.Morph.exactly n5 (Morph.keep |> Morph.one))
                |> Morph.grab .second followingByte
            )
        |> Morph.tryRow ThreeBytes
            (Morph.succeed (\f s t -> { first = f, second = s, third = t })
                |> Morph.match (Bit.Morph.only Bit.I |> Morph.one)
                |> Morph.match (Bit.Morph.only Bit.I |> Morph.one)
                |> Morph.match (Bit.Morph.only Bit.I |> Morph.one)
                |> Morph.match (Bit.Morph.only Bit.O |> Morph.one)
                |> Morph.grab .first (ArraySized.Morph.exactly n4 (Morph.keep |> Morph.one))
                |> Morph.grab .second followingByte
                |> Morph.grab .third followingByte
            )
        |> Morph.tryRow FourBytes
            (Morph.succeed (\fi s t fo -> { first = fi, second = s, third = t, fourth = fo })
                |> Morph.match (Bit.Morph.only Bit.I |> Morph.one)
                |> Morph.match (Bit.Morph.only Bit.I |> Morph.one)
                |> Morph.match (Bit.Morph.only Bit.I |> Morph.one)
                |> Morph.match (Bit.Morph.only Bit.I |> Morph.one)
                |> Morph.match (Bit.Morph.only Bit.O |> Morph.one)
                |> Morph.grab .first (ArraySized.Morph.exactly n3 (Morph.keep |> Morph.one))
                |> Morph.grab .second followingByte
                |> Morph.grab .third followingByte
                |> Morph.grab .fourth followingByte
            )
        |> Morph.choiceFinish


followingByte : MorphRow (ArraySized Bit (Exactly (On N6))) Bit
followingByte =
    Morph.succeed identity
        |> Morph.match (Bit.Morph.only Bit.I |> Morph.one)
        |> Morph.match (Bit.Morph.only Bit.O |> Morph.one)
        |> Morph.grab identity (ArraySized.Morph.exactly n6 (Morph.keep |> Morph.one))
