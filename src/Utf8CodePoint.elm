module Utf8CodePoint exposing (charBits)

import ArraySized exposing (ArraySized)
import ArraySized.Morph
import Bit exposing (Bit)
import Bit.Morph
import BitArray
import Bitwise
import Linear exposing (Direction(..))
import Morph exposing (MorphOrError, MorphRow)
import N exposing (Exactly, N3, N4, N5, N6, N7, On, n0, n11, n16, n3, n4, n5, n6, n7)
import N.Local exposing (n21)


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


charBits : MorphRow Char Bit
charBits =
    Morph.named "UTF-8 code point"
        (Morph.oneToOne Char.fromCode Char.toCode
            |> Morph.over toInt
            |> Morph.overRow bits
        )


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
        |> Morph.rowTry OneByte
            (Morph.narrow identity
                |> Morph.match (Bit.Morph.only Bit.O |> Morph.one)
                |> Morph.grab identity (ArraySized.Morph.exactly n7 (Morph.keep |> Morph.one))
            )
        |> Morph.rowTry TwoBytes
            (Morph.narrow (\f s -> { first = f, second = s })
                |> Morph.match (Bit.Morph.only Bit.I |> Morph.one)
                |> Morph.match (Bit.Morph.only Bit.I |> Morph.one)
                |> Morph.match (Bit.Morph.only Bit.O |> Morph.one)
                |> Morph.grab .first (ArraySized.Morph.exactly n5 (Morph.keep |> Morph.one))
                |> Morph.grab .second followingByte
            )
        |> Morph.rowTry ThreeBytes
            (Morph.narrow (\f s t -> { first = f, second = s, third = t })
                |> Morph.match (Bit.Morph.only Bit.I |> Morph.one)
                |> Morph.match (Bit.Morph.only Bit.I |> Morph.one)
                |> Morph.match (Bit.Morph.only Bit.I |> Morph.one)
                |> Morph.match (Bit.Morph.only Bit.O |> Morph.one)
                |> Morph.grab .first (ArraySized.Morph.exactly n4 (Morph.keep |> Morph.one))
                |> Morph.grab .second followingByte
                |> Morph.grab .third followingByte
            )
        |> Morph.rowTry FourBytes
            (Morph.narrow (\fi s t fo -> { first = fi, second = s, third = t, fourth = fo })
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
    Morph.narrow identity
        |> Morph.match (Bit.Morph.only Bit.I |> Morph.one)
        |> Morph.match (Bit.Morph.only Bit.O |> Morph.one)
        |> Morph.grab identity (ArraySized.Morph.exactly n6 (Morph.keep |> Morph.one))


toInt : MorphOrError Int Utf8CodePoint error_
toInt =
    -- Granted this is far from elegant
    Morph.oneToOne
        (\utf8CodePointChoice ->
            case utf8CodePointChoice of
                OneByte oneByte ->
                    oneByte |> BitArray.toN |> N.toInt

                TwoBytes bytes ->
                    (bytes.first |> BitArray.toN |> N.toInt |> Bitwise.shiftLeftBy 6)
                        + (bytes.second |> BitArray.toN |> N.toInt)

                ThreeBytes bytes ->
                    (bytes.first |> BitArray.toN |> N.toInt |> Bitwise.shiftLeftBy 12)
                        + (bytes.second |> BitArray.toN |> N.toInt |> Bitwise.shiftLeftBy 6)
                        + (bytes.third |> BitArray.toN |> N.toInt)

                FourBytes bytes ->
                    (bytes.first |> BitArray.toN |> N.toInt |> Bitwise.shiftLeftBy 18)
                        + (bytes.second |> BitArray.toN |> N.toInt |> Bitwise.shiftLeftBy 12)
                        + (bytes.third |> BitArray.toN |> N.toInt |> Bitwise.shiftLeftBy 6)
                        + (bytes.fourth |> BitArray.toN |> N.toInt)
        )
        (\codeChoice ->
            if codeChoice <= 0x7F then
                OneByte (codeChoice |> N.intToAtLeast n0 |> BitArray.fromN n7)

            else if codeChoice <= 0x07FF then
                let
                    bytes =
                        codeChoice - 0x80 |> N.intToAtLeast n0 |> BitArray.fromN n11
                in
                TwoBytes
                    { first = bytes |> ArraySized.toSize Up n5 (\_ -> Bit.O)
                    , second = bytes |> ArraySized.toSize Down n6 (\_ -> Bit.O)
                    }

            else if codeChoice <= 0xFFFF then
                let
                    bytes =
                        codeChoice - 0x0800 |> N.intToAtLeast n0 |> BitArray.fromN n16
                in
                ThreeBytes
                    { first = bytes |> ArraySized.toSize Up n4 (\_ -> Bit.O)
                    , second = bytes |> ArraySized.drop Up n4 |> ArraySized.toSize Down n6 (\_ -> Bit.O)
                    , third = bytes |> ArraySized.toSize Down n6 (\_ -> Bit.O)
                    }

            else
                -- codeChoice should be <= 0x10FFFF
                let
                    bytes =
                        codeChoice - 0x00010000 |> N.intToAtLeast n0 |> BitArray.fromN n21
                in
                FourBytes
                    { first = bytes |> ArraySized.toSize Up n3 (\_ -> Bit.O)
                    , second = bytes |> ArraySized.drop Up n3 |> ArraySized.toSize Down n6 (\_ -> Bit.O)
                    , third = bytes |> ArraySized.drop Up (n3 |> N.add n6) |> ArraySized.toSize Down n6 (\_ -> Bit.O)
                    , fourth = bytes |> ArraySized.toSize Down n6 (\_ -> Bit.O)
                    }
        )
