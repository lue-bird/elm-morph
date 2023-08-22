module Bit.Morph.Internal exposing (char, only)

import Bit exposing (Bit)
import Char.Morph.Internal
import Morph exposing (Morph)


only : Bit -> Morph () Bit
only broadConstant =
    Morph.only
        (\bit -> bit |> Morph.toBroad char |> String.fromChar)
        broadConstant


char : Morph Bit Char
char =
    Morph.choice
        (\o i bit ->
            case bit of
                Bit.O ->
                    o ()

                Bit.I ->
                    i ()
        )
        |> Morph.try (\() -> Bit.O) (Char.Morph.Internal.only '0')
        |> Morph.try (\() -> Bit.I) (Char.Morph.Internal.only '1')
        |> Morph.choiceFinish
