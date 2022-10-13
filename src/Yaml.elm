module Yaml exposing (Any, LiteralAny, StructureAny)

{-| TODO [YAML](https://en.wikipedia.org/wiki/YAML) [`Morph`](#Morph)

  - previous art impl <https://github.com/MaybeJustJames/yaml/tree/master/src/Yaml>
      - fork <https://github.com/myrho/yaml/tree/master/src/Yaml>

-}

import ArraySized exposing (ArraySized)
import Decimal exposing (Decimal)
import Morph exposing (MorphRow, translate)
import N exposing (Exactly, Fixed, Min, N, N0, n0, n1)
import String.Morph
import Value exposing (LiteralOrStructure(..))


type alias Any =
    LiteralOrStructure LiteralAny StructureAny


type alias LiteralAny =
    Literaly String Int Float


type Literaly stringy floaty inty
    = Stringy stringy
    | Floaty floaty
    | Inty inty


type StructureAny
    = YmlAny (ArraySized (Min (Fixed N0)) Any)


text : MorphRow Char Any
text =
    textIndented n0


textIndented : N (Exactly n) -> MorphRow Char Any
textIndented indentation =
    Morph.succeed YmlAny
        |> grab (\(YmlAny list) -> list)
            (atLeast n1 (Debug.todo ""))
        |> skip
            (translate
                (\_ -> ())
                (\() -> ArraySized.repeat () indentation)
                |> Morph.overRow
                    (exactly indentation (String.Morph.only "    "))
            )
