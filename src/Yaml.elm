module Yaml exposing (Any, AtomAny, ComposedAny)

{-| TODO [YAML](https://en.wikipedia.org/wiki/YAML) [`Morph`](#Morph)

  - previous art impl <https://github.com/MaybeJustJames/yaml/tree/master/src/Yaml>
      - fork <https://github.com/myrho/yaml/tree/master/src/Yaml>

-}

import ArraySized exposing (ArraySized)
import Decimal exposing (Decimal)
import Morph exposing (MorphRow, translate)
import N exposing (Exactly, Min, N, N0, On, n0, n1)
import String.Morph
import Value exposing (AtomOrComposed(..))


type alias Any =
    AtomOrComposed AtomAny ComposedAny


type alias AtomAny =
    Atomy String Int Float


type Atomy stringy floaty inty
    = Stringy stringy
    | Floaty floaty
    | Inty inty


type ComposedAny
    = YmlAny (ArraySized (Min (On N0)) Any)


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
