module Tree.Morph exposing (value)

{-| [`Morph`](Morph#Morph) for a [`Tree` from `zwilias/elm-rosetree`](https://dark.elm.dmy.fr/packages/zwilias/elm-rosetree/latest/)

@docs value

-}

import List.Morph
import Morph
import Tree exposing (Tree)
import Value.Morph.Internal exposing (MorphValue)


{-| [`MorphValue`](Value-Morph#MorphValue) from a [`Tree` from `zwilias/elm-rosetree`](https://dark.elm.dmy.fr/packages/zwilias/elm-rosetree/latest/)
-}
value : MorphValue a -> MorphValue (Tree a)
value elementValueMorph =
    Morph.recursive "tree"
        (\step ->
            Value.Morph.Internal.group Tree.tree
                |> Value.Morph.Internal.part ( Tree.label, "label" ) elementValueMorph
                |> Value.Morph.Internal.part ( Tree.children, "children" ) (List.Morph.value step)
                |> Value.Morph.Internal.groupFinish
        )
