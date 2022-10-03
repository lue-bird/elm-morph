module Maybe.Morph exposing (value)

{-| [`Morph`](Morph#Morph) a `Maybe`


## transform

@docs value

-}

import Morph
import Value exposing (MorphValue)


{-| `Maybe` [`MorphValue`](Value#MorphValue)
-}
value : MorphValue element -> MorphValue (Maybe element)
value contentMorph =
    Morph.choice
        (\just nothing narrowMaybe ->
            case narrowMaybe of
                Nothing ->
                    nothing ()

                Just content ->
                    content |> just
        )
        |> Value.variant ( Just, "Just" ) contentMorph
        |> Value.variant ( \() -> Nothing, "Nothing" ) unit
        |> Value.choiceFinish
