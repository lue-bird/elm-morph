module Maybe.Morph exposing (row, value)

{-| [`Morph`](Morph#Morph) a `Maybe`


## transform

@docs row, value

-}

import Morph exposing (MorphRow)
import Value


{-| `Maybe` [`Value.Morph`](Value#Morph)
-}
value : Value.Morph element -> Value.Morph (Maybe element)
value contentMorph =
    Morph.choice
        (\just nothing narrowMaybe ->
            case narrowMaybe of
                Nothing ->
                    nothing ()

                Just content ->
                    content |> just
        )
        |> Value.try ( Just, "Just" ) contentMorph
        |> Value.try ( \() -> Nothing, "Nothing" ) Value.unit
        |> Value.choiceFinish


{-| [`Morph`](Morph#Morph) an optional value and return it as a `Maybe`

> ℹ️ Equivalent regular expression: `?`

    import Char.Morph exposing (letter)
    import String.Morph as Text

    -- maybe we get `Just` a letter
    "a"
        |> Text.narrowTo
            (Maybe.Morph.row (AToZ.char |> Morph.one))
    --> Ok (Just 'a')

    -- maybe we get `Nothing`
    "123abc"
        |> Text.narrowTo
            (Maybe.Morph.row (AToZ.char |> Morph.one))
    --> Ok Nothing

-}
row :
    MorphRow contentNarrow broadElement
    -> MorphRow (Maybe contentNarrow) broadElement
row contentMorphRow =
    Morph.choice
        (\nothingVariant justVariant maybeNarrow ->
            case maybeNarrow of
                Nothing ->
                    nothingVariant ()

                Just justValue ->
                    justVariant justValue
        )
        |> Morph.tryRow (\() -> Nothing) (Morph.succeed ())
        |> Morph.tryRow Just contentMorphRow
        |> Morph.choiceRowFinish
