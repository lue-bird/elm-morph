module Maybe.Morph exposing (value, row)

{-| [`Morph`](Morph#Morph) a `Maybe`


## transform

@docs value, row

-}

import Morph exposing (MorphRow)
import Value.Morph.Internal exposing (MorphValue)


{-| `Maybe` [`MorphValue`](Value-Morph#MorphValue)
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
        |> Value.Morph.Internal.variant ( Just, "Just" ) contentMorph
        |> Value.Morph.Internal.variant ( \() -> Nothing, "Nothing" ) Value.Morph.Internal.unit
        |> Value.Morph.Internal.choiceFinish


{-| [`Morph`](Morph#Morph) an optional value and return it as a `Maybe`

> ℹ️ Equivalent regular expression: `?`

    import Char.Morph exposing (letter)
    import String.Morph as Text

    -- maybe we get `Just` a letter
    "a"
        |> Text.toNarrow
            (Maybe.Morph.row (AToZ.Morph.char |> Morph.one))
    --> Ok (Just 'a')

    -- maybe we get `Nothing`
    "123abc"
        |> Text.toNarrow
            (Maybe.Morph.row (AToZ.Morph.char |> Morph.one))
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
        |> Morph.choiceFinish
