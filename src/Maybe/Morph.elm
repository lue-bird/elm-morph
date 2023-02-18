module Maybe.Morph exposing (row, value)

{-| [`Morph`](Morph#Morph) a `Maybe`


## transform

@docs row, value

-}

import Choice
import Morph exposing (MorphRow)
import Value


{-| `Maybe` [`Value.Morph`](Value#Morph)
-}
value : Value.Morph element -> Value.Morph (Maybe element)
value contentMorph =
    Choice.between
        (\just nothing narrowMaybe ->
            case narrowMaybe of
                Nothing ->
                    nothing ()

                Just content ->
                    content |> just
        )
        |> Choice.variantValue ( Just, "Just" ) contentMorph
        |> Choice.variantValue ( \() -> Nothing, "Nothing" ) Value.unit
        |> Choice.finishValue


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
    Choice.between
        (\nothingVariant justVariant maybeNarrow ->
            case maybeNarrow of
                Nothing ->
                    nothingVariant ()

                Just justValue ->
                    justVariant justValue
        )
        |> Choice.tryRow (\() -> Nothing) (Morph.succeed ())
        |> Choice.tryRow Just contentMorphRow
        |> Choice.finishRow
