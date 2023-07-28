module Maybe.Morph exposing (value, toJust, row)

{-| [`Morph`](Morph#Morph) a `Maybe`

@docs value, toJust, row

-}

import Morph exposing (MorphIndependently, MorphRow)
import Value.Morph.Internal exposing (MorphValue)


{-| `Just content` succeeds with the `content`, `Nothing` fails.

    import Morph
    import String.Morph

    Just "Hi"
        |> Morph.toNarrow
            (String.Morph.only "Hi"
                |> Morph.over Maybe.Morph.toJust
            )
    --> Ok ()

-}
toJust :
    MorphIndependently
        (Maybe narrowContent -> Result Morph.Error narrowContent)
        (broadContent -> Maybe broadContent)
toJust =
    Morph.custom "just"
        { toBroad = Just
        , toNarrow =
            \maybe ->
                case maybe of
                    Nothing ->
                        "nothing" |> Err

                    Just content ->
                        content |> Ok
        }


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


{-| Try to [`Morph`](Morph#Morph) a value and return it as a `Just`.
If something fails, go back to where you started and return `Nothing`

> ℹ️ Equivalent regular expression: `?`

    import AToZ
    import AToZ.Morph
    import List.Morph
    import Morph

    -- maybe we get `Just` a letter
    "a"
        |> Morph.toNarrow
            (Maybe.Morph.row (AToZ.Morph.lowerChar |> Morph.one)
                |> Morph.rowFinish
                |> Morph.over List.Morph.string
            )
    --> Ok (Just AToZ.A)

    -- maybe we get `Nothing`
    ""
        |> Morph.toNarrow
            (Maybe.Morph.row (AToZ.Morph.char |> Morph.one)
                |> Morph.rowFinish
                |> Morph.over List.Morph.string
            )
    --> Ok Nothing

-}
row :
    MorphRow contentNarrow broadElement
    -> MorphRow (Maybe contentNarrow) broadElement
row contentMorphRow =
    Morph.choice
        (\justVariant nothingVariant maybeNarrow ->
            case maybeNarrow of
                Nothing ->
                    nothingVariant ()

                Just justValue ->
                    justVariant justValue
        )
        |> Morph.rowTry Just contentMorphRow
        |> Morph.rowTry (\() -> Nothing)
            (Morph.named "nothing" (Morph.succeed ()))
        |> Morph.choiceFinish
