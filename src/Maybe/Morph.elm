module Maybe.Morph exposing (row, value)

{-| [`Morph`](Morph#Morph) a `Maybe`


## transform

@docs row, value

-}

import Choice
import Group
import Morph exposing (MorphRow)
import Unit
import Value exposing (MorphValue)


{-| `Maybe` [`MorphValue`](Value#MorphValue)
-}
value : MorphValue element -> MorphValue (Maybe element)
value contentMorph =
    Choice.between
        (\just nothing narrowMaybe ->
            case narrowMaybe of
                Nothing ->
                    nothing ()

                Just content ->
                    content |> just
        )
        |> Choice.tryValue ( Just, "Just" ) contentMorph
        |> Choice.tryValue ( \() -> Nothing, "Nothing" ) Unit.value
        |> Choice.finishValue


{-| [`Morph`](Morph#Morph) an optional value and return it as a `Maybe`

> ℹ️ Equivalent regular expression: `?`

    import Char.Morph exposing (letter)
    import String.Morph as Text

    -- maybe we get `Just` a letter
    "a"
        |> Text.narrowWith
            (Maybe.Morph.row (AToZ.Morph.char |> Morph.one))
    --> Ok (Just 'a')

    -- maybe we get `Nothing`
    "123abc"
        |> Text.narrowWith
            (Maybe.Morph.row (AToZ.Morph.char |> Morph.one))
    --> Ok Nothing

-}
row :
    MorphRow broadElement contentNarrow
    -> MorphRow broadElement (Maybe contentNarrow)
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
