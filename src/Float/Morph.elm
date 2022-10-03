module Float.Morph exposing (value)

import Morph
import Value exposing (MorphValue)
import Value.Unexposed


{-| `Float` [`Morph`](#Morph)
-}
value : MorphValue Float
value =
    Morph.to "Float"
        Value.Unexposed.float
