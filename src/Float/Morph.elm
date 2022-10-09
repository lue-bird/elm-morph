module Float.Morph exposing (value)

import Morph
import Value exposing (MorphValue)
import Value.PackageInternal


{-| `Float` [`Morph`](#Morph)
-}
value : MorphValue Float
value =
    Value.PackageInternal.float
