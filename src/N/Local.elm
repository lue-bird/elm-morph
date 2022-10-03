module N.Local exposing (Add31, Add32, N31, N32, Up31, Up32, n31, n32)

{-| When you need many small-ish numbers or a few medium sized ones.
-}

import N exposing (In, N, N0OrAdd1, To, Up, add, n1, n16, n2, n4, n8)
import Possibly exposing (Possibly)


{-| The [natural number](Nhttps://dark.elm.dmy.fr/packages/lue-bird/bounded-nat/latest/#N0OrAdd1) `31 +` a given `n`
-}
type alias Add31 n =
    N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never n))))))))))))))))))))))))))))))


{-| The [natural number](https://dark.elm.dmy.fr/packages/lue-bird/bounded-nat/latest/N#N0OrAdd1) `31`
-}
type alias N31 =
    N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Possibly Never)))))))))))))))))))))))))))))))


{-| `31` as the difference `Up x To (Add31 x)`
-}
type alias Up31 x =
    Up x To (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never x)))))))))))))))))))))))))))))))


{-| The [`N`](Nhttps://dark.elm.dmy.fr/packages/lue-bird/bounded-nat/latest/#N) `31`
-}
n31 : N (In (Up31 minX_) (Up31 maxX_))
n31 =
    n16 |> add n8 |> add n4 |> add n2 |> add n1


{-| The [natural number](Nhttps://dark.elm.dmy.fr/packages/lue-bird/bounded-nat/latest/#N0OrAdd1) `32 +` a given `n`
-}
type alias Add32 n =
    N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never n)))))))))))))))))))))))))))))))


{-| The [natural number](https://dark.elm.dmy.fr/packages/lue-bird/bounded-nat/latest/N#N0OrAdd1) `32`
-}
type alias N32 =
    N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Possibly Never))))))))))))))))))))))))))))))))


{-| `32` as the difference `Up x To (Add32 x)`
-}
type alias Up32 x =
    Up x To (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never x))))))))))))))))))))))))))))))))


{-| The [`N`](Nhttps://dark.elm.dmy.fr/packages/lue-bird/bounded-nat/latest/#N) `32`
-}
n32 : N (In (Up32 minX_) (Up32 maxX_))
n32 =
    n16 |> add n16
