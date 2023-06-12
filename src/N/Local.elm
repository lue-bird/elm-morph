module N.Local exposing (Add18, Add21, Add24, Add31, Add32, N18, N21, N24, N31, N32, Up18, Up21, Up24, Up31, Up32, n18, n21, n24, n31, n32)

{-| When you need many small-ish numbers or a few medium sized ones.
-}

import N exposing (In, N, N0OrAdd1, To, Up, add, n1, n16, n2, n4, n8)
import Possibly exposing (Possibly)


{-| The [natural number](https://dark.elm.dmy.fr/packages/lue-bird/bounded-nat/latest/N#N0OrAdd1) `21 +` a given `n`
-}
type alias Add21 n =
    N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never n))))))))))))))))))))


{-| The [natural number](https://dark.elm.dmy.fr/packages/lue-bird/bounded-nat/latest/N#N0OrAdd1) `21`
-}
type alias N21 =
    N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Possibly Never)))))))))))))))))))))


{-| `21` as the difference `Up x To (Add21 x)`
-}
type alias Up21 x =
    Up x To (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never x)))))))))))))))))))))


{-| The [`N`](https://dark.elm.dmy.fr/packages/lue-bird/bounded-nat/latest/N#N) `21`
-}
n21 : N (In (Up21 minX_) (Up21 maxX_))
n21 =
    n16 |> add n4 |> add n1


{-| The [natural number](https://dark.elm.dmy.fr/packages/lue-bird/bounded-nat/latest/N#N0OrAdd1) `24 +` a given `n`
-}
type alias Add24 n =
    N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never n)))))))))))))))))))))))


{-| The [natural number](https://dark.elm.dmy.fr/packages/lue-bird/bounded-nat/latest/N#N0OrAdd1) `24`
-}
type alias N24 =
    N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Possibly Never))))))))))))))))))))))))


{-| `24` as the difference `Up x To (Add24 x)`
-}
type alias Up24 x =
    Up x To (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never x))))))))))))))))))))))))


{-| The [`N`](https://dark.elm.dmy.fr/packages/lue-bird/bounded-nat/latest/N#N) `24`
-}
n24 : N (In (Up24 minX_) (Up24 maxX_))
n24 =
    n16 |> add n8


{-| The [natural number](https://dark.elm.dmy.fr/packages/lue-bird/bounded-nat/latest/N#N0OrAdd1) `18 +` a given `n`
-}
type alias Add18 n =
    N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never n)))))))))))))))))


{-| The [natural number](https://dark.elm.dmy.fr/packages/lue-bird/bounded-nat/latest/N#N0OrAdd1) `18`
-}
type alias N18 =
    N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Possibly Never))))))))))))))))))


{-| `18` as the difference `Up x To (Add18 x)`
-}
type alias Up18 x =
    Up x To (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never (N0OrAdd1 Never x))))))))))))))))))


{-| The [`N`](https://dark.elm.dmy.fr/packages/lue-bird/bounded-nat/latest/N#N) `18`
-}
n18 : N (In (Up18 minX_) (Up18 maxX_))
n18 =
    n16 |> add n2


{-| The [natural number](https://dark.elm.dmy.fr/packages/lue-bird/bounded-nat/latest/N#N0OrAdd1) `31 +` a given `n`
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


{-| The [`N`](https://dark.elm.dmy.fr/packages/lue-bird/bounded-nat/latest/N#N) `31`
-}
n31 : N (In (Up31 minX_) (Up31 maxX_))
n31 =
    n16 |> add n8 |> add n4 |> add n2 |> add n1


{-| The [natural number](https://dark.elm.dmy.fr/packages/lue-bird/bounded-nat/latest/N#N0OrAdd1) `32 +` a given `n`
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


{-| The [`N`](https://dark.elm.dmy.fr/packages/lue-bird/bounded-nat/latest/N#N) `32`
-}
n32 : N (In (Up32 minX_) (Up32 maxX_))
n32 =
    n16 |> add n16
