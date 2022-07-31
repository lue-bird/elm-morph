module ArraySized.Morph exposing
    ( fromList, toList
    , fromStackEmptiable, toStackEmptiable
    , fromStackFilled, toStackFilled
    , elementEach
    )

{-| [`Morph`](Morph) an `Array`


## structure

@docs fromList, toList
@docs fromStackEmptiable, toStackEmptiable
@docs fromStackFilled, toStackFilled


## transform

Also available: [`toggle`](Morph#toggle) [`Array.Extra.reverse`](https://dark.elm.dmy.fr/packages/elm-community/array-extra/latest/Array-Extra#reverse)

@docs elementEach

-}

import ArraySized exposing (ArraySized)
import Emptiable exposing (Emptiable)
import Morph exposing (Morph, MorphInProgress, Translate, broaden, narrow, translate, translateOn)
import N exposing (Fixed, Min, N0, N1)
import Possibly exposing (Possibly)
import Stack exposing (Stacked)


{-| [`Translate`](Morph#Translate) from `List` to `ArraySized`

    import N exposing (n0)
    import ArraySized

    [ 0, 1, 2, 3 ]
        |> Morph.map ArraySized.Morph.fromList
    --: ArraySized (Min (Fixed N0)) number_

-}
fromList :
    Morph
        (List element)
        (ArraySized
            (Min (Fixed N0))
            element
        )
        error_
fromList =
    translate ArraySized.toList ArraySized.fromList


{-| [`Translate`](Morph#Translate) from `ArraySized` to `List`

    import N exposing (n0)
    import ArraySized

    ArraySized.l4 0 1 2 3
        |> ArraySized.minLower n0
        |> Morph.map ArraySized.Morph.toList
    --> [ 0, 1, 2, 3 ]

-}
toList :
    Morph
        (ArraySized (Min (Fixed N0)) element)
        (List element)
        error_
toList =
    translate ArraySized.fromList ArraySized.toList


{-| [`Translate`](Morph#Translate) from `Emptiable (Stacked ...) Possibly` to `ArraySized`

    import N exposing (n0)
    import ArraySized

    Stack.topDown 0 [ 1, 2, 3, 4 ]
        |> (ArraySized.Morph.fromStackEmptiable |> Morph.map)
    --: ArraySized (Min (Fixed N0)) number_

Have `>= 1` element (`Emptiable (Stacked ...) Never`)? → [`fromStackFilled`](#fromStackFilled)

-}
fromStackEmptiable :
    Morph
        (ArraySized (Min (Fixed N0)) element)
        (Emptiable (Stacked element) Possibly)
        error_
fromStackEmptiable =
    translate ArraySized.fromStackEmptiable ArraySized.toStackEmptiable


{-| [`Translate`](Morph#Translate) from `ArraySized` to `Emptiable (Stacked ...) Possibly`

    import N exposing (n0)
    import ArraySized

    ArraySized.l4 0 1 2 3
        |> ArraySized.minLower n0
        |> (ArraySized.Morph.toStackEmptiable |> Morph.map)
    --> Stack.topDown 0 [ 1, 2, 3 ]
    --: Emptiable (Stacked number_) Possibly

Have `>= 1` element (`Emptiable (Stacked ...) Never`)? → [`toStackFilled`](#toStackFilled)

-}
toStackEmptiable :
    Morph
        (Emptiable (Stacked element) Possibly)
        (ArraySized (Min (Fixed N0)) element)
        error_
toStackEmptiable =
    translate ArraySized.toStackEmptiable ArraySized.fromStackEmptiable


{-| [`Translate`](Morph#Translate) from `Emptiable (Stacked ...) Never` to `ArraySized`

    import N exposing (n0)
    import ArraySized

    Stack.topDown 0 [ 1, 2, 3, 4 ]
        |> Morph.map ArraySized.Morph.fromStackFilled
    --: ArraySized (Min (Fixed N1)) number_

Have `>= 1` element (`Emptiable (Stacked ...) Possibly`)? → [`fromStackEmptiable`](#fromStackEmptiable)

-}
fromStackFilled :
    Morph
        (Emptiable (Stacked element) Never)
        (ArraySized
            (Min (Fixed N1))
            element
        )
        error_
fromStackFilled =
    translate ArraySized.toStackFilled ArraySized.fromStackFilled


{-| [`Translate`](Morph#Translate) from `ArraySized` to `Emptiable (Stacked ...) Never`

    import N exposing (n0)
    import ArraySized

    ArraySized.l4 0 1 2 3
        |> ArraySized.minLower n0
        |> Morph.map ArraySized.Morph.toStackFilled
    --> Stack.topDown 0 [ 1, 2, 3 ]
    --: Emptiable (Stacked number_) Never

Have `>= 1` element (`Emptiable (Stacked ...) Possibly`)? → [`toStackEmptiable`](#toStackEmptiable)

-}
toStackFilled :
    Morph
        (ArraySized (Min (Fixed N1)) element)
        (Emptiable (Stacked element) Never)
        error_
toStackFilled =
    translate ArraySized.fromStackFilled ArraySized.toStackFilled



--


{-| [`Translate`](Morph#Translate) each element in an `Array`.
-}
elementEach :
    Translate mapped unmapped
    ->
        Morph
            (ArraySized lengthRange mapped)
            (ArraySized lengthRange unmapped)
            error_
elementEach elementTranslate =
    ( ArraySized.map, ArraySized.map )
        |> translateOn elementTranslate
