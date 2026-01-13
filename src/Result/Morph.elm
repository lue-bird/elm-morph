module Result.Morph exposing (toOk, toErr)

{-| [`Morph`](Morph#Morph) an [`elm/core` `Result error success`](https://dark.elm.dmy.fr/packages/elm/core/latest/Result#Result)

@docs toOk, toErr

-}

import Morph exposing (MorphIndependently)


{-| `Ok success` succeeds with the `success`, `Err error` fails with the `error`.

    import Morph
    import Char.Morph

    Ok 'Y'
        |> Morph.toNarrow
            (Char.Morph.only 'Y'
                |> Morph.over Result.Morph.toOk
            )
    --> Ok ()

    Err [ "Hiyo", "!" ]
        |> Morph.toNarrow Result.Morph.toOk
    --> Err (Morph.DeadEnd "err")

If your error type is not a `String`,
you will need [`|> Morph.errorMap`](Morph#errorMap) [`(Morph.deadEndMap ..your error to String..)`](Morph#deadEndMap)
to make dead end types unify.

-}
toOk :
    MorphIndependently
        (Result narrowError_ narrowSuccess
         -> Result Morph.Error narrowSuccess
        )
        (broadSuccess -> Result broadSuccess_ broadSuccess)
toOk =
    Morph.custom "ok"
        { toBroad = Ok
        , toNarrow =
            \maybe ->
                case maybe of
                    Ok success ->
                        success |> Ok

                    Err _ ->
                        Err "err"
        }


{-| `Err error` succeeds with the `error`, `Ok` fails.

    import Morph
    import Char.Morph

    Err 'y'
        |> Morph.toNarrow
            (Char.Morph.only 'Y'
                |> Morph.over Result.Morph.toErr
            )
    --> Ok ()

    Ok [ "Bye", "!" ]
        |> Morph.toNarrow Result.Morph.toErr
    --> Err (Morph.DeadEnd "ok")

-}
toErr :
    MorphIndependently
        (Result narrowError narrowSuccess_
         -> Result Morph.Error narrowError
        )
        (broadError -> Result broadError broadSuccess_)
toErr =
    Morph.custom "err"
        { toBroad = Err
        , toNarrow =
            \maybe ->
                case maybe of
                    Err error ->
                        error |> Ok

                    Ok _ ->
                        "ok" |> Err
        }
