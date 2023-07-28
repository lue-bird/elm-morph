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
    --> Err (Morph.DeadEnd [ "Hiyo", "!" ])

If your error type is not a `String`,
you will need [`|> Morph.narrowErrorMap ..your error to String..`](Morph#narrowErrorMap)
to make dead end types unify.

-}
toOk :
    MorphIndependently
        (Result narrowError narrowSuccess
         -> Result (Morph.ErrorWithDeadEnd narrowError) narrowSuccess
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

                    Err error ->
                        error |> Err
        }


{-| `Err error` succeeds with the `error`, `Ok` fails.

    import Morph
    import Char.Morph

    Err 'Y'
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
