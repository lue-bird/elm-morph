module Result.Morph exposing (toOk, toErr)

{-| [`Morph`](Morph#Morph) a `Result error success`

@docs toOk, toErr

-}

import Morph exposing (MorphIndependently, MorphRow)
import Value.Morph.Internal exposing (MorphValue)


{-| `Ok success` succeeds with the `success`, `Err error` fails with the `error`.

    import Morph
    import String.Morph

    Ok "Hi"
        |> Morph.toNarrow
            (String.Morph.only "Hi"
                |> Morph.over Result.Morph.toOk
            )
    --> Ok ()

    Err [ "Bye", "!" ]
        |> Morph.toNarrow Result.Morph.toOk
    --> Err (Morph.DeadEnd [ "Bye", "!" ])

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
    import String.Morph

    Err "Hi"
        |> Morph.toNarrow
            (String.Morph.only "Hi"
                |> Morph.over Result.Morph.toOk
            )
    --> Ok ()

    Ok [ "Bye", "!" ]
        |> Morph.toNarrow Result.Morph.toOk
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
