module Morph.Internal exposing (inSequenceErrorWith, sequenceDescriptionFromStack)

{-| Morph helpers that can be used inside the package but aren't exposed to the public.
-}

import Emptiable exposing (Emptiable)
import Morph
import Stack exposing (Stacked)


sequenceDescriptionFromStack : Emptiable (Stacked Morph.Description) Never -> Morph.Description
sequenceDescriptionFromStack =
    \stack ->
        case stack |> Stack.removeTop of
            Emptiable.Empty _ ->
                stack |> Stack.top

            Emptiable.Filled stacked ->
                Morph.SequenceDescription
                    { early = stack |> Stack.top
                    , late = stacked |> Emptiable.filled |> sequenceDescriptionFromStack
                    }


inSequenceErrorWith :
    { startsDown : Emptiable (Stacked Int) Never
    , error : Morph.ErrorWithDeadEnd deadEnd
    }
    -> Morph.ErrorWithDeadEnd deadEnd
inSequenceErrorWith { startsDown, error } =
    case startsDown |> Stack.removeTop of
        Emptiable.Empty _ ->
            Morph.SequenceError
                { place = Morph.InSequenceEarly
                , startDownInBroadList = startsDown |> Stack.top
                , error = error
                }

        Emptiable.Filled startsDownStacked ->
            Morph.SequenceError
                { place = Morph.InSequenceLate
                , startDownInBroadList = startsDown |> Stack.top
                , error =
                    inSequenceErrorWith
                        { startsDown = startsDownStacked |> Emptiable.filled
                        , error = error
                        }
                }
