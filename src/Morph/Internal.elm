module Morph.Internal exposing (inSequenceErrorWith, sequenceDescriptionFromStack)

{-| Morph helpers that can be used inside the package but aren't exposed to the public.
-}

import Morph
import Stack exposing (Stacked)


sequenceDescriptionFromStack : Stacked Morph.Description -> Morph.Description
sequenceDescriptionFromStack stack =
    case stack |> Stack.tail of
        [] ->
            stack |> Stack.head

        el1 :: el2Up ->
            Morph.SequenceDescription
                { early = stack |> Stack.head
                , late = ( el1, el2Up ) |> sequenceDescriptionFromStack
                }


inSequenceErrorWith :
    { startsDown : Stacked Int
    , error : Morph.Error
    }
    -> Morph.Error
inSequenceErrorWith { startsDown, error } =
    case startsDown |> Stack.tail of
        [] ->
            Morph.SequenceError
                { place = Morph.SequencePlaceEarly
                , startDownInBroadList = startsDown |> Stack.head
                , error = error
                }

        startsDown0 :: startsDown1Up ->
            Morph.SequenceError
                { place = Morph.SequencePlaceLate
                , startDownInBroadList = startsDown |> Stack.head
                , error =
                    inSequenceErrorWith
                        { startsDown = ( startsDown0, startsDown1Up )
                        , error = error
                        }
                }
