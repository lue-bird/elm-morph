module Stack exposing (Stacked, cons, head, init, last, length, map, one, reverse, tail, toList)

{-| TODO rename to ListFilled
-}


type alias Stacked a =
    ( a, List a )


one : a -> Stacked a
one onlyElement =
    ( onlyElement, [] )


cons : a -> Stacked a -> Stacked a
cons el0 ( el1, el2Up ) =
    ( el0, el1 :: el2Up )


map : (a -> b) -> Stacked a -> Stacked b
map f ( head_, tail_ ) =
    ( head_ |> f, tail_ |> List.map f )


reverse : Stacked a -> Stacked a
reverse ( head_, tail_ ) =
    -- can be optimized
    case List.reverse tail_ of
        [] ->
            ( head_, [] )

        newHead :: newTailBeforeLast ->
            ( newHead, newTailBeforeLast ++ [ head_ ] )


head : Stacked a -> a
head ( head_, _ ) =
    head_


tail : Stacked a -> List a
tail ( _, tail_ ) =
    tail_


length : Stacked a -> Int
length ( _, tail_ ) =
    1 + (tail_ |> List.length)


toList : Stacked a -> List a
toList ( head_, tail_ ) =
    head_ :: tail_


init : Stacked a -> List a
init ( el0, el1Up ) =
    case el1Up of
        [] ->
            []

        el1 :: el2Up ->
            el0 :: init ( el1, el2Up )


last : Stacked a -> a
last ( el0, el1Up ) =
    case el1Up of
        [] ->
            el0

        el1 :: el2Up ->
            last ( el1, el2Up )



--
