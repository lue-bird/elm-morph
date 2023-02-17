module Trie exposing
    ( Trie(..)
    , fromKeySet
    )

{-| For doing fast row matching

            Trie.empty
                |> Trie.add "john" ()
                |> Trie.add "josh" ()
                     "j" (Nothing)
                       \
                       "o" (Nothing)
                      /  \
          (Nothing) "h"   "s" (Nothing)
                   /        \
        (Just ()) "n"       "h" (Just ())

-}

import Char exposing (Char)
import Emptiable exposing (Emptiable)
import KeySet exposing (KeySet, Sorting)
import Linear exposing (Direction(..))
import Order exposing (Ordering)
import Possibly exposing (Possibly)
import Typed


{-| -}
type Trie element tag
    = Trie
        (Emptiable
            (KeySet
                { element : element
                , completeness : Completeness
                , after : Trie element tag
                }
                (ByElement tag)
            )
            Possibly
        )


byElement :
    KeySet.Sorting element key tag
    ->
        KeySet.Sorting
            { element : element
            , completeness : Completeness
            , after : Trie element tag
            }
            key
            (ByElement tag)
byElement elementSorting =
    KeySet.sortingKey
        (.element >> (elementSorting |> Typed.untag |> .key))
        { tag = ByElement
        , order = elementSorting |> Typed.untag |> .keyOrder
        }


type ByElement tag
    = ByElement


type Completeness
    = Complete
    | Incomplete


empty : Trie element tag_
empty =
    Trie Emptiable.empty


insert :
    Sorting element key tag
    -> List element
    ->
        (Trie element tag
         -> Trie element tag
        )
insert sorting list =
    \trie -> trie |> insertHelper sorting [] list


{-| To avoid stack overflow we create functions that will
wait for the result of the recursive part and add all of them
to a list. Once we finished recursing we just go though the list
applying all the computed elements
-}
insertHelper :
    Sorting element key tag
    -> List ( element, Trie element tag )
    -> List element
    ->
        (Trie element tag
         -> Trie element tag
        )
insertHelper sorting nodeCreators elements =
    case elements of
        [] ->
            \_ -> empty |> joinNodes sorting nodeCreators

        head :: tail ->
            \(Trie trie) ->
                let
                    nextNode =
                        trie |> KeySet.element sorting head
                in
                insertHelper
                    sorting
                    (( head, Trie trie ) :: nodeCreators)
                    tail
                    nextNode


joinNodes :
    Sorting element key_ tag
    -> List ( element, Trie element tag )
    ->
        (Trie element tag
         -> Trie element tag
        )
joinNodes sorting pres leaf =
    List.foldl
        (\( element, Trie trie ) node ->
            Trie
                { completeness = trie.completeness
                , after =
                    trie.after
                        |> KeySet.insert (byElement sorting) element
                }
        )
        leaf
        pres


fromKeySet :
    KeySet.Sorting element key tag
    ->
        (Emptiable (KeySet (List element) tag) possiblyOrNever_
         -> Trie element tag
        )
fromKeySet sorting =
    \keySet ->
        keySet
            |> KeySet.fold Up
                (insert sorting)
                empty
