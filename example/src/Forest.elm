module Forest exposing (Forest, ForestPath, alterAt, map)

import Tree exposing (Tree)


type alias Forest label =
    List (Tree label)


map : (aLabel -> bLabel) -> Forest aLabel -> Forest bLabel
map labelChange =
    List.map (Tree.map labelChange)


type alias ForestPath =
    { index : Int, indexPath : TreePath }


type alias TreePath =
    List Int


alterAt : ForestPath -> (label -> label) -> Forest label -> Forest label
alterAt forestPath labelChange nodes =
    List.indexedMap
        (\index ->
            if index == forestPath.index then
                treeAlterAt forestPath.indexPath labelChange

            else
                identity
        )
        nodes


treeAlterAt : TreePath -> (label -> label) -> Tree label -> Tree label
treeAlterAt path labelChange tree =
    case path of
        [] ->
            Tree.mapLabel labelChange tree

        index :: indexPath ->
            tree
                |> Tree.mapChildren
                    (alterAt { index = index, indexPath = indexPath } labelChange)
