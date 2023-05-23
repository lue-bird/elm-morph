module StructureMorph exposing (StructureMorphCreator, add, finish, for)

import Emptiable exposing (Emptiable)
import Morph exposing (Description, ErrorWithDeadEnd, MorphIndependently)
import Possibly exposing (Possibly)
import Stack exposing (Stacked)


type alias StructureMorphCreator morph =
    { description :
        { structure : String
        , inner : Emptiable (Stacked Description) Possibly
        }
    , morph : morph
    }


for :
    String
    -> morph
    -> StructureMorphCreator morph
for structureName morph =
    { description = { structure = structureName, inner = Emptiable.empty }
    , morph = morph
    }


add :
    MorphIndependently
        (partBeforeNarrow -> Result (ErrorWithDeadEnd deadEnd) partNarrow)
        partToBroad
    ->
        StructureMorphCreator
            (MorphIndependently
                (partBeforeNarrow -> Result (ErrorWithDeadEnd deadEnd) partNarrow)
                partToBroad
             -> morph
            )
    -> StructureMorphCreator morph
add partMorph =
    \structureCreatorSoFar ->
        { description =
            { structure = structureCreatorSoFar.description.structure
            , inner =
                structureCreatorSoFar.description.inner
                    |> Stack.onTopLay (partMorph |> Morph.description)
            }
        , morph =
            structureCreatorSoFar.morph
                { partMorph
                    | toNarrow =
                        \beforeToNarrow ->
                            beforeToNarrow
                                |> Morph.toNarrow partMorph
                                |> Result.mapError
                                    (\error ->
                                        Morph.InStructureError
                                            { index = structureCreatorSoFar.description.inner |> Stack.length
                                            , error = error
                                            }
                                    )
                }
        }


finish :
    StructureMorphCreator { morph_ | toNarrow : toNarrow, toBroad : toBroad }
    -> MorphIndependently toNarrow toBroad
finish =
    \structureCreator ->
        { description =
            { custom = Emptiable.empty
            , inner =
                Morph.StructureDescription
                    structureCreator.description.structure
                    (structureCreator.description.inner |> Stack.reverse)
            }
        , toNarrow = structureCreator.morph.toNarrow
        , toBroad = structureCreator.morph.toBroad
        }
