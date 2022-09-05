module Elm.CodeGen.Extra exposing
    ( aliasExpose, funExpose
    , arrayType, neverType, funType
    , Declaration(..), Doc, ExposedOrLocal(..), Module, ModuleRoleInPackage(..), PackageExposed(..), PackageInternal(..), PackageInternalDeclaration, PackageInternalModule, TypeConstructorExposed(..), aliasDeclaration, docTagsFrom, exposedToJust, exposingAll, exposingExplicit, funDeclaration, importAlias, localAliasDecl, localFunDecl, localTypeDecl, noAlias, noExposing, packageExposedAliasDecl, packageExposedFunDecl, packageExposedTypeDecl, packageInternalExposedAliasDecl, packageInternalExposedFunDecl, packageInternalExposedTypeDecl, stringFromModuleFile, toDeclaration, toDocComment, toModuleComment, typeDecl, zipEntryFromModule
    )

{-| Content to create a `Elm.CodeGen.file`.


## basic


### expose

@docs aliasExpose, funExpose


### ann

@docs arrayType, neverType, funType

-}

import Bytes.Encode
import Elm.CodeGen as Generation
import Elm.Pretty as Pretty
import Time
import Zip.Entry


{-| Content to create an `Elm.CodeGen.file`.
-}
type alias Module tag =
    { name : Generation.ModuleName
    , roleInPackage : ModuleRoleInPackage tag
    , imports : List Generation.Import
    , declarations : List (Declaration tag)
    }


type alias PackageInternalModule =
    Module Never


docTagsFrom :
    tag
    -> List (Declaration tag)
    -> Generation.Comment Generation.FileComment
    -> Generation.Comment Generation.FileComment
docTagsFrom tag declarations =
    Generation.docTagsFromExposings
        (declarations
            |> List.filterMap
                (\(Declaration decl) ->
                    decl.exposedOrLocal
                        |> Maybe.andThen
                            (\{ makeExposing, maybeTag } ->
                                maybeTag
                                    |> Maybe.map
                                        (\declTag ->
                                            { expose = makeExposing (.name decl), tag = declTag }
                                        )
                            )
                )
            |> List.filter (.tag >> (==) tag)
            |> List.map .expose
        )


noAlias : Maybe Generation.ModuleName
noAlias =
    Nothing


importAlias : String -> Maybe Generation.ModuleName
importAlias aliasName =
    Just [ aliasName ]


noExposing : Maybe Generation.Exposing
noExposing =
    Nothing


exposingExplicit :
    List Generation.TopLevelExpose
    -> Maybe Generation.Exposing
exposingExplicit exposings =
    Just (Generation.exposeExplicit exposings)


exposingAll : Maybe Generation.Exposing
exposingAll =
    Just Generation.exposeAll


type ModuleRoleInPackage tag
    = PackageExposedModule
        { moduleComment :
            List (Declaration tag)
            -> Doc Generation.FileComment
        }
    | PackageInternalModule


zipEntryFromModule : ( Time.Zone, Time.Posix ) -> Module tag_ -> Zip.Entry.Entry
zipEntryFromModule time moduleFile =
    stringFromModuleFile moduleFile
        |> Bytes.Encode.string
        |> Bytes.Encode.encode
        |> Zip.Entry.store
            { path = (.name moduleFile |> String.join "/") ++ ".elm"
            , lastModified = time
            , comment = Nothing
            }


type alias Doc comment =
    List
        (Generation.Comment comment
         -> Generation.Comment comment
        )


toDocComment : Doc Generation.DocComment -> Generation.Comment Generation.DocComment
toDocComment =
    List.foldl (<|) Generation.emptyDocComment


toModuleComment : Doc Generation.FileComment -> Generation.Comment Generation.FileComment
toModuleComment =
    List.foldl (<|) Generation.emptyFileComment



--


toDeclaration : Declaration tag_ -> Generation.Declaration
toDeclaration (Declaration declaration) =
    declaration.make declaration.name


type Declaration tag
    = Declaration
        { make : String -> Generation.Declaration
        , name : String
        , exposedOrLocal :
            Maybe
                { makeExposing : String -> Generation.TopLevelExpose
                , maybeTag : Maybe tag
                }
        }


type alias PackageInternalDeclaration =
    Declaration Never


type PackageInternal
    = PackageInternal


type PackageExposed
    = PackageExposed


exposedToJust :
    a
    -> ExposedOrLocal tag
    -> Maybe { makeExposing : a, maybeTag : Maybe tag }
exposedToJust exposeIfJust exposedOrLocal =
    case exposedOrLocal of
        Local ->
            Nothing

        Exposed tag ->
            Just { makeExposing = exposeIfJust, maybeTag = tag }


type ExposedOrLocal tag
    = Exposed (Maybe tag)
    | Local


type TypeConstructorExposed
    = OpenType
    | ClosedType



--


funDeclaration :
    ExposedOrLocal tag
    -> Maybe (Doc Generation.DocComment)
    -> Generation.TypeAnnotation
    -> String
    -> List Generation.Pattern
    -> Generation.Expression
    -> Declaration tag
funDeclaration exposedOrLocal comment typeType name argumentPatterns expression =
    { name = name
    , make =
        \name_ ->
            Generation.funDecl
                (comment |> Maybe.map toDocComment)
                (Just typeType)
                name_
                argumentPatterns
                expression
    , exposedOrLocal =
        exposedOrLocal |> exposedToJust Generation.funExpose
    }
        |> Declaration


packageExposedFunDecl :
    tag
    -> Doc Generation.DocComment
    -> Generation.TypeAnnotation
    -> String
    -> List String
    -> Generation.Expression
    -> Declaration tag
packageExposedFunDecl tag comment typeType name argumentNames expression =
    funDeclaration (Exposed (Just tag))
        (Just comment)
        typeType
        name
        (argumentNames |> List.map Generation.varPattern)
        expression


localFunDecl :
    Generation.TypeAnnotation
    -> String
    -> List Generation.Pattern
    -> Generation.Expression
    -> Declaration tag_
localFunDecl typeType name argumentPatterns expression =
    funDeclaration Local Nothing typeType name argumentPatterns expression


packageInternalExposedFunDecl :
    Generation.TypeAnnotation
    -> String
    -> List Generation.Pattern
    -> Generation.Expression
    -> PackageInternalDeclaration
packageInternalExposedFunDecl typeType name argumentPatterns expression =
    funDeclaration (Exposed Nothing) Nothing typeType name argumentPatterns expression


aliasDeclaration :
    ExposedOrLocal tag
    -> Maybe (Doc Generation.DocComment)
    -> String
    -> List String
    -> Generation.TypeAnnotation
    -> Declaration tag
aliasDeclaration exposedOrLocal comment name arguments annotation =
    { name = name
    , make =
        \name_ ->
            Generation.aliasDecl
                (comment |> Maybe.map toDocComment)
                name_
                arguments
                annotation
    , exposedOrLocal =
        exposedOrLocal
            |> exposedToJust Generation.typeOrAliasExpose
    }
        |> Declaration


packageExposedAliasDecl :
    tag
    -> Doc Generation.DocComment
    -> String
    -> List String
    -> Generation.TypeAnnotation
    -> Declaration tag
packageExposedAliasDecl tag comment name arguments annotation =
    aliasDeclaration (Exposed (Just tag))
        (Just comment)
        name
        arguments
        annotation


localAliasDecl :
    String
    -> List String
    -> Generation.TypeAnnotation
    -> PackageInternalDeclaration
localAliasDecl name arguments annotation =
    aliasDeclaration Local
        Nothing
        name
        arguments
        annotation


packageInternalExposedAliasDecl :
    String
    -> List String
    -> Generation.TypeAnnotation
    -> PackageInternalDeclaration
packageInternalExposedAliasDecl name arguments annotation =
    aliasDeclaration (Exposed Nothing)
        Nothing
        name
        arguments
        annotation


typeDecl :
    Maybe ( TypeConstructorExposed, Maybe tag )
    -> Maybe (Doc Generation.DocComment)
    -> String
    -> List String
    -> List ( String, List Generation.TypeAnnotation )
    -> Declaration tag
typeDecl exposedOrLocal comment name argumentNames choiceConstructors =
    { name = name
    , make =
        \name_ ->
            Generation.customTypeDecl
                (comment |> Maybe.map toDocComment)
                name_
                argumentNames
                choiceConstructors
    , exposedOrLocal =
        exposedOrLocal
            |> Maybe.map
                (\( openOrClosed, maybeTag ) ->
                    { maybeTag = maybeTag
                    , makeExposing =
                        case openOrClosed of
                            ClosedType ->
                                Generation.closedTypeExpose

                            OpenType ->
                                Generation.openTypeExpose
                    }
                )
    }
        |> Declaration


packageExposedTypeDecl :
    tag
    -> TypeConstructorExposed
    -> Doc Generation.DocComment
    -> String
    -> List String
    -> List ( String, List Generation.TypeAnnotation )
    -> Declaration tag
packageExposedTypeDecl tag constructorExposed comment name argumentNames choiceConstructors =
    typeDecl (Just ( constructorExposed, Just tag ))
        (Just comment)
        name
        argumentNames
        choiceConstructors


packageInternalExposedTypeDecl :
    TypeConstructorExposed
    -> String
    -> List String
    -> List ( String, List Generation.TypeAnnotation )
    -> PackageInternalDeclaration
packageInternalExposedTypeDecl constructorExposed name argumentNames choiceConstructors =
    typeDecl (Just ( constructorExposed, Nothing ))
        Nothing
        name
        argumentNames
        choiceConstructors


localTypeDecl :
    String
    -> List String
    -> List ( String, List Generation.TypeAnnotation )
    -> Declaration tag_
localTypeDecl name argumentNames choiceConstructors =
    typeDecl Nothing
        Nothing
        name
        argumentNames
        choiceConstructors



--


aliasExpose : List String -> List Generation.TopLevelExpose
aliasExpose names =
    List.map Generation.typeOrAliasExpose names


funExpose : List String -> List Generation.TopLevelExpose
funExpose names =
    List.map Generation.funExpose names



--


stringFromModuleFile : Module tag_ -> String
stringFromModuleFile moduleFile =
    let
        fromModuleComment moduleComment =
            let
                unpackedDecls =
                    .declarations moduleFile
                        |> List.map (\(Declaration decl) -> decl)

                decls =
                    unpackedDecls
                        |> List.map (\decl -> decl.make decl.name)

                exposings =
                    unpackedDecls
                        |> List.filterMap
                            (\{ exposedOrLocal, name } ->
                                exposedOrLocal
                                    |> Maybe.map (\{ makeExposing } -> makeExposing name)
                            )
            in
            Generation.file
                (Generation.normalModule moduleFile.name exposings)
                moduleFile.imports
                decls
                moduleComment
    in
    case .roleInPackage moduleFile of
        PackageExposedModule { moduleComment } ->
            Pretty.pretty 64
                (fromModuleComment
                    (toModuleComment
                        (moduleComment (.declarations moduleFile))
                        |> Just
                    )
                )

        PackageInternalModule ->
            Pretty.pretty 5000
                (fromModuleComment Nothing)



--


arrayType : Generation.TypeAnnotation -> Generation.TypeAnnotation
arrayType element =
    Generation.typed "Array" [ element ]


funType :
    List Generation.TypeAnnotation
    -> Generation.TypeAnnotation
    -> Generation.TypeAnnotation
funType parameters result =
    parameters
        |> List.foldr Generation.funAnn
            result


neverType : List Generation.TypeAnnotation
neverType =
    [ Generation.typed "Never" [] ]
