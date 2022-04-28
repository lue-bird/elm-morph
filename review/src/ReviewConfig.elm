module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import Review.Rule as Rule exposing (Rule)
import Docs.ReviewLinksAndSections
import Docs.NoMissing
import Docs.ReviewAtDocs
import NoBooleanCase
import NoDebug.Log
import NoDebug.TodoOrToString
import NoExposingEverything
import NoForbiddenWords
import NoImportingEverything
import ReviewPipelineStyles
import NoMissingTypeAnnotation
import NoMissingTypeExpose
import NoSinglePatternCase
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import OnlyAllSingleUseTypeVarsEndWith_
import Simplify
import NoRecordAliasConstructor
import NoFunctionOutsideOfModules


config : List Rule
config =
    [ Docs.ReviewLinksAndSections.rule
    , Docs.ReviewAtDocs.rule
    , Docs.NoMissing.rule
        { document = Docs.NoMissing.onlyExposed
        , from = Docs.NoMissing.exposedModules
        }
    , NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
        |> Rule.ignoreErrorsForDirectories [ "tests/" ]
    , NoExposingEverything.rule
    , NoForbiddenWords.rule [ "REPLACEME", "TODO", "todo" ]
    , NoImportingEverything.rule [ "Nats" ]
    , NoMissingTypeAnnotation.rule
    , NoMissingTypeExpose.rule
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.CustomTypeConstructorArgs.rule
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoUnused.Modules.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , Simplify.rule Simplify.defaults
    , OnlyAllSingleUseTypeVarsEndWith_.rule
    , NoSinglePatternCase.rule
        (NoSinglePatternCase.fixInArgument
            |> NoSinglePatternCase.ifAsPatternRequired
                (NoSinglePatternCase.fixInLetInstead
                    |> NoSinglePatternCase.andIfNoLetExists
                        NoSinglePatternCase.createNewLet
                )
        )
    , ReviewPipelineStyles.forbid ReviewPipelineStyles.leftPizzaPipelines
        |> ReviewPipelineStyles.that ReviewPipelineStyles.haveAnUnnecessaryInputStep
        |> ReviewPipelineStyles.andTryToFixThemBy ReviewPipelineStyles.eliminatingInputStep
        |> ReviewPipelineStyles.andCallThem "<| pipeline with simple input"
    , NoBooleanCase.rule
    , OnlyAllSingleUseTypeVarsEndWith_.rule
    , NoRecordAliasConstructor.rule
    , NoFunctionOutsideOfModules.rule
        [ ( [ "Elm.CodeGen.fqTyped"
            , "Elm.CodeGen.typed"
            , "Elm.CodeGen.boolAnn"
            , "Elm.CodeGen.intAnn"
            , "Elm.CodeGen.floatAnn"
            , "Elm.CodeGen.stringAnn"
            , "Elm.CodeGen.charAnn"
            , "Elm.CodeGen.listAnn"
            , "Elm.CodeGen.setAnn"
            , "Elm.CodeGen.dictAnn"
            , "Elm.CodeGen.maybeAnn"
            ]
          , [ "Elm.Code.Type" ]
          )
        , ( [ "Elm.CodeGen.fqVal"
            , "Elm.CodeGen.fqFun"
            , "Elm.CodeGen.fqConstruct"
            , "Elm.CodeGen.val"
            , "Elm.CodeGen.fun"
            , "Elm.CodeGen.construct"
            ]
          , [ "Elm.Code.Expression" ]
          )
        , ( [ "Elm.CodeGen.fqNamedPattern"
            , "Elm.CodeGen.namedPattern"
            ]
          , [ "Elm.Code.Pattern" ]
          )
        ]
    ]
