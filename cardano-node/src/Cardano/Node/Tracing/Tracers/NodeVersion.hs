{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Cardano.Node.Tracing.Tracers.NodeVersion
(
    NodeVersionTrace (..)
  , getNodeVersion
  , getCardanoBuildInfo
)
 where

import           Data.Aeson (toJSON, (.=))
import           Data.Proxy (Proxy(..))
import           Data.Text (Text, pack)
import           Data.Version (Version (..), showVersion)
import           Debug.Trace (trace)
#if MIN_VERSION_base(4,15,0)
import           System.Info (arch, fullCompilerVersion, os)
#else
import           System.Info (arch, compilerVersion, os)
#endif
import qualified System.Info as SI


import           Cardano.Git.Rev (gitRev)
import           Cardano.Logging

import           Paths_cardano_node (version)
import           Autodocodec (HasObjectCodec(..), codecViaAeson, requiredField,
                   requiredFieldWith)
import qualified Autodocodec as AC


-- | Node version information

data NodeVersionTrace = NodeVersionTrace
  { applicationName    :: Text
  , applicationVersion :: Version
  , osName             :: Text
  , architecture       :: Text
  , compilerName       :: Text
  , compilerVersion    :: Version
  , gitRevision        :: Text
  } deriving (Eq, Show)

-- | Get the node version information

getComplierVersion :: Version
#if MIN_VERSION_base(4,15,0)
getComplierVersion =  System.Info.fullCompilerVersion
#else
getComplierVersion =  System.Info.compilerVersion
#endif

getNodeVersion :: NodeVersionTrace
getNodeVersion =
  let applicationName = "cardano-node"
      applicationVersion = version
      osName = pack os
      architecture = pack arch
      compilerName = pack SI.compilerName
      compilerVersion = getComplierVersion
      gitRevision = $(gitRev)
  in NodeVersionTrace {..}


instance MetaTrace NodeVersionTrace where
  namespaceFor NodeVersionTrace {}  =
    Namespace [] ["NodeVersion"]
  severityFor (Namespace _ ["NodeVersion"]) _ = Just Info
  severityFor _ _ = Nothing

  documentFor (Namespace _ ["NodeVersion"]) = Just "Node version information"

  documentFor _ = Nothing

  metricsDocFor (Namespace _ ["NodeVersion"]) =
    [("cardano_version_major", "Cardano node version information")
    ,("cardano_version_minor", "Cardano node version information")
    ,("cardano_version_patch", "Cardano node version information")
    ,("haskell_compiler_major", "Cardano compiler version information")
    ,("haskell_compiler_minor", "Cardano compiler version information")

#if MIN_VERSION_base(4,15,0)
    ,("haskell_compiler_patch", "Cardano compiler version information")
#endif
    ,("cardano_build_info", "Cardano node build info")
    ]
  metricsDocFor _ = []

  schemaFor _ = Just $ getSchema (DNormal) (Proxy :: Proxy NodeVersionTrace)

  allNamespaces = [Namespace [] ["NodeVersion"]]

instance HasObjectCodec NodeVersionTrace where
  objectCodec =
    NodeVersionTrace
      <$> requiredField "applicationName" "Application name."
          AC..= applicationName
      <*> requiredFieldWith "applicationVersion" (codecViaAeson "Version")
          "Application version."
          AC..= applicationVersion
      <*> requiredField "osName" "Operating system name."
          AC..= osName
      <*> requiredField "architecture" "CPU architecture."
          AC..= architecture
      <*> requiredField "compilerName" "Compiler name."
          AC..= compilerName
      <*> requiredFieldWith "compilerVersion" (codecViaAeson "Version")
          "Compiler version."
          AC..= compilerVersion
      <*> requiredField "gitRevision" "Git revision."
          AC..= gitRevision

instance LogFormattingCodec NodeVersionTrace

instance LogFormatting NodeVersionTrace where
  forHuman NodeVersionTrace {..} = mconcat
    [ "cardano-node ", pack (showVersion applicationVersion)
    , " git rev ", gitRevision
    , " - ", pack os, "-", pack arch
    , " - ", compilerName, "-", pack (showVersion compilerVersion)
    ]

  forMachine dl t@NodeVersionTrace{..} =
    let codecObj = forMachineViaCodec dl t
        manualObj =
          mconcat
            [ "applicationName" .= applicationName
            , "applicationVersion" .= toJSON applicationVersion
            , "gitRevision" .= gitRevision
            , "osName" .= osName
            , "architecture" .= architecture
            , "compilerName" .= compilerName
            , "compilerVersion" .= toJSON compilerVersion
            ]
    in if codecObj == manualObj
         then codecObj
         else trace ("NodeVersionTrace: codec/manual mismatch in forMachine manual: " ++
          show manualObj ++ " codec: " ++ show codecObj) codecObj

  asMetrics nvt@NodeVersionTrace {..} =
    [ IntM "cardano_version_major" (fromIntegral (getMajor applicationVersion))
    , IntM "cardano_version_minor" (fromIntegral (getMinor applicationVersion))
    , IntM "cardano_version_patch" (fromIntegral (getPatch applicationVersion))
    , IntM "haskell_compiler_major" (fromIntegral (getMajor compilerVersion))
    , IntM "haskell_compiler_minor" (fromIntegral (getMinor compilerVersion))
#if MIN_VERSION_base(4,15,0)
    , IntM "haskell_compiler_patch" (fromIntegral (getPatch compilerVersion))
#endif
    , PrometheusM "cardano_build_info" (getCardanoBuildInfo nvt)
    ]

getCardanoBuildInfo :: NodeVersionTrace -> [(Text,Text)]
getCardanoBuildInfo NodeVersionTrace {..} =
  [ ("version_major", pack (show (getMajor applicationVersion)))
  , ("version_minor", pack (show (getMinor applicationVersion)))
  , ("version_patch", pack (show (getPatch applicationVersion)))
  , ("version", pack (showVersion applicationVersion))
  , ("revision", gitRevision)
  , ("compiler_name", compilerName)
  , ("compiler_version", pack (showVersion compilerVersion))
  , ("compiler_version_major", pack (show (getMajor compilerVersion)))
  , ("compiler_version_minor", pack (show (getMinor compilerVersion)))
#if MIN_VERSION_base(4,15,0)
  , ("compiler_version_patch", pack (show (getPatch compilerVersion)))
#endif
  , ("architecture", architecture)
  , ("os_name", osName)
  ]

getMajor :: Version -> Int
getMajor (Version (x:_) _) = x
getMajor _ = 0

getMinor :: Version -> Int
getMinor (Version (_:x:_) _) = x
getMinor _ = 0

getPatch :: Version -> Int
getPatch (Version (_:_:x:_) _) = x
getPatch _ = 0

