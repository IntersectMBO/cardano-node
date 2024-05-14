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
import           Data.Text (Text, pack)

import           Data.Version (Version (..), showVersion)
import           System.Info (arch, compilerName, compilerVersion, os)


import           Cardano.Git.Rev (gitRev)
import           Cardano.Logging


import           Paths_cardano_node (version)



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

getNodeVersion :: NodeVersionTrace
getNodeVersion =
  let applicationName = "cardano-node"
      applicationVersion = version
      osName = pack os
      architecture = pack arch
      compilerName = pack System.Info.compilerName
      compilerVersion = System.Info.compilerVersion

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
    --,("haskell_compiler_patch", "Cardano compiler version information")
    ,("cardano_build_info", "Cardano node build info")
    ]
  metricsDocFor _ = []

  allNamespaces = [Namespace [] ["NodeVersion"]]

instance LogFormatting NodeVersionTrace where
  forHuman NodeVersionTrace {..} = mconcat
    [ "cardano-node ", pack (showVersion applicationVersion)
    , " git rev ", gitRevision
    , " - ", pack os, "-", pack arch
    , " - ", compilerName, "-", pack (showVersion compilerVersion)
    ]

  forMachine _dtal NodeVersionTrace {..} = mconcat

    [ "applicationName" .= applicationName
    , "applicationVersion" .= toJSON applicationVersion
    , "gitRevision" .= gitRevision
    , "osName" .= osName
    , "architecture" .= architecture
    , "compilerName" .= compilerName
    , "compilerVersion" .= toJSON compilerVersion
    ]

  asMetrics nvt@NodeVersionTrace {..} =
    [ IntM "cardano_version_major" (fromIntegral (getMajor applicationVersion))
    , IntM "cardano_version_minor" (fromIntegral (getMinor applicationVersion))
    , IntM "cardano_version_patch" (fromIntegral (getPatch applicationVersion))
    , IntM "haskell_compiler_major" (fromIntegral (getMajor compilerVersion))
    , IntM "haskell_compiler_minor" (fromIntegral (getMinor compilerVersion))
    --, IntM "haskell_compiler_patch" (fromIntegral (getPatch compilerVersion))
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
  -- , ("compiler_version_patch", pack (show (getPatch compilerVersion)))
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



