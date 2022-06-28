module Cardano.Benchmarking.Version
where

import           Data.Aeson as A
import           Data.Text as Text
import           Data.Version (showVersion)
import           Paths_tx_generator (version)
import           System.Info (arch, compilerName, compilerVersion, os)

import           Cardano.Git.Rev (gitRev)

data Version = Version
  { _package :: !Text
  , _os      :: !Text
  , _arch    :: !Text
  , _compilerName :: !Text
  , _compilerVersion :: !Text
  , _gitRev :: !Text
  }
  deriving (Show)

txGeneratorVersion :: Version
txGeneratorVersion = Version
  { _package = renderVersion version
  , _os      = Text.pack os
  , _arch    = Text.pack arch
  , _compilerName    = Text.pack compilerName
  , _compilerVersion = renderVersion compilerVersion
  , _gitRev = gitRev
  }
  where
    renderVersion = Text.pack . showVersion

multilineVersionMsg :: Version -> Text
multilineVersionMsg v
  = mconcat
    [ "tx-generator ", _package v
    , " - ", _os v, "-", _arch v
    , " - ", _compilerName v, "-", _compilerVersion v
    , "\ngit rev ", _gitRev v
    ]

toJsonLogMsg :: Version -> A.Object
toJsonLogMsg v = mconcat
  [ "package"    .= A.String ( _package v)
  , "os"         .= A.String ( _os v)
  , "arch"       .= A.String ( _arch v)
  , "compilerName"    .= A.String ( _compilerName v)
  , "compilerVersion" .= A.String ( _compilerVersion v)
  , "gitRev"          .= A.String ( _gitRev v)
  ]
