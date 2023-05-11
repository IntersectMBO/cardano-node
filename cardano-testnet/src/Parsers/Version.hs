module Parsers.Version
  ( VersionOptions(..)
  , cmdVersion
  , runVersionOptions
  ) where

import           Cardano.Git.Rev (gitRev)
import qualified Data.Text as T
import           Data.Version (showVersion)
import           Options.Applicative
import           Paths_cardano_testnet (version)
import           System.Info (arch, compilerName, compilerVersion, os)
import qualified System.IO as IO

import           Cardano.CLI.Common.Parsers


data VersionOptions = VersionOptions
  deriving (Eq, Show)



optsVersion :: Parser VersionOptions
optsVersion = pure VersionOptions

runVersionOptions :: VersionOptions -> IO ()
runVersionOptions VersionOptions = do
  IO.putStrLn $ mconcat
    [ "cardano-node ", showVersion version
    , " - ", os, "-", arch
    , " - ", compilerName, "-", showVersion compilerVersion
    , "\ngit rev ", T.unpack gitRev
    ]

cmdVersion :: Mod CommandFields VersionOptions
cmdVersion = command' "version" "Show cardano-testnet version" optsVersion
