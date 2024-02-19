module Cardano.Chairman.Commands.Version
  ( VersionOptions(..)
  , cmdVersion
  , runVersionOptions
  ) where

import           Cardano.Git.Rev (gitRev)

import qualified Data.Text as T
import           Data.Version (showVersion)
import           Options.Applicative
import           System.Info (arch, compilerName, compilerVersion, os)
import qualified System.IO as IO

import           Paths_cardano_node_chairman (version)

data VersionOptions = VersionOptions deriving (Eq, Show)

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

cmdVersion :: Mod CommandFields (IO ())
cmdVersion = command "version" $ flip info idm $ runVersionOptions <$> optsVersion
