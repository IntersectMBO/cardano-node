module Testnet.Parsers
  ( commands
  , runTestnetCmd
  , pref
  , opts
  ) where

import           Data.Foldable
import           Options.Applicative
import qualified Options.Applicative as Opt
import           Parsers.Babbage
import           Parsers.Byron
import           Parsers.Cardano
import           Parsers.Shelley
import           Parsers.Version

pref :: ParserPrefs
pref = Opt.prefs $ showHelpOnEmpty <> showHelpOnError

opts :: ParserInfo Testnet.Parsers.CardanoTestnetCommands
opts = Opt.info (commands <**> helper) idm

-- TODO: Remove StartBabbageTestnet and StartShelleyTestnet
-- by allowing the user to start testnets in any era (excluding Byron)
-- via StartCardanoTestnet
data CardanoTestnetCommands
  = StartBabbageTestnet BabbageOptions
  | StartByrontestnet ByronOptions -- TODO: Do we care about being able to start a Byron only testnet?
  | StartCardanoTestnet CardanoOptions
  | StartShelleyTestnet ShelleyOptions
  | GetVersion VersionOptions

commands :: Parser CardanoTestnetCommands
commands =
  asum [ fmap StartCardanoTestnet (subparser cmdCardano)
       , fmap StartByrontestnet (subparser cmdByron)
       , fmap StartShelleyTestnet (subparser cmdShelley)
       , fmap StartBabbageTestnet (subparser cmdBabbage)
       , fmap GetVersion (subparser cmdVersion)
       ]

runTestnetCmd :: CardanoTestnetCommands -> IO ()
runTestnetCmd (StartBabbageTestnet cmdOpts) = runBabbageOptions cmdOpts
runTestnetCmd (StartByrontestnet cmdOpts) = runByronOptions cmdOpts
runTestnetCmd (StartCardanoTestnet cmdOpts) = runCardanoOptions cmdOpts
runTestnetCmd (StartShelleyTestnet cmdOpts) = runShelleyOptions cmdOpts
runTestnetCmd (GetVersion cmdOpts) = runVersionOptions cmdOpts
