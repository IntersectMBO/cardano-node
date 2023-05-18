module Testnet.Parsers
  ( commands
  , runTestnetCmd
  ) where

import           Options.Applicative
import           Parsers.Babbage
import           Parsers.Byron
import           Parsers.Cardano
import           Parsers.Shelley
import           Parsers.Version

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
runTestnetCmd (StartBabbageTestnet opts) = runBabbageOptions opts
runTestnetCmd (StartByrontestnet opts) = runByronOptions opts
runTestnetCmd (StartCardanoTestnet opts) = runCardanoOptions opts
runTestnetCmd (StartShelleyTestnet opts) = runShelleyOptions opts
runTestnetCmd (GetVersion opts) = runVersionOptions opts
