module Testnet
  ( TestnetOptions(..)
  , babbageDefaultTestnetOptions
  , cardanoDefaultTestnetOptions
  , shelleyDefaultTestnetOptions
  , Testnet.testnet
  ) where

import           Control.Monad
import           Prelude



import           Hedgehog
import           Hedgehog.Extras.Test.Base (Integration, noteShow_)
import           Testnet.Babbage as Babbage
import           Testnet.Cardano as Cardano
import           Testnet.Conf
import qualified Testnet.Options as Options
import           Testnet.Options
import           Testnet.Shelley as Shelley (ShelleyTestnetOptions, defaultTestnetOptions,
                   shelleyTestnet)

data TestnetOptions
  = ShelleyOnlyTestnetOptions ShelleyTestnetOptions
  | BabbageOnlyTestnetOptions BabbageTestnetOptions
  | CardanoOnlyTestnetOptions CardanoTestnetOptions
  deriving (Eq, Show)

-- | There are certain conditions that need to be met in order to run
-- a valid node cluster.
testnetMinimumConfigurationRequirements :: CardanoTestnetOptions -> Integration ()
testnetMinimumConfigurationRequirements cTestnetOpts = do
  when (length (cardanoNodes cTestnetOpts) < 2) $ do
     noteShow_ ("Need at least two nodes to run a cluster" :: String)
     noteShow_ cTestnetOpts
     assert False

  when (cardanoNumPoolNodes (cardanoNodes cTestnetOpts) < 1) $ do
     noteShow_ ("Need at least one SPO to run a cluster" :: String)
     noteShow_ cTestnetOpts
     assert False

testnet :: TestnetOptions -> Conf -> Integration TestnetRuntime
testnet options conf = case options of
  ShelleyOnlyTestnetOptions o -> shelleyTestnet o conf
  BabbageOnlyTestnetOptions o -> babbageTestnet o conf
  CardanoOnlyTestnetOptions o -> do
    testnetMinimumConfigurationRequirements o
    cardanoTestnet o conf

babbageDefaultTestnetOptions :: BabbageTestnetOptions
babbageDefaultTestnetOptions = Options.defaultTestnetOptions

cardanoDefaultTestnetOptions :: CardanoTestnetOptions
cardanoDefaultTestnetOptions = Cardano.defaultTestnetOptions

shelleyDefaultTestnetOptions :: ShelleyTestnetOptions
shelleyDefaultTestnetOptions = Shelley.defaultTestnetOptions
