module Testnet
  ( TestnetOptions(..)
  , babbageDefaultTestnetOptions
  , cardanoDefaultTestnetOptions
  , shelleyDefaultTestnetOptions
  , Testnet.testnet
  ) where

import           Data.Eq (Eq)
import           Text.Show (Show)

import qualified Hedgehog.Extras.Test.Base as H

import           Testnet.Babbage as Babbage
import           Testnet.Cardano as Cardano
import           Testnet.Conf
import           Testnet.Shelley as Shelley

data TestnetOptions
  = ShelleyOnlyTestnetOptions ShelleyTestnetOptions
  | BabbageOnlyTestnetOptions BabbageTestnetOptions
  | CardanoOnlyTestnetOptions CardanoTestnetOptions
  deriving (Eq, Show)

testnet :: TestnetOptions -> Conf -> H.Integration TestnetRuntime
testnet options = case options of
  ShelleyOnlyTestnetOptions o -> shelleyTestnet o
  BabbageOnlyTestnetOptions o -> babbageTestnet o
  CardanoOnlyTestnetOptions o -> cardanoTestnet o

babbageDefaultTestnetOptions :: BabbageTestnetOptions
babbageDefaultTestnetOptions = Babbage.defaultTestnetOptions

cardanoDefaultTestnetOptions :: CardanoTestnetOptions
cardanoDefaultTestnetOptions = Cardano.defaultTestnetOptions

shelleyDefaultTestnetOptions :: ShelleyTestnetOptions
shelleyDefaultTestnetOptions = Shelley.defaultTestnetOptions
