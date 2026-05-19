module Cardano.TxSubmit.CLI.Types
  ( ConfigFile (..)
  , GenesisFile (..)
  , TxSubmitCommand(..)
  , TxSubmitNodeParams (..)
  ) where

import           Cardano.Api (ConsensusModeParams, NetworkId (..), SocketPath)

import           Cardano.TxSubmit.Rest.Types (WebserverConfig)

data TxSubmitCommand
  = TxSubmitRun !TxSubmitNodeParams
  | TxSubmitVersion

-- | The product type of all command line arguments
data TxSubmitNodeParams = TxSubmitNodeParams
  { tspConfigFile :: !ConfigFile
  , tspProtocol :: !ConsensusModeParams
  , tspNetworkId :: !NetworkId
  , tspSocketPath :: !SocketPath
  , tspWebserverConfig :: !WebserverConfig
  , tspMetricsPort :: !Int
  }

newtype ConfigFile = ConfigFile
  { unConfigFile :: FilePath
  }

newtype GenesisFile = GenesisFile
  { unGenesisFile :: FilePath
  }

