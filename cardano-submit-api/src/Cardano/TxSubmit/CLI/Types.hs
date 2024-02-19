module Cardano.TxSubmit.CLI.Types
  ( ConfigFile (..)
  , GenesisFile (..)
  , TxSubmitNodeParams (..)
  ) where

import           Cardano.Api (ConsensusModeParams, NetworkId (..), SocketPath)

import           Cardano.TxSubmit.Rest.Types (WebserverConfig)

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

