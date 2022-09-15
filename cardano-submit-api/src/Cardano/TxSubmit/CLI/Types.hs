module Cardano.TxSubmit.CLI.Types
  ( ConfigFile (..)
  , GenesisFile (..)
  , SocketPath (..)
  , TxSubmitNodeParams (..)
  ) where

import           Cardano.Api (AnyConsensusModeParams, NetworkId (..))
import           Cardano.TxSubmit.Rest.Types (WebserverConfig)
import           Data.Int
import           System.IO (FilePath)

-- | The product type of all command line arguments
data TxSubmitNodeParams = TxSubmitNodeParams
  { tspConfigFile :: !ConfigFile
  , tspProtocol :: !AnyConsensusModeParams
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

