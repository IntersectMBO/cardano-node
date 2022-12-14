module Cardano.Api.IPC.NtcVersionOf
  ( NtcVersionOf (..)
  , MinNodeToClientVersion

  -- *** Error types
  , UnsupportedNtcVersionError(..)
  ) where

import           Data.Eq (Eq)
import           Text.Show (Show)
import           Ouroboros.Network.NodeToClient.Version (NodeToClientVersion (..))

-- | The query 'a' is a versioned query, which means it requires the Node to support a minimum
-- Node-to-Client version.
class NtcVersionOf a where
  ntcVersionOf :: a -> NodeToClientVersion

type MinNodeToClientVersion = NodeToClientVersion

data UnsupportedNtcVersionError = UnsupportedNtcVersionError !MinNodeToClientVersion !NodeToClientVersion
  deriving (Eq, Show)
