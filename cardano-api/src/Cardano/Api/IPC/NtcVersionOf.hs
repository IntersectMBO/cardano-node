module Cardano.Api.IPC.NtcVersionOf
  ( NtcVersionOf (..)
  ) where

import           Ouroboros.Network.NodeToClient.Version (NodeToClientVersion (..))

-- | The query 'a' is a versioned query, which means it requires the Node to support a minimum
-- Node-to-Client version.
class NtcVersionOf a where
  ntcVersionOf :: a -> NodeToClientVersion
