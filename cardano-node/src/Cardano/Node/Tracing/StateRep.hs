{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Node.Tracing.StateRep
  ( NodeState (..)
  ) where

import Cardano.Prelude
import Data.Aeson (FromJSON, ToJSON)

import Cardano.Node.Handlers.Shutdown (ShutdownTrace (..))

type PeerInfoPP = Text     -- The result of 'ppPeer' function.
type StartupTracePP = Text -- The result of 'ppStartupInfoTrace' function.

-- | The representation of the current state of node.
--   All node states prior to tracing system going online are effectively invisible.
data NodeState blk
  = NodeTracingOnlineConfiguring -- ^ initTraceDispatcher
  | NodeStartup StartupTracePP
  | NodePeers [PeerInfoPP]       -- ^ The peers information here is for demonstration only.
  | NodeShutdown ShutdownTrace

deriving instance Generic (NodeState blk)

instance ToJSON (NodeState blk)

-- Strictly speaking, we mustn't provide 'FromJSON' instance here,
-- but it will be convenient for acceptor application.
instance FromJSON (NodeState blk)
