{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Node.Tracing.Peers
  ( NodePeers (..)
  , traceNodePeers
  ) where

import           Cardano.Prelude
import           Data.Aeson (FromJSON, ToJSON)

import           Cardano.Logging

import           Cardano.Node.Tracing.Tracers.Peer (PeerT, ppPeer)

type PeerInfoPP = Text -- The result of 'ppPeer' function.

-- | This type contains an information about current peers of the node.
--   It will be asked by external applications as a DataPoint.
newtype NodePeers = NodePeers [PeerInfoPP]

deriving instance Generic NodePeers

instance ToJSON NodePeers
instance FromJSON NodePeers

traceNodePeers
  :: Trace IO NodePeers
  -> [PeerT blk]
  -> IO ()
traceNodePeers tr ev = traceWith tr $ NodePeers (map ppPeer ev)
