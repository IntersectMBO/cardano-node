{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Node.Tracing.Peers
  ( NodePeers (..)
  , traceNodePeers
  ) where

import           Cardano.Logging
import           Cardano.Node.Tracing.Tracers.Peer (PeerT, ppPeer)
import           Cardano.Logging.Types.NodePeers (NodePeers(..))

instance MetaTrace NodePeers where
  namespaceFor NodePeers {}  =
    Namespace [] ["NodePeers"]
  severityFor  (Namespace _ ["NodePeers"]) _ =
    Just Info
  severityFor _ns _ =
    Nothing
  documentFor  (Namespace _ ["NodePeers"]) =
    Just ""
  documentFor _ns =
    Nothing
  allNamespaces = [ Namespace [] ["NodePeers"]]

traceNodePeers
  :: Trace IO NodePeers
  -> [PeerT blk]
  -> IO ()
traceNodePeers tr ev = traceWith tr $ NodePeers (fmap ppPeer ev)
