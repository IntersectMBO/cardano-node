{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Node.Tracing.Peers
  ( NodePeers (..)
  , traceNodePeers
  ) where

import           Cardano.Logging
import           Cardano.Node.Tracing.Tracers.Peer (PeerT, ppPeer)

import           Control.DeepSeq (NFData)
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Text (Text)
import           GHC.Generics (Generic)

type PeerInfoPP = Text -- The result of 'ppPeer' function.

-- | This type contains an information about current peers of the node.
--   It will be asked by external applications as a DataPoint.
newtype NodePeers = NodePeers [PeerInfoPP]

deriving instance Generic NodePeers
deriving instance NFData NodePeers

instance ToJSON NodePeers
instance FromJSON NodePeers


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
