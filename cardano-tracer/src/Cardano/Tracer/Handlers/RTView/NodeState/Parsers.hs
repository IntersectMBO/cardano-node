{-# LANGUAGE FlexibleInstances #-}

module Cardano.RTView.NodeState.Parsers
    ( extractPeersInfo
    ) where

import           Data.Aeson (Object, (.:))
import qualified Data.Aeson as A
import           Data.Text (Text)

import           Cardano.RTView.NodeState.Types (PeerInfo (..))

extractPeersInfo :: Object -> [PeerInfo]
extractPeersInfo peersObj =
  case result of
    A.Success (ConnectedPeers cPeers) ->
      flip map cPeers $ \p ->
        PeerInfo
          { piEndpoint   = peerAddress p
          , piBytesInF   = peerBytesInF p
          , piReqsInF    = peerReqsInF p
          , piBlocksInF  = peerBlocksInF p
          , piSlotNumber = peerSlotNo p
          , piStatus     = peerStatus p
          }
    A.Error _ -> []
 where
  result :: A.Result ConnectedPeers
  result = A.fromJSON $ A.Object peersObj

-- Types for decoding from JSON-representation.

newtype ConnectedPeers = ConnectedPeers [ConnectedPeer]

data ConnectedPeer
  = ConnectedPeer
      { peerBytesInF  :: !Text
      , peerReqsInF   :: !Text
      , peerBlocksInF :: !Text
      , peerAddress   :: !Text
      , peerSlotNo    :: !Text
      , peerStatus    :: !Text
      }

instance A.FromJSON ConnectedPeers where
  parseJSON = A.withObject "ConnectedPeers" $ \v -> ConnectedPeers
    <$> v .: "peers"

instance A.FromJSON ConnectedPeer where
  parseJSON = A.withObject "ConnectedPeer" $ \v -> ConnectedPeer
    <$> v .: "peerBytesInF"
    <*> v .: "peerReqsInF"
    <*> v .: "peerBlocksInF"
    <*> v .: "peerAddress"
    <*> v .: "peerSlotNo"
    <*> v .: "peerStatus"
