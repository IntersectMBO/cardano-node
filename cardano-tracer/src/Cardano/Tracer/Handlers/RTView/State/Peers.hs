module Cardano.Tracer.Handlers.RTView.State.Peers
  ( PeerAddress
  , PeersForNode
  , Peers
  , addPeer
  , doesPeerExist
  , getPeersAddresses
  , initPeers
  , removePeer
  ) where

import           Cardano.Tracer.Types (NodeId)

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)

type PeerAddress  = Text
type PeersForNode = Set PeerAddress
type Peers        = TVar (Map NodeId PeersForNode)

initPeers :: IO Peers
initPeers = newTVarIO M.empty

addPeer
  :: Peers
  -> NodeId
  -> PeerAddress
  -> IO ()
addPeer peers nodeId peerAddr = atomically $
  modifyTVar' peers $ \currentPeers ->
    case M.lookup nodeId currentPeers of
      Nothing ->
        M.insert nodeId (S.singleton peerAddr) currentPeers
      Just peersForNode ->
        M.adjust (const $ S.insert peerAddr peersForNode) nodeId currentPeers

removePeer
  :: Peers
  -> NodeId
  -> PeerAddress
  -> IO ()
removePeer peers nodeId peerAddr = atomically $
  modifyTVar' peers $ \currentPeers ->
    case M.lookup nodeId currentPeers of
      Nothing -> currentPeers
      Just peersForNode ->
        M.adjust (const $ S.delete peerAddr peersForNode) nodeId currentPeers

doesPeerExist
  :: Peers
  -> NodeId
  -> PeerAddress
  -> IO Bool
doesPeerExist peers nodeId peerAddr = do
  peers' <- readTVarIO peers
  case M.lookup nodeId peers' of
    Nothing -> return False
    Just peersForNode -> return $ S.member peerAddr peersForNode

getPeersAddresses
  :: Peers
  -> NodeId
  -> IO (Set PeerAddress)
getPeersAddresses peers nodeId = do
  peers' <- readTVarIO peers
  case M.lookup nodeId peers' of
    Nothing           -> return S.empty
    Just peersForNode -> return peersForNode
