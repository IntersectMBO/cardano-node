{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.Peers
  ( updateNodesPeers
  ) where

import           Control.Concurrent.Extra (Lock)
import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad (forM_, void)
import           Control.Monad.Extra (whenJustM)
import           Data.List (find)
import           Data.List.Extra (notNull)
import           Data.Maybe (mapMaybe)
import           Data.Set ((\\))
import qualified Data.Set as S
import           Data.Text (unpack)
import qualified Data.Text as T
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Node.Tracing.Peers

import           Cardano.Tracer.Handlers.RTView.State.Peers
import           Cardano.Tracer.Handlers.RTView.UI.HTML.Node.Peers
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Handlers.RTView.Update.Utils
import           Cardano.Tracer.Types
import           Cardano.Tracer.Utils

updateNodesPeers
  :: UI.Window
  -> ConnectedNodes
  -> DataPointRequestors
  -> Lock
  -> Peers
  -> UI ()
updateNodesPeers window connectedNodes dpRequestors currentDPLock displayedPeers = do
  connected <- liftIO $ readTVarIO connectedNodes
  forM_ connected $ \nodeId -> do
    whenJustM (liftIO $ askDataPoint dpRequestors currentDPLock nodeId "NodePeers") $
      doUpdatePeers window nodeId displayedPeers

doUpdatePeers
  :: UI.Window
  -> NodeId
  -> Peers
  -> NodePeers
  -> UI ()
doUpdatePeers window nodeId@(NodeId anId) displayedPeers (NodePeers peersParts) = do
  -- Update peers number.
  setTextValue (anId <> "__node-peers-num") (showT (length peersParts))
  -- If there is at least one connected peer, we enable 'Details' button.
  findAndSet (set UI.enabled $ notNull peersParts)
             window $ anId <> "__node-peers-details-button"
  -- Update particular info about peers.
  let connectedPeers = getConnectedPeers
      connectedPeersAddresses = getConnectedPeersAddresses
  displayedPeersAddresses <- liftIO $ getPeersAddresses displayedPeers nodeId
  if displayedPeersAddresses /= connectedPeersAddresses
    then do
      -- There are some changes with number of peers: some new were connected
      -- and/or some displayed ones were disconnected.
      let disconnectedPeers   = displayedPeersAddresses \\ connectedPeersAddresses -- Not in connected
          newlyConnectedPeers = connectedPeersAddresses \\ displayedPeersAddresses -- Not in displayed
      deleteRowsForDisconnected disconnectedPeers
      addRowsForNewlyConnected newlyConnectedPeers connectedPeers
    else
      -- No changes with number of peers, only their data was changed.
      updateConnectedPeersData connectedPeers
 where
  getConnectedPeers = S.fromList $
    mapMaybe
      (\peerPart -> let peerData = T.words peerPart in
                    if length peerData == 6 then Just peerData else Nothing
      ) peersParts

  getConnectedPeersAddresses = S.map head getConnectedPeers

  deleteRowsForDisconnected disconnected =
    forM_ disconnected $ \peerAddr -> do
      deletePeerRow window nodeId peerAddr
      liftIO $ removePeer displayedPeers nodeId peerAddr

  addRowsForNewlyConnected newlyConnectedPeers connectedPeers =
    forM_ newlyConnectedPeers $ \peerAddr -> do
      case find (\peerDataList -> head peerDataList == peerAddr) connectedPeers of
        Just [_, status, slotNo, reqsInF, blocksInF, bytesInF] -> do
          let idPrefix = anId <> peerAddr
          addPeerRow idPrefix peerAddr status slotNo reqsInF blocksInF bytesInF
          liftIO $ addPeer displayedPeers nodeId peerAddr
        _ -> return ()

  addPeerRow idPrefix peerAddr status slotNo reqsInF blocksInF bytesInF = do
    let idPrefix' = unpack idPrefix
    whenJustM (UI.getElementById window (unpack anId <> "__node-peers-tbody")) $ \el ->
      void $ element el #+
        [ UI.tr ## (idPrefix' <> "__node-peer-row") #+
            [ UI.td #+
                [ UI.span ## (idPrefix' <> "__address")
                          #. "is-family-monospace"
                          # set text (unpack peerAddr)
                ]
            , UI.td #+
                [ UI.span ## (idPrefix' <> "__status")
                          # set text (unpack status)
                ]
            , UI.td #+
                [ UI.span ## (idPrefix' <> "__slotNo")
                          # set text (unpack $ checkSlot slotNo)
                ]
            , UI.td #+
                [ UI.span ## (idPrefix' <> "__reqsInF")
                          # set text (unpack reqsInF)
                ]
            , UI.td #+
                [ UI.span ## (idPrefix' <> "__blocksInF")
                          # set text (unpack blocksInF)
                ]
            , UI.td #+
                [ UI.span ## (idPrefix' <> "__bytesInF")
                          # set text (unpack bytesInF)
                ]
            ]
        ]

  updateConnectedPeersData connectedPeers = do
    let allPeersData = concatMap collectDataToUpdate (S.toList connectedPeers)
    -- Update values for all peers by one single FFI-call.
    setTextValues allPeersData

  collectDataToUpdate [peerAddr, status, slotNo, reqsInF, blocksInF, bytesInF] =
    let idPrefix = anId <> peerAddr
    in [ (idPrefix <> "__status",    status)
       , (idPrefix <> "__slotNo",    checkSlot slotNo)
       , (idPrefix <> "__reqsInF",   reqsInF)
       , (idPrefix <> "__blocksInF", blocksInF)
       , (idPrefix <> "__bytesInF",  bytesInF)
       ]
  collectDataToUpdate _ = []

  checkSlot slotNo = if slotNo == "???" then "—" else slotNo
