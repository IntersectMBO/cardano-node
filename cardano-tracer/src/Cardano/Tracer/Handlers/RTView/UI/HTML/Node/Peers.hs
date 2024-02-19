{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.Node.Peers
  ( mkPeersTable
  , deletePeerRow
  ) where

import           Cardano.Tracer.Handlers.RTView.State.Peers
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Types

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

mkPeersTable :: String -> UI Element
mkPeersTable anId = do
  closeIt <- UI.button #. "delete"
  peerTable <-
    UI.div #. "modal" #+
      [ UI.div #. "modal-background" #+ []
      , UI.div #. "modal-card rt-view-peer-modal" #+
          [ UI.header #. "modal-card-head rt-view-peer-head" #+
              [ UI.p #. "modal-card-title rt-view-peer-title" #+
                  [ string "Peers of "
                  , UI.span ## (anId <> "__node-name-for-peers")
                            #. "has-text-weight-bold"
                            # set text anId
                  ]
              , element closeIt
              ]
          , UI.mkElement "section" #. "modal-card-body rt-view-peer-body" #+
              [ UI.div ## (anId <> "__peer-table-container") #. "table-container" #+
                  [ UI.table ## (anId <> "__peer-table") #. "table is-fullwidth rt-view-peer-table" #+
                      [ UI.mkElement "thead" #+
                          [ UI.tr #+
                              [ UI.th #+ [UI.span # set text "Address"]
                              , UI.th #+ [UI.mkElement "abbr" # set UI.title__ "Block fetch status" # set text "BF status"]
                              , UI.th #+ [UI.span # set text "Slot no."]
                              , UI.th #+ [UI.mkElement "abbr" # set UI.title__ "Requests in flight" # set text "Req"]
                              , UI.th #+ [UI.mkElement "abbr" # set UI.title__ "Blocks in flight" # set text "Blk"]
                              , UI.th #+ [UI.mkElement "abbr" # set UI.title__ "Bytes in flight" # set text "Bts"]
                              ]
                          ]
                      , UI.mkElement "tbody" ## (anId <> "__node-peers-tbody") #+ []
                      ]
                  ]
              ]
          ]
      ]
  on UI.click closeIt . const $ element peerTable #. "modal"
  return peerTable

-- | The peer was disconnected, so its row should be deleted.
deletePeerRow
  :: UI.Window
  -> NodeId
  -> PeerAddress
  -> UI ()
deletePeerRow window (NodeId anId) peerAddr = do
  let peerRowElId = anId <> peerAddr <> "__node-peer-row"
  findAndDo window peerRowElId delete'
