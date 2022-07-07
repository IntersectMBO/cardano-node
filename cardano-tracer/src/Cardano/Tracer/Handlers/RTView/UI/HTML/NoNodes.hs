{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.NoNodes
  ( mkNoNodesInfo
  ) where

import           Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import           Data.String.QQ
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.Utils

-- | If the user doesn't see connected nodes - possible reason of it is
--   misconfiguration of 'cardano-tracer' and/or 'cardano-node'.
--   So we have to show basic explanation, which is based on current
--   configuration of 'cardano-tracer'.
mkNoNodesInfo :: Network -> UI Element
mkNoNodesInfo networkConfig = do
  closeIt <- UI.button #. "delete" # set (UI.attr "aria-label") "delete"
  infoNote <-
    UI.mkElement "article" ## "no-nodes-info"
                           #. "container message is-link rt-view-no-nodes-info" #+
      [ UI.div #. "message-header" #+
          [ UI.p # set text "«Hey, where are my nodes?»"
          , element closeIt
          ]
      , UI.div #. "message-body" #+
          [ UI.p #+
              [ UI.span # set UI.html pleaseWait
              ]
          , UI.p #. "mt-5" #+
              [ string intro
              ]
          , UI.p #. "mt-5" #+
              [ UI.span # set UI.html cardanoTracerNote
              ]
          , UI.p #. "mt-5" #+
              [ UI.span # set UI.html cardanoNodeNote
              ]
          , UI.p #. "mt-5" #+
              [ UI.span # set UI.html nodeNameNote
              ]
          , UI.p #. "mt-5" #+
              [ UI.span # set UI.html sshNote
              ]
          , UI.p #. "mt-5" #+
              [ string "For more details, please read "
              , UI.anchor # set UI.href "https://github.com/input-output-hk/cardano-node/blob/master/cardano-tracer/docs/cardano-tracer.md#configuration"
                          # set text "our documentation"
                          # set UI.target "_blank"
              , image "rt-view-href-icon" externalLinkSVG
              , string "."
              ]
          ]
      ]
  on UI.click closeIt . const $ element infoNote # hideIt
  return infoNote
 where
  pleaseWait =
    "If your nodes and <code>cardano-tracer</code> are configured properly, "
    <> "the connection between them will be established automatically, "
    <> "but it can take some time."

  intro =
    "However, if there is no connection after 1 minute, please check your configuration files."

  cardanoTracerNote =
    case networkConfig of
      AcceptAt (LocalSocket p) ->
        "Currently, your <code>cardano-tracer</code> is configured as a server, "
        <> "so it accepts connections from your nodes via the local socket <code>"
        <> p <> "</code>."
      ConnectTo addrs ->
        let manySocks = NE.length addrs > 1 in
        "Currently, your <code>cardano-tracer</code> is configured as a client, "
        <> "so it connects to your "
        <> (if manySocks then "nodes" else "node")
        <> " via the local "
        <> (if manySocks
              then
                let socks = map (\(LocalSocket p) -> "<code>" <> p <> "</code>") $ NE.toList addrs
                in "sockets " <> intercalate ", " socks <> "."
              else
                "socket <code>" <> let LocalSocket p = NE.head addrs in p <> "</code>.")

  cardanoNodeNote =
    case networkConfig of
      AcceptAt (LocalSocket _p) ->
        "Correspondingly, your nodes tracing sockets should be configured to initiate connections. Make sure their command line invocations "
        <> "contains <code>--tracer-socket-path-connect LOCAL-SOCKET</code>."
      ConnectTo{} ->
        "Correspondingly, your nodes tracing sockets should be configured to accept connections. Make sure their command line invocations "
        <> "contains <code>--tracer-socket-path-accept LOCAL-SOCKET</code>."

  nodeNameNote =
    "Also, please add a meaningful name for your nodes using <code>TraceOptionNodeName</code> field. "
    <> "For example: <pre>" <> traceOptionNodeName <> "</pre>"

  sshNote =
    "If your <code>cardano-tracer</code> and your nodes are running on different machines, the only "
    <> "way to connect them is SSH tunneling with your credentials."

traceOptionNodeName :: String
traceOptionNodeName = [s|
"TraceOptionNodeName": "stk-a-1-IOG1"
|]
