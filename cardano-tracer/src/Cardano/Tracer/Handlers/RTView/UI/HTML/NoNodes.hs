{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.NoNodes
  ( mkNoNodesInfo
  , hideNoNodes
  , showNoNodes
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
  window <- askWindow
  closeIt <- UI.button #. "delete" # set (UI.attr "aria-label") "delete"
  on UI.click closeIt . const $ findAndHide window "no-nodes-info"

  UI.div ## "no-nodes" #. "container is-max-widescreen" #+
    [ UI.p #. "has-text-centered" #+
        [ image "rt-view-no-nodes-icon" noNodesSVG ## "no-nodes-icon"
        ]
    , UI.p ## "no-nodes-message" #. "rt-view-no-nodes-message" #+
        [ string "There are no connected nodes. Yet."
        ]
    , UI.div #. "rt-view-no-nodes-progress" #+
        [ UI.mkElement "progress" ## "no-nodes-progress"
                                  #. "progress is-small is-link"
                                  # set UI.value "0"
                                  # set (attr "max") "60"
        ]
    , UI.mkElement "article" ## "no-nodes-info"
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
                [ UI.span # set UI.html localNote
                ]
            , UI.p #. "mt-5" #+
                [ UI.span # set UI.html sshNote
                , UI.anchor # set UI.href "https://github.com/input-output-hk/cardano-node/blob/master/cardano-tracer/docs/cardano-tracer.md#distributed-scenario"
                            # set text "here"
                            # set UI.target "_blank"
                , image "rt-view-href-icon" externalLinkSVG
                , string "."
                ]
            , UI.p #. "mt-5" #+
                [ UI.span # set UI.html nodeNameNote
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
    ]
 where
  pleaseWait =
    "If your nodes and <code>cardano-tracer</code> are configured properly, "
    <> "the connection between them will be established automatically, "
    <> "but it may take some time."

  intro =
    "However, if there is no connection after 1 minute, please check your configuration."

  cardanoTracerNote =
    case networkConfig of
      AcceptAt (LocalSocket p) ->
        "Currently, your <code>cardano-tracer</code> is configured as a server, "
        <> "so it accepts connections from your nodes via the local socket <code>"
        <> p <> "</code>. Correspondingly, your nodes should be configured to "
        <> "initiate connections using tracing socket."
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
        <> " Correspondingly, our nodes should be configured to accept connections using tracing socket."

  localNote =
    case networkConfig of
      AcceptAt (LocalSocket p) ->
        "Thus, if your <code>cardano-tracer</code> and your nodes are running on the same machine, run "
         <> "the nodes with this argument: <code>--tracer-socket-path-connect " <> p <> "</code>."
      ConnectTo{} ->
        "Thus, if your <code>cardano-tracer</code> and your nodes are running on the same machine, run "
         <> "the nodes with this argument: <code>--tracer-socket-path-accept SOCKET</code>, where "
         <> "<code>SOCKET</code> is the path to one of sockets specified in <code>ConnectTo</code>-list "
         <> "in <code>cardano-tracer</code> configuration file."

  sshNote =
    "But if your <code>cardano-tracer</code> and your nodes are running on different machines, the only "
    <> "way to connect them is SSH tunneling with your credentials. Please see an explanation with the "
    <> "real-life example "

  nodeNameNote =
    "Also, please add a meaningful name for your nodes using <code>TraceOptionNodeName</code> field"
    <> " in their configuration files. For example: <pre>" <> traceOptionNodeName <> "</pre>"

traceOptionNodeName :: String
traceOptionNodeName = [s|
"TraceOptionNodeName": "stk-a-1-IOG1"
|]

hideNoNodes, showNoNodes :: UI.Window -> UI.Timer -> UI ()
hideNoNodes window aTimer = do
  findAndHide window "no-nodes"
  findAndHide window "no-nodes-info"
  UI.stop aTimer
showNoNodes window aTimer = do
  let elId = "no-nodes-progress"
  findAndSet (set UI.value "0") window elId
  findAndSet visibleOnly window elId
  findAndShow window "no-nodes"
  findAndShow window "no-nodes-info"
  UI.start aTimer
