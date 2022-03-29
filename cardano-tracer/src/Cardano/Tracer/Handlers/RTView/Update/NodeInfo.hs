{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.Update.NodeInfo
  ( askNSetNodeInfo
  ) where

import           Control.Monad (forM_, unless)
import           Control.Monad.Extra (whenJustM)
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Node.Startup (NodeInfo (..))

import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Handlers.RTView.Update.Utils
import           Cardano.Tracer.Types

askNSetNodeInfo
  :: UI.Window
  -> DataPointRequestors
  -> Set NodeId
  -> DisplayedElements
  -> UI ()
askNSetNodeInfo window dpRequestors newlyConnected displayedElements =
  unless (S.null newlyConnected) $
    forM_ newlyConnected $ \nodeId@(NodeId anId) ->
      whenJustM (liftIO $ askDataPoint dpRequestors nodeId "NodeInfo") $ \ni -> do
        let nodeNameElId = anId <> "__node-name"
            shortName = shortenName $ niName ni

        setTextValues
          [ (nodeNameElId,             shortName)
          , (anId <> "__node-version", niVersion ni)
          , (anId <> "__node-commit",  T.take 7 $ niCommit ni)
          , (anId <> "__node-name-for-peers", shortName)
          , (anId <> "__node-name-for-ekg-metrics", shortName)
          , (anId <> "__node-name-for-errors", shortName)
          ]

        findAndSet (set UI.href $ nodeLink (niCommit ni)) window (anId <> "__node-commit")

        setProtocol (niProtocol ni) (anId <> "__node-protocol")

        let nodeStartElId = anId <> "__node-start-time"
        setTime (niStartTime ni) nodeStartElId
        setTime (niSystemStartTime ni) (anId <> "__node-system-start-time")

        liftIO $ saveDisplayedValue displayedElements nodeId nodeStartElId (T.pack . show $ niStartTime ni)
        liftIO $ saveDisplayedValue displayedElements nodeId nodeNameElId (niName ni)
 where
  nodeLink commit = T.unpack $ "https://github.com/input-output-hk/cardano-node/commit/" <> T.take 7 commit

  setProtocol p id' = do
    justCleanText id'
    let byronTag   = UI.span #. "tag is-warning is-rounded is-medium" # set text "Byron"
        shelleyTag = UI.span #. "tag is-info is-rounded is-medium ml-3" # set text "Shelley"
    case p of
      "Byron"   -> findAndAdd [byronTag] window id'
      "Shelley" -> findAndAdd [shelleyTag] window id'
      _         -> findAndAdd [byronTag, shelleyTag] window id'

  setTime ts id' = do
    justCleanText id'
    let time = formatTime defaultTimeLocale "%b %e, %Y %T" ts
        tz   = formatTime defaultTimeLocale "%Z" ts
    findAndAdd [ string time
               , UI.span #. "has-text-weight-normal is-size-6 ml-2" # set text tz
               ] window id'
