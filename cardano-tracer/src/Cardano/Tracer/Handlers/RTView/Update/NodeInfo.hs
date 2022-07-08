{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.Update.NodeInfo
  ( askNSetNodeInfo
  ) where

import           Control.Concurrent.Extra (Lock)
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
  -> Lock
  -> Set NodeId
  -> DisplayedElements
  -> UI ()
askNSetNodeInfo window dpRequestors currentDPLock newlyConnected displayedElements =
  unless (S.null newlyConnected) $
    forM_ newlyConnected $ \nodeId@(NodeId anId) ->
      whenJustM (liftIO $ askDataPoint dpRequestors currentDPLock nodeId "NodeInfo") $ \ni -> do
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
    case p of
      "Byron"   -> setTextValue id' "Byron"
      "Shelley" -> setTextValue id' "Shelley"
      _         -> setTextValue id' "Cardano"

  setTime ts id' = do
    justCleanText id'
    let time = formatTime defaultTimeLocale "%b %e, %Y %T" ts
        tz   = formatTime defaultTimeLocale "%Z" ts
    findAndAdd [ string time
               , UI.span #. "has-text-weight-normal is-size-6 ml-2" # set text tz
               ] window id'
