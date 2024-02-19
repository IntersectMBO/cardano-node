{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.Update.NodeInfo
  ( askNSetNodeInfo
  ) where

import           Cardano.Node.Startup (NodeInfo (..))
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Handlers.RTView.Update.Utils
import           Cardano.Tracer.Types

import           Control.Monad (forM_)
import           Control.Monad.Extra (whenJustM)
import           Data.Set (Set)
import qualified Data.Text as T
import           Data.Time.Format (defaultTimeLocale, formatTime)

import           Graphics.UI.Threepenny.Core

askNSetNodeInfo
  :: TracerEnv
  -> Set NodeId
  -> DisplayedElements
  -> UI ()
askNSetNodeInfo TracerEnv{teDPRequestors, teCurrentDPLock} newlyConnected displayedElements =
  forM_ newlyConnected $ \nodeId@(NodeId anId) ->
    whenJustM (liftIO $ askDataPoint teDPRequestors teCurrentDPLock nodeId "NodeInfo") $ \ni -> do
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

      setProtocol (niProtocol ni) (anId <> "__node-protocol")

      window <- askWindow
      let nodeStartElId = anId <> "__node-start-time"
      setTime window (niStartTime ni) nodeStartElId
      setTime window (niSystemStartTime ni) (anId <> "__node-system-start-time")

      liftIO $ saveDisplayedValue displayedElements nodeId nodeStartElId (T.pack . show $ niStartTime ni)
      liftIO $ saveDisplayedValue displayedElements nodeId nodeNameElId (niName ni)
 where
  setProtocol p id' = do
    justCleanText id'
    case p of
      "Byron"   -> setTextValue id' "Byron"
      "Shelley" -> setTextValue id' "Shelley"
      _         -> setTextValue id' "Cardano"

  setTime window ts id' = do
    justCleanText id'
    let time = formatTime defaultTimeLocale "%D %T" ts
    findAndAdd [string time] window id'
