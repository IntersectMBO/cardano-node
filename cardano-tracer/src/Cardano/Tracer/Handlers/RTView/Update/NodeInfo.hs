{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.Update.NodeInfo
  ( askNSetNodeInfo
  ) where

import           Control.Monad (forM_)
import           Control.Monad.Extra (whenJustM)
import           Data.Set (Set)
import qualified Data.Text as T
import           Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Node.Startup (NodeInfo (..))

import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Handlers.RTView.Update.Utils
import           Cardano.Tracer.Types

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

      window <- askWindow
      findAndSet (set UI.href $ nodeLink (niCommit ni)) window (anId <> "__node-commit")

      setProtocol (niProtocol ni) (anId <> "__node-protocol")

      let nodeStartElId = anId <> "__node-start-time"
      setTime window (niStartTime ni) nodeStartElId
      setTime window (niSystemStartTime ni) (anId <> "__node-system-start-time")

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

  setTime window ts id' = do
    justCleanText id'
    let time = formatTime defaultTimeLocale "%b %e, %Y %T" ts
        tz   = formatTime defaultTimeLocale "%Z" ts
    findAndAdd [ string time
               , UI.span #. "has-text-weight-normal is-size-6 ml-2" # set text tz
               ] window id'
