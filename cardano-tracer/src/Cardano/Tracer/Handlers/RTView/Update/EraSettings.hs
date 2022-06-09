{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.EraSettings
  ( runEraSettingsUpdater
  ) where

import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad (forM_, forever)
import           Control.Monad.Extra (whenJust)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Text as T
import           System.Time.Extra (sleep)

import           Cardano.Tracer.Handlers.RTView.State.EraSettings
import           Cardano.Tracer.Handlers.RTView.State.TraceObjects
import           Cardano.Tracer.Handlers.RTView.Update.Utils
import           Cardano.Tracer.Types

runEraSettingsUpdater
  :: ConnectedNodes
  -> ErasSettings
  -> SavedTraceObjects
  -> IO ()
runEraSettingsUpdater connectedNodes settings savedTO = forever $ do
  connected <- readTVarIO connectedNodes
  updateErasSettings connected settings savedTO
  sleep 1.0

updateErasSettings
  :: Set NodeId
  -> ErasSettings
  -> SavedTraceObjects
  -> IO ()
updateErasSettings connected settings savedTO = do
  savedTraceObjects <- readTVarIO savedTO
  forM_ connected $ \nodeId ->
    whenJust (M.lookup nodeId savedTraceObjects) $ \savedTOForNode ->
      whenJust (M.lookup "Startup.ShelleyBased" savedTOForNode) $ \(trObValue, _, _) ->
        -- Example: "Era Alonzo, Slot length 1s, Epoch length 432000, Slots per KESPeriod 129600"
        case T.words $ T.replace "," "" trObValue of
          [_, era, _, _, slotLen, _, _, epochLen, _, _, _, kesPeriod] ->
            addEraSettings settings nodeId $
              EraSettings
                { esEra             = era
                , esSlotLengthInS   = readInt (T.init slotLen) 0
                , esEpochLength     = readInt epochLen 0
                , esKESPeriodLength = readInt kesPeriod 0
                }
          _ -> return ()
