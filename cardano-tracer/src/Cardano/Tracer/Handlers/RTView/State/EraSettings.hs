module Cardano.Tracer.Handlers.RTView.State.EraSettings
  ( EraSettings (..)
  , ErasSettings
  , addEraSettings
  , initErasSettings
  ) where

import           Cardano.Tracer.Types (NodeId)

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)

data EraSettings = EraSettings
  { esEra             :: !Text
  , esSlotLengthInS   :: !Int
  , esEpochLength     :: !Int
  , esKESPeriodLength :: !Int
  } deriving (Eq, Show)

type ErasSettings = TVar (Map NodeId EraSettings)

initErasSettings :: IO ErasSettings
initErasSettings = newTVarIO M.empty

addEraSettings
  :: ErasSettings
  -> NodeId
  -> EraSettings
  -> IO ()
addEraSettings nodesSettings nodeId settingsForIt = atomically $
  modifyTVar' nodesSettings $ \currentSettings ->
    case M.lookup nodeId currentSettings of
      Nothing ->
        M.insert nodeId settingsForIt currentSettings
      Just savedSettings ->
        -- The settings for the same era shouldn't be changed.
        if esEra savedSettings == esEra settingsForIt
          then currentSettings
          else M.adjust (const settingsForIt) nodeId currentSettings
