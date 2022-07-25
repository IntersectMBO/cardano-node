{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Tracer.Handlers.RTView.Utils
  ( forConnected
  , forConnected_
  , forConnectedUI
  , forConnectedUI_
  ) where

import           Control.Concurrent.STM.TVar (readTVarIO)
import qualified Data.Set as S
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Environment
import           Cardano.Tracer.Types

forConnected :: TracerEnv -> (NodeId -> IO b) -> IO [b]
forConnected TracerEnv{teConnectedNodes} action =
  mapM action . S.toList =<< readTVarIO teConnectedNodes

forConnected_ :: TracerEnv -> (NodeId -> IO ()) -> IO ()
forConnected_ TracerEnv{teConnectedNodes} action =
  mapM_ action =<< readTVarIO teConnectedNodes

forConnectedUI :: TracerEnv -> (NodeId -> UI b) -> UI [b]
forConnectedUI TracerEnv{teConnectedNodes} action =
  mapM action . S.toList =<< liftIO (readTVarIO teConnectedNodes)

forConnectedUI_ :: TracerEnv -> (NodeId -> UI ()) -> UI ()
forConnectedUI_ TracerEnv{teConnectedNodes} action =
  mapM_ action =<< liftIO (readTVarIO teConnectedNodes)
