{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.Tracer.Handlers.RTView.State.DataPoint
  ( getDataPointFromNode
  ) where

import           Control.Concurrent.STM.TVar (readTVarIO)
import           Data.Aeson (FromJSON, decode')
import           Data.Functor ((<&>))
import qualified Data.Map.Strict as M

import           Trace.Forward.Utils.DataPoint (askForDataPoints)
import           Trace.Forward.Protocol.DataPoint.Type (DataPointName)

import           Cardano.Tracer.Types

-- | There are different information about the node that should be asked
--   _explicitly_ as 'DataPoint's. RTView needs such a data (for example,
--   node's basic info) to display them on the web-page. Each 'DataPoint'
--   contains lazy bytestring that can be decoded to the value of particular type.
getDataPointFromNode
  :: FromJSON a
  => DataPointAskers
  -> NodeId
  -> DataPointName
  -> IO (Maybe a)
getDataPointFromNode dpAskers nodeId dpName =
  readTVarIO dpAskers <&> M.lookup nodeId >>= \case
    Nothing -> return Nothing
    Just dpAsker ->
      askForDataPoints dpAsker [dpName] <&> lookup dpName >>= \case
        Just (Just rawValue) -> return $ decode' rawValue
        _ -> return Nothing





















