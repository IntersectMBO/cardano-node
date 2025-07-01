{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Cardano.Logging.Tracer.DataPoint
  (
    DataPoint (..)
  , DataPointStore
  , initDataPointStore
  , writeToStore
  , dataPointTracer
  , mkDataPointTracer
  ) where

import           Cardano.Logging.DocuGenerator
import           Cardano.Logging.Trace
import           Cardano.Logging.Types

import Control.DeepSeq ( NFData, NFData, deepseq )
import           Control.Monad.IO.Class
import qualified Control.Tracer as NT
import           Data.Text (Text, intercalate)

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar
import           Data.Aeson
import qualified Data.Map.Strict as M

---------------------------------------------------------------------------
--
-- | Type wrapper for some value of type 'v'. The only reason we need this
--   wrapper is an ability to store different values in the same 'DataPointStore'.
--
--   Please note that when the acceptor application will read the value of type 'v'
--   from the store, this value is just as unstructured JSON, but not Haskell
--   value of type 'v'. That's why 'FromJSON' instance for type 'v' should be
--   available for the acceptor application, to decode unstructured JSON.
--
data DataPoint where
  DataPoint :: (ToJSON v, NFData v) => v -> DataPoint

type DataPointName   = Text
type DataPointStore = TVar (M.Map DataPointName DataPoint)

initDataPointStore :: IO DataPointStore
initDataPointStore = newTVarIO M.empty

-- | Write 'DataPoint' to the store.
writeToStore
  :: DataPointStore
  -> DataPointName
  -> DataPoint
  -> IO ()
writeToStore dpStore dpName (DataPoint obj) = atomically $
  modifyTVar' dpStore $ \store ->
    if dpName `M.member` store
      then M.adjust (const (DataPoint (deepseq obj obj))) dpName store
      else M.insert dpName (DataPoint (deepseq obj obj)) store

dataPointTracer :: forall m. MonadIO m
  => DataPointStore
  -> Trace m DataPoint
dataPointTracer dataPointStore =
    Trace $ NT.arrow $ NT.emit $ uncurry output
  where
    output ::
         LoggingContext
      -> Either TraceControl DataPoint
      -> m ()
    output LoggingContext {..} (Right val) =
      liftIO $ writeToStore dataPointStore (nameSpaceToText (lcNSPrefix ++ lcNSInner)) val
    output LoggingContext {} (Left TCReset) = liftIO $ do
      pure ()
    output lk (Left c@TCDocument {}) = do
      docIt DatapointBackend (lk, Left c)
    output LoggingContext {} _  = pure ()

    nameSpaceToText :: [Text] -> Text
    nameSpaceToText = intercalate "."

-- A simple dataPointTracer which supports building a namespace.
mkDataPointTracer :: forall dp. (ToJSON dp, MetaTrace dp, NFData dp)
  => Trace IO DataPoint
  -> IO (Trace IO dp)
mkDataPointTracer trDataPoint = do
    let tr = NT.contramap DataPoint trDataPoint
    pure $ withInnerNames tr
