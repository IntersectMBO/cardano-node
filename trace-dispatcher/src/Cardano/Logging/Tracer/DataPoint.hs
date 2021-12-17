{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.Tracer.DataPoint
  (
    dataPointTracer
  ) where

import           Control.Concurrent.STM.TVar (modifyTVar)
import           Control.Monad.IO.Class
import           Control.Monad.STM (atomically)
import           Data.Map.Strict (insert)
import           Data.List (intersperse)
import           Data.Text (Text)
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder (fromText, singleton, toLazyText)

import qualified Control.Tracer as T
import           Trace.Forward.Utils.DataPoint (DataPoint (..), DataPointStore)

-- import           Cardano.Logging.DocuGenerator
import           Cardano.Logging.Types
import           Cardano.Logging.Utils (uncurry3)

---------------------------------------------------------------------------

dataPointTracer :: forall m. MonadIO m
  => DataPointStore -- TVar (HM.HashMap DataPointName DataPoint)
  -> Trace m DataPoint
dataPointTracer dataPointStore =
    Trace $ T.arrow $ T.emit $ uncurry3 output
  where
    output ::
         LoggingContext
      -> Maybe TraceControl
      -> DataPoint
      -> m ()
    output LoggingContext {..} Nothing val =
      liftIO $ atomically $ do
         modifyTVar
          dataPointStore
          (insert
            (nameSpaceToText lcNamespace)
            val)
    output LoggingContext {} (Just Reset) _msg = liftIO $ do
      pure ()
    output _lk (Just _c@Document {}) _val = do
      pure ()
      -- TODO docIt DataPoint (lk, Just c, val)
    output LoggingContext {} _ _a = pure ()

    nameSpaceToText :: Namespace -> Text
    nameSpaceToText namespace = toStrict $ toLazyText $
      mconcat (intersperse (singleton '.')
        (map fromText namespace))
