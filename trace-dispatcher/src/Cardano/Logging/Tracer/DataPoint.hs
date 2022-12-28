{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.Tracer.DataPoint
  (
    dataPointTracer
  , mkDataPointTracer
  ) where

import           Control.Monad.IO.Class
import           Data.Aeson.Types (ToJSON)
import           Data.List (intersperse)
import           Data.Text (Text)
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder (fromText, singleton, toLazyText)

import qualified Control.Tracer as NT
import           Trace.Forward.Utils.DataPoint (DataPoint (..), DataPointStore, writeToStore)

import           Cardano.Logging.Trace
import           Cardano.Logging.Types

---------------------------------------------------------------------------

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
    output LoggingContext {} (Left Reset) = liftIO $ do
      pure ()
    output _lk (Left _c@TCDocument {}) = do
      pure ()
      -- TODO docIt DataPoint (lk, Just c, val)
    output LoggingContext {} _  = pure ()

    nameSpaceToText :: [Text] -> Text
    nameSpaceToText namespace = toStrict $ toLazyText $
      mconcat (intersperse (singleton '.')
        (map fromText namespace))

-- A simple dataPointTracer which supports building a namespace.
mkDataPointTracer :: forall dp. (ToJSON dp, MetaTrace dp)
  => Trace IO DataPoint
  -> IO (Trace IO dp)
mkDataPointTracer trDataPoint = do
    let tr = NT.contramap DataPoint trDataPoint
    pure $ withInnerNames tr