{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.Test.Tracer (
    testTracer
  ) where

import           Control.Monad.IO.Class
import           Data.IORef

import           Cardano.Logging

testTracer :: MonadIO m
  => IORef [FormattedMessage]
  -> m (Trace m FormattedMessage)
testTracer ioRef = liftIO $
    pure $ Trace $ arrow $ emit output
  where
    output (LoggingContext{}, Nothing, !msg) = liftIO $ do
      modifyIORef ioRef (msg :)
    output (LoggingContext{}, _, _) = pure ()
