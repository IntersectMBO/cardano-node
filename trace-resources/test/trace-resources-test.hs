{-# OPTIONS_GHC -Wno-unused-imports  #-}

import           Control.Monad.IO.Class
import           Data.IORef
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Cardano.Logging
import           Cardano.Logging.Resources
import           Cardano.Logging.Resources.Types


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = localOption (QuickCheckTests 10) $ testGroup "trace-resources"
    [ testProperty "resources available" playScript
    ]

-- | Plays a script in a single thread
playScript :: Property
playScript = ioProperty $ do
  stdoutTrRef     <- newIORef []
  stdoutTracer'   <- testTracer stdoutTrRef
  forwardTrRef    <- newIORef []
  forwardTracer'  <- testTracer forwardTrRef
  ekgTrRef        <- newIORef []
  ekgTracer'      <- testTracer ekgTrRef
  tr              <- mkCardanoTracer
                      stdoutTracer'
                      forwardTracer'
                      (Just ekgTracer')
                      "Test"
                      (const ["ResourceStats"])
                      (const Info)
                      (const Public)
  configureTracers emptyTraceConfig docResourceStats [tr]
  traceIt tr 10

traceIt :: Trace IO ResourceStats -> Int -> IO Bool
traceIt _ 0 = pure True
traceIt tr n = do
  mbResources <- readResourceStats
  case mbResources of
    Nothing -> pure False
    Just res -> do
      traceWith tr res
      traceIt tr (n - 1)


testTracer :: MonadIO m
  => IORef [FormattedMessage]
  -> m (Trace m FormattedMessage)
testTracer ioRef = liftIO $ do
    pure $ Trace $ arrow $ emit output'
  where
    output' (LoggingContext{}, Nothing, msg) = liftIO $ do
      modifyIORef ioRef (msg :)
    output' (LoggingContext{}, _, _) = pure ()
