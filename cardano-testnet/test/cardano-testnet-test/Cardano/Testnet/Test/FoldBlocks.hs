{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.FoldBlocks where

import           Cardano.Api hiding (cardanoEra)
import qualified Cardano.Api as Api
import           Cardano.Api.Error
import qualified Cardano.Api.Shelley as Api

import           Cardano.Testnet as TN

import           Prelude

import           Control.Concurrent.Async (async, link)
import qualified Control.Concurrent.STM as STM
import           Control.Exception (Exception, throw)
import           Control.Monad
import qualified System.Directory as IO
import           System.FilePath ((</>))
import qualified System.IO as IO

import qualified Testnet.Property.Utils as H
import           Testnet.Runtime

import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as H
import qualified Hedgehog.Extras.Test as H

newtype FoldBlocksException = FoldBlocksException Api.FoldBlocksError
instance Exception FoldBlocksException
instance Show FoldBlocksException where
  show (FoldBlocksException a) = displayError a

prop_foldBlocks :: H.Property
prop_foldBlocks = H.integrationRetryWorkspace 2 "foldblocks" $ \tempAbsBasePath' -> do
  -- Start testnet
  conf <- TN.mkConf tempAbsBasePath'

  let tempAbsPath' = unTmpAbsPath $ tempAbsPath conf
      era = BabbageEra
      options = cardanoDefaultTestnetOptions
                  { cardanoNodeEra = AnyCardanoEra era -- TODO: We should only support the latest era and the upcoming era
                  }

  runtime@TestnetRuntime{configurationFile} <- cardanoTestnetDefault options conf

  -- Get socketPath
  socketPathAbs <- do
    socketPath' <- H.sprocketArgumentName <$> H.headM (poolSprockets runtime)
    H.noteIO (IO.canonicalizePath $ tempAbsPath' </> socketPath')

  H.evalIO $ IO.appendFile "out.txt" "test 3\n"

  tDone <- H.evalIO $ STM.newTVarIO False

  -- Start foldBlocks in a separate thread
  H.evalIO $ do
    a <- async $
      forM (id @[Int] [1, 2 ..]) $ \i -> do
        IO.appendFile "out.txt" $ "== " <> show i <> "==\n"
        let handler :: Env -> LedgerState -> [Api.LedgerEvent] -> BlockInMode -> () -> IO ((), FoldStatus)
            handler _env _ledgerState _ledgerEvents blockInCardanoMode _ = do
              IO.appendFile "out.txt" $ show i <> ": " <> take 400 (show blockInCardanoMode) <> "\n"
              STM.atomically $ STM.writeTVar tDone True
              pure ((), ContinueFold)
        e <- runExceptT (Api.foldBlocks (File configurationFile) (Api.File socketPathAbs) Api.QuickValidation () handler)
        either (throw . FoldBlocksException) (\_ -> pure ()) e
    link a -- Throw async thread's exceptions in main thread

  _ <- H.evalIO $ H.timeout 30_000_000 $ STM.atomically $ do
      done <- STM.readTVar tDone
      unless done STM.retry

  H.evalIO $ IO.appendFile "out.txt" "test done\n"

  H.threadDelay 10_000_000

  H.evalIO $ IO.appendFile "out.txt" "test end\n"

  H.assert True

