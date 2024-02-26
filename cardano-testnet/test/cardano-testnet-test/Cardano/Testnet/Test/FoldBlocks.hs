{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Cardano.Testnet.Test.FoldBlocks where

import           Cardano.Api hiding (cardanoEra)
import qualified Cardano.Api as Api
import           Cardano.Api.Error (displayError)
import qualified Cardano.Api.Shelley as Api

import           Cardano.Testnet as TN

import qualified Control.Concurrent as IO
import           Control.Concurrent.Async (async, link)
import           Control.Exception (Exception, throw)
import           Control.Monad (forever)
import qualified System.Directory as IO
import           System.FilePath ((</>))

import qualified Testnet.Property.Utils as H
import           Testnet.Runtime

import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as HE
import qualified Hedgehog.Extras.Test as HE
import qualified Hedgehog.Extras.Test.Base as H

newtype FoldBlocksException = FoldBlocksException Api.FoldBlocksError
instance Exception FoldBlocksException
instance Show FoldBlocksException where
  show (FoldBlocksException a) = displayError a

-- | This test starts a testnet with wery short timing, then starts
-- `foldBlocks` in another thread to listen for ledger state, ledger
-- events and block, and on reception writes this to the `lock` `MVar`
-- that main thread blocks on.
prop_foldBlocks :: H.Property
prop_foldBlocks = H.integrationRetryWorkspace 2 "foldblocks" $ \tempAbsBasePath' -> do
  -- Start testnet
  conf <- TN.mkConf tempAbsBasePath'

  let tempAbsPath' = unTmpAbsPath $ tempAbsPath conf
      era = BabbageEra
      options = cardanoDefaultTestnetOptions
                          { cardanoNodes = cardanoDefaultTestnetNodeOptions
                          , cardanoSlotLength = 0.1
                          , cardanoNodeEra = AnyCardanoEra era -- TODO: We should only support the latest era and the upcoming era
                          }

  runtime@TestnetRuntime{configurationFile} <- cardanoTestnetDefault options conf

  -- Get socketPath
  socketPathAbs <- do
    socketPath' <- HE.sprocketArgumentName <$> HE.headM (nodeSprocket . poolRuntime <$>  poolNodes runtime)
    H.noteIO (IO.canonicalizePath $ tempAbsPath' </> socketPath')

  -- Start foldBlocks in a separate thread
  lock <- H.evalIO IO.newEmptyMVar
  H.evalIO $ do
    a <- async $
      -- The `forever` is here because `foldBlocks` drains blocks
      -- until current slot and then quits -- even if there are no
      -- permanent (= older than the k parameter) blocks created. In
      -- that case we simply restart `foldBlocks` again.
      forever $ do
        let handler :: Env -> LedgerState -> [Api.LedgerEvent] -> BlockInMode -> () -> IO ((), FoldStatus)
            handler _env _ledgerState _ledgerEvents _blockInCardanoMode _ = (, ContinueFold) <$> IO.putMVar lock ()
        e <- runExceptT (Api.foldBlocks (File configurationFile) (Api.File socketPathAbs) Api.QuickValidation () handler)
        either (throw . FoldBlocksException) (\_ -> pure ()) e
    link a -- Throw async thread's exceptions in main thread

  -- The `lock` is written to from within the `handler` above. It
  -- tests that `foldBlocks` receives ledger state; once that happens,
  -- handler is called, which then writes to the `lock` and allows the
  -- test to finish.
  _ <- H.evalIO $ IO.readMVar lock
  H.assert True
