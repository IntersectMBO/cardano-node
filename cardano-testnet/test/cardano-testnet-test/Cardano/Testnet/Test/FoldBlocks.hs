{-# LANGUAGE OverloadedStrings #-}

module Cardano.Testnet.Test.FoldBlocks where

import           Cardano.Api hiding (cardanoEra)
import qualified Cardano.Api as C

import           Cardano.Testnet as TN

import           Prelude

import qualified Control.Concurrent as IO
import           Control.Concurrent.Async (async, link)
import           Control.Exception (Exception, throw)
import           Control.Monad (forever)
import           Control.Monad.Trans.Except (runExceptT)
import qualified Data.Text as TS
import qualified System.Directory as IO
import           System.FilePath ((</>))

import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as HE
import qualified Hedgehog.Extras.Test as HE
import qualified Hedgehog.Extras.Test.Base as H

import qualified Testnet.Property.Utils as H
import           Testnet.Runtime

newtype FoldBlocksException = FoldBlocksException C.FoldBlocksError
instance Exception FoldBlocksException
instance Show FoldBlocksException where
  show (FoldBlocksException a) = TS.unpack $ C.renderFoldBlocksError a

-- | This test starts a testnet with wery short timing, then starts
-- `foldBlocks` in another thread to listen for ledger state, ledger
-- events and block, and on reception writes this to the `lock` `MVar`
-- that main thread blocks on.
prop_foldBlocks :: H.Property
prop_foldBlocks = H.integrationRetryWorkspace 2 "foldblocks" $ \tempAbsBasePath' -> do

  -- Start testnet
  conf <- HE.noteShowM $ TN.mkConf (tempAbsBasePath' <> "/")

  let tempAbsPath' = unTmpAbsPath $ tempAbsPath conf
      era = BabbageEra
      options = cardanoDefaultTestnetOptions
                          { cardanoNodes = cardanoDefaultTestnetNodeOptions
                          , cardanoEpochLength = 1000
                          , cardanoSlotLength = 0.02
                          , cardanoNodeEra = AnyCardanoEra era -- TODO: We should only support the latest era and the upcoming era
                          }
      fastTestnetOptions = CardanoOnlyTestnetOptions options
  runtime <- testnet fastTestnetOptions conf

  -- Get socketPath
  socketPathAbs <- do
    socketPath' <- HE.sprocketArgumentName <$> HE.headM (nodeSprocket . poolRuntime <$>  poolNodes runtime)
    H.noteIO (IO.canonicalizePath $ tempAbsPath' </> socketPath')

  configFile <- H.noteShow $ tempAbsPath' </> "configuration.yaml"

  -- Start foldBlocks in a separate thread
  lock <- H.evalIO IO.newEmptyMVar
  H.evalIO $ do
    a <- async $
      -- The `forever` is here because `foldBlocks` drains blocks
      -- until current slot and then quits -- even if there are no
      -- permanent (= older than the k parameter) blocks created. In
      -- that case we simply restart `foldBlocks` again.
      forever $ do
        let handler _env _ledgerState _ledgerEvents _blockInCardanoMode _ = IO.putMVar lock ()
        e <- runExceptT (C.foldBlocks (File configFile) (C.File socketPathAbs) C.QuickValidation () handler)
        either (throw . FoldBlocksException) (\_ -> pure ()) e
    link a -- Throw async thread's exceptions in main thread

  -- The `lock` is written to from within the `handler` above. It
  -- tests that `foldBlocks` receives ledger state; once that happens,
  -- handler is called, which then writes to the `lock` and allows the
  -- test to finish.
  _ <- H.evalIO $ IO.readMVar lock
  H.assert True
