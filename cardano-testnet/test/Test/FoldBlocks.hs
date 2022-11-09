{-# LANGUAGE OverloadedStrings #-}

module Test.FoldBlocks where

import qualified Control.Concurrent as IO
import           Control.Concurrent.Async (async, link)
import           Control.Exception (Exception, throw)
import           Control.Monad (forever)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (runExceptT)
import qualified Data.Text as TS
import           Prelude
import qualified System.Directory as IO
import           System.FilePath ((</>))

import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as HE
import qualified Hedgehog.Extras.Test as HE
import qualified Hedgehog.Extras.Test.Base as H

import qualified Cardano.Api as C
import           Cardano.Testnet as TN
import qualified Testnet.Util.Base as U
import           Testnet.Util.Runtime


newtype FoldBlocksException = FoldBlocksException C.FoldBlocksError
instance Exception FoldBlocksException
instance Show FoldBlocksException where
  show (FoldBlocksException a) = TS.unpack $ C.renderFoldBlocksError a

-- | This test starts a testnet with wery short timing, then starts
-- `foldBlocks` in another thread to listen for ledger state, ledger
-- events and block, and on reception writes this to the `lock` `MVar`
-- that main thread blocks on.
prop_foldBlocks :: H.Property
prop_foldBlocks = U.integration . H.runFinallies . H.workspace "chairman" $ \tempAbsBasePath' -> do

  -- Start testnet
  base <- HE.noteM $ liftIO . IO.canonicalizePath =<< HE.getProjectBase
  configTemplate <- H.noteShow $ base </> "configuration/defaults/byron-mainnet/configuration.yaml"
  conf <- HE.noteShowM $
    TN.mkConf (TN.ProjectBase base) (TN.YamlFilePath configTemplate)
      (tempAbsBasePath' <> "/")
      Nothing

  let options = CardanoOnlyTestnetOptions $ cardanoDefaultTestnetOptions
        -- NB! The `activeSlotsCoeff` value is very important for
        -- chain extension for the two-node/one-pool testnet that
        -- `defaultTestnetOptions` define. The default 0.2 often fails
        -- to extend the chain in a reasonable time (< 90s, e.g as the
        -- deadline is defined in Testnet.Cardano).
        { cardanoActiveSlotsCoeff = 0.9 }
  runtime <- testnet options conf

  -- Get socketPath
  socketPathAbs <- do
    socketPath' <- HE.sprocketArgumentName <$> HE.headM (nodeSprocket <$> bftNodes runtime)
    H.note =<< liftIO (IO.canonicalizePath $ tempAbsPath conf </> socketPath')

  configFile <- H.noteShow $ tempAbsPath conf </> "configuration.yaml"

  -- Start foldBlocks in a separate thread
  lock <- liftIO IO.newEmptyMVar
  liftIO $ do
    a <- async $
      -- The `forever` is here because `foldBlocks` drains blocks
      -- until current slot and then quits -- even if there are no
      -- permanent (= older than the k parameter) blocks created. In
      -- that case we simply restart `foldBlocks` again.
      forever $ do
        let handler _env _ledgerState _ledgerEvents _blockInCardanoMode _ = IO.putMVar lock ()
        e <- runExceptT (C.foldBlocks configFile socketPathAbs  C.QuickValidation () handler)
        either (throw . FoldBlocksException) (\_ -> pure ()) e
    link a -- Throw async thread's exceptions in main thread

  -- The `lock` is written to from within the `handler` above. It
  -- tests that `foldBlocks` receives ledger state; once that happens,
  -- handler is called, which then writes to the `lock` and allows the
  -- test to finish.
  _ <- liftIO $ IO.readMVar lock
  H.assert True
