{-# LANGUAGE OverloadedStrings  #-}

module Spec.FoldBlocks where

import Prelude
import System.FilePath ((</>))
import qualified System.Directory as IO
import qualified Control.Concurrent as IO
import           Control.Monad (forever)
import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text as TS
import           Control.Concurrent.Async (async, link)
import           Control.Exception (Exception, throw)

import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test as HE
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as HE
import Test.Tasty.Hedgehog (testPropertyNamed)
import Test.Tasty (TestTree, testGroup)

import qualified Cardano.Api as C
import qualified Testnet.Cardano as TN
import qualified Testnet.Conf as TC (Conf (..), ProjectBase (ProjectBase), YamlFilePath (YamlFilePath), mkConf)
import qualified Test.Base as U
import qualified Test.Runtime as U


newtype FoldBlocksException = FoldBlocksException C.FoldBlocksError
instance Exception FoldBlocksException
instance Show FoldBlocksException where
  show (FoldBlocksException a) = TS.unpack $ C.renderFoldBlocksError a

tests :: TestTree
tests = testGroup "FoldBlocks"
  [ testPropertyNamed "foldBlocks receives ledger state" "prop_foldBlocks_fails" prop_foldBlocks
  ]

-- | This test starts a testnet with wery short timing, then starts
-- foldBlocks in another thread to listen for ledger state, ledger
-- events and block, and on reception writes this to an MVar that main
-- thread blocks on.
prop_foldBlocks :: H.Property
prop_foldBlocks = U.integration . H.runFinallies . H.workspace "chairman" $ \tempAbsBasePath' -> do

  -- Start testnet
  base <- HE.noteM $ liftIO . IO.canonicalizePath =<< HE.getProjectBase
  configurationTemplate <- H.noteShow $ base </> "configuration/defaults/byron-mainnet/configuration.yaml"
  conf <- HE.noteShowM $
    TC.mkConf (TC.ProjectBase base) (TC.YamlFilePath configurationTemplate)
      (tempAbsBasePath' <> "/")
      Nothing

  let options = TN.defaultTestnetOptions
        -- NB! The `activeSlotsCoeff` value is very important for
        -- chain extension for the two-node/one-pool testnet that
        -- `defaultTestnetOptions` define. The default 0.2 often fails
        -- to extend the chain in a reasonable time (< 90s, e.g as the
        -- deadline is defined in Testnet.Cardano).
        { TN.activeSlotsCoeff = 0.9 }
  runtime <- TN.testnet options conf

  -- Get socketPath
  socketPathAbs <- do
    socketPath' <- HE.sprocketArgumentName <$> HE.headM (U.nodeSprocket <$> TN.bftNodes runtime)
    H.note =<< liftIO (IO.canonicalizePath $ TC.tempAbsPath conf </> socketPath')

  configurationFile <- H.noteShow $ TC.tempAbsPath conf </> "configuration.yaml"

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
        e <- runExceptT (C.foldBlocks configurationFile socketPathAbs  C.QuickValidation () handler)
        either (throw . FoldBlocksException) (\_ -> pure ()) e
    link a -- Throw async thread's exceptions in main thread

  _ <- liftIO $ IO.readMVar lock
  H.assert True
