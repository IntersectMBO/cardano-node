{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Testnet.Test.FoldEpochState where

import           Cardano.Api hiding (cardanoEra)
import qualified Cardano.Api as Api

import           Cardano.Testnet as TN

import           Prelude

import           Control.Concurrent.Async ()
import           Control.Monad.Trans.State.Strict
import qualified System.Directory as IO
import           System.FilePath ((</>))

import qualified Testnet.Property.Util as H
import           Testnet.Runtime

import           Hedgehog ((===))
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as H
import qualified Hedgehog.Extras.Test as H

prop_foldEpochState :: H.Property
prop_foldEpochState = H.integrationWorkspace "foldEpochState" $ \tempAbsBasePath' -> do
  conf <- TN.mkConf tempAbsBasePath'

  let tempAbsPath' = unTmpAbsPath $ tempAbsPath conf
      era = BabbageEra
      options = cardanoDefaultTestnetOptions
        { cardanoNodeEra = AnyCardanoEra era
        }

  runtime@TestnetRuntime{configurationFile} <- cardanoTestnetDefault options conf

  socketPathAbs <- do
    socketPath' <- H.sprocketArgumentName <$> H.headM (poolSprockets runtime)
    H.noteIO (IO.canonicalizePath $ tempAbsPath' </> socketPath')

  let handler :: ()
        => AnyNewEpochState
        -> SlotNo
        -> BlockNo
        -> StateT [(SlotNo, BlockNo)] IO LedgerStateCondition
      handler _ slotNo blockNo = do
        modify ((slotNo, blockNo):)
        s <- get
        if length s >= 10
          then pure ConditionMet
          else pure ConditionNotMet

  (_, nums) <- H.leftFailM $ H.evalIO $ runExceptT $
    Api.foldEpochState (File configurationFile) (Api.File socketPathAbs) Api.QuickValidation (EpochNo maxBound) [] handler

  length nums === 10
