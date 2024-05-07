{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Testnet.Test.Gov.TreasuryGrowth where

import           Cardano.Api hiding (cardanoEra)
import qualified Cardano.Api as Api
import           Cardano.Api.Ledger (Coin (..))

import qualified Cardano.Ledger.Shelley.LedgerState as L
import           Cardano.Testnet as TN

import           Prelude

import           Control.Monad.Trans.State.Strict
import           Data.List (sortOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Lens.Micro ((^.))
import qualified System.Directory as IO
import           System.FilePath ((</>))

import           Testnet.Components.TestWatchdog
import qualified Testnet.Property.Utils as H
import           Testnet.Runtime

import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as H
import qualified Hedgehog.Extras.Test as H

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Treasury Growth/"'@
prop_check_if_treasury_is_growing :: H.Property
prop_check_if_treasury_is_growing = H.integrationRetryWorkspace 0 "growing-treasury" $ \tempAbsBasePath' -> runWithDefaultWatchdog_ $ do
  -- Start testnet
  conf@Conf{tempAbsPath=TmpAbsolutePath tempAbsPath'} <- TN.mkConf tempAbsBasePath'

  let era = BabbageEra
      options = cardanoDefaultTestnetOptions
                  { cardanoEpochLength = 100
                  , cardanoNodeEra = AnyCardanoEra era -- TODO: We should only support the latest era and the upcoming era
                  , cardanoActiveSlotsCoeff = 0.3
                  }

  runtime@TestnetRuntime{configurationFile} <- cardanoTestnetDefault options conf

  -- Get socketPath
  socketPathAbs <- do
    socketPath' <- H.sprocketArgumentName <$> H.headM (poolSprockets runtime)
    H.noteIO (IO.canonicalizePath $ tempAbsPath' </> socketPath')

  (_condition, treasuryValues) <- H.leftFailM . H.evalIO . runExceptT $
    Api.foldEpochState (File configurationFile) (Api.File socketPathAbs) Api.QuickValidation (EpochNo 10) M.empty handler
  H.note_ $ "treasury for last 5 epochs: " <> show treasuryValues

  let treasuriesSortedByEpoch =
        map snd
        . sortOn fst
        . M.assocs
        $ treasuryValues

  if checkNonDecreasing treasuriesSortedByEpoch && checkHasIncreased treasuriesSortedByEpoch
     then H.success
     else do
       H.note_ "treasury is not growing"
       H.failure
  where
    handler :: AnyNewEpochState -> SlotNo -> BlockNo -> StateT (Map EpochNo Integer) IO LedgerStateCondition
    handler (AnyNewEpochState _ newEpochState) _slotNo _blockNo = do
      let (Coin coin) = newEpochState ^. L.nesEsL . L.esAccountStateL . L.asTreasuryL
          epochNo = newEpochState ^. L.nesELL
      -- handler is executed multiple times per epoch, so we keep only the latest treasury value
      modify $ M.insert epochNo coin
      pure $ if epochNo >= EpochNo 5
         then ConditionMet
         else ConditionNotMet

    -- | Check if the last element > first element
    checkHasIncreased :: (Ord a) => [a] -> Bool
    checkHasIncreased = \case
      [] -> False
      x1:xs -> case reverse xs of
                [] -> False
                xn:_ -> xn > x1

    checkNonDecreasing :: (Ord a) => [a] -> Bool
    checkNonDecreasing = \case
      [] -> False
      [_] -> True
      (x:y:xs) -> x <= y && checkNonDecreasing (y:xs)

