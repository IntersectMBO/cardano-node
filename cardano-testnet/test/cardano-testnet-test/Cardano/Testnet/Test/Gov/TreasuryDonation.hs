{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.Gov.TreasuryDonation
  ( hprop_ledger_events_treasury_donation
  ) where

import           Cardano.Api
import           Cardano.Api.Ledger

import qualified Cardano.Ledger.Coin as L
import           Cardano.Testnet

import           Prelude

import           Control.Monad (unless, void)
import           Control.Monad.Catch (MonadCatch)
import           Data.Default.Class
import qualified Data.Text as Text
import           GHC.Stack (HasCallStack)
import           System.Exit
import           System.FilePath ((</>))

import           Testnet.Components.Query
import           Testnet.Process.Run (execCli', execCliAny, mkExecConfig)
import           Testnet.Property.Util (integrationWorkspace)
import           Testnet.Start.Types
import           Testnet.Types

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- | Test that donating to the treasury indeed increases the treasury
-- Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Treasury Donation/"'@
hprop_ledger_events_treasury_donation :: Property
hprop_ledger_events_treasury_donation = integrationWorkspace "treasury-donation" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  conf@Conf { tempAbsPath=tempAbsPath@(TmpAbsolutePath work) }
    <- mkConf tempAbsBasePath'
  let tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath

  let ceo = ConwayEraOnwardsConway
      sbe = conwayEraOnwardsToShelleyBasedEra ceo
      fastTestnetOptions = def { cardanoNodeEra = AnyShelleyBasedEra sbe }
      shelleyOptions = def { shelleyEpochLength = 100 }

  TestnetRuntime
    { testnetMagic
    , poolNodes
    , wallets=wallet0:_
    , configurationFile
    }
    <- cardanoTestnetDefault fastTestnetOptions shelleyOptions conf

  PoolNode{poolRuntime} <- H.headM poolNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket poolRuntime
  execConfig <- mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic
  let socketPath = nodeSocketPath poolRuntime

  epochStateView <- getEpochStateView configurationFile socketPath

  H.note_ $ "Sprocket: " <> show poolSprocket1
  H.note_ $ "Abs path: " <> tempAbsBasePath'
  H.note_ $ "Socketpath: " <> unFile socketPath
  H.note_ $ "Foldblocks config file: " <> unFile configurationFile

  L.Coin currentTreasury <- getTreasuryValue epochStateView
  H.note_ $ "currentTreasury: " <> show currentTreasury
  currentTreasury H.=== 0 -- Treasury should initially be 0

  let doOneDonation = doTreasuryDonation sbe execConfig work epochStateView wallet0

  doOneDonation 0 Nothing 500
  doOneDonation 1 Nothing 500_013
  doOneDonation 2 Nothing (-497) -- Test donation that should fail because donation is negative
  doOneDonation 3 (Just 1_234) (-497) -- Test donation that should fail because current treasury value is wrong

doTreasuryDonation :: ()
  => (HasCallStack, MonadCatch m, MonadTest m, MonadIO m, H.MonadAssertion m)
  => ShelleyBasedEra era
  -> H.ExecConfig
  -> FilePath -- ^ Where temporary files can be stored
  -> EpochStateView
  -> PaymentKeyInfo-- ^ The key paying the fee
  -> Int -- ^ The number of the call, used to create unique temporary file names. Starts at 0.
  -> Maybe Int -- ^ The current treasury value to use. If unspecified, it will obtained from the node.
  -> Int -- ^ The amount to donate
  -> m ()
doTreasuryDonation sbe execConfig work epochStateView wallet0 idx currentTreasury' treasuryDonation = do
  currentTreasury <-
    case currentTreasury' of
      Nothing -> do
        v <- unCoin <$> getTreasuryValue epochStateView
        H.note_ $ "currentTreasury: " <> show v
        return v
      Just x -> pure $ toInteger x

  txBodyFp <- H.note $ work </> "treasury-donation-" <> show idx <> ".body"
  txViewFp <- H.note $ work </> "treasury-donation-" <> show idx <> ".body.view.json"
  signedTxFp <- H.note $ work </> "treasury-donation-" <> show idx <> ".signed"

  txIn0 <- findLargestUtxoForPaymentKey epochStateView sbe wallet0

  (exitCode, stdout, stderr) <- execCliAny execConfig
    [ "conway", "transaction", "build"
    , "--tx-in", Text.unpack $ renderTxIn txIn0
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet0
    , "--treasury-donation", show treasuryDonation
    , "--out-file", txBodyFp
    ]
  H.note_ $ "exitCode: " <> show exitCode
  unless (null stdout) (H.note_ stdout)
  unless (null stderr) (H.note_ stderr)

  case exitCode of
    ExitFailure _ -> do
      H.assertWith (currentTreasury, treasuryDonation) $ \(ct, td) ->
        -- If we failed, either the specified treasury was negative,
        -- or the donation was negative.
        ct < 0 || td < 0
      return ()
    ExitSuccess -> do
      H.assertWith (currentTreasury, treasuryDonation) $ \(ct, td) ->
        -- If we succeeded, both the specified treasury and donation were
        -- greater or equal to zero.
        ct >= 0 && td >= 0
      H.noteM_ $ execCli' execConfig
        [ "debug", "transaction", "view" , "--tx-file", txBodyFp
        , "--output-json", "--out-file", txViewFp]

      H.noteM_ $ execCli' execConfig
        [ "conway", "transaction", "sign"
        , "--tx-body-file", txBodyFp
        , "--signing-key-file", signingKeyFp $ paymentKeyInfoPair wallet0
        , "--out-file", signedTxFp
        ]

      H.noteM_ $ execCli' execConfig
        [ "debug", "transaction", "view" , "--tx-file", signedTxFp ]

      H.noteM_ $ execCli' execConfig
        [ "conway", "transaction", "submit" , "--tx-file", signedTxFp ]

      void $ waitForEpochs epochStateView (EpochInterval 3)

      L.Coin finalTreasury <- getTreasuryValue epochStateView
      H.note_ $ "finalTreasury: " <> show finalTreasury
      finalTreasury H.=== (currentTreasury + toInteger treasuryDonation)
