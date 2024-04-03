{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Testnet.Test.LedgerEvents.Gov.DRepDeposits
  ( hprop_ledger_events_drep_deposits
  ) where

import           Cardano.Api (AnyCardanoEra (..), ConwayEra, EpochNo, File (..), FileDirection (In),
                   NodeConfigFile, ShelleyBasedEra (..), SocketPath, ToCardanoEra (..), renderTxIn)
import           Cardano.Api.Ledger (Coin (..), DRepState (..))

import           Cardano.Testnet
                   (CardanoTestnetOptions (cardanoEpochLength, cardanoNodeEra, cardanoNumDReps, cardanoSlotLength),
                   Conf (Conf, tempAbsPath), NodeRuntime (nodeSprocket),
                   TmpAbsolutePath (unTmpAbsPath), cardanoDefaultTestnetOptions,
                   cardanoTestnetDefault, makeTmpBaseAbsPath, mkConf)

import           Prelude

import           Control.Monad (void)
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as Text
import           GHC.IO.Exception (ExitCode (..))
import           GHC.Stack (HasCallStack)
import qualified GHC.Stack as GHC
import           System.FilePath ((</>))

import           Testnet.Components.Query (EpochStateView, findLargestUtxoForPaymentKey,
                   getDRepInfo, getEpochStateView)
import qualified Testnet.Process.Run as H
import qualified Testnet.Property.Utils as H
import           Testnet.Runtime (PaymentKeyInfo (paymentKeyInfoAddr, paymentKeyInfoPair),
                   PaymentKeyPair (..), PoolNode (poolRuntime),
                   TestnetRuntime (TestnetRuntime, configurationFile, poolNodes, testnetMagic, wallets))

import           Hedgehog (MonadTest, Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO


-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/DRep Deposits/"'@
hprop_ledger_events_drep_deposits :: Property
hprop_ledger_events_drep_deposits = H.integrationWorkspace "drep-deposits" $ \tempAbsBasePath' -> do

  -- Start a local test net
  conf@Conf { tempAbsPath } <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath

  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  let sbe = ShelleyBasedEraConway
      era = toCardanoEra sbe
      cEra = AnyCardanoEra era
      fastTestnetOptions = cardanoDefaultTestnetOptions
        { cardanoEpochLength = 100
        , cardanoSlotLength = 0.1
        , cardanoNodeEra = cEra
        , cardanoNumDReps = 0
        }

  TestnetRuntime
    { testnetMagic
    , poolNodes
    , wallets=wallet0:wallet1:_
    , configurationFile
    }
    <- cardanoTestnetDefault fastTestnetOptions conf

  poolNode1 <- H.headM poolNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket $ poolRuntime poolNode1
  execConfig <- H.mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic

  let socketName' = IO.sprocketName poolSprocket1
      socketBase = IO.sprocketBase poolSprocket1 -- /tmp
      socketPath = socketBase </> socketName'

  epochStateView <- getEpochStateView (File configurationFile) (File socketPath)

  H.note_ $ "Sprocket: " <> show poolSprocket1
  H.note_ $ "Abs path: " <> tempAbsBasePath'
  H.note_ $ "Socketpath: " <> socketPath
  H.note_ $ "Foldblocks config file: " <> configurationFile

  gov <- H.createDirectoryIfMissing $ work </> "governance"

  -- DRep 1 (not enough deposit)

  drepDir1 <- H.createDirectoryIfMissing $ gov </> "drep1"

  drepKeyPair1 <- generateDRepKeyPair execConfig drepDir1 "keys"
  drepRegCert1 <- generateRegistrationCertificate execConfig drepDir1 "reg-cert"
                                                  drepKeyPair1 999_999
  drepRegTxBody1 <- createDRepRegistrationTxBody execConfig epochStateView sbe drepDir1 "reg-cert-txbody"
                                                 drepRegCert1 wallet0
  drepSignedRegTx1 <- signTx execConfig drepDir1 "signed-reg-tx"
                             drepRegTxBody1 [drepKeyPair1, paymentKeyInfoPair wallet0]

  failToSubmitTx execConfig drepSignedRegTx1

  -- DRep 2 (enough deposit)

  drepDir2 <- H.createDirectoryIfMissing $ gov </> "drep2"

  drepKeyPair2 <- generateDRepKeyPair execConfig drepDir2 "keys"
  drepRegCert2 <- generateRegistrationCertificate execConfig drepDir2 "reg-cert"
                                                  drepKeyPair2 1_000_000
  drepRegTxBody2 <- createDRepRegistrationTxBody execConfig epochStateView sbe drepDir2 "reg-cert-txbody"
                                                 drepRegCert2 wallet1
  drepSignedRegTx2 <- signTx execConfig drepDir2 "signed-reg-tx"
                             drepRegTxBody2 [drepKeyPair2, paymentKeyInfoPair wallet1]

  submitTx execConfig drepSignedRegTx2

  deposits <- H.evalMaybeM $ getDRepDeposits sbe (File configurationFile) (File socketPath) 10 1

  deposits H.=== [1_000_000]

-- DRep key pair generation

generateDRepKeyPair :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack) => H.ExecConfig -> FilePath -> String -> m PaymentKeyPair
generateDRepKeyPair execConfig work prefix = do
  baseDir <- H.createDirectoryIfMissing $ work </> prefix
  let dRepKeyPair = PaymentKeyPair { paymentVKey = baseDir </> "verification.vkey"
                                   , paymentSKey = baseDir </> "signature.skey"
                                   }
  void $ H.execCli' execConfig [ "conway", "governance", "drep", "key-gen"
                               , "--verification-key-file", paymentVKey dRepKeyPair
                               , "--signing-key-file", paymentSKey dRepKeyPair
                               ]
  return dRepKeyPair

-- DRep registration certificate generation

newtype DRepRegistrationCertificate = DRepRegistrationCertificate { registrationCertificateFile :: FilePath }

generateRegistrationCertificate
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => H.ExecConfig
  -> FilePath
  -> String
  -> PaymentKeyPair
  -> Int
  -> m DRepRegistrationCertificate
generateRegistrationCertificate execConfig work prefix drepKeyPair depositAmount = do
  let dRepRegistrationCertificate = DRepRegistrationCertificate (work </> prefix <> ".regcert")
  void $ H.execCli' execConfig [ "conway", "governance", "drep", "registration-certificate"
                               , "--drep-verification-key-file", paymentVKey drepKeyPair
                               , "--key-reg-deposit-amt", show @Int depositAmount
                               , "--out-file", registrationCertificateFile dRepRegistrationCertificate
                               ]
  return dRepRegistrationCertificate

-- DRep registration transaction composition (without signing)

newtype TxBody = TxBody { txBodyFile :: FilePath }

createDRepRegistrationTxBody
  :: (H.MonadAssertion m, MonadTest m, MonadCatch m, MonadIO m)
  => H.ExecConfig
  -> EpochStateView
  -> ShelleyBasedEra era
  -> FilePath
  -> String
  -> DRepRegistrationCertificate
  -> PaymentKeyInfo
  -> m TxBody
createDRepRegistrationTxBody execConfig epochStateView sbe work prefix drepRegCert wallet = do
  let dRepRegistrationTxBody = TxBody (work </> prefix <> ".txbody")
  walletLargestUTXO <- findLargestUtxoForPaymentKey epochStateView sbe wallet
  void $ H.execCli' execConfig
    [ "conway", "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet
    , "--tx-in", Text.unpack $ renderTxIn walletLargestUTXO
    , "--certificate-file", registrationCertificateFile drepRegCert
    , "--witness-override", show @Int 2
    , "--out-file", txBodyFile dRepRegistrationTxBody
    ]
  return dRepRegistrationTxBody

-- Transaction signing

newtype SignedTx = SignedTx { signedTxFile :: FilePath }

signTx :: (MonadTest m, MonadCatch m, MonadIO m)
  => H.ExecConfig
  -> FilePath
  -> String
  -> TxBody
  -> [PaymentKeyPair]
  -> m SignedTx
signTx execConfig work prefix txBody signatoryKeyPairs = do
  let signedTx = SignedTx (work </> prefix <> ".tx")
  void $ H.execCli' execConfig $
    [ "conway", "transaction", "sign"
    , "--tx-body-file", txBodyFile txBody
    ] ++ (concat [["--signing-key-file", paymentSKey kp] | kp <- signatoryKeyPairs]) ++
    [ "--out-file", signedTxFile signedTx
    ]
  return signedTx

-- Transaction submission

submitTx
  :: (MonadTest m, MonadCatch m, MonadIO m)
  => H.ExecConfig
  -> SignedTx
  -> m ()
submitTx execConfig signedTx =
  void $ H.execCli' execConfig
    [ "conway", "transaction", "submit"
    , "--tx-file", signedTxFile signedTx
    ]

-- Attempt to submit transaction that must fail

failToSubmitTx
  :: (MonadTest m, MonadCatch m, MonadIO m)
  => H.ExecConfig
  -> SignedTx
  -> m ()
failToSubmitTx execConfig signedTx = GHC.withFrozenCallStack $ do
  (exitCode, _, _) <- H.execFlexAny' execConfig "cardano-cli" "CARDANO_CLI"
                                     [ "conway", "transaction", "submit"
                                     , "--tx-file", signedTxFile signedTx
                                     ]
  case exitCode of
    ExitSuccess -> H.failMessage GHC.callStack "Transaction submission was expected to fail but it succeeded"
    _ -> return ()

-- Obtains the amounts of the DRep deposits given some assumptions

getDRepDeposits ::
  (HasCallStack, MonadCatch m, MonadIO m, MonadTest m)
  => ShelleyBasedEra ConwayEra -- ^ The era in which the test runs
  -> NodeConfigFile In
  -> SocketPath
  -> EpochNo -- ^ The termination epoch: the constitution proposal must be found *before* this epoch
  -> Int -- ^ The expected numbers of DReps. If this number is not reached until the termination epoch, this function fails the test.
  -> m (Maybe [Integer]) -- ^ The DReps when the expected number of DReps was attained.
getDRepDeposits sbe nodeConfigFile socketPath maxEpoch expectedDRepsNb = do
  mDRepInfo <- getDRepInfo sbe nodeConfigFile socketPath maxEpoch expectedDRepsNb
  return $ map (unCoin . drepDeposit) <$> mDRepInfo
