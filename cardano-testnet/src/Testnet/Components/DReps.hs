{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Testnet.Components.DReps
  ( generateDRepKeyPair
  , generateRegistrationCertificate
  , createDRepRegistrationTxBody
  , generateVoteFiles
  , createVotingTxBody
  , signTx
  , submitTx
  , failToSubmitTx
  , retrieveTransactionId
  ) where

import           Cardano.Api (AnyCardanoEra (..), FileDirection (In), ShelleyBasedEra (..),
                   renderTxIn)

import           Cardano.CLI.Types.Common (File (..))

import           Prelude

import           Control.Monad (forM, void)
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as Text
import           Data.Word (Word32)
import           GHC.IO.Exception (ExitCode (..))
import           GHC.Stack (HasCallStack)
import qualified GHC.Stack as GHC
import           System.FilePath ((</>))

import           Testnet.Components.Query (EpochStateView, findLargestUtxoForPaymentKey)
import qualified Testnet.Process.Run as H
import           Testnet.Runtime (PaymentKeyInfo (paymentKeyInfoAddr), PaymentKeyPair (..))
import           Testnet.Start.Types (anyEraToString)

import           Hedgehog (MonadTest)
import qualified Hedgehog.Extras as H

-- | Generates a key pair for a decentralized representative (DRep) using @cardano-cli@.
--
-- The function takes three parameters:
--
-- * 'execConfig': Specifies the CLI execution configuration.
-- * 'work': Base directory path where keys will be stored.
-- * 'prefix': Name for the subfolder that will be created under 'work' folder to store the output keys.
--
-- Returns the generated 'PaymentKeyPair' containing paths to the verification and
-- signing key files.
generateDRepKeyPair :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => H.ExecConfig
  -> FilePath
  -> String
  -> m PaymentKeyPair
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

data DRepRegistrationCertificate

-- | Generates a registration certificate for a decentralized representative (DRep)
-- using @cardano-cli@.
--
-- The function takes five parameters:
--
-- * 'execConfig': Specifies the CLI execution configuration.
-- * 'work': Base directory path where the certificate file will be stored.
-- * 'prefix': Prefix for the output certificate file name. The extension will be @.regcert@.
-- * 'drepKeyPair': Payment key pair associated with the DRep. Can be generated using
--                  'generateDRepKeyPair'.
-- * 'depositAmount': Deposit amount required for DRep registration. The right amount
--                    can be obtained using 'getMinDRepDeposit'.
--
-- Returns the generated @File DRepRegistrationCertificate In@ file path to the
-- registration certificate.
generateRegistrationCertificate
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => H.ExecConfig
  -> FilePath
  -> String
  -> PaymentKeyPair
  -> Integer
  -> m (File DRepRegistrationCertificate In)
generateRegistrationCertificate execConfig work prefix drepKeyPair depositAmount = do
  let dRepRegistrationCertificate = File (work </> prefix <> ".regcert")
  void $ H.execCli' execConfig [ "conway", "governance", "drep", "registration-certificate"
                               , "--drep-verification-key-file", paymentVKey drepKeyPair
                               , "--key-reg-deposit-amt", show @Integer depositAmount
                               , "--out-file", unFile dRepRegistrationCertificate
                               ]
  return dRepRegistrationCertificate

-- DRep registration transaction composition (without signing)

data TxBody

-- | Composes a decentralized representative (DRep) registration transaction body
--   (without signing) using @cardano-cli@.
--
-- This function takes seven parameters:
--
-- * 'execConfig': Specifies the CLI execution configuration.
-- * 'epochStateView': Current epoch state view for transaction building. It can be obtained
--                     using the 'getEpochStateView' function.
-- * 'sbe': The Shelley-based era (e.g., 'ShelleyEra') in which the transaction will be constructed.
-- * 'work': Base directory path where the transaction body file will be stored.
-- * 'prefix': Prefix for the output transaction body file name. The extension will be @.txbody@.
-- * 'drepRegCert': The file name of the registration certificate for the DRep, obtained using
--                  'generateRegistrationCertificate'.
-- * 'wallet': Payment key information associated with the transaction,
--             as returned by 'cardanoTestnetDefault'.
--
-- Returns the generated @File TxBody In@ file path to the transaction body.
createDRepRegistrationTxBody
  :: (H.MonadAssertion m, MonadTest m, MonadCatch m, MonadIO m)
  => H.ExecConfig
  -> EpochStateView
  -> ShelleyBasedEra era
  -> FilePath
  -> String
  -> File DRepRegistrationCertificate In
  -> PaymentKeyInfo
  -> m (File TxBody In)
createDRepRegistrationTxBody execConfig epochStateView sbe work prefix drepRegCert wallet = do
  let dRepRegistrationTxBody = File (work </> prefix <> ".txbody")
  walletLargestUTXO <- findLargestUtxoForPaymentKey epochStateView sbe wallet
  void $ H.execCli' execConfig
    [ "conway", "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet
    , "--tx-in", Text.unpack $ renderTxIn walletLargestUTXO
    , "--certificate-file", unFile drepRegCert
    , "--witness-override", show @Int 2
    , "--out-file", unFile dRepRegistrationTxBody
    ]
  return dRepRegistrationTxBody

-- DRep vote file generation
data DRepVoteFile

-- | Generates decentralized representative (DRep) voting files (without signing)
-- using @cardano-cli@.
--
-- This function takes the following parameters:
--
-- * 'execConfig': Specifies the CLI execution configuration.
-- * 'work': Base directory path where the voting files and directories will be
--           stored.
-- * 'prefix': Name for the subfolder that will be created under 'work' to store
--             the output voting files.
-- * 'governanceActionTxId': Transaction ID string of the governance action.
-- * 'governanceActionIndex': Index of the governance action.
-- * 'allVotes': List of tuples where each tuple contains a 'PaymentKeyPair'
--               representing the DRep key pair and a 'String' representing the
--               vote type (i.e: "yes", "no", or "abstain").
--
-- Returns a list of generated @File DRepVoteFile In@ representing the paths to
-- the generated voting files.
generateVoteFiles :: (MonadTest m, MonadIO m, MonadCatch m)
  => H.ExecConfig
  -> FilePath
  -> String
  -> String
  -> Word32
  -> [(PaymentKeyPair, [Char])]
  -> m [File DRepVoteFile In]
generateVoteFiles execConfig work prefix governanceActionTxId governanceActionIndex allVotes = do
  baseDir <- H.createDirectoryIfMissing $ work </> prefix
  forM (zip [(1 :: Integer)..] allVotes) $ \(idx, (drepKeyPair, vote)) -> do
    let path = File (baseDir </> "vote-" <> show idx)
    void $ H.execCli' execConfig
      [ "conway", "governance", "vote", "create"
      , "--" ++ vote
      , "--governance-action-tx-id", governanceActionTxId
      , "--governance-action-index", show @Word32 governanceActionIndex
      , "--drep-verification-key-file", paymentVKey drepKeyPair
      , "--out-file", unFile path
      ]
    return path

-- | Composes a decentralized representative (DRep) voting transaction body
-- (without signing) using @cardano-cli@.
--
-- This function takes seven parameters:
--
-- * 'execConfig': Specifies the CLI execution configuration.
-- * 'epochStateView': Current epoch state view for transaction building. It can be obtained
--                     using the 'getEpochStateView' function.
-- * 'sbe': The Shelley-based era (e.g., 'ShelleyEra') in which the transaction will be constructed.
-- * 'work': Base directory path where the transaction body file will be stored.
-- * 'prefix': Prefix for the output transaction body file name. The extension will be @.txbody@.
-- * 'votes': List of voting files (@File DRepVoteFile In@) to include in the transaction,
--            obtained using 'generateVoteFiles'.
-- * 'wallet': Payment key information associated with the transaction,
--             as returned by 'cardanoTestnetDefault'.
--
-- Returns the generated @File TxBody In@ file path to the transaction body.
createVotingTxBody
  :: (H.MonadAssertion m, MonadTest m, MonadCatch m, MonadIO m)
  => H.ExecConfig
  -> EpochStateView
  -> ShelleyBasedEra era
  -> FilePath
  -> String
  -> [File DRepVoteFile In]
  -> PaymentKeyInfo
  -> m (File TxBody In)
createVotingTxBody execConfig epochStateView sbe work prefix votes wallet = do
  let dRepVotingTxBody = File (work </> prefix <> ".txbody")
  walletLargestUTXO <- findLargestUtxoForPaymentKey epochStateView sbe wallet
  void $ H.execCli' execConfig $
    [ "conway", "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet
    , "--tx-in", Text.unpack $ renderTxIn walletLargestUTXO
    ] ++ (concat [["--vote-file", voteFile] | File voteFile <- votes]) ++
    [ "--witness-override", show @Int (length votes)
    , "--out-file", unFile dRepVotingTxBody
    ]
  return dRepVotingTxBody

-- Transaction signing

data SignedTx

-- | Calls @cardano-cli@ to signs a transaction body using the specified key pairs.
--
-- This function takes five parameters:
--
-- * 'execConfig': Specifies the CLI execution configuration.
-- * 'cEra': Specifies the current Cardano era.
-- * 'work': Base directory path where the signed transaction file will be stored.
-- * 'prefix': Prefix for the output signed transaction file name. The extension will be @.tx@.
-- * 'txBody': Transaction body to be signed, obtained using 'createDRepRegistrationTxBody' or similar.
-- * 'signatoryKeyPairs': List of payment key pairs used for signing the transaction.
--
-- Returns the generated @File SignedTx In@ file path to the signed transaction file.
signTx :: (MonadTest m, MonadCatch m, MonadIO m)
  => H.ExecConfig
  -> AnyCardanoEra
  -> FilePath
  -> String
  -> File TxBody In
  -> [PaymentKeyPair]
  -> m (File SignedTx In)
signTx execConfig cEra work prefix txBody signatoryKeyPairs = do
  let signedTx = File (work </> prefix <> ".tx")
  void $ H.execCli' execConfig $
    [ anyEraToString cEra, "transaction", "sign"
    , "--tx-body-file", unFile txBody
    ] ++ (concat [["--signing-key-file", paymentSKey kp] | kp <- signatoryKeyPairs]) ++
    [ "--out-file", unFile signedTx
    ]
  return signedTx

-- | Submits a signed transaction using @cardano-cli@.
--
-- This function takes two parameters:
--
-- * 'execConfig': Specifies the CLI execution configuration.
-- * 'cEra': Specifies the current Cardano era.
-- * 'signedTx': Signed transaction to be submitted, obtained using 'signTx'.
submitTx
  :: (MonadTest m, MonadCatch m, MonadIO m)
  => H.ExecConfig
  -> AnyCardanoEra
  -> File SignedTx In
  -> m ()
submitTx execConfig cEra signedTx =
  void $ H.execCli' execConfig
    [ anyEraToString cEra, "transaction", "submit"
    , "--tx-file", unFile signedTx
    ]

-- | Attempts to submit a transaction that is expected to fail using @cardano-cli@.
--
-- This function takes two parameters:
--
-- * 'execConfig': Specifies the CLI execution configuration.
-- * 'cEra': Specifies the current Cardano era.
-- * 'signedTx': Signed transaction to be submitted, obtained using 'signTx'.
--
-- If the submission fails (the expected behavior), the function succeeds.
-- If the submission succeeds unexpectedly, it raises a failure message that is
-- meant to be caught by @Hedgehog@.
failToSubmitTx
  :: (MonadTest m, MonadCatch m, MonadIO m)
  => H.ExecConfig
  -> AnyCardanoEra
  -> File SignedTx In
  -> m ()
failToSubmitTx execConfig cEra signedTx = GHC.withFrozenCallStack $ do
  (exitCode, _, _) <- H.execFlexAny' execConfig "cardano-cli" "CARDANO_CLI"
                                     [ anyEraToString cEra, "transaction", "submit"
                                     , "--tx-file", unFile signedTx
                                     ]
  case exitCode of
    ExitSuccess -> H.failMessage GHC.callStack "Transaction submission was expected to fail but it succeeded"
    _ -> return ()

-- | Retrieves the transaction ID (governance action ID) from a signed
-- transaction file using @cardano-cli@.
--
-- This function takes the following parameters:
--
-- * 'execConfig': Specifies the CLI execution configuration.
-- * 'signedTx': Signed transaction to be submitted, obtained using 'signTx'.
--
-- Returns the transaction ID (governance action ID) as a 'String'.
retrieveTransactionId
  :: (MonadTest m, MonadCatch m, MonadIO m)
  => H.ExecConfig
  -> File SignedTx In
  -> m String
retrieveTransactionId execConfig signedTxBody = do
  txidOutput <- H.execCli' execConfig
    [ "transaction", "txid"
    , "--tx-file", unFile signedTxBody
    ]
  return $ mconcat $ lines txidOutput
