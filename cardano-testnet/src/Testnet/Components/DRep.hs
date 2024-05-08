{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Testnet.Components.DRep
  ( VoteFile
  , generateDRepKeyPair
  , generateRegistrationCertificate
  , createCertificatePublicationTxBody
  , generateVoteFiles
  , createVotingTxBody
  , signTx
  , submitTx
  , failToSubmitTx
  , retrieveTransactionId
  , registerDRep
  , delegateToDRep
  , getLastPParamUpdateActionId
  ) where

import           Cardano.Api (AnyCardanoEra (..), ConwayEra, ConwayEraOnwards, EpochNo (EpochNo),
                   FileDirection (In), MonadIO, ShelleyBasedEra (..), ToCardanoEra (toCardanoEra),
                   conwayEraOnwardsToShelleyBasedEra, renderTxIn)

import           Cardano.CLI.Types.Common (File (..))

import           Prelude

import           Control.Monad (forM, void)
import           Control.Monad.Catch (MonadCatch)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Lens as AL
import           Data.List (isInfixOf)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Word (Word32)
import           GHC.IO.Exception (ExitCode (..))
import           GHC.Stack (HasCallStack)
import qualified GHC.Stack as GHC
import           Lens.Micro ((^?))
import           System.FilePath ((</>))

import           Testnet.Components.Query (EpochStateView, findLargestUtxoForPaymentKey,
                   getCurrentEpochNo, getMinDRepDeposit, waitUntilEpoch)
import qualified Testnet.Process.Cli as H
import qualified Testnet.Process.Run as H
import           Testnet.Runtime (KeyPair (..),
                   PaymentKeyInfo (paymentKeyInfoAddr, paymentKeyInfoPair), PaymentKeyPair (..),
                   SomeKeyPair (..), StakingKeyPair (..))
import           Testnet.Start.Types (anyEraToString)

import           Hedgehog (MonadTest, evalMaybe)
import qualified Hedgehog.Extras as H

-- | Generates a key pair for a decentralized representative (DRep) using @cardano-cli@.
--
-- Returns the generated 'PaymentKeyPair' containing paths to the verification and
-- signing key files.
generateDRepKeyPair :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => H.ExecConfig -- ^ Specifies the CLI execution configuration.
  -> FilePath -- ^ Base directory path where keys will be stored.
  -> String -- ^ Name for the subfolder that will be created under 'work' folder to store the output keys.
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

data Certificate

-- | Generates a registration certificate for a decentralized representative (DRep)
-- using @cardano-cli@.
--
-- Returns the generated @File DRepRegistrationCertificate In@ file path to the
-- registration certificate.
generateRegistrationCertificate
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => H.ExecConfig -- ^ Specifies the CLI execution configuration.
  -> FilePath -- ^ Base directory path where the certificate file will be stored.
  -> String -- ^ Prefix for the output certificate file name. The extension will be @.regcert@.
  -> PaymentKeyPair -- ^ Payment key pair associated with the DRep. Can be generated using
                    -- 'generateDRepKeyPair'.
  -> Integer -- ^ Deposit amount required for DRep registration. The right amount
             -- can be obtained using 'getMinDRepDeposit'.
  -> m (File Certificate In)
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

-- | Composes a certificate publication transaction body (without signing) using @cardano-cli@.
--
-- Returns the generated @File TxBody In@ file path to the transaction body.
createCertificatePublicationTxBody
  :: (H.MonadAssertion m, MonadTest m, MonadCatch m, MonadIO m)
  => H.ExecConfig -- ^ Specifies the CLI execution configuration.
  -> EpochStateView -- ^ Current epoch state view for transaction building. It can be obtained
                    -- using the 'getEpochStateView' function.
  -> ShelleyBasedEra era -- ^ The Shelley-based era (e.g., 'ShelleyBasedEraShelley') in which the transaction will be constructed.
  -> FilePath -- ^ Base directory path where the transaction body file will be stored.
  -> String -- ^ Prefix for the output transaction body file name. The extension will be @.txbody@.
  -> File Certificate In -- ^ The file name of the certificate.
  -> PaymentKeyInfo -- ^ Payment key information associated with the transaction,
                    -- as returned by 'cardanoTestnetDefault'.
  -> m (File TxBody In)
createCertificatePublicationTxBody execConfig epochStateView sbe work prefix cert wallet = do
  let dRepRegistrationTxBody = File (work </> prefix <> ".txbody")
  walletLargestUTXO <- findLargestUtxoForPaymentKey epochStateView sbe wallet
  void $ H.execCli' execConfig
    [ "conway", "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet
    , "--tx-in", Text.unpack $ renderTxIn walletLargestUTXO
    , "--certificate-file", unFile cert
    , "--witness-override", show @Int 2
    , "--out-file", unFile dRepRegistrationTxBody
    ]
  return dRepRegistrationTxBody

-- Vote file generation
data VoteFile

-- | Generates decentralized representative (DRep) voting files (without signing)
-- using @cardano-cli@.
--
-- Returns a list of generated @File VoteFile In@ representing the paths to
-- the generated voting files.
generateVoteFiles :: (MonadTest m, MonadIO m, MonadCatch m)
  => H.ExecConfig -- ^ Specifies the CLI execution configuration.
  -> FilePath -- ^ Base directory path where the voting files and directories will be
              -- stored.
  -> String -- ^ Name for the subfolder that will be created under 'work' to store
            -- the output voting files.
  -> String -- ^ Transaction ID string of the governance action.
  -> Word32 -- ^ Index of the governance action.
  -> [(PaymentKeyPair, [Char])] -- ^ List of tuples where each tuple contains a 'PaymentKeyPair'
                                -- representing the DRep key pair and a 'String' representing the
                                -- vote type (i.e: "yes", "no", or "abstain").
  -> m [File VoteFile In]
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

-- | Composes a voting transaction body file using @cardano-cli@.
-- For the transaction to be valid it needs witnesses corresponding
-- to the spent UTxOs and votes issued (typically these witnesses are
-- cryptographic signatures). This function does not sign the transaction,
-- that can be done with 'signTx'.
--
-- Returns the generated @File TxBody In@ file path to the transaction body.
createVotingTxBody
  :: (H.MonadAssertion m, MonadTest m, MonadCatch m, MonadIO m)
  => H.ExecConfig -- ^ Specifies the CLI execution configuration.
  -> EpochStateView -- ^ Current epoch state view for transaction building. It can be obtained
                    -- using the 'getEpochStateView' function.
  -> ShelleyBasedEra era -- ^ The Shelley-based era (e.g., 'ShelleyBasedEraShelley') in which the transaction will be constructed.
  -> FilePath -- ^ Base directory path where the transaction body file will be stored.
  -> String -- ^ Prefix for the output transaction body file name. The extension will be @.txbody@.
  -> [File VoteFile In] -- ^ List of voting files (@File VoteFile In@) to include in the transaction,
                        -- obtained using 'generateVoteFiles'.
  -> PaymentKeyInfo -- ^ Payment key information associated with the transaction,
                    -- as returned by 'cardanoTestnetDefault'.
  -> m (File TxBody In)
createVotingTxBody execConfig epochStateView sbe work prefix votes wallet = do
  let votingTxBody = File (work </> prefix <> ".txbody")
  walletLargestUTXO <- findLargestUtxoForPaymentKey epochStateView sbe wallet
  void $ H.execCli' execConfig $
    [ "conway", "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet
    , "--tx-in", Text.unpack $ renderTxIn walletLargestUTXO
    ] ++ (concat [["--vote-file", voteFile] | File voteFile <- votes]) ++
    [ "--witness-override", show @Int (length votes)
    , "--out-file", unFile votingTxBody
    ]
  return votingTxBody

-- Transaction signing

data SignedTx

-- | Calls @cardano-cli@ to signs a transaction body using the specified key pairs.
--
-- This function takes five parameters:
--
-- Returns the generated @File SignedTx In@ file path to the signed transaction file.
signTx :: (MonadTest m, MonadCatch m, MonadIO m, KeyPair k)
  => H.ExecConfig -- ^ Specifies the CLI execution configuration.
  -> AnyCardanoEra -- ^ Specifies the current Cardano era.
  -> FilePath -- ^ Base directory path where the signed transaction file will be stored.
  -> String -- ^ Prefix for the output signed transaction file name. The extension will be @.tx@.
  -> File TxBody In -- ^ Transaction body to be signed, obtained using 'createCertificatePublicationTxBody' or similar.
  -> [k] -- ^ List of payment key pairs used for signing the transaction.
  -> m (File SignedTx In)
signTx execConfig cEra work prefix txBody signatoryKeyPairs = do
  let signedTx = File (work </> prefix <> ".tx")
  void $ H.execCli' execConfig $
    [ anyEraToString cEra, "transaction", "sign"
    , "--tx-body-file", unFile txBody
    ] ++ (concat [["--signing-key-file", secretKey kp] | kp <- signatoryKeyPairs]) ++
    [ "--out-file", unFile signedTx
    ]
  return signedTx

-- | Submits a signed transaction using @cardano-cli@.
submitTx
  :: (MonadTest m, MonadCatch m, MonadIO m)
  => H.ExecConfig -- ^ Specifies the CLI execution configuration.
  -> AnyCardanoEra -- ^ Specifies the current Cardano era.
  -> File SignedTx In -- ^ Signed transaction to be submitted, obtained using 'signTx'.
  -> m ()
submitTx execConfig cEra signedTx =
  void $ H.execCli' execConfig
    [ anyEraToString cEra, "transaction", "submit"
    , "--tx-file", unFile signedTx
    ]

-- | Attempts to submit a transaction that is expected to fail using @cardano-cli@.
--
-- If the submission fails (the expected behavior), the function succeeds.
-- If the submission succeeds unexpectedly, it raises a failure message that is
-- meant to be caught by @Hedgehog@.
failToSubmitTx
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => H.ExecConfig -- ^ Specifies the CLI execution configuration.
  -> AnyCardanoEra -- ^ Specifies the current Cardano era.
  -> File SignedTx In -- ^ Signed transaction to be submitted, obtained using 'signTx'.
  -> String -- ^ Substring of the error to check for to ensure submission failed for
            -- the right reason.
  -> m ()
failToSubmitTx execConfig cEra signedTx reasonForFailure = GHC.withFrozenCallStack $ do
  (exitCode, _, stderr) <- H.execFlexAny' execConfig "cardano-cli" "CARDANO_CLI"
                                     [ anyEraToString cEra, "transaction", "submit"
                                     , "--tx-file", unFile signedTx
                                     ]
  case exitCode of -- Did it fail?
    ExitSuccess -> H.failMessage GHC.callStack "Transaction submission was expected to fail but it succeeded"
    _ -> if reasonForFailure `isInfixOf` stderr -- Did it fail for the expected reason?
         then return ()
         else H.failMessage GHC.callStack $ "Transaction submission failed for the wrong reason (not " ++
                                            show reasonForFailure ++ "): " ++ stderr

-- | Retrieves the transaction ID (governance action ID) from a signed
-- transaction file using @cardano-cli@.
--
-- Returns the transaction ID (governance action ID) as a 'String'.
retrieveTransactionId
  :: (MonadTest m, MonadCatch m, MonadIO m)
  => H.ExecConfig -- ^ Specifies the CLI execution configuration.
  -> File SignedTx In -- ^ Signed transaction to be submitted, obtained using 'signTx'.
  -> m String
retrieveTransactionId execConfig signedTxBody = do
  txidOutput <- H.execCli' execConfig
    [ "transaction", "txid"
    , "--tx-file", unFile signedTxBody
    ]
  return $ mconcat $ lines txidOutput

-- | Register a Delegate Representative (DRep) using @cardano-cli@,
-- generating a fresh key pair in the process.
--
-- Returns the key pair for the DRep as a 'PaymentKeyPair'.
registerDRep :: (MonadCatch m, MonadIO m, MonadTest m, H.MonadAssertion m)
  => H.ExecConfig -- ^ Specifies the CLI execution configuration.
  -> EpochStateView -- ^ Current epoch state view for transaction building. It can be obtained
                    -- using the 'getEpochStateView' function.
  -> ConwayEraOnwards ConwayEra -- ^ The conway era onwards witness for the era in which the transaction will be constructed.
  -> FilePath -- ^ Base directory path where the signed transaction file will be stored.
  -> FilePath -- ^ Name for the subfolder that will be created under 'work' folder to store the output keys.
  -> PaymentKeyInfo -- ^ Payment key information associated with the transaction,
                    -- as returned by 'cardanoTestnetDefault'.
  -> m PaymentKeyPair
registerDRep execConfig epochStateView ceo work prefix wallet = do
  let sbe = conwayEraOnwardsToShelleyBasedEra ceo
      era = toCardanoEra sbe
      cEra = AnyCardanoEra era

  minDRepDeposit <- getMinDRepDeposit epochStateView ceo

  baseDir <- H.createDirectoryIfMissing $ work </> prefix
  drepKeyPair <- generateDRepKeyPair execConfig baseDir "keys"
  drepRegCert <- generateRegistrationCertificate execConfig baseDir "reg-cert"
                                                  drepKeyPair minDRepDeposit
  drepRegTxBody <- createCertificatePublicationTxBody execConfig epochStateView sbe baseDir "reg-cert-txbody"
                                                       drepRegCert wallet
  drepSignedRegTx <- signTx execConfig cEra baseDir "signed-reg-tx"
                             drepRegTxBody [drepKeyPair, paymentKeyInfoPair wallet]
  submitTx execConfig cEra drepSignedRegTx

  return drepKeyPair

-- | Delegate to a Delegate Representative (DRep) by creating and submitting
-- a vote delegation certificate transaction using @cardano-cli@.
delegateToDRep
  :: (MonadTest m, MonadIO m, H.MonadAssertion m, MonadCatch m)
  => H.ExecConfig -- ^ Specifies the CLI execution configuration.
  -> EpochStateView -- ^ Current epoch state view for transaction building. It can be obtained
  -> FilePath -- ^ Path to the node configuration file as returned by 'cardanoTestnetDefault'.
  -> FilePath -- ^ Path to the cardano-node unix socket file.
  -> ShelleyBasedEra ConwayEra -- ^ The Shelley-based era (e.g., 'ConwayEra') in which the transaction will be constructed.
  -> FilePath -- ^ Base directory path where generated files will be stored.
  -> String -- ^ Name for the subfolder that will be created under 'work' folder.
  -> PaymentKeyInfo -- ^ Wallet that will pay for the transaction.
  -> StakingKeyPair -- ^ Staking key pair used for delegation.
  -> PaymentKeyPair -- ^ Delegate Representative (DRep) key pair ('PaymentKeyPair') to which delegate.
  -> m ()
delegateToDRep execConfig epochStateView configurationFile socketPath sbe work prefix
               payingWallet skeyPair@(StakingKeyPair vKeyFile _sKeyFile)
               (PaymentKeyPair drepVKey _drepSKey) = do

  let era = toCardanoEra sbe
      cEra = AnyCardanoEra era

  baseDir <- H.createDirectoryIfMissing $ work </> prefix

  -- Create vote delegation certificate
  let voteDelegationCertificatePath = baseDir </> "delegation-certificate.delegcert"
  void $ H.execCli' execConfig
    [ "conway", "stake-address", "vote-delegation-certificate"
    , "--drep-verification-key-file", drepVKey
    , "--stake-verification-key-file", vKeyFile
    , "--out-file", voteDelegationCertificatePath
    ]

  -- Compose transaction to publish delegation certificate
  repRegTxBody1 <- createCertificatePublicationTxBody execConfig epochStateView sbe baseDir "del-cert-txbody"
                                                      (File voteDelegationCertificatePath) payingWallet

  -- Sign transaction
  repRegSignedRegTx1 <- signTx execConfig cEra baseDir "signed-reg-tx"
                               repRegTxBody1 [ SomeKeyPair (paymentKeyInfoPair payingWallet)
                                             , SomeKeyPair skeyPair]

  -- Submit transaction
  submitTx execConfig cEra repRegSignedRegTx1

  -- Wait two epochs
  (EpochNo epochAfterProp) <- getCurrentEpochNo epochStateView
  void $ waitUntilEpoch (File configurationFile) (File socketPath) (EpochNo (epochAfterProp + 2))

-- | This function obtains the identifier for the last enacted parameter update proposal
-- if any.
--
-- If no previous proposal was enacted, the function returns 'Nothing'.
-- If there was a previous enacted proposal, the function returns a tuple with its transaction
-- identifier (as a 'String') and the action index (as a 'Word32').
getLastPParamUpdateActionId :: (MonadTest m, MonadCatch m, MonadIO m)
  => H.ExecConfig -- ^ Specifies the CLI execution configuration.
  -> m (Maybe (String, Word32))
getLastPParamUpdateActionId execConfig = do
  govStateJSON :: Aeson.Value <- H.execCliStdoutToJson execConfig
    [ "conway", "query", "gov-state"
    , "--volatile-tip"
    ]
  let mLastPParamUpdateActionId :: Maybe Aeson.Value
      mLastPParamUpdateActionId = govStateJSON
                             ^? AL.key "nextRatifyState"
                              . AL.key "nextEnactState"
                              . AL.key "prevGovActionIds"
                              . AL.key "PParamUpdate"
  lastPParamUpdateActionId <- evalMaybe mLastPParamUpdateActionId

  if lastPParamUpdateActionId == Aeson.Null
  then return Nothing
  else do let mActionIx :: Maybe Integer
              mActionIx = lastPParamUpdateActionId
                        ^? AL.key "govActionIx"
                         . AL._Integer
              mTxId :: Maybe Text
              mTxId = lastPParamUpdateActionId
                    ^? AL.key "txId"
                     . AL._String
          actionIx <- evalMaybe mActionIx
          txId <- evalMaybe mTxId
          return (Just (Text.unpack txId, fromIntegral actionIx))
