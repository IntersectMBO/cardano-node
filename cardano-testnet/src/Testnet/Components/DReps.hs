{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Testnet.Components.DReps
  ( SomeKeyPair(..)
  , VoteFile
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
import qualified Data.ByteString.Lazy.Char8 as LBS
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
import qualified Testnet.Process.Run as H
import           Testnet.Runtime (PaymentKeyInfo (paymentKeyInfoAddr, paymentKeyInfoPair),
                   PaymentKeyPair (..), StakingKeyPair (StakingKeyPair, stakingSKey))
import           Testnet.Start.Types (anyEraToString)

import           Hedgehog (MonadTest, evalMaybe)
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

data Certificate

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
-- This function takes seven parameters:
--
-- * 'execConfig': Specifies the CLI execution configuration.
-- * 'epochStateView': Current epoch state view for transaction building. It can be obtained
--                     using the 'getEpochStateView' function.
-- * 'sbe': The Shelley-based era (e.g., 'ShelleyBasedEraShelley') in which the transaction will be constructed.
-- * 'work': Base directory path where the transaction body file will be stored.
-- * 'prefix': Prefix for the output transaction body file name. The extension will be @.txbody@.
-- * 'certificate': The file name of the certificate.
-- * 'wallet': Payment key information associated with the transaction,
--             as returned by 'cardanoTestnetDefault'.
--
-- Returns the generated @File TxBody In@ file path to the transaction body.
createCertificatePublicationTxBody
  :: (H.MonadAssertion m, MonadTest m, MonadCatch m, MonadIO m)
  => H.ExecConfig
  -> EpochStateView
  -> ShelleyBasedEra era
  -> FilePath
  -> String
  -> File Certificate In
  -> PaymentKeyInfo
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
-- Returns a list of generated @File VoteFile In@ representing the paths to
-- the generated voting files.
generateVoteFiles :: (MonadTest m, MonadIO m, MonadCatch m)
  => H.ExecConfig
  -> FilePath
  -> String
  -> String
  -> Word32
  -> [(PaymentKeyPair, [Char])]
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

-- | Composes a decentralized representative (DRep) voting transaction body
-- (without signing) using @cardano-cli@.
--
-- This function takes seven parameters:
--
-- * 'execConfig': Specifies the CLI execution configuration.
-- * 'epochStateView': Current epoch state view for transaction building. It can be obtained
--                     using the 'getEpochStateView' function.
-- * 'sbe': The Shelley-based era (e.g., 'ShelleyBasedEraShelley') in which the transaction will be constructed.
-- * 'work': Base directory path where the transaction body file will be stored.
-- * 'prefix': Prefix for the output transaction body file name. The extension will be @.txbody@.
-- * 'votes': List of voting files (@File VoteFile In@) to include in the transaction,
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
  -> [File VoteFile In]
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

class KeyPair a where
  secretKey :: a -> FilePath

instance KeyPair PaymentKeyPair where
  secretKey :: PaymentKeyPair -> FilePath
  secretKey = paymentSKey

instance KeyPair StakingKeyPair where
  secretKey :: StakingKeyPair -> FilePath
  secretKey = stakingSKey

data SomeKeyPair = forall a . KeyPair a => SomeKeyPair a

instance KeyPair SomeKeyPair where
  secretKey :: SomeKeyPair -> FilePath
  secretKey (SomeKeyPair x) = secretKey x

-- | Calls @cardano-cli@ to signs a transaction body using the specified key pairs.
--
-- This function takes five parameters:
--
-- * 'execConfig': Specifies the CLI execution configuration.
-- * 'cEra': Specifies the current Cardano era.
-- * 'work': Base directory path where the signed transaction file will be stored.
-- * 'prefix': Prefix for the output signed transaction file name. The extension will be @.tx@.
-- * 'txBody': Transaction body to be signed, obtained using 'createCertificatePublicationTxBody' or similar.
-- * 'signatoryKeyPairs': List of payment key pairs used for signing the transaction.
--
-- Returns the generated @File SignedTx In@ file path to the signed transaction file.
signTx :: (MonadTest m, MonadCatch m, MonadIO m, KeyPair k)
  => H.ExecConfig
  -> AnyCardanoEra
  -> FilePath
  -> String
  -> File TxBody In
  -> [k]
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

-- | Register a Delegate Representative (DRep) using @cardano-cli@,
-- generating a fresh key pair in the process.
--
-- This function takes the following parameters:
--
-- * 'execConfig': Specifies the CLI execution configuration.
-- * 'epochStateView': Current epoch state view for transaction building. It can be obtained
--                     using the 'getEpochStateView' function.
-- * 'configurationFile': Path to the node configuration file as returned by 'cardanoTestnetDefault'.
-- * 'socketPath': Path to the cardano-node unix socket file.
-- * 'sbe': The conway era onwards witness for the era in which the transaction will be constructed.
-- * 'work': Base directory path where the signed transaction file will be stored.
-- * 'prefix': Name for the subfolder that will be created under 'work' folder to store the output keys.
-- * 'wallet': Payment key information associated with the transaction,
--             as returned by 'cardanoTestnetDefault'.
--
-- Returns the key pair for the DRep as a 'PaymentKeyPair'.
registerDRep :: (MonadCatch m, MonadIO m, MonadTest m, H.MonadAssertion m)
  => H.ExecConfig
  -> EpochStateView
  -> ConwayEraOnwards ConwayEra
  -> FilePath
  -> FilePath
  -> PaymentKeyInfo
  -> m PaymentKeyPair
registerDRep execConfig epochStateView ceo work prefix wallet = do
  let sbe = conwayEraOnwardsToShelleyBasedEra ceo
      era = toCardanoEra sbe
      cEra = AnyCardanoEra era

  minDRepDeposit <- getMinDRepDeposit execConfig ceo

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
--
-- This function takes the following parameters:
--
-- * 'execConfig': Specifies the CLI execution configuration.
-- * 'epochStateView': Current epoch state view for transaction building. It can be obtained
--                     using the 'getEpochStateView' function.
-- * 'configurationFile': Path to the node configuration file as returned by 'cardanoTestnetDefault'.
-- * 'socketPath': Path to the cardano-node unix socket file.
-- * 'sbe': The Shelley-based era (e.g., 'ConwayEra') in which the transaction will be constructed.
-- * 'work': Base directory path where generated files will be stored.
-- * 'prefix': Name for the subfolder that will be created under 'work' folder.
-- * 'payingWallet': Wallet that will pay for the transaction.
-- * 'skeyPair': Staking key pair used for delegation.
-- * 'drepKeyPair': Delegate Representative (DRep) key pair ('PaymentKeyPair') to which delegate.
delegateToDRep
  :: (MonadTest m, MonadIO m, H.MonadAssertion m, MonadCatch m)
  => H.ExecConfig
  -> EpochStateView
  -> FilePath
  -> FilePath
  -> ShelleyBasedEra ConwayEra
  -> FilePath
  -> String
  -> PaymentKeyInfo
  -> StakingKeyPair
  -> PaymentKeyPair
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
-- This function takes the following parameter:
--
-- * 'execConfig': Specifies the CLI execution configuration.
--
-- If no previous proposal was enacted, the function returns 'Nothing'.
-- If there was a previous enacted proposal, the function returns a tuple with its transaction
-- identifier (as a 'String') and the action index (as a 'Word32').
getLastPParamUpdateActionId :: (MonadTest m, MonadCatch m, MonadIO m) => H.ExecConfig -> m (Maybe (String, Word32))
getLastPParamUpdateActionId execConfig = do
  govStateString <- H.execCli' execConfig
    [ "conway", "query", "gov-state"
    , "--volatile-tip"
    ]

  govStateJSON <- H.nothingFail (Aeson.decode (LBS.pack govStateString) :: Maybe Aeson.Value)
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
