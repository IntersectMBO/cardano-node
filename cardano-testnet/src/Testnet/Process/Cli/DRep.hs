{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Testnet.Process.Cli.DRep
  ( generateDRepKeyPair
  , generateRegistrationCertificate
  , createCertificatePublicationTxBody
  , generateVoteFiles
  , createVotingTxBody
  , registerDRep
  , delegateToDRep
  , getLastPParamUpdateActionId
  ) where

import           Cardano.Api hiding (Certificate, TxBody)

import           Prelude

import           Control.Monad (forM, void)
import           Control.Monad.Catch (MonadCatch)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Lens as AL
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Word (Word32)
import           GHC.Stack
import           Lens.Micro ((^?))
import           System.FilePath ((</>))

import           Testnet.Components.Query
import           Testnet.Process.Cli.Transaction
import           Testnet.Process.Run (execCli', execCliStdoutToJson)
import           Testnet.Types

import           Hedgehog (MonadTest, evalMaybe)
import qualified Hedgehog.Extras as H

-- | Generates a key pair for a decentralized representative (DRep) using @cardano-cli@.
--
-- Returns the generated 'PaymentKeyPair' containing paths to the verification and
-- signing key files.
generateDRepKeyPair
  :: MonadTest m
  => MonadCatch m
  => MonadIO m
  => HasCallStack
  => H.ExecConfig -- ^ Specifies the CLI execution configuration.
  -> FilePath -- ^ Base directory path where keys will be stored.
  -> String -- ^ Name for the subfolder that will be created under 'work' folder to store the output keys.
  -> m (KeyPair PaymentKey)
generateDRepKeyPair execConfig work prefix = do
  baseDir <- H.createDirectoryIfMissing $ work </> prefix
  let dRepKeyPair = KeyPair { verificationKey = File $ baseDir </> "verification.vkey"
                            , signingKey = File $ baseDir </> "signature.skey"
                            }
  void $ execCli' execConfig [ "conway", "governance", "drep", "key-gen"
                               , "--verification-key-file", verificationKeyFp dRepKeyPair
                               , "--signing-key-file", signingKeyFp dRepKeyPair
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
  :: MonadTest m
  => MonadCatch m
  => MonadIO m
  => HasCallStack
  => H.ExecConfig -- ^ Specifies the CLI execution configuration.
  -> FilePath -- ^ Base directory path where the certificate file will be stored.
  -> String -- ^ Prefix for the output certificate file name. The extension will be @.regcert@.
  -> KeyPair PaymentKey -- ^ Payment key pair associated with the DRep. Can be generated using
                    -- 'generateDRepKeyPair'.
  -> Integer -- ^ Deposit amount required for DRep registration. The right amount
             -- can be obtained using 'getMinDRepDeposit'.
  -> m (File Certificate In)
generateRegistrationCertificate execConfig work prefix drepKeyPair depositAmount = do
  let dRepRegistrationCertificate = File (work </> prefix <> ".regcert")
  void $ execCli' execConfig [ "conway", "governance", "drep", "registration-certificate"
                               , "--drep-verification-key-file", verificationKeyFp drepKeyPair
                               , "--key-reg-deposit-amt", show @Integer depositAmount
                               , "--out-file", unFile dRepRegistrationCertificate
                               ]
  return dRepRegistrationCertificate

-- DRep registration transaction composition (without signing)


-- | Composes a certificate publication transaction body (without signing) using @cardano-cli@.
--
-- Returns the generated @File TxBody In@ file path to the transaction body.
createCertificatePublicationTxBody
  :: H.MonadAssertion m
  => MonadTest m
  => MonadCatch m
  => MonadIO m
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
  void $ execCli' execConfig
    [ "conway", "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet
    , "--tx-in", Text.unpack $ renderTxIn walletLargestUTXO
    , "--certificate-file", unFile cert
    , "--witness-override", show @Int 2
    , "--out-file", unFile dRepRegistrationTxBody
    ]
  return dRepRegistrationTxBody

-- Vote file generation

-- | Generates decentralized representative (DRep) voting files (without signing)
-- using @cardano-cli@.
--
-- Returns a list of generated @File VoteFile In@ representing the paths to
-- the generated voting files.
-- TODO: unify with SPO.generateVoteFiles
generateVoteFiles
  :: MonadTest m
  => MonadIO m
  => MonadCatch m
  => H.ExecConfig -- ^ Specifies the CLI execution configuration.
  -> FilePath -- ^ Base directory path where the voting files and directories will be
              -- stored.
  -> String -- ^ Name for the subfolder that will be created under 'work' to store
            -- the output voting files.
  -> String -- ^ Transaction ID string of the governance action.
  -> Word32 -- ^ Index of the governance action.
  -> [(KeyPair PaymentKey, [Char])] -- ^ List of tuples where each tuple contains a 'PaymentKeyPair'
                                -- representing the DRep key pair and a 'String' representing the
                                -- vote type (i.e: "yes", "no", or "abstain").
  -> m [File VoteFile In]
generateVoteFiles execConfig work prefix governanceActionTxId governanceActionIndex allVotes = do
  baseDir <- H.createDirectoryIfMissing $ work </> prefix
  forM (zip [(1 :: Integer)..] allVotes) $ \(idx, (drepKeyPair, vote)) -> do
    let path = File (baseDir </> "vote-drep-" <> show idx)
    void $ execCli' execConfig
      [ "conway", "governance", "vote", "create"
      , "--" ++ vote
      , "--governance-action-tx-id", governanceActionTxId
      , "--governance-action-index", show @Word32 governanceActionIndex
      , "--drep-verification-key-file", verificationKeyFp drepKeyPair
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
  :: H.MonadAssertion m
  => MonadTest m
  => MonadCatch m
  => MonadIO m
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
  void $ execCli' execConfig $
    [ "conway", "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet
    , "--tx-in", Text.unpack $ renderTxIn walletLargestUTXO
    ] ++ (concat [["--vote-file", voteFile] | File voteFile <- votes]) ++
    [ "--witness-override", show @Int (length votes)
    , "--out-file", unFile votingTxBody
    ]
  return votingTxBody

-- | Register a Delegate Representative (DRep) using @cardano-cli@,
-- generating a fresh key pair in the process.
--
-- Returns the key pair for the DRep as a 'PaymentKeyPair'.
registerDRep
  :: HasCallStack
  => MonadCatch m
  => MonadIO m
  => MonadTest m
  => H.MonadAssertion m
  => H.ExecConfig -- ^ Specifies the CLI execution configuration.
  -> EpochStateView -- ^ Current epoch state view for transaction building. It can be obtained
                    -- using the 'getEpochStateView' function.
  -> ConwayEraOnwards ConwayEra -- ^ The conway era onwards witness for the era in which the transaction will be constructed.
  -> FilePath -- ^ Base directory path where the signed transaction file will be stored.
  -> FilePath -- ^ Name for the subfolder that will be created under 'work' folder to store the output keys.
  -> PaymentKeyInfo -- ^ Payment key information associated with the transaction,
                    -- as returned by 'cardanoTestnetDefault'.
  -> m (KeyPair PaymentKey)
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
                             drepRegTxBody [SomeKeyPair drepKeyPair, SomeKeyPair $ paymentKeyInfoPair wallet]
  submitTx execConfig cEra drepSignedRegTx

  return drepKeyPair

-- | Delegate to a Delegate Representative (DRep) by creating and submitting
-- a vote delegation certificate transaction using @cardano-cli@.
delegateToDRep
  :: HasCallStack
  => MonadTest m
  => MonadIO m
  => H.MonadAssertion m
  => MonadCatch m
  => H.ExecConfig -- ^ Specifies the CLI execution configuration.
  -> EpochStateView -- ^ Current epoch state view for transaction building. It can be obtained
  -> NodeConfigFile In -- ^ Path to the node configuration file as returned by 'cardanoTestnetDefault'.
  -> SocketPath  -- ^ Path to the cardano-node unix socket file.
  -> ShelleyBasedEra ConwayEra -- ^ The Shelley-based era (e.g., 'ConwayEra') in which the transaction will be constructed.
  -> FilePath -- ^ Base directory path where generated files will be stored.
  -> String -- ^ Name for the subfolder that will be created under 'work' folder.
  -> PaymentKeyInfo -- ^ Wallet that will pay for the transaction.
  -> KeyPair StakingKey -- ^ Staking key pair used for delegation.
  -> KeyPair PaymentKey -- ^ Delegate Representative (DRep) key pair ('PaymentKeyPair') to which delegate.
  -> m ()
delegateToDRep execConfig epochStateView configurationFile' socketPath sbe work prefix
               payingWallet skeyPair@KeyPair{verificationKey=File vKeyFile}
               KeyPair{verificationKey=File drepVKey}  = do

  let era = toCardanoEra sbe
      cEra = AnyCardanoEra era

  baseDir <- H.createDirectoryIfMissing $ work </> prefix

  -- Create vote delegation certificate
  let voteDelegationCertificatePath = baseDir </> "delegation-certificate.delegcert"
  void $ execCli' execConfig
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
                               repRegTxBody1 [ SomeKeyPair $ paymentKeyInfoPair payingWallet
                                             , SomeKeyPair skeyPair]

  -- Submit transaction
  submitTx execConfig cEra repRegSignedRegTx1

  -- Wait two epochs
  (EpochNo epochAfterProp) <- getCurrentEpochNo epochStateView
  void $ waitUntilEpoch configurationFile' socketPath (EpochNo (epochAfterProp + 2))

-- | This function obtains the identifier for the last enacted parameter update proposal
-- if any.
--
-- If no previous proposal was enacted, the function returns 'Nothing'.
-- If there was a previous enacted proposal, the function returns a tuple with its transaction
-- identifier (as a 'String') and the action index (as a 'Word32').
getLastPParamUpdateActionId
  :: HasCallStack
  => MonadTest m
  => MonadCatch m
  => MonadIO m
  => H.ExecConfig -- ^ Specifies the CLI execution configuration.
  -> m (Maybe (String, Word32))
getLastPParamUpdateActionId execConfig = do
  govStateJSON :: Aeson.Value <- execCliStdoutToJson execConfig
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
