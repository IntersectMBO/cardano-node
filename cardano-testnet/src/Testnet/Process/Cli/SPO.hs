{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Testnet.Process.Cli.SPO
  ( checkStakeKeyRegistered
  , createScriptStakeRegistrationCertificate
  , createScriptStakeDelegationCertificate
  , createStakeDelegationCertificate
  , createStakeKeyRegistrationCertificate
  , createStakeKeyDeregistrationCertificate
  , registerSingleSpo
  , generateVoteFiles
  ) where

import qualified Cardano.Api.Ledger as L
import           Cardano.Api.Shelley hiding (cardanoEra)

import qualified Cardano.Ledger.Api.State.Query as L
import qualified Cardano.Ledger.Shelley.LedgerState as L
import qualified Cardano.Ledger.UMap as L

import           Control.Monad
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.State.Strict as StateT
import qualified Data.Aeson as Aeson
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Data.Word (Word16)
import           GHC.Stack (HasCallStack)
import qualified GHC.Stack as GHC
import           Lens.Micro
import           System.FilePath.Posix ((</>))

import           Testnet.Filepath
import           Testnet.Process.Cli.Keys
import           Testnet.Process.Cli.Transaction
import           Testnet.Process.Run (execCli, execCli', execCli_)
import           Testnet.Start.Types
import           Testnet.Types

import           Hedgehog
import           Hedgehog.Extras (ExecConfig)
import qualified Hedgehog.Extras as H

checkStakePoolRegistered
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => TmpAbsolutePath
  -> ExecConfig
  -> File (VKey StakePoolKey) In -- ^ Stake pool cold verification key file
  -> FilePath -- ^ Output file path of stake pool info
  -> m String -- ^ Stake pool ID
checkStakePoolRegistered tempAbsP execConfig (File poolColdVkeyFp) outputFp =
  GHC.withFrozenCallStack $ do
    let tempAbsPath' = unTmpAbsPath tempAbsP
        oFpAbs = tempAbsPath' </> outputFp

    stakePoolId' <- filter ( /= '\n') <$>
      execCli [ "latest", "stake-pool", "id"
                , "--cold-verification-key-file", poolColdVkeyFp
                ]

    -- Check to see if stake pool was registered
    void $ execCli' execConfig
      [ "latest", "query", "stake-pools"
      , "--out-file", oFpAbs
      ]

    currRegPools <- H.leftFailM $ H.readJsonFile oFpAbs
    poolIds <- H.noteShowM $ H.jsonErrorFail $ Aeson.fromJSON @(Set PoolId) currRegPools
    H.note_ "Check stake pool was successfully registered"

    let stakePoolNotRegisteredFailure = unlines [ "Stake pool was not registered"
                                                , "Expected to be registed: ", show stakePoolId'
                                                , "Currently registered: ", show (Set.toList $ Set.map serialiseToBech32 poolIds)
                                                ]
    if Set.member stakePoolId' $ Set.map (Text.unpack . serialiseToBech32) poolIds
    then return stakePoolId'
    else H.failWithCustom GHC.callStack Nothing stakePoolNotRegisteredFailure

checkStakeKeyRegistered
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => TmpAbsolutePath
  -> NodeConfigFile 'In
  -> SocketPath
  -> EpochNo -- ^ Termination epoch
  -> ExecConfig
  -> String -- ^ Stake address
  -> FilePath -- ^ Output file path of stake address info
  -> m DelegationsAndRewards
checkStakeKeyRegistered tempAbsP nodeConfigFile sPath terminationEpoch execConfig stakeAddr outputFp  =
  GHC.withFrozenCallStack $ do
    let tempAbsPath' = unTmpAbsPath tempAbsP
        oFpAbs = tempAbsPath' </> outputFp

    sAddr <- case deserialiseAddress AsStakeAddress $ Text.pack stakeAddr of
               Just sAddr -> return sAddr
               Nothing -> H.failWithCustom GHC.callStack Nothing $ "Invalid stake address: " <> stakeAddr
    result <- runExceptT $ foldEpochState
                            nodeConfigFile
                            sPath
                            QuickValidation
                            terminationEpoch
                            (DelegationsAndRewards (mempty, mempty))
                            (handler sAddr)

    case result of
      Right (_, dag) -> return dag
      Left e -> do
        void $ execCli' execConfig
          [ "latest", "query", "stake-address-info"
          , "--address", stakeAddr
          , "--out-file", oFpAbs
          ]

        DelegationsAndRewards (rewardsMap, _delegMap) <- H.noteShowM $ H.readJsonFileOk oFpAbs

        H.failWithCustom GHC.callStack Nothing
          $ unlines [ "Stake address in question: "
                    , Text.unpack (serialiseToBech32 sAddr)
                    , "was not registered"
                    , "Current stake info for address in question: "
                    , show $ map serialiseToBech32 $ Map.keys rewardsMap
                    , "foldEpochStateError: " <> show e
                    ]
 where
  handler :: StakeAddress -> AnyNewEpochState -> SlotNo -> BlockNo -> StateT DelegationsAndRewards IO ConditionResult
  handler (StakeAddress network sCred) (AnyNewEpochState sbe newEpochState) _ _ =
    let umap = shelleyBasedEraConstraints sbe $ newEpochState ^. L.nesEsL . L.epochStateUMapL
        dag = L.filterStakePoolDelegsAndRewards umap $ Set.singleton sCred
        allStakeCredentials = umap ^. L.umElemsL -- This does not include pointer addresses
        delegsAndRewards = shelleyBasedEraConstraints sbe $ toDelegationsAndRewards network sbe dag
    in case Map.lookup sCred allStakeCredentials of
         Nothing -> return ConditionNotMet
         Just _ -> StateT.put delegsAndRewards >> return ConditionMet

  toDelegationsAndRewards
    :: L.EraCrypto (ShelleyLedgerEra era) ~ L.StandardCrypto
    => L.Network
    -> ShelleyBasedEra era
    -> (Map (L.Credential L.Staking (L.EraCrypto (ShelleyLedgerEra era))) (L.KeyHash L.StakePool (L.EraCrypto (ShelleyLedgerEra era))), Map (L.Credential 'L.Staking (L.EraCrypto (ShelleyLedgerEra era))) L.Coin)
    -> DelegationsAndRewards
  toDelegationsAndRewards n _ (delegationMap, rewardsMap) =
    let apiDelegationMap = Map.map toApiPoolId $ Map.mapKeys (toApiStakeAddress n) delegationMap
        apiRewardsMap = Map.mapKeys (toApiStakeAddress n) rewardsMap
    in DelegationsAndRewards (apiRewardsMap, apiDelegationMap)

toApiStakeAddress :: L.Network -> L.Credential 'L.Staking L.StandardCrypto -> StakeAddress
toApiStakeAddress = StakeAddress

toApiPoolId ::  L.KeyHash L.StakePool L.StandardCrypto -> PoolId
toApiPoolId = StakePoolKeyHash

createStakeDelegationCertificate
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => TmpAbsolutePath
  -> ShelleyBasedEra era
  -> File (VKey StakeKey) In -- ^ Delegate stake verification key file
  -> String -- ^ Pool id
  -> FilePath
  -> m ()
createStakeDelegationCertificate tempAbsP sbe (File delegatorStakeVerKey) poolId outputFp =
  GHC.withFrozenCallStack $ do
    let tempAbsPath' = unTmpAbsPath tempAbsP
    execCli_
      [ eraToString sbe
      , "stake-address", "stake-delegation-certificate"
      , "--stake-verification-key-file", delegatorStakeVerKey
      , "--stake-pool-id", poolId
      , "--out-file", tempAbsPath' </> outputFp
      ]

createStakeKeyRegistrationCertificate
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => TmpAbsolutePath
  -> AnyShelleyBasedEra
  -> File (VKey StakeKey) In -- ^ Stake verification key file
  -> L.Coin -- ^ deposit amount used only in Conway
  -> FilePath -- ^ Output file path
  -> m ()
createStakeKeyRegistrationCertificate tempAbsP (AnyShelleyBasedEra sbe) (File stakeVerKey) (L.Coin deposit) outputFp = GHC.withFrozenCallStack $ do
  let tempAbsPath' = unTmpAbsPath tempAbsP
      extraArgs = monoidForEraInEon @ConwayEraOnwards (toCardanoEra sbe) $
        const ["--key-reg-deposit-amt", show deposit]
  execCli_ $
    [ eraToString sbe
    , "stake-address", "registration-certificate"
    , "--stake-verification-key-file", stakeVerKey
    , "--out-file", tempAbsPath' </> outputFp
    ]
    <> extraArgs

createScriptStakeRegistrationCertificate
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => TmpAbsolutePath
  -> AnyCardanoEra
  -> FilePath -- ^ Script file
  -> L.Coin -- ^ Registration deposit amount used only in Conway
  -> FilePath -- ^ Output file path
  -> m ()
createScriptStakeRegistrationCertificate tempAbsP (AnyCardanoEra cEra) scriptFile (L.Coin deposit) outputFp =
  GHC.withFrozenCallStack $ do
    let tempAbsPath' = unTmpAbsPath tempAbsP
        extraArgs = monoidForEraInEon @ConwayEraOnwards cEra $
          const ["--key-reg-deposit-amt", show deposit]
    execCli_ $
      [ eraToString cEra
      , "stake-address", "registration-certificate"
      , "--stake-script-file", scriptFile
      , "--out-file", tempAbsPath' </> outputFp
      ]
      <> extraArgs

createScriptStakeDelegationCertificate
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => TmpAbsolutePath
  -> AnyCardanoEra
  -> FilePath -- ^ Script file
  -> File (VKey StakePoolKey) In -- ^ Cold stake pool key
  -> FilePath -- ^ Output file path
  -> m ()
createScriptStakeDelegationCertificate tempAbsP (AnyCardanoEra cEra) scriptFile (File coldvkey) outputFp =
  GHC.withFrozenCallStack $ do
    let tempAbsPath' = unTmpAbsPath tempAbsP
    execCli_
      [ eraToString cEra
      , "stake-address", "stake-delegation-certificate"
      , "--stake-script-file", scriptFile
      , "--cold-verification-key-file", coldvkey
      , "--out-file", tempAbsPath' </> outputFp
      ]

createStakeKeyDeregistrationCertificate
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => TmpAbsolutePath
  -> ShelleyBasedEra era
  -> File (VKey StakeKey) In -- ^ Stake verification key file
  -> L.Coin -- ^ deposit amount used only in Conway
  -> FilePath -- ^ Output file path
  -> m ()
createStakeKeyDeregistrationCertificate tempAbsP sbe (File stakeVerKey) (L.Coin deposit) outputFp =
  GHC.withFrozenCallStack $ do
    let tempAbsPath' = unTmpAbsPath tempAbsP
        extraArgs = monoidForEraInEon @ConwayEraOnwards (toCardanoEra sbe) $
          const ["--key-reg-deposit-amt", show deposit]
    execCli_ $
      [ eraToString sbe
      , "stake-address" , "deregistration-certificate"
      , "--stake-verification-key-file", stakeVerKey
      , "--out-file", tempAbsPath' </> outputFp
      ]
      <> extraArgs

-- | Related documentation: https://github.com/input-output-hk/cardano-node-wiki/blob/main/docs/stake-pool-operations/8_register_stakepool.md
registerSingleSpo
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => AnyShelleyBasedEra
  -> Int -- ^ Identifier for stake pool
  -> TmpAbsolutePath
  -> NodeConfigFile 'In
  -> SocketPath
  -> EpochNo -- ^ Termination epoch
  -> Int -- ^ Testnet magic
  -> L.Coin -- ^ key deposit
  -> ExecConfig
  -> (TxIn, File (SKey PaymentKey) In, String)
  -> m ( String
       , KeyPair StakePoolKey
       , KeyPair VrfKey
       ) -- ^ Result tuple:
         --   1. String: Registered stake pool ID
         --   2. Stake pool cold keys
         --   3. Stake pool VRF keys
registerSingleSpo asbe identifier tap@(TmpAbsolutePath tempAbsPath') nodeConfigFile socketPath termEpoch testnetMag keyDeposit execConfig
                  (fundingInput, File fundingSigninKey, changeAddr) = GHC.withFrozenCallStack $ do
  workDir <- H.note tempAbsPath'

  -- In order to register a stake pool we need two certificates:
  --   1. Delegation certificate pledge
  --   2. Stake pool registration certificate

  -- Stake pool registration certificate construction

  -- 1. Generate stake pool owner stake key pair
  -- NB: This can be used as the pool reward account as well
  let spoReqDir = workDir </>  "spo-"<> show identifier <> "-requirements"

  H.createDirectoryIfMissing_ spoReqDir
  let poolOwnerStakeKeys = KeyPair
        { verificationKey = File $ spoReqDir </> "pool-owner-stake.vkey"
        , signingKey = File $ spoReqDir </> "pool-owner-stake.skey"
        }

  cliStakeAddressKeyGen poolOwnerStakeKeys

  poolownerstakeaddr <- filter (/= '\n')
                          <$> execCli
                                [ "latest", "stake-address", "build"
                                , "--stake-verification-key-file", verificationKeyFp poolOwnerStakeKeys
                                , "--testnet-magic", show @Int testnetMag
                                ]

  -- 2. Generate stake pool owner payment key pair
  let poolOwnerPaymentKeys = KeyPair
        { verificationKey = File $ spoReqDir </> "pool-owner-payment.vkey"
        , signingKey = File $ spoReqDir </> "pool-owner-payment.skey"
        }
  cliAddressKeyGen poolOwnerPaymentKeys

  poolowneraddresswstakecred <-
    execCli [ "latest", "address", "build"
              , "--payment-verification-key-file", verificationKeyFp poolOwnerPaymentKeys
              , "--stake-verification-key-file",  verificationKeyFp poolOwnerStakeKeys
              , "--testnet-magic", show @Int testnetMag
              ]

  -- 3. Generate pool cold keys
  let poolColdKeys = KeyPair
        { verificationKey = File $ spoReqDir </> "pool-cold.vkey"
        , signingKey = File $ spoReqDir </> "pool-cold.skey"
        }

  execCli_
    [ "latest", "node", "key-gen"
    , "--cold-verification-key-file", verificationKeyFp poolColdKeys
    , "--cold-signing-key-file", signingKeyFp poolColdKeys
    , "--operational-certificate-issue-counter-file", spoReqDir </> "operator.counter"
    ]

  -- 4. Generate VRF keys
  let vrfKeys = KeyPair
        { verificationKey = File $ spoReqDir </> "pool-vrf.vkey"
        , signingKey = File $ spoReqDir </> "pool-vrf.skey"
        }
  cliNodeKeyGenVrf vrfKeys

  -- 5. Create registration certificate
  let poolRegCertFp = spoReqDir </> "registration.cert"

  -- The pledge, pool cost and pool margin can all be 0
  execCli_
    [ anyShelleyBasedEraToString asbe
    , "stake-pool", "registration-certificate"
    , "--testnet-magic", show @Int testnetMag
    , "--pool-pledge", "0"
    , "--pool-cost", "0"
    , "--pool-margin", "0"
    , "--cold-verification-key-file", verificationKeyFp poolColdKeys
    , "--vrf-verification-key-file", verificationKeyFp vrfKeys
    , "--reward-account-verification-key-file", verificationKeyFp poolOwnerStakeKeys
    , "--pool-owner-stake-verification-key-file", verificationKeyFp poolOwnerStakeKeys
    , "--out-file", poolRegCertFp
    ]

  -- Create pledge delegation certificate
  -- NB: Pledger and owner can be the same

  -- Create pledger registration certificate
  createStakeKeyRegistrationCertificate tap asbe
    (verificationKey poolOwnerStakeKeys)
    keyDeposit
    (workDir </> "pledger.regcert")

  void $ execCli' execConfig
    [ anyShelleyBasedEraToString asbe
    , "transaction", "build"
    , "--change-address", changeAddr
    , "--tx-in", Text.unpack $ renderTxIn fundingInput
    , "--tx-out", poolowneraddresswstakecred <> "+" <> show @Int 5_000_000
    , "--witness-override", show @Int 3
    , "--certificate-file", workDir </> "pledger.regcert"
    , "--certificate-file", poolRegCertFp
    , "--out-file", workDir </> "pledge-registration-cert.txbody"
    ]

  let pledgeAndPoolRegistrationTx = workDir </> "pledger-and-pool-registration-cert.tx"

  void $ execCli
    [ "latest", "transaction", "sign"
    , "--tx-body-file", workDir </> "pledge-registration-cert.txbody"
    , "--testnet-magic", show @Int testnetMag
    , "--signing-key-file", fundingSigninKey
    , "--signing-key-file", signingKeyFp poolOwnerStakeKeys
    , "--signing-key-file", signingKeyFp poolColdKeys
    , "--out-file", pledgeAndPoolRegistrationTx
    ]

  H.note_ "Submitting pool owner/pledger stake registration cert and funding stake pool owner address..."

  void $ execCli' execConfig
           [ "latest", "transaction", "submit"
           , "--tx-file", pledgeAndPoolRegistrationTx
           ]

  -- Check the pledger/owner stake key was registered
  delegsAndRewards <-
      checkStakeKeyRegistered
        tap
        nodeConfigFile
        socketPath
        termEpoch
        execConfig
        poolownerstakeaddr
        ("spo-"<> show identifier <> "-requirements" </> "pledger.stake.info")

  (pledgerSAddr, _rewards, _poolId) <- H.headM $ mergeDelegsAndRewards delegsAndRewards

  -- Pledger and owner are and can be the same
  Text.unpack (serialiseAddress pledgerSAddr) === poolownerstakeaddr
  let currentRegistedPoolsJson = workDir </> "current-registered.pools.json"

  poolId <- checkStakePoolRegistered
              tap
              execConfig
              (verificationKey poolColdKeys)
              currentRegistedPoolsJson
  return (poolId, poolColdKeys, vrfKeys)

-- | Generates Stake Pool Operator (SPO) voting files, using @cardano-cli@.
--
-- Returns a list of generated @File VoteFile In@ representing the paths to
-- the generated voting files.
-- TODO: unify with DRep.generateVoteFiles
generateVoteFiles :: (HasCallStack, MonadTest m, MonadIO m, MonadCatch m)
  => ConwayEraOnwards era -- ^ The conway era onwards witness for the era in which the
                          -- transaction will be constructed.
  -> H.ExecConfig -- ^ Specifies the CLI execution configuration.
  -> FilePath -- ^ Base directory path where the voting files and directories will be
              -- stored.
  -> String -- ^ Name for the subfolder that will be created under 'work' to store
            -- the output voting files.
  -> String -- ^ Transaction ID string of the governance action.
  -> Word16 -- ^ Index of the governance action.
  -> [(SpoNodeKeys, [Char])] -- ^ List of tuples where each tuple contains a 'SpoNodeKeys'
                              -- representing the SPO keys and a 'String' representing the
                              -- vote type (i.e: "yes", "no", or "abstain").
  -> m [File VoteFile In]
generateVoteFiles ceo execConfig work prefix governanceActionTxId governanceActionIndex allVotes = do
  baseDir <- H.createDirectoryIfMissing $ work </> prefix
  forM (zip [(1 :: Integer)..] allVotes) $ \(idx, (spoKeys, vote)) -> do
    let path = File (baseDir </> "vote-spo-" <> show idx)
    void $ execCli' execConfig
      [ eraToString $ toCardanoEra ceo , "governance", "vote", "create"
      , "--" ++ vote
      , "--governance-action-tx-id", governanceActionTxId
      , "--governance-action-index", show @Word16 governanceActionIndex
      , "--cold-verification-key-file", verificationKeyFp $ poolNodeKeysCold spoKeys
      , "--out-file", unFile path
      ]
    return path
