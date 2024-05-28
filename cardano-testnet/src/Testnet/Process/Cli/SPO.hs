{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Testnet.Process.Cli.SPO
  ( checkStakeKeyRegistered
  , createScriptStakeRegistrationCertificate
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
import           Data.Word (Word32)
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
  -> FilePath -- ^ Stake pool cold verification key file
  -> FilePath -- ^ Output file path of stake pool info
  -> m String -- ^ Stake pool ID
checkStakePoolRegistered tempAbsP execConfig poolColdVkeyFp outputFp =
  GHC.withFrozenCallStack $ do
    let tempAbsPath' = unTmpAbsPath tempAbsP
        oFpAbs = tempAbsPath' </> outputFp

    stakePoolId' <- filter ( /= '\n') <$>
      execCli [ "stake-pool", "id"
                , "--cold-verification-key-file", poolColdVkeyFp
                ]

    -- Check to see if stake pool was registered
    void $ execCli' execConfig
      [ "query", "stake-pools"
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
          [ "query", "stake-address-info"
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
  handler :: StakeAddress -> AnyNewEpochState -> SlotNo -> BlockNo -> StateT DelegationsAndRewards IO LedgerStateCondition
  handler (StakeAddress network sCred) (AnyNewEpochState sbe newEpochState _) _ _ =
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
  -> AnyCardanoEra
  -> FilePath -- ^ Delegate stake verification key file
  -> String -- ^ Pool id
  -> FilePath
  -> m ()
createStakeDelegationCertificate tempAbsP (AnyCardanoEra cEra) delegatorStakeVerKey poolId outputFp =
  GHC.withFrozenCallStack $ do
    let tempAbsPath' = unTmpAbsPath tempAbsP
    execCli_
      [ eraToString cEra
      , "stake-address", "stake-delegation-certificate"
      , "--stake-verification-key-file", delegatorStakeVerKey
      , "--stake-pool-id", poolId
      , "--out-file", tempAbsPath' </> outputFp
      ]

createStakeKeyRegistrationCertificate
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => TmpAbsolutePath
  -> AnyCardanoEra
  -> FilePath -- ^ Stake verification key file
  -> Int -- ^ deposit amount used only in Conway
  -> FilePath -- ^ Output file path
  -> m ()
createStakeKeyRegistrationCertificate tempAbsP (AnyCardanoEra cEra) stakeVerKey deposit outputFp = GHC.withFrozenCallStack $ do
  let tempAbsPath' = unTmpAbsPath tempAbsP
      extraArgs = monoidForEraInEon @ConwayEraOnwards cEra $
        const ["--key-reg-deposit-amt", show deposit]
  execCli_ $
    [ eraToString cEra
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
  -> Int -- ^ Registration deposit amount used only in Conway
  -> FilePath -- ^ Output file path
  -> m ()
createScriptStakeRegistrationCertificate tempAbsP (AnyCardanoEra cEra) scriptFile deposit outputFp =
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

createStakeKeyDeregistrationCertificate
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => TmpAbsolutePath
  -> AnyCardanoEra
  -> FilePath -- ^ Stake verification key file
  -> Int -- ^ deposit amount used only in Conway
  -> FilePath -- ^ Output file path
  -> m ()
createStakeKeyDeregistrationCertificate tempAbsP (AnyCardanoEra cEra) stakeVerKey deposit outputFp =
  GHC.withFrozenCallStack $ do
    let tempAbsPath' = unTmpAbsPath tempAbsP
        extraArgs = monoidForEraInEon @ConwayEraOnwards cEra $
          const ["--key-reg-deposit-amt", show deposit]
    execCli_ $
      [ eraToString cEra
      , "stake-address" , "deregistration-certificate"
      , "--stake-verification-key-file", stakeVerKey
      , "--out-file", tempAbsPath' </> outputFp
      ]
      <> extraArgs

-- | Related documentation: https://github.com/input-output-hk/cardano-node-wiki/blob/main/docs/stake-pool-operations/8_register_stakepool.md
registerSingleSpo
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => Int -- ^ Identifier for stake pool
  -> TmpAbsolutePath
  -> NodeConfigFile 'In
  -> SocketPath
  -> EpochNo -- ^ Termination epoch
  -> CardanoTestnetOptions
  -> ExecConfig
  -> (TxIn, FilePath, String)
  -> m ( String
       , FilePath
       , FilePath
       , FilePath
       , FilePath
       ) -- ^ Result tuple:
         --   1. String: Registered stake pool ID
         --   2. FilePath: Stake pool cold signing key
         --   3. FilePath: Stake pool cold verification key
         --   4. FilePath: Stake pool VRF signing key
         --   5. FilePath: Stake pool VRF verification key
registerSingleSpo identifier tap@(TmpAbsolutePath tempAbsPath') nodeConfigFile socketPath termEpoch cTestnetOptions execConfig
                  (fundingInput, fundingSigninKey, changeAddr) = GHC.withFrozenCallStack $ do
  let testnetMag = cardanoTestnetMagic cTestnetOptions

  workDir <- H.note tempAbsPath'

  -- In order to register a stake pool we need two certificates:
  --   1. Delegation certificate pledge
  --   2. Stake pool registration certificate

  -- Stake pool registration certificate construction

  -- 1. Generate stake pool owner stake key pair
  -- NB: This can be used as the pool reward account as well
  let spoReqDir = workDir </>  "spo-"<> show identifier <> "-requirements"

  H.createDirectoryIfMissing_ spoReqDir
  let poolOwnerstakeVkeyFp = spoReqDir </> "pool-owner-stake.vkey"
      poolOwnerstakeSKeyFp = spoReqDir </> "pool-owner-stake.skey"

  cliStakeAddressKeyGen
    $ KeyPair (File poolOwnerstakeVkeyFp) (File poolOwnerstakeSKeyFp)

  poolownerstakeaddr <- filter (/= '\n')
                          <$> execCli
                                [ "stake-address", "build"
                                , "--stake-verification-key-file", poolOwnerstakeVkeyFp
                                , "--testnet-magic", show @Int testnetMag
                                ]

  -- 2. Generate stake pool owner payment key pair
  let poolOwnerPaymentVkeyFp = spoReqDir </> "pool-owner-payment.vkey"
      poolOwnerPaymentSkeyFp = spoReqDir </> "pool-owner-payment.skey"
  cliAddressKeyGen
     $ KeyPair (File poolOwnerPaymentVkeyFp) (File poolOwnerPaymentSkeyFp)

  poolowneraddresswstakecred <-
    execCli [ "address", "build"
              , "--payment-verification-key-file", poolOwnerPaymentVkeyFp
              , "--stake-verification-key-file",  poolOwnerstakeVkeyFp
              , "--testnet-magic", show @Int testnetMag
              ]

  -- 3. Generate pool cold keys
  let poolColdVkeyFp = spoReqDir </> "pool-cold.vkey"
      poolColdSkeyFp = spoReqDir </> "pool-cold.skey"

  execCli_
    [ "node", "key-gen"
    , "--cold-verification-key-file", poolColdVkeyFp
    , "--cold-signing-key-file", poolColdSkeyFp
    , "--operational-certificate-issue-counter-file", spoReqDir </> "operator.counter"
    ]

  -- 4. Generate VRF keys
  let vrfVkeyFp = spoReqDir </> "pool-vrf.vkey"
      vrfSkeyFp = spoReqDir </> "pool-vrf.skey"
  cliNodeKeyGenVrf
     $ KeyPair (File vrfVkeyFp) (File vrfSkeyFp)

  -- 5. Create registration certificate
  let poolRegCertFp = spoReqDir </> "registration.cert"
  let era = cardanoNodeEra cTestnetOptions

  -- The pledge, pool cost and pool margin can all be 0
  execCli_
    [ anyEraToString era
    , "stake-pool", "registration-certificate"
    , "--testnet-magic", show @Int testnetMag
    , "--pool-pledge", "0"
    , "--pool-cost", "0"
    , "--pool-margin", "0"
    , "--cold-verification-key-file", poolColdVkeyFp
    , "--vrf-verification-key-file", vrfVkeyFp
    , "--reward-account-verification-key-file", poolOwnerstakeVkeyFp
    , "--pool-owner-stake-verification-key-file", poolOwnerstakeVkeyFp
    , "--out-file", poolRegCertFp
    ]

  -- Create pledge delegation certificate
  -- NB: Pledger and owner can be the same

  -- Create pledger registration certificate

  createStakeKeyRegistrationCertificate tap era
    poolOwnerstakeVkeyFp
    2_000_000
    (workDir </> "pledger.regcert")

  void $ execCli' execConfig
    [ anyEraToString era
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
    [ "transaction", "sign"
    , "--tx-body-file", workDir </> "pledge-registration-cert.txbody"
    , "--testnet-magic", show @Int testnetMag
    , "--signing-key-file", fundingSigninKey
    , "--signing-key-file", poolOwnerstakeSKeyFp
    , "--signing-key-file", poolColdSkeyFp
    , "--out-file", pledgeAndPoolRegistrationTx
    ]

  H.note_ "Submitting pool owner/pledger stake registration cert and funding stake pool owner address..."

  void $ execCli' execConfig
           [ "transaction", "submit"
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
              poolColdVkeyFp
              currentRegistedPoolsJson
  return (poolId, poolColdSkeyFp, poolColdVkeyFp, vrfSkeyFp, vrfVkeyFp)

-- | Generates Stake Pool Operator (SPO) voting files, using @cardano-cli@.
--
-- Returns a list of generated @File VoteFile In@ representing the paths to
-- the generated voting files.
-- TODO: unify with DRep.generateVoteFiles
generateVoteFiles :: (MonadTest m, MonadIO m, MonadCatch m)
  => ConwayEraOnwards era -- ^ The conway era onwards witness for the era in which the
                          -- transaction will be constructed.
  -> H.ExecConfig -- ^ Specifies the CLI execution configuration.
  -> FilePath -- ^ Base directory path where the voting files and directories will be
              -- stored.
  -> String -- ^ Name for the subfolder that will be created under 'work' to store
            -- the output voting files.
  -> String -- ^ Transaction ID string of the governance action.
  -> Word32 -- ^ Index of the governance action.
  -> [(PoolNodeKeys, [Char])] -- ^ List of tuples where each tuple contains a 'PoolNodeKeys'
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
      , "--governance-action-index", show @Word32 governanceActionIndex
      , "--cold-verification-key-file", verificationKeyFp $ poolNodeKeysCold spoKeys
      , "--out-file", unFile path
      ]
    return path
