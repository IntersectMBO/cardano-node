{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}


module Testnet.Components.SPO
  ( checkStakeKeyRegistered
  , convertToEraFlag
  , createScriptStakeRegistrationCertificate
  , createStakeDelegationCertificate
  , createStakeKeyRegistrationCertificate
  , decodeEraUTxO
  , registerSingleSpo
  ) where

import           Cardano.Api.Shelley hiding (cardanoEra)

import           Control.Monad
import           Control.Monad.Catch (MonadCatch)
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import           GHC.Stack (HasCallStack)
import qualified GHC.Stack as GHC
import           System.FilePath.Posix ((</>))

import           Testnet.Filepath
import           Testnet.Process.Cli
import           Testnet.Process.Run (execCli, execCli', execCli_)
import           Testnet.Property.Utils
import           Testnet.Start.Types

import           Hedgehog
import           Hedgehog.Extras (ExecConfig, threadDelay)
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

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
  -> ExecConfig
  -> String -- ^ Stake address
  -> FilePath -- ^ Output file path of stake address info
  -> m DelegationsAndRewards
checkStakeKeyRegistered tempAbsP execConfig stakeAddr outputFp  =
  GHC.withFrozenCallStack $ do
    let tempAbsPath' = unTmpAbsPath tempAbsP
        oFpAbs = tempAbsPath' </> outputFp

    sAddr <- case deserialiseAddress AsStakeAddress $ Text.pack stakeAddr of
               Just sAddr -> return sAddr
               Nothing -> H.failWithCustom GHC.callStack Nothing $ "Invalid stake address: " <> stakeAddr

    void $ execCli' execConfig
      [ "query", "stake-address-info"
      , "--address", stakeAddr
      , "--out-file", oFpAbs
      ]

    pledgerStakeInfo <- H.leftFailM $ H.readJsonFile oFpAbs
    dag@(DelegationsAndRewards (rewardsMap, _delegMap)) <- H.noteShowM $ H.jsonErrorFail $ Aeson.fromJSON @DelegationsAndRewards pledgerStakeInfo
    case Map.lookup sAddr rewardsMap of
      Nothing -> H.failWithCustom GHC.callStack Nothing
                   $ unlines [ "Stake address: "
                             , Text.unpack (serialiseToBech32 sAddr)
                             , "was not registered"
                             , "Current registered stake keys: "
                             , show $ map serialiseToBech32 $ Map.keys rewardsMap
                             ]
      Just _ -> return dag


createStakeDelegationCertificate
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => TmpAbsolutePath
  -> AnyCardanoEra
  -> FilePath -- ^ Delegate stake verification key file
  -> String -- ^ Pool id
  -> FilePath
  -> m ()
createStakeDelegationCertificate tempAbsP anyCera delegatorStakeVerKey poolId outputFp =
  GHC.withFrozenCallStack $ do
    let tempAbsPath' = unTmpAbsPath tempAbsP
    void $ execCli
      [ "stake-address", "delegation-certificate"
      , convertToEraFlag anyCera
      , "--stake-verification-key-file", delegatorStakeVerKey
      , "--stake-pool-id", poolId
      , "--out-file", tempAbsPath' </> outputFp
      ]

createStakeKeyRegistrationCertificate
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => TmpAbsolutePath
  -> AnyCardanoEra
  -> FilePath -- ^ Stake verification key file
  -> FilePath -- ^ Output file path
  -> m ()
createStakeKeyRegistrationCertificate tempAbsP anyCEra stakeVerKey outputFp =
  GHC.withFrozenCallStack $ do
    let tempAbsPath' = unTmpAbsPath tempAbsP

    void $ execCli
      [ "stake-address", "registration-certificate"
      , convertToEraFlag anyCEra
      , "--stake-verification-key-file", stakeVerKey
      , "--out-file", tempAbsPath' </> outputFp
      ]

createScriptStakeRegistrationCertificate
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => TmpAbsolutePath
  -> AnyCardanoEra
  -> FilePath -- ^ Script file
  -> Int -- ^ Registration deposit amount
  -> FilePath -- ^ Output file path
  -> m ()
createScriptStakeRegistrationCertificate tempAbsP anyCEra scriptFile deposit outputFp =
  GHC.withFrozenCallStack $ do
    let tempAbsPath' = unTmpAbsPath tempAbsP

    void $ execCli
      [ convertToEraString anyCEra
      , "stake-address", "registration-certificate"
      , "--stake-script-file", scriptFile
      , "--key-reg-deposit-amt", show deposit
      , "--out-file", tempAbsPath' </> outputFp
      ]


-- TODO: Remove me and replace with new era based commands
-- i.e "conway", "babbage" etc
convertToEraFlag :: AnyCardanoEra -> String
convertToEraFlag (AnyCardanoEra e) =
  case e of
    ConwayEra -> "--conway-era"
    BabbageEra -> "--babbage-era"
    AlonzoEra -> "--alonzo-era"
    MaryEra -> "--mary-era"
    AllegraEra -> "--allegra-era"
    ShelleyEra -> "--shelley-era"
    ByronEra -> "--byron-era"

-- | Related documentation: https://github.com/input-output-hk/cardano-node-wiki/blob/main/docs/stake-pool-operations/8_register_stakepool.md
registerSingleSpo
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => Int -- ^ Identifier for stake pool
  -> TmpAbsolutePath
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
registerSingleSpo identifier tap@(TmpAbsolutePath tempAbsPath') cTestnetOptions execConfig
                  (fundingInput, fundingSigninKey, changeAddr) = GHC.withFrozenCallStack $ do
  let testnetMag = cardanoTestnetMagic cTestnetOptions
      eraFlag= convertToEraFlag $ cardanoNodeEra cTestnetOptions

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

  _ <- cliStakeAddressKeyGen tempAbsPath'
    $ KeyNames poolOwnerstakeVkeyFp poolOwnerstakeSKeyFp

  poolownerstakeaddr <- filter (/= '\n')
                          <$> execCli
                                [ "stake-address", "build"
                                , "--stake-verification-key-file", poolOwnerstakeVkeyFp
                                , "--testnet-magic", show @Int testnetMag
                                ]

  -- 2. Generate stake pool owner payment key pair
  let poolOwnerPaymentVkeyFp = spoReqDir </> "pool-owner-payment.vkey"
      poolOwnerPaymentSkeyFp = spoReqDir </> "pool-owner-payment.skey"
  _ <- cliAddressKeyGen tempAbsPath'
         $ KeyNames poolOwnerPaymentVkeyFp poolOwnerPaymentSkeyFp

  poolowneraddresswstakecred <- execCli [ "address", "build"
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
  _ <- cliNodeKeyGenVrf tempAbsPath'
         $ KeyNames vrfVkeyFp vrfSkeyFp

  -- 5. Create registration certificate
  let poolRegCertFp = spoReqDir </> "registration.cert"

  -- The pledge, pool cost and pool margin can all be 0
  execCli_
    [ "stake-pool", "registration-certificate"
    , "--babbage-era"
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

  createStakeKeyRegistrationCertificate
    tap
    (cardanoNodeEra cTestnetOptions)
    poolOwnerstakeVkeyFp
    (workDir </> "pledger.regcert")

  void $ execCli' execConfig
    [ "transaction", "build"
    , eraFlag
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
  -- TODO: Currently we can't propagate the error message thrown by checkStakeKeyRegistered when using byDurationM
  -- Instead we wait 15 seconds
  threadDelay 15_000000
  -- Check the pledger/owner stake key was registered
  delegsAndRewards <-
    checkStakeKeyRegistered
      tap
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

