{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Cli.LeiosStakePoolRegistration
  ( hprop_leios_stake_pool_registration
  ) where

import           Cardano.Api as Api
import           Cardano.Api.Experimental.Certificate (PoolId)

import           Cardano.Testnet

import           Prelude

import           Control.Monad
import           Control.Monad.Catch (MonadCatch)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BSC
import           Data.Default.Class
import           Data.List (isInfixOf)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import           GHC.Stack (HasCallStack, withFrozenCallStack)
import           System.FilePath ((</>))

import           Testnet.Process.Cli.Keys
import           Testnet.Process.Run (execCli, execCli', execCli_, mkExecConfig)
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Start.Types
import           Testnet.Types

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- | Register a stake pool that carries a Leios (BLS) voting key.
--
-- The offline half exercises @cardano-cli@ alone: it generates a BLS key pair
-- with @node key-gen-BLS@ and builds the Dijkstra registration certificate from
-- the BLS signing key, which is the only way to build one -- the certificate
-- carries the verification key and a proof of possession, and the CLI derives
-- both from the signing key.
--
-- The on-chain half submits that certificate and checks that the pool shows up
-- in @query stake-pools@ with the registered BLS key visible in
-- @query pool-state@.
--
-- Note that this test talks to the node only through @cardano-cli@ queries,
-- and the testnet starts in Conway and hard forks into Dijkstra at epoch 1,
-- so the test waits for the era before doing anything on chain. Both choices
-- date from when @cardano-api@ ignored @TestDijkstraHardForkAtEpoch@ and
-- @foldBlocks@ could not follow the chain into Dijkstra; the pinned api now
-- honours the trigger, so the epoch-state helpers and an epoch-0 fork should
-- also work and this test could be simplified.
--
-- Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/leios stake pool registration/"'@
hprop_leios_stake_pool_registration :: Property
hprop_leios_stake_pool_registration = integrationRetryWorkspace 2 "leios-stake-pool-registration" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  -- Start a local test net
  conf@Conf { tempAbsPath } <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath

  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  -- The BLS key material a stake pool registers for Leios only exists from
  -- Dijkstra onwards.
  let sbe = ShelleyBasedEraDijkstra
      asbe = AnyShelleyBasedEra sbe
      eraName = eraToString sbe
      creationOptions = def
        { creationEra = asbe
        , creationGenesisOptions = def { genesisEpochLength = 200 }
        }
      -- The new epoch state logger reconstructs the ledger state with
      -- 'foldBlocks', which cannot follow this testnet across the Dijkstra hard
      -- fork.
      runtimeOptions = def { runtimeEnableNewEpochStateLogging = False }

  TestnetRuntime
    { testnetMagic
    , testnetNodes
    , wallets=wallet0:_
    }
    <- createAndRunTestnet creationOptions runtimeOptions conf

  node <- H.headM testnetNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket node
  execConfig <- mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic

  H.note_ $ "Sprocket: " <> show poolSprocket1
  H.note_ $ "Abs path: " <> tempAbsBasePath'
  H.note_ $ "Socketpath: " <> unFile (nodeSocketPath node)

  --------------------------------------------------------------------------
  -- Offline: BLS key material and the three registration certificates
  --------------------------------------------------------------------------

  H.note_ "Generate the pool's Leios BLS key pair"
  let blsKeys = KeyPair { verificationKey = File $ work </> "bls.vkey"
                        , signingKey = File $ work </> "bls.skey"
                        }

  execCli_
    [ eraName, "node", "key-gen-BLS"
    , "--verification-key-file", verificationKeyFp blsKeys
    , "--signing-key-file", signingKeyFp blsKeys
    ]

  H.note_ "Generate the pool's cold, VRF and reward account keys"
  let poolColdKeys = KeyPair { verificationKey = File $ work </> "pool-cold.vkey"
                             , signingKey = File $ work </> "pool-cold.skey"
                             }
      vrfKeys = KeyPair { verificationKey = File $ work </> "pool-vrf.vkey"
                        , signingKey = File $ work </> "pool-vrf.skey"
                        }
      -- The pool owner's stake key doubles as the pool's reward account.
      poolOwnerStakeKeys = KeyPair { verificationKey = File $ work </> "pool-owner-stake.vkey"
                                   , signingKey = File $ work </> "pool-owner-stake.skey"
                                   }

  cliNodeKeyGen poolColdKeys (File $ work </> "operator.counter")
  cliNodeKeyGenVrf vrfKeys
  cliStakeAddressKeyGen poolOwnerStakeKeys

  -- The pledge, pool cost and pool margin can all be 0.
  let registrationCertArgs outFp blsArgs =
        [ eraName, "stake-pool", "registration-certificate"
        , "--testnet-magic", show @Int testnetMagic
        , "--pool-pledge", "0"
        , "--pool-cost", "0"
        , "--pool-margin", "0"
        , "--cold-verification-key-file", verificationKeyFp poolColdKeys
        , "--vrf-verification-key-file", verificationKeyFp vrfKeys
        , "--pool-reward-account-verification-key-file", verificationKeyFp poolOwnerStakeKeys
        , "--pool-owner-stake-verification-key-file", verificationKeyFp poolOwnerStakeKeys
        ]
          <> blsArgs
          <> [ "--out-file", outFp ]

      certFromSigningKeyFp = work </> "registration-from-signing-key.cert"

  H.note_ "Build the registration certificate from the BLS signing key"
  execCli_ $ registrationCertArgs certFromSigningKeyFp
    [ "--bls-signing-key-file", signingKeyFp blsKeys ]

  --------------------------------------------------------------------------
  -- On-chain: submit the certificate built from the verification key
  --------------------------------------------------------------------------

  H.note_ "Wait for the chain to hard fork into Dijkstra"
  H.byDurationM 1 120 "The testnet never hard forked into Dijkstra" $ do
    tip :: Map Text Aeson.Value <-
      queryJson execConfig (work </> "tip.json") [eraName, "query", "tip"]
    tipEra <- H.nothingFail (Map.lookup "era" tip) >>= H.jsonErrorFail . Aeson.fromJSON @Text
    Text.toLower tipEra === Text.pack eraName

  -- The reward account has to be registered before the pool can name it, and
  -- the certificate has to name the deposit the protocol currently asks for.
  pparams :: Map Text Aeson.Value <-
    queryJson execConfig (work </> "protocol-parameters.json")
      [eraName, "query", "protocol-parameters"]
  keyDeposit <- H.noteShowM $
    H.nothingFail (Map.lookup "stakeAddressDeposit" pparams)
      >>= H.jsonErrorFail . Aeson.fromJSON @Integer

  let ownerRegCertFp = work </> "pool-owner-stake.regcert"
  execCli_
    [ eraName, "stake-address", "registration-certificate"
    , "--stake-verification-key-file", verificationKeyFp poolOwnerStakeKeys
    , "--key-reg-deposit-amt", show keyDeposit
    , "--out-file", ownerRegCertFp
    ]

  poolRegistrationTxBodyFp <- H.note $ work </> "pool-registration.txbody"
  poolRegistrationTxFp <- H.note $ work </> "pool-registration.tx"

  -- Spend every UTxO the funding wallet has, so that the pool deposit and the
  -- stake key deposit are certainly covered.
  utxo :: Map Text Aeson.Value <-
    queryJson execConfig (work </> "wallet0-utxo.json")
      [eraName, "query", "utxo", "--address", Text.unpack $ paymentKeyInfoAddr wallet0]
  txIns <- H.noteShow $ concat [["--tx-in", Text.unpack txIn] | txIn <- Map.keys utxo]
  H.assertWith txIns (not . null)

  void $ execCli' execConfig $
    [ eraName, "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet0
    ]
      <> txIns
      <> [ "--certificate-file", ownerRegCertFp
         , "--certificate-file", certFromSigningKeyFp
         , "--witness-override", show @Int 3
         , "--out-file", poolRegistrationTxBodyFp
         ]

  -- The registration is witnessed by the funding key, by the reward account /
  -- owner stake key, and by the pool's cold key.
  void $ execCli' execConfig
    [ eraName, "transaction", "sign"
    , "--tx-body-file", poolRegistrationTxBodyFp
    , "--signing-key-file", signingKeyFp $ paymentKeyInfoPair wallet0
    , "--signing-key-file", signingKeyFp poolOwnerStakeKeys
    , "--signing-key-file", signingKeyFp poolColdKeys
    , "--out-file", poolRegistrationTxFp
    ]

  poolId <- H.noteM $ filter (/= '\n') <$>
    execCli
      [ eraName, "stake-pool", "id"
      , "--cold-verification-key-file", verificationKeyFp poolColdKeys
      ]

  H.note_ "Check that the stake pool isn't registered yet"
  registeredPools <- queryRegisteredStakePools execConfig eraName (work </> "stake-pools-before.json")
  H.assertWith registeredPools (Set.notMember poolId)

  void $ execCli' execConfig
    [ eraName, "transaction", "submit"
    , "--tx-file", poolRegistrationTxFp
    ]

  H.note_ "Check that the stake pool is registered"
  H.byDurationM 1 60 "The stake pool was never registered" $ do
    pools <- queryRegisteredStakePools execConfig eraName (work </> "stake-pools-after.json")
    H.assertWith pools (Set.member poolId)

  H.note_ "Check that the registered pool carries the BLS key we generated"
  blsVKey <- H.leftFailM . H.evalIO $
    readFileTextEnvelope @(VerificationKey BlsKey) (verificationKey blsKeys)
  blsVKeyHex <- H.noteShow . BSC.unpack $ serialiseToRawBytesHex blsVKey

  let poolStateFp = work </> "pool-state.json"
  void $ execCli' execConfig
    [ eraName, "query", "pool-state"
    , "--stake-pool-id", poolId
    , "--output-json"
    , "--out-file", poolStateFp
    ]
  poolState <- H.readFile poolStateFp
  H.note_ poolState
  H.assertWith poolState (blsVKeyHex `isInfixOf`)

-- | Run a @cardano-cli@ query that writes JSON to a file, and decode it.
queryJson
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack, Aeson.FromJSON a)
  => H.ExecConfig
  -> FilePath -- ^ where to dump the query output
  -> [String] -- ^ the query, without its output options
  -> m a
queryJson execConfig outFp args = withFrozenCallStack $ do
  void $ execCli' execConfig $ args <> ["--output-json", "--out-file", outFp]
  H.leftFailM (H.readJsonFile outFp) >>= H.jsonErrorFail . Aeson.fromJSON

-- | The set of stake pool ids the node currently has registered, rendered the
-- same way @stake-pool id@ renders them.
queryRegisteredStakePools
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => H.ExecConfig
  -> String -- ^ era name
  -> FilePath -- ^ where to dump the query output
  -> m (Set String)
queryRegisteredStakePools execConfig eraName outFp = withFrozenCallStack $ do
  void $ execCli' execConfig
    [ eraName, "query", "stake-pools"
    , "--output-json"
    , "--out-file", outFp
    ]
  poolsJson <- H.leftFailM $ H.readJsonFile outFp
  poolIds <- H.jsonErrorFail $ Aeson.fromJSON @(Set PoolId) poolsJson
  pure $ Set.map (Text.unpack . serialiseToBech32) poolIds
