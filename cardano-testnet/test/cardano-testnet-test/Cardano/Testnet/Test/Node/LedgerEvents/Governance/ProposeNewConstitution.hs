{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Node.LedgerEvents.Governance.ProposeNewConstitution
  ( hprop_ledger_events_propose_new_constitution
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.Testnet

import           Prelude

import qualified Cardano.Ledger.Conway.Governance as Ledger
import           Control.Monad
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra
import qualified Data.Map.Strict as Map
import           Data.String
import qualified Data.Text as Text
import           GHC.IO.Exception (IOException)
import           GHC.Stack (callStack)
import           System.FilePath ((</>))

import           Hedgehog
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Testnet.Process.Cli as P
import qualified Testnet.Process.Run as H

import           Testnet.Components.SPO
import qualified Testnet.Property.Utils as H
import           Testnet.Runtime

newtype AdditionalCatcher
  = IOE IOException
  deriving Show


hprop_ledger_events_propose_new_constitution :: Property
hprop_ledger_events_propose_new_constitution = H.integrationRetryWorkspace 2 "ledger-events-governance-propose-new-constitution" $ \tempAbsBasePath' -> do
  -- Start a local test net
  conf@Conf { tempAbsPath } <- H.noteShowM $ mkConf tempAbsBasePath'
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
        }

  testnetRuntime@TestnetRuntime
    { testnetMagic
    , poolNodes
    , wallets
    }
    <- cardanoTestnet fastTestnetOptions conf
  -- H.failMessage callStack "After cardanoTestnet" -- FAILED SUCCESFULLY HERE

  poolNode1 <- H.headM poolNodes

  poolSprocket1 <- H.noteShow $ nodeSprocket $ poolRuntime poolNode1

  execConfig <- H.mkExecConfig tempBaseAbsPath poolSprocket1

  let socketName' = IO.sprocketName poolSprocket1
      socketBase = IO.sprocketBase poolSprocket1 -- /tmp
      socketPath = socketBase </> socketName'

  H.note_ $ "Sprocket: " <> show poolSprocket1
  H.note_ $ "Abs path: " <> tempAbsBasePath'
  H.note_ $ "Socketpath: " <> socketPath
  H.note_ $ "Foldblocks config file: " <> configurationFile testnetRuntime

  -- Create Conway constitution
  gov <- H.createDirectoryIfMissing $ work </> "governance"
  proposalAnchorFile <- H.note $ work </> gov </> "sample-propoal-anchor"
  consitutionFile <- H.note $ work </> gov </> "sample-constitution"
  constitutionActionFp <- H.note $ work </> gov </> "constitution.action"

  H.writeFile proposalAnchorFile "dummy anchor data"
  H.writeFile consitutionFile "dummy constitution data"
  constitutionHash <- H.execCli' execConfig
    [ "conway", "governance"
    , "hash", "--file-text", consitutionFile
    ]

  proposalAnchorDataHash <- H.execCli' execConfig
    [ "conway", "governance"
    , "hash", "--file-text", proposalAnchorFile
    ]

  let stakeVkeyFp = gov </> "stake.vkey"
      stakeSKeyFp = gov </> "stake.skey"

  _ <- P.cliStakeAddressKeyGen tempAbsPath'
         $ P.KeyNames { P.verificationKeyFile = stakeVkeyFp
                      , P.signingKeyFile = stakeSKeyFp
                      }

  void $ H.execCli' execConfig
    [ "conway", "governance", "action", "create-constitution"
    , "--testnet"
    , "--governance-action-deposit", show @Int 0 -- TODO: Get this from the node
    , "--deposit-return-stake-verification-key-file", stakeVkeyFp
    , "--anchor-url", "https://tinyurl.com/3wrwb2as"
    , "--anchor-data-hash", proposalAnchorDataHash
    , "--constitution-url", "https://tinyurl.com/2pahcy6z"
    , "--constitution-hash", constitutionHash
    , "--out-file", constitutionActionFp
    ]

  txbodyFp <- H.note $ work </> "tx.body"
  txbodySignedFp <- H.note $ work </> "tx.body.signed"

  void $ H.execCli' execConfig
    [ "conway", "query", "utxo"
    , "--address", Text.unpack $ paymentKeyInfoAddr $ head wallets
    , "--cardano-mode"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "utxo-1.json"
    ]

  utxo1Json <- H.leftFailM . H.readJsonFile $ work </> "utxo-1.json"
  UTxO utxo1 <- H.noteShowM $ H.noteShowM $ decodeEraUTxO sbe utxo1Json
  txin1 <- H.noteShow =<< H.headM (Map.keys utxo1)

  void $ H.execCli' execConfig
    [ "conway", "transaction", "build"
    , "--testnet-magic", show @Int testnetMagic
    , "--change-address", Text.unpack $ paymentKeyInfoAddr $ head wallets
    , "--tx-in", Text.unpack $ renderTxIn txin1
    , "--tx-out", Text.unpack (paymentKeyInfoAddr (head wallets)) <> "+" <> show @Int 5_000_001
    , "--proposal-file", constitutionActionFp
    , "--out-file", txbodyFp
    ]

  void $ H.execCli' execConfig
    [ "conway", "transaction", "sign"
    , "--testnet-magic", show @Int testnetMagic
    , "--tx-body-file", txbodyFp
    , "--signing-key-file", paymentSKey $ paymentKeyInfoPair $ wallets !! 0
    , "--out-file", txbodySignedFp
    ]

  void $ H.execCli' execConfig
    [ "conway", "transaction", "submit"
    , "--testnet-magic", show @Int testnetMagic
    , "--tx-file", txbodySignedFp
    ]

  txid <- H.execCli' execConfig
    [ "transaction", "txid"
    , "--tx-file", txbodySignedFp
    ]

  !ret <- runExceptT $ handleIOExceptT IOE
                   $ runExceptT $ foldBlocks
                       (File $ configurationFile testnetRuntime)
                       (File socketPath)
                       FullValidation
                       [] -- Initial accumulator state
                       (foldBlocksAccumulator (fromString . mconcat $ lines txid))


  -- TODO: Need dreps to create votes
  -- TODO: Vote on constitution
  -- TODO: Check for ratification
  case ret of
    Left (IOE e) ->
      H.failMessage callStack $ "foldBlocks failed with: " <> show e
    Right (Left e) ->
      H.failMessage callStack
        $ "foldBlocksCheckConstitutionWasRatified failed with: " <> Text.unpack (renderFoldBlocksError e)
    Right (Right _events) -> success


foldBlocksAccumulator
  :: TxId -- TxId of submitted tx
  -> Env
  -> LedgerState
  -> [LedgerEvent]
  -> BlockInMode -- Block i
  -> [LedgerEvent] -- ^ Accumulator at block i - 1
  -> IO ([LedgerEvent], FoldStatus) -- ^ Accumulator at block i and fold status
foldBlocksAccumulator txid _ _ allEvents _ _ =
  if any filterPoolReap allEvents
  then return (allEvents , StopFold)
  else return ([], ContinueFold)
 where
  filterPoolReap :: LedgerEvent -> Bool
  filterPoolReap (NewGovernanceProposals eventTxId (AnyProposals props)) =
    let _govActionStates = Ledger.proposalsGovActionStates props
    in fromShelleyTxId eventTxId == txid
  filterPoolReap _ = False


