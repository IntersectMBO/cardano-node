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
import           Data.Word
import           GHC.IO.Exception (IOException)
import           GHC.Stack (callStack)
import           System.FilePath ((</>))

import           Hedgehog
import qualified Hedgehog.Extras as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
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

  let drepVkeyFp :: Int -> FilePath
      drepVkeyFp n = gov </> "drep-keys" <>"drep" <> show n <> ".vkey"

      drepSKeyFp :: Int -> FilePath
      drepSKeyFp n = gov </> "drep-keys" <>"drep" <> show n <> ".skey"

  -- Create DReps -- TODO: Refactor with shelleyKeyGen
  forM_ [(1::Int)..3] $ \n -> do
   H.execCli' execConfig
     [ "conway", "governance", "drep", "key-gen"
     , "--verification-key-file", drepVkeyFp n
     , "--signing-key-file", drepSKeyFp n
     ]

  -- Create Drep registration certificates
  let drepCertFile :: Int -> FilePath
      drepCertFile n = gov </> "drep-keys" <>"drep" <> show n <> ".regcert"
  forM_ [(1::Int)..3] $ \n -> do
    H.execCli' execConfig
       [ "conway", "governance", "drep", "registration-certificate"
       , "--drep-verification-key-file", drepVkeyFp n
       , "--key-reg-deposit-amt", show @Int 0
       , "--out-file", drepCertFile n
       ]

  -- Retrieve UTxOs for registration submission

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

  drepRegTxbodyFp <- H.note $ work </> "drep.registration.txbody"
  drepRegTxSignedFp <- H.note $ work </> "drep.registration.tx"

  void $ H.execCli' execConfig
    [ "conway", "transaction", "build"
    , "--testnet-magic", show @Int testnetMagic
    , "--change-address", Text.unpack $ paymentKeyInfoAddr $ head wallets
    , "--tx-in", Text.unpack $ renderTxIn txin1
    , "--tx-out", Text.unpack (paymentKeyInfoAddr (wallets !! 1)) <> "+" <> show @Int 5_000_000
    , "--certificate-file", drepCertFile 1
    , "--certificate-file", drepCertFile 2
    , "--certificate-file", drepCertFile 3
    , "--witness-override", show @Int 4
    , "--out-file", drepRegTxbodyFp
    ]

  void $ H.execCli' execConfig
    [ "conway", "transaction", "sign"
    , "--testnet-magic", show @Int testnetMagic
    , "--tx-body-file", drepRegTxbodyFp
    , "--signing-key-file", paymentSKey $ paymentKeyInfoPair $ head wallets
    , "--signing-key-file", drepSKeyFp 1
    , "--signing-key-file", drepSKeyFp 2
    , "--signing-key-file", drepSKeyFp 3
    , "--out-file", drepRegTxSignedFp
    ]

  void $ H.execCli' execConfig
    [ "conway", "transaction", "submit"
    , "--testnet-magic", show @Int testnetMagic
    , "--tx-file", drepRegTxSignedFp
    ]

  -- Create constitution proposal

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
    , "--address", Text.unpack $ paymentKeyInfoAddr $ wallets !! 1
    , "--cardano-mode"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "utxo-2.json"
    ]

  utxo2Json <- H.leftFailM . H.readJsonFile $ work </> "utxo-2.json"
  UTxO utxo2 <- H.noteShowM $ H.noteShowM $ decodeEraUTxO sbe utxo2Json
  txin2 <- H.noteShow =<< H.headM (Map.keys utxo2)

  void $ H.execCli' execConfig
    [ "conway", "transaction", "build"
    , "--testnet-magic", show @Int testnetMagic
    , "--change-address", Text.unpack $ paymentKeyInfoAddr $ wallets !! 1
    , "--tx-in", Text.unpack $ renderTxIn txin2
    , "--tx-out", Text.unpack (paymentKeyInfoAddr (wallets !! 0)) <> "+" <> show @Int 5_000_000
    , "--proposal-file", constitutionActionFp
    , "--out-file", txbodyFp
    ]

  void $ H.execCli' execConfig
    [ "conway", "transaction", "sign"
    , "--testnet-magic", show @Int testnetMagic
    , "--tx-body-file", txbodyFp
    , "--signing-key-file", paymentSKey $ paymentKeyInfoPair $ wallets !! 1
    , "--out-file", txbodySignedFp
    ]

  void $ H.execCli' execConfig
    [ "conway", "transaction", "submit"
    , "--testnet-magic", show @Int testnetMagic
    , "--tx-file", txbodySignedFp
    ]

  txidString <- mconcat . lines <$> H.execCli' execConfig
    [ "transaction", "txid"
    , "--tx-file", txbodySignedFp
    ]
  !propSubmittedResult
    <- runExceptT $ handleIOExceptT IOE
                  $ runExceptT $ foldBlocks
                      (File $ configurationFile testnetRuntime)
                      (File socketPath)
                      FullValidation
                      [] -- Initial accumulator state
                      (foldBlocksCheckProposalWasSubmitted (fromString txidString))

  newProposalEvent <- case propSubmittedResult of
                        Left (IOE e) ->
                          H.failMessage callStack
                            $ "foldBlocksCheckProposalWasSubmitted failed with: " <> show e
                        Right (Left e) ->
                          H.failMessage callStack
                            $ "foldBlocksCheckProposalWasSubmitted failed with: " <> Text.unpack (renderFoldBlocksError e)
                        Right (Right events) -> return events

  governanceActionIndex <- retrieveGoveranceActionIndex (fromString txidString) newProposalEvent

  let voteFp :: Int -> FilePath
      voteFp n = work </> gov </> "vote-" <> show n

  -- Proposal was successfully submitted, now we vote on the proposal and confirm it was ratified
  forM_ [(1::Int)..3] $ \n -> do
    H.execCli' execConfig
      [ "conway", "governance", "vote", "create"
      , "--yes"
      , "--governance-action-tx-id", txidString
      , "--governance-action-index", show @Word32 governanceActionIndex
      , "--drep-verification-key-file", drepVkeyFp n
      , "--out-file", voteFp n
      ]

  -- We need more UTxOs

  void $ H.execCli' execConfig
   [ "conway", "query", "utxo"
   , "--address", Text.unpack $ paymentKeyInfoAddr $ head wallets
   , "--cardano-mode"
   , "--testnet-magic", show @Int testnetMagic
   , "--out-file", work </> "utxo-3.json"
   ]

  utxo3Json <- H.leftFailM . H.readJsonFile $ work </> "utxo-3.json"
  UTxO utxo3 <- H.noteShowM $ H.noteShowM $ decodeEraUTxO sbe utxo3Json
  txin3 <- H.noteShow =<< H.headM (Map.keys utxo3)

  voteTxFp <- H.note $ work </> gov </> "vote.tx"
  voteTxBodyFp <- H.note $ work </> gov </> "vote.txbody"

  -- Submit votes
  void $ H.execCli' execConfig
    [ "conway", "transaction", "build"
    , "--testnet-magic", show @Int testnetMagic
    , "--change-address", Text.unpack $ paymentKeyInfoAddr $ head wallets
    , "--tx-in", Text.unpack $ renderTxIn txin3
    , "--tx-out", Text.unpack (paymentKeyInfoAddr (wallets !! 1)) <> "+" <> show @Int 3_000_000
    , "--vote-file", voteFp 1
    , "--vote-file", voteFp 2
    , "--vote-file", voteFp 3
    , "--witness-override", show @Int 4
    , "--out-file", voteTxBodyFp
    ]
  let _wallet1VKeyFp = paymentVKey . paymentKeyInfoPair $ head  wallets
      _wallet2VKeyFp = paymentVKey . paymentKeyInfoPair $ wallets !! 1
      _wallet3VKeyFp = paymentVKey . paymentKeyInfoPair $ wallets !! 2


  void $ H.execCli' execConfig
    [ "conway", "transaction", "sign"
    , "--testnet-magic", show @Int testnetMagic
    , "--tx-body-file", voteTxBodyFp
    , "--signing-key-file", paymentSKey $ paymentKeyInfoPair $ head wallets
    , "--signing-key-file", drepSKeyFp 1
    , "--signing-key-file", drepSKeyFp 2
    , "--signing-key-file", drepSKeyFp 3
    , "--out-file", voteTxFp
    ]

  void $ H.execCli' execConfig
    [ "conway", "transaction", "submit"
    , "--testnet-magic", show @Int testnetMagic
    , "--tx-file", voteTxFp
    ]

  success --TODO: check if The proposal was ratified


foldBlocksCheckProposalWasSubmitted
  :: TxId -- TxId of submitted tx
  -> Env
  -> LedgerState
  -> [LedgerEvent]
  -> BlockInMode -- Block i
  -> [LedgerEvent] -- ^ Accumulator at block i - 1
  -> IO ([LedgerEvent], FoldStatus) -- ^ Accumulator at block i and fold status
foldBlocksCheckProposalWasSubmitted txid _ _ allEvents _ _ =
  if any (filterNewGovProposals txid) allEvents
  then return (allEvents , StopFold)
  else return ([], ContinueFold)

retrieveGoveranceActionIndex
  :: MonadTest m => TxId -> [LedgerEvent] -> m Word32
retrieveGoveranceActionIndex txid events = do
  let newGovProposals = filter (filterNewGovProposals txid) events
  if null newGovProposals
  then H.failMessage callStack "retrieveGoveranceActionIndex: No new governance proposals found"
  else
    -- In this test there will only be one
    case head newGovProposals of
      NewGovernanceProposals _ (AnyProposals props) -> do
        let govActionStates = [i
                              | Ledger.GovActionIx i <- map Ledger.gaidGovActionIx . Map.keys $ Ledger.proposalsGovActionStates props
                              ]
        return $ head  govActionStates
      other -> H.failMessage callStack $ "retrieveGoveranceActionIndex: Expected NewGovernanceProposals, got: " <> show other


filterNewGovProposals :: TxId -> LedgerEvent -> Bool
filterNewGovProposals txid (NewGovernanceProposals eventTxId (AnyProposals props)) =
  let _govActionStates = Ledger.proposalsGovActionStates props
  in fromShelleyTxId eventTxId == txid
filterNewGovProposals _ _ = False

