{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.LedgerEvents.Gov.NewConstitutionalCommittee
  ( hprop_ledger_events_new_constitutional_committee
  ) where

import           Cardano.Api
import           Cardano.Api.Ledger
import           Cardano.Api.Shelley

import           Cardano.Testnet

import           Prelude

import qualified Cardano.Ledger.Credential as Ledger
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

import           Cardano.Testnet.Test.LedgerEvents.Utils
import           Control.Monad
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra
import           Testnet.Components.SPO
import qualified Testnet.Property.Utils as H
import           Testnet.Runtime


newtype AdditionalCatcher
  = IOE IOException
  deriving Show


hprop_ledger_events_new_constitutional_committee :: Property
hprop_ledger_events_new_constitutional_committee = H.integrationWorkspace "propose-new-constitutional-committee" $ \tempAbsBasePath' -> do
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
  gov <- H.createDirectoryIfMissing $ work </> "governance"

  -- Set up dreps so we can vote on the new committee proposal
  -- TODO: Refactor this into a function that generates n dreps
  let stakeVkeyFp = gov </> "stake.vkey"
      stakeSKeyFp = gov </> "stake.skey"

  _ <- P.cliStakeAddressKeyGen tempAbsPath'
         $ P.KeyNames { P.verificationKeyFile = stakeVkeyFp
                      , P.signingKeyFile = stakeSKeyFp
                      }

  let drepDir = gov </> "drep-keys"
      drepVkeyFp :: Int -> FilePath
      drepVkeyFp n =  drepDir </> "drep" <> show n <> ".vkey"

      drepSKeyFp :: Int -> FilePath
      drepSKeyFp n = drepDir </> "drep" <> show n <> ".skey"

  H.createDirectoryIfMissing_ drepDir

  -- Create DReps -- TODO: Refactor with shelleyKeyGen
  forM_ [1..3] $ \n -> do
   H.execCli' execConfig
     [ "conway", "governance", "drep", "key-gen"
     , "--verification-key-file", drepVkeyFp n
     , "--signing-key-file", drepSKeyFp n
     ]

  -- Create Drep registration certificates
  let drepCertFile :: Int -> FilePath
      drepCertFile n = gov </> "drep-keys" <>"drep" <> show n <> ".regcert"
  forM_ [1..3] $ \n -> do
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

  proposalAnchorFile <- H.note $ work </> gov </> "sample-proposal-anchor"
  H.writeFile proposalAnchorFile "dummy anchor data"
  proposalAnchorDataHash <- H.execCli' execConfig
    [ "conway", "governance"
    , "hash", "--file-text", proposalAnchorFile
    ]

  -- Create Conway update committee action
  let coldKeyDir = gov </> "committee"
      ccColdVkeyFp :: Int -> FilePath
      ccColdVkeyFp n = coldKeyDir </> "committee-member" <> show n <> ".vkey"
      ccColdSKeyFp :: Int -> FilePath
      ccColdSKeyFp n = coldKeyDir </> "committee-member" <> show n <> ".skey"

  H.createDirectoryIfMissing_ coldKeyDir
  forM_ [1] $ \n -> do
   H.execCli' execConfig
     [ "conway", "governance", "committee", "key-gen-cold"
     , "--cold-verification-key-file", ccColdVkeyFp n
     , "--cold-signing-key-file", ccColdSKeyFp n
     ]

  ccColdVkeyHash <- mconcat . lines <$> H.execCli' execConfig
    [ "conway", "governance", "committee"
    , "key-hash", "--verification-key-file", ccColdVkeyFp 1
    ]

  updateCommitteeActionFile <- H.note $ work </> gov </> "update-committee.action"
  let committeeMemExpiryEpoch = 1000
      quorum = "1/2"

  void $ H.execCli' execConfig
    [ "conway", "governance", "action", "update-committee"
    , "--testnet"
    , "--governance-action-deposit", show @Int 0 -- TODO: Get this from the node
    , "--deposit-return-stake-verification-key-file", stakeVkeyFp
    , "--anchor-url", "https://shorturl.at/asIJ6"
    , "--anchor-data-hash", proposalAnchorDataHash
    , "--add-cc-cold-verification-key-hash", ccColdVkeyHash
    , "--epoch", show @Int committeeMemExpiryEpoch
    , "--quorum", quorum
    , "--out-file", updateCommitteeActionFile
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
    , "--tx-out", Text.unpack (paymentKeyInfoAddr (head wallets)) <> "+" <> show @Int 5_000_000
    , "--proposal-file", updateCommitteeActionFile
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

  -- Vote on governance action
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
                      Nothing -- Initial accumulator state
                      (foldBlocksCheckProposalWasSubmitted (fromString txidString))

  newProposalEvents <- case propSubmittedResult of
                        Left (IOE e) ->
                          H.failMessage callStack
                            $ "foldBlocksCheckProposalWasSubmitted failed with: " <> show e
                        Right (Left e) ->
                          H.failMessage callStack
                            $ "foldBlocksCheckProposalWasSubmitted failed with: " <> Text.unpack (renderFoldBlocksError e)
                        Right (Right events) -> return events

  governanceActionIndex <- retrieveGovernanceActionIndex newProposalEvents

  let voteFp :: Int -> FilePath
      voteFp n = work </> gov </> "vote-" <> show n

  -- Proposal was successfully submitted, now we vote on the proposal and confirm it was ratified
  forM_ [1..3] $ \n -> do
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
  let CommitteeColdKeyHash cred = fromString ccColdVkeyHash
      ledgerCredential :: Credential 'ColdCommitteeRole StandardCrypto
      ledgerCredential = Ledger.KeyHashObj cred

  !eConstitutionAdopted
    <- runExceptT $ handleIOExceptT IOE
                  $ runExceptT $ foldBlocks
                      (File $ configurationFile testnetRuntime)
                      (File socketPath)
                      FullValidation
                      Nothing -- Initial accumulator state
                      (foldBlocksConsitutionalCommitteeMemberCheck ledgerCredential Present)

  case eConstitutionAdopted of
    Left (IOE e) ->
      H.failMessage callStack
        $ "foldBlocksConsitutionalCommitteeMemberCheck failed with: " <> show e
    Right (Left e) ->
      H.failMessage callStack
        $ "foldBlocksConsitutionalCommitteeMemberCheck failed with: " <> Text.unpack (renderFoldBlocksError e)
    Right (Right _) -> success


  cState' <- H.note $ work </> gov </> "committee-member-authorized-gov.state"
  void $ H.execCli' execConfig
        [ "conway", "query", "committee-state"
        , "--testnet-magic", show @Int testnetMagic
        , "--out-file", cState'
        ]

  -- REMOVE COMMITTEE MEMBER --

  void $ H.execCli' execConfig
    [ "conway", "governance", "action", "update-committee"
    , "--testnet"
    , "--governance-action-deposit", show @Int 0 -- TODO: Get this from the node
    , "--deposit-return-stake-verification-key-file", stakeVkeyFp
    , "--anchor-url", "https://shorturl.at/asIJ6"
    , "--anchor-data-hash", proposalAnchorDataHash
    , "--remove-cc-cold-verification-key-hash", ccColdVkeyHash
    , "--prev-governance-action-tx-id", txidString
    , "--prev-governance-action-index", show @Word32 governanceActionIndex
    , "--quorum", quorum
    , "--out-file", updateCommitteeActionFile
    ]

  txbodyFpRemove <- H.note $ work </> "tx-remove-cc-member.body"
  txbodySignedFpRemove <- H.note $ work </> "tx-remove-cc-member.body.signed"
  H.threadDelay 3_000_000
  utxoOut <- H.note $ work </> "utxo-4.json"

  void $ H.execCli' execConfig
   [ "conway", "query", "utxo"
   , "--address", Text.unpack $ paymentKeyInfoAddr $ wallets !! 2
   , "--cardano-mode"
   , "--testnet-magic", show @Int testnetMagic
   , "--out-file", utxoOut
   ]

  utxo4Json <- H.leftFailM . H.readJsonFile $ work </> "utxo-4.json"
  UTxO utxo4 <- H.noteShowM $ H.noteShowM $ decodeEraUTxO sbe utxo4Json
  txin4 <- H.noteShow =<< H.headM (Map.keys utxo4)

  void $ H.execCli' execConfig
    [ "conway", "transaction", "build"
    , "--testnet-magic", show @Int testnetMagic
    , "--change-address", Text.unpack $ paymentKeyInfoAddr $ head wallets
    , "--tx-in", Text.unpack $ renderTxIn txin4
    , "--tx-out", Text.unpack (paymentKeyInfoAddr (head wallets)) <> "+" <> show @Int 4_000_000
    , "--proposal-file", updateCommitteeActionFile
    , "--out-file", txbodyFpRemove
    ]

  void $ H.execCli' execConfig
    [ "conway", "transaction", "sign"
    , "--testnet-magic", show @Int testnetMagic
    , "--tx-body-file", txbodyFpRemove
    , "--signing-key-file", paymentSKey $ paymentKeyInfoPair $ wallets !! 2
    , "--out-file", txbodySignedFpRemove
    ]

  void $ H.execCli' execConfig
    [ "conway", "transaction", "submit"
    , "--testnet-magic", show @Int testnetMagic
    , "--tx-file", txbodySignedFpRemove
    ]
  -- Vote on governance action
  txidStringRemoval <- mconcat . lines <$> H.execCli' execConfig
    [ "transaction", "txid"
    , "--tx-file", txbodySignedFpRemove
    ]
  !propSubmittedResultRemoval
    <- runExceptT $ handleIOExceptT IOE
                  $ runExceptT $ foldBlocks
                      (File $ configurationFile testnetRuntime)
                      (File socketPath)
                      FullValidation
                      Nothing -- Initial accumulator state
                      (foldBlocksCheckProposalWasSubmitted (fromString txidStringRemoval))

  removalProposalEvents
    <- case propSubmittedResultRemoval of
         Left (IOE e) ->
           H.failMessage callStack
             $ "foldBlocksCheckProposalWasSubmitted failed with: " <> show e
         Right (Left e) ->
           H.failMessage callStack
             $ "foldBlocksCheckProposalWasSubmitted failed with: " <> Text.unpack (renderFoldBlocksError e)
         Right (Right events) -> return events

  removalGovActionIndex <- retrieveGovernanceActionIndex removalProposalEvents

  -- Vote on the removal

  forM_ [1..3] $ \n -> do
    H.execCli' execConfig
      [ "conway", "governance", "vote", "create"
      , "--yes"
      , "--governance-action-tx-id", txidStringRemoval
      , "--governance-action-index", show @Word32 removalGovActionIndex
      , "--drep-verification-key-file", drepVkeyFp n
      , "--out-file", voteFp n
      ]
-- We need more UTxOs

  void $ H.execCli' execConfig
   [ "conway", "query", "utxo"
   , "--address", Text.unpack $ paymentKeyInfoAddr $ head wallets
   , "--cardano-mode"
   , "--testnet-magic", show @Int testnetMagic
   , "--out-file", work </> "utxo-5.json"
   ]

  utxo5Json <- H.leftFailM . H.readJsonFile $ work </> "utxo-5.json"
  UTxO utxo5 <- H.noteShowM $ H.noteShowM $ decodeEraUTxO sbe utxo5Json
  txin5 <- H.noteShow =<< H.headM (Map.keys utxo5)

  voteTxFpRemoval <- H.note $ work </> gov </> "vote.tx"
  voteTxBodyFpRemoval <- H.note $ work </> gov </> "vote.txbody"

  -- Submit votes
  void $ H.execCli' execConfig
    [ "conway", "transaction", "build"
    , "--testnet-magic", show @Int testnetMagic
    , "--change-address", Text.unpack $ paymentKeyInfoAddr $ head wallets
    , "--tx-in", Text.unpack $ renderTxIn txin5
    , "--tx-out", Text.unpack (paymentKeyInfoAddr (wallets !! 1)) <> "+" <> show @Int 3_000_000
    , "--vote-file", voteFp 1
    , "--vote-file", voteFp 2
    , "--vote-file", voteFp 3
    , "--witness-override", show @Int 4
    , "--out-file", voteTxBodyFpRemoval
    ]

  void $ H.execCli' execConfig
    [ "conway", "transaction", "sign"
    , "--testnet-magic", show @Int testnetMagic
    , "--tx-body-file", voteTxBodyFpRemoval
    , "--signing-key-file", paymentSKey $ paymentKeyInfoPair $ head wallets
    , "--signing-key-file", drepSKeyFp 1
    , "--signing-key-file", drepSKeyFp 2
    , "--signing-key-file", drepSKeyFp 3
    , "--out-file", voteTxFpRemoval
    ]

  void $ H.execCli' execConfig
    [ "conway", "transaction", "submit"
    , "--testnet-magic", show @Int testnetMagic
    , "--tx-file", voteTxFpRemoval
    ]

  H.threadDelay 60_000_000
  cState <- H.note $ work </> gov </> "committee-member-not-authorized-gov.state"
  void $ H.execCli' execConfig
        [ "conway", "query", "committee-state"
        , "--testnet-magic", show @Int testnetMagic
        , "--out-file", cState
        ]
  H.failMessage callStack "Force"
  !eCommitteMemberRemoved
    <- runExceptT $ handleIOExceptT IOE
                  $ runExceptT $ foldBlocks
                      (File $ configurationFile testnetRuntime)
                      (File socketPath)
                      FullValidation
                      Nothing -- Initial accumulator state
                      (foldBlocksConsitutionalCommitteeMemberCheck ledgerCredential NotPresent)

  case eCommitteMemberRemoved of
    Left (IOE e) ->
      H.failMessage callStack
        $ "foldBlocksConsitutionalCommitteeMemberCheck failed with: " <> show e
    Right (Left e) ->
      H.failMessage callStack
        $ "foldBlocksConsitutionalCommitteeMemberCheck failed with: " <> Text.unpack (renderFoldBlocksError e)
    Right (Right _) -> do
      cState <- H.note $ work </> gov </> "committee-member-not-authorized-gov.state"
      void $ H.execCli' execConfig
            [ "conway", "query", "gov-state"
            , "--testnet-magic", show @Int testnetMagic
            , "--out-file", cState
            ]
      H.failMessage callStack "Force"
      success
