{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.LedgerEvents.Gov.NewConstitutionalCommittee
  ( hprop_ledger_events_new_constitutional_committee
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.Testnet

import           Prelude

import           Cardano.Crypto.Hash.Class
import qualified Cardano.Ledger.Conway.Governance as Ledger
import qualified Cardano.Ledger.SafeHash as Ledger
import           Control.Monad
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import           Data.Word
import           GHC.IO.Exception (IOException)
import           GHC.Stack (HasCallStack, callStack)
import           Lens.Micro
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
  let committeeMemExpiryEpoch = 5
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
  success
{-
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

  -- We check that constitution was succcessfully ratified

  !eConstitutionAdopted
    <- runExceptT $ handleIOExceptT IOE
                  $ runExceptT $ foldBlocks
                      (File $ configurationFile testnetRuntime)
                      (File socketPath)
                      FullValidation
                      [] -- Initial accumulator state
                      (foldBlocksCheckConstitutionWasRatified constitutionHash)


  case eConstitutionAdopted of
    Left (IOE e) ->
      H.failMessage callStack
        $ "foldBlocksCheckConstitutionWasRatified failed with: " <> show e
    Right (Left e) ->
      H.failMessage callStack
        $ "foldBlocksCheckConstitutionWasRatified failed with: " <> Text.unpack (renderFoldBlocksError e)
    Right (Right _events) -> success
-}

foldBlocksCheckProposalWasSubmitted
  :: TxId -- TxId of submitted tx
  -> Env
  -> LedgerState
  -> [LedgerEvent]
  -> BlockInMode -- Block i
  -> Maybe LedgerEvent -- ^ Accumulator at block i - 1
  -> IO (Maybe LedgerEvent, FoldStatus) -- ^ Accumulator at block i and fold status
foldBlocksCheckProposalWasSubmitted txid _ _ allEvents _ _ = do
  let newGovProposal = filter (filterNewGovProposals txid) allEvents
  if null newGovProposal
  then return (Nothing, ContinueFold)
  else return (Just $ head newGovProposal , StopFold)


retrieveGovernanceActionIndex
  :: (HasCallStack, MonadTest m)
  => Maybe LedgerEvent -> m Word32
retrieveGovernanceActionIndex mEvent = do
  case mEvent of
    Nothing -> H.failMessage callStack "retrieveGovernanceActionIndex: No new governance proposals found"
    Just (NewGovernanceProposals _ (AnyProposals props)) ->
    -- In this test there will only be one
        let govActionStates = [i
                              | Ledger.GovActionIx i <- map Ledger.gaidGovActionIx . Map.keys $ Ledger.proposalsGovActionStates props
                              ]
        in return $ head  govActionStates
    Just unexpectedEvent ->
      H.failMessage callStack
        $ mconcat ["retrieveGovernanceActionIndex: Expected NewGovernanceProposals, got: "
                  , show unexpectedEvent
                  ]


filterNewGovProposals :: TxId -> LedgerEvent -> Bool
filterNewGovProposals txid (NewGovernanceProposals eventTxId (AnyProposals props)) =
  let _govActionStates = Ledger.proposalsGovActionStates props
  in fromShelleyTxId eventTxId == txid
filterNewGovProposals _ _ = False


foldBlocksCheckConstitutionWasRatified
  :: String -- submitted constitution hash
  -> Env
  -> LedgerState
  -> [LedgerEvent]
  -> BlockInMode -- Block i
  -> [LedgerEvent] -- ^ Accumulator at block i - 1
  -> IO ([LedgerEvent], FoldStatus) -- ^ Accumulator at block i and fold status
foldBlocksCheckConstitutionWasRatified submittedConstitutionHash _ _ allEvents _ _ =
  if any (filterRatificationState submittedConstitutionHash) allEvents
  then return (allEvents , StopFold)
  else return ([], ContinueFold)

filterRatificationState
  :: String -- ^ Submitted constitution anchor hash
  -> LedgerEvent
  -> Bool
filterRatificationState c (EpochBoundaryRatificationState (AnyRatificationState rState)) =
  let constitutionAnchorHash = Ledger.anchorDataHash $ Ledger.constitutionAnchor (rState ^. Ledger.rsEnactStateL . Ledger.ensConstitutionL)
  in Text.pack c == renderSafeHashAsHex constitutionAnchorHash
filterRatificationState _ _ = False

-- TODO: Move to cardano-api and share with
-- https://github.com/input-output-hk/cardano-cli/blob/694782210c6d73a1b5151400214ef691f6f3ecb0/cardano-cli/src/Cardano/CLI/EraBased/Run/Governance/Hash.hs#L67
-- when doing so
renderSafeHashAsHex :: Ledger.SafeHash c tag -> Text.Text
renderSafeHashAsHex = hashToTextAsHex . Ledger.extractHash
