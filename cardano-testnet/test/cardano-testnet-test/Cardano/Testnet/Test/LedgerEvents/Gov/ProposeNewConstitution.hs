{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

{- HLINT ignore "Use head" -}

module Cardano.Testnet.Test.LedgerEvents.Gov.ProposeNewConstitution
  ( hprop_ledger_events_propose_new_constitution
  , retrieveGovernanceActionIndex
  ) where

import           Cardano.Api as Api
import           Cardano.Api.Error (displayError)
import           Cardano.Api.Shelley

import qualified Cardano.Crypto.Hash as L
import qualified Cardano.Ledger.Conway.Governance as L
import qualified Cardano.Ledger.Conway.Governance as Ledger
import qualified Cardano.Ledger.Hashes as L
import qualified Cardano.Ledger.Shelley.LedgerState as L
import           Cardano.Testnet
import           Cardano.Testnet.Test.Utils (filterNewGovProposals, foldBlocksFindLedgerEvent)

import           Prelude

import           Control.Monad
import           Control.Monad.State.Strict (StateT)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Maybe.Strict
import           Data.String
import           Data.Text (unpack)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import           Data.Word
import           GHC.Stack (HasCallStack, callStack)
import           Lens.Micro
import           System.FilePath ((</>))

import           Hedgehog
import qualified Hedgehog.Extras as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO

import           Testnet.Components.Configuration
import           Testnet.Components.Query
import           Testnet.Defaults
import qualified Testnet.Process.Cli as P
import qualified Testnet.Process.Run as H
import qualified Testnet.Property.Utils as H
import           Testnet.Runtime



-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/ProposeAndRatifyNewConstitution/"'@
hprop_ledger_events_propose_new_constitution :: Property
hprop_ledger_events_propose_new_constitution = H.integrationWorkspace "propose-new-constitution" $ \tempAbsBasePath' -> do
  -- Start a local test net
  conf@Conf { tempAbsPath } <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath

  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  -- Generate model for votes
  let allVotes :: [(String, Int)]
      allVotes = voteList [("yes", 4), ("no", 3), ("abstain", 2)]
  annotateShow allVotes

  let numVotes :: Int
      numVotes = length allVotes
  annotateShow numVotes

  let sbe = ShelleyBasedEraConway
      era = toCardanoEra sbe
      cEra = AnyCardanoEra era
      fastTestnetOptions = cardanoDefaultTestnetOptions
        { cardanoEpochLength = 100
        , cardanoSlotLength = 0.1
        , cardanoNodeEra = cEra
        , cardanoNumDReps = numVotes
        }

  TestnetRuntime
    { testnetMagic
    , poolNodes
    , wallets
    , configurationFile
    }
    <- cardanoTestnetDefault fastTestnetOptions conf

  poolNode1 <- H.headM poolNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket $ poolRuntime poolNode1
  execConfig <- H.mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic

  let socketName' = IO.sprocketName poolSprocket1
      socketBase = IO.sprocketBase poolSprocket1 -- /tmp
      socketPath = socketBase </> socketName'

  epochStateView <- getEpochStateView (File configurationFile) (File socketPath)

  H.note_ $ "Sprocket: " <> show poolSprocket1
  H.note_ $ "Abs path: " <> tempAbsBasePath'
  H.note_ $ "Socketpath: " <> socketPath
  H.note_ $ "Foldblocks config file: " <> configurationFile

  -- Create Conway constitution
  gov <- H.createDirectoryIfMissing $ work </> "governance"
  proposalAnchorFile <- H.note $ work </> gov </> "sample-proposFal-anchor"
  consitutionFile <- H.note $ work </> gov </> "sample-constitution"
  constitutionActionFp <- H.note $ work </> gov </> "constitution.action"

  H.writeFile proposalAnchorFile "dummy anchor data"
  H.writeFile consitutionFile "dummy constitution data"
  constitutionHash <- H.execCli' execConfig
    [ "conway", "governance"
    , "hash", "anchor-data", "--file-text", consitutionFile
    ]

  proposalAnchorDataHash <- H.execCli' execConfig
    [ "conway", "governance"
    , "hash", "anchor-data", "--file-text", proposalAnchorFile
    ]

  let stakeVkeyFp = gov </> "stake.vkey"
      stakeSKeyFp = gov </> "stake.skey"

  _ <- P.cliStakeAddressKeyGen tempAbsPath'
         $ P.KeyNames { P.verificationKeyFile = stakeVkeyFp
                      , P.signingKeyFile = stakeSKeyFp
                      }

  -- TODO share this piece of code that is duplicated in multiple tests
  let drepVkeyFp :: Int -> FilePath
      drepVkeyFp n = tempAbsPath' </> "drep-keys" </> ("drep" <> show n) </> "drep.vkey"

      drepSKeyFp :: Int -> FilePath
      drepSKeyFp n = tempAbsPath' </> "drep-keys" </> ("drep" <> show n) </> "drep.skey"

  -- Create Drep registration certificates
  let drepCertFile :: Int -> FilePath
      drepCertFile n = gov </> "drep-keys" <> "drep" <> show n <> ".regcert"

  forM_ allVotes $ \(_, n) -> do
    H.execCli' execConfig
       [ "conway", "governance", "drep", "registration-certificate"
       , "--drep-verification-key-file", drepVkeyFp n
       , "--key-reg-deposit-amt", show @Int 1_000_000 -- TODO: retrieve this from conway genesis.
       , "--out-file", drepCertFile n
       ]

  -- Retrieve UTxOs for registration submission
  regTxIn1 <- findLargestUtxoForPaymentKey epochStateView sbe $ wallets !! 0

  drepRegTxbodyFp <- H.note $ work </> "drep.registration.txbody"
  drepRegTxSignedFp <- H.note $ work </> "drep.registration.tx"

  void $ H.execCli' execConfig $
    [ "conway", "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr $ wallets !! 0
    , "--tx-in", Text.unpack $ renderTxIn regTxIn1
    , "--tx-out", Text.unpack (paymentKeyInfoAddr (wallets !! 1)) <> "+" <> show @Int 5_000_000
    ] ++ (concat [["--certificate-file", drepCertFile n] | (_, n) <- allVotes]) ++
    [ "--witness-override", show @Int (numVotes + 1)
    , "--out-file", drepRegTxbodyFp
    ]

  void $ H.execCli' execConfig $
    [ "conway", "transaction", "sign"
    , "--tx-body-file", drepRegTxbodyFp
    , "--signing-key-file", paymentSKey $ paymentKeyInfoPair $ wallets !! 0
    ] ++ (concat [["--signing-key-file", drepSKeyFp n] | (_, n) <- allVotes]) ++
    [ "--out-file", drepRegTxSignedFp
    ]

  void $ H.execCli' execConfig
    [ "conway", "transaction", "submit"
    , "--tx-file", drepRegTxSignedFp
    ]

  -- Create constitution proposal

  guardRailScriptFp <- H.note $ work </> "guard-rail-script.plutusV3"
  H.writeFile guardRailScriptFp $ Text.unpack plutusV3NonSpendingScript
  -- TODO: Update help text for policyid. The script hash is not
  -- only useful for minting scripts
  constitutionScriptHash <- filter (/= '\n') <$>
    H.execCli' execConfig
      [ anyEraToString cEra, "transaction"
      , "policyid"
      , "--script-file", guardRailScriptFp
      ]
  void $ H.execCli' execConfig
    [ "conway", "governance", "action", "create-constitution"
    , "--testnet"
    , "--governance-action-deposit", show @Int 1_000_000 -- TODO: Get this from the node
    , "--deposit-return-stake-verification-key-file", stakeVkeyFp
    , "--anchor-url", "https://tinyurl.com/3wrwb2as"
    , "--anchor-data-hash", proposalAnchorDataHash
    , "--constitution-url", "https://tinyurl.com/2pahcy6z"
    , "--constitution-hash", constitutionHash
    , "--constitution-script-hash", constitutionScriptHash
    , "--out-file", constitutionActionFp
    ]

  txbodyFp <- H.note $ work </> "tx.body"
  txbodySignedFp <- H.note $ work </> "tx.body.signed"

  txin2 <- findLargestUtxoForPaymentKey epochStateView sbe $ wallets !! 1

  void $ H.execCli' execConfig
    [ "conway", "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr $ wallets !! 1
    , "--tx-in", Text.unpack $ renderTxIn txin2
    , "--tx-out", Text.unpack (paymentKeyInfoAddr (wallets !! 0)) <> "+" <> show @Int 5_000_000
    , "--proposal-file", constitutionActionFp
    , "--out-file", txbodyFp
    ]

  void $ H.execCli' execConfig
    [ "conway", "transaction", "sign"
    , "--tx-body-file", txbodyFp
    , "--signing-key-file", paymentSKey $ paymentKeyInfoPair $ wallets !! 1
    , "--out-file", txbodySignedFp
    ]

  void $ H.execCli' execConfig
    [ "conway", "transaction", "submit"
    , "--tx-file", txbodySignedFp
    ]

  txidString <- mconcat . lines <$> H.execCli' execConfig
    [ "transaction", "txid"
    , "--tx-file", txbodySignedFp
    ]
  !propSubmittedResult <- foldBlocksFindLedgerEvent (filterNewGovProposals (fromString txidString))
                                                    configurationFile
                                                    socketPath

  newProposalEvents <- case propSubmittedResult of
                        Left e ->
                          H.failMessage callStack
                            $ "foldBlocksFindLedgerEvent failed with: " <> displayError e
                        Right events -> return events

  governanceActionIndex <- retrieveGovernanceActionIndex newProposalEvents

  let voteFp :: Int -> FilePath
      voteFp n = work </> gov </> "vote-" <> show n

  -- Proposal was successfully submitted, now we vote on the proposal and confirm it was ratified
  forM_ allVotes $ \(vote, n) -> do
    H.execCli' execConfig
      [ "conway", "governance", "vote", "create"
      , "--" ++ vote
      , "--governance-action-tx-id", txidString
      , "--governance-action-index", show @Word32 governanceActionIndex
      , "--drep-verification-key-file", drepVkeyFp n
      , "--out-file", voteFp n
      ]

  -- We need more UTxOs

  txin3 <- findLargestUtxoForPaymentKey epochStateView sbe $ wallets !! 0

  voteTxFp <- H.note $ work </> gov </> "vote.tx"
  voteTxBodyFp <- H.note $ work </> gov </> "vote.txbody"

  -- Submit votes
  void $ H.execCli' execConfig $
    [ "conway", "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr $ wallets !! 0
    , "--tx-in", Text.unpack $ renderTxIn txin3
    , "--tx-out", Text.unpack (paymentKeyInfoAddr (wallets !! 1)) <> "+" <> show @Int 3_000_000
    ] ++ (concat [["--vote-file", voteFp n] | (_, n) <- allVotes]) ++
    [ "--witness-override", show @Int (numVotes + 1)
    , "--out-file", voteTxBodyFp
    ]

  void $ H.execCli' execConfig $
    [ "conway", "transaction", "sign"
    , "--tx-body-file", voteTxBodyFp
    , "--signing-key-file", paymentSKey $ paymentKeyInfoPair $ wallets !! 0
    ] ++ (concat [["--signing-key-file", drepSKeyFp n] | (_, n) <- allVotes]) ++
    [ "--out-file", voteTxFp
    ]

  void $ H.execCli' execConfig
    [ "conway", "transaction", "submit"
    , "--tx-file", voteTxFp
    ]

  -- We check that constitution was succcessfully ratified

  !eConstitutionAdopted
    <- runExceptT $ foldEpochState
                      (File configurationFile)
                      (File socketPath)
                      FullValidation
                      (EpochNo 10)
                      ()
                      (\epochState _ _ -> foldBlocksCheckConstitutionWasRatified constitutionHash constitutionScriptHash epochState)

  void $ evalEither eConstitutionAdopted

  finalGovState <- H.note $ work </> gov </> "final_gov_state.json"

  void $ H.execCli' execConfig
    [ "conway", "query", "gov-state"
    , "--volatile-tip"
    , "--out-file", finalGovState
    ]

  -- Tally registered votes

  finalGovFileBS <- liftIO $ LBS.readFile finalGovState

  votes <- H.nothingFail $ do (Aeson.Object jsonValue) <- Aeson.decode finalGovFileBS
                              (Aeson.Array proposals) <- jsonValue KeyMap.!? "proposals"
                              (Aeson.Object proposal) <- Vector.headM proposals
                              (Aeson.Object votes) <- proposal KeyMap.!? "dRepVotes"
                              KeyMap.foldl' extractVote (Just []) votes

  length (filter (== "VoteYes") votes) === 4
  length (filter (== "VoteNo") votes) === 3
  length (filter (== "Abstain") votes) === 2
  length votes === numVotes

  where
    extractVote :: Maybe [String] -> Aeson.Value -> Maybe [String]
    extractVote (Just acc) (Aeson.String x) = Just $ unpack x:acc
    extractVote _ _ = Nothing

    voteList :: [(a, Int)] -> [(a, Int)]
    voteList l = go 1 l
      where
        go :: Int -> [(a, Int)] -> [(a, Int)]
        go _ [] = []
        go n ((s, m):r) | m > 0 = (s, n):go (n + 1) ((s, m - 1):r)
                        | otherwise = go n r


retrieveGovernanceActionIndex
  :: (HasCallStack, MonadTest m)
  => Maybe LedgerEvent -> m Word32
retrieveGovernanceActionIndex mEvent = do
  case mEvent of
    Nothing -> H.failMessage callStack "retrieveGovernanceActionIndex: No new governance proposals found"
    Just (NewGovernanceProposals _ (AnyProposals props)) ->
    -- In this test there will only be one
        let govActionStates = [i
                              | Ledger.GovActionIx i <- map Ledger.gaidGovActionIx . Map.keys $ Ledger.proposalsActionsMap props
                              ]
        in return $ govActionStates !! 0
    Just unexpectedEvent ->
      H.failMessage callStack
        $ mconcat ["retrieveGovernanceActionIndex: Expected NewGovernanceProposals, got: "
                  , show unexpectedEvent
                  ]

foldBlocksCheckConstitutionWasRatified
  :: String -- submitted constitution hash
  -> String -- submitted guard rail script hash
  -> AnyNewEpochState
  -> StateT s IO LedgerStateCondition -- ^ Accumulator at block i and fold status
foldBlocksCheckConstitutionWasRatified submittedConstitutionHash submittedGuardRailScriptHash anyNewEpochState =
  if filterRatificationState submittedConstitutionHash submittedGuardRailScriptHash anyNewEpochState
  then return ConditionMet
  else return ConditionNotMet

-- cgsDRepPulsingStateL . ratifyStateL
filterRatificationState
  :: String -- ^ Submitted constitution anchor hash
  -> String -- ^ Submitted guard rail script hash
  -> AnyNewEpochState
  -> Bool
filterRatificationState c guardRailScriptHash (AnyNewEpochState sbe newEpochState) =
  caseShelleyToBabbageOrConwayEraOnwards
    (const $ error "filterRatificationState: Only conway era supported")

    (const $ do
      let rState = Ledger.extractDRepPulsingState $ newEpochState ^. L.newEpochStateGovStateL . L.drepPulsingStateGovStateL
          constitution = rState ^. Ledger.rsEnactStateL . Ledger.ensConstitutionL
          constitutionAnchorHash = Ledger.anchorDataHash $ Ledger.constitutionAnchor constitution
          L.ScriptHash constitutionScriptHash = fromMaybe (error "filterRatificationState: consitution does not have a guardrail script")
                                                $ strictMaybeToMaybe $ constitution ^. Ledger.constitutionScriptL
      Text.pack c == renderSafeHashAsHex constitutionAnchorHash && L.hashToTextAsHex constitutionScriptHash == Text.pack guardRailScriptHash

    )
    sbe

