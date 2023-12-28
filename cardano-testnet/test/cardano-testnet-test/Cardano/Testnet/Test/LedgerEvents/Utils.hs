{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Cardano.Testnet.Test.LedgerEvents.Utils
  ( IsPresent (..)
  , filterConstitutionalCommitteeMember
  , filterNewGovProposals
  , foldBlocksCheckProposalWasSubmitted
  , foldBlocksConsitutionalCommitteeMemberCheck
  , retrieveGovernanceActionIndex
  ) where

import           Cardano.Api
import           Cardano.Api.Ledger
import           Cardano.Api.Shelley


import           Prelude

import           Cardano.Crypto.Hash (hashToTextAsHex)
import qualified Cardano.Ledger.Conway.Governance as Ledger
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Text (Text)
import           Data.Word
import           GHC.Stack (HasCallStack, callStack)


import           Hedgehog
import qualified Hedgehog.Extras as H

import qualified Cardano.Ledger.Conway.Governance as Conway
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Credential as Ledger
import           Lens.Micro ((^.))


foldBlocksConsitutionalCommitteeMemberCheck
  :: Ledger.Credential ColdCommitteeRole StandardCrypto
  -> IsPresent -- Whether or not the member should be present
  -> Env
  -> LedgerState
  -> [LedgerEvent]
  -> BlockInMode -- Block i
  -> Maybe LedgerEvent -- ^ Accumulator at block i - 1
  -> IO (Maybe LedgerEvent, FoldStatus) -- ^ Accumulator at block i and fold status
foldBlocksConsitutionalCommitteeMemberCheck ccMem isPresent _ _ allEvents _ _ = do
  let memAddedEvent = filter (filterConstitutionalCommitteeMember isPresent ccMem) allEvents
  if null memAddedEvent
  then return (Nothing, ContinueFold)
  else return (Just $ head memAddedEvent, StopFold)

data IsPresent
  = Present
  | NotPresent

filterConstitutionalCommitteeMember
  :: IsPresent
  -> Ledger.Credential ColdCommitteeRole StandardCrypto -- Committee cold key verification key hash
  -> LedgerEvent
  -> Bool
filterConstitutionalCommitteeMember isPresent commCred (EpochBoundaryRatificationState (AnyRatificationState rState)) =
  let comCredHash = credToHashAsHexText commCred
  in case getCommitteeMembers' rState of
       SJust c -> case isPresent of
                    Present -> isJust $ Map.lookup comCredHash c
                    NotPresent -> isNothing $ Map.lookup comCredHash c
       SNothing -> False
filterConstitutionalCommitteeMember _ _ _ = False

getCommitteeMembers'
  :: Ledger.RatifyState era
  -> StrictMaybe (Map Text EpochNo)
getCommitteeMembers' rState = do
  members <- Conway.committeeMembers <$> rState ^. Ledger.rsEnactStateL . Ledger.ensCommitteeL
  SJust $ Map.mapKeys credToHashAsHexText members

credToHashAsHexText :: Credential kr c -> Text
credToHashAsHexText (ScriptHashObj (Ledger.ScriptHash hash)) =  hashToTextAsHex hash
credToHashAsHexText (KeyHashObj (KeyHash has)) = hashToTextAsHex has


filterNewGovProposals :: TxId -> LedgerEvent -> Bool
filterNewGovProposals txid (NewGovernanceProposals eventTxId (AnyProposals props)) =
  let _govActionStates = Ledger.proposalsGovActionStates props
  in fromShelleyTxId eventTxId == txid
filterNewGovProposals _ _ = False

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
