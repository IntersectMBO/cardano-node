module Cardano.Testnet.Test.Utils
  ( filterNewGovProposals
  , foldBlocksFindLedgerEvent
  ) where


import           Cardano.Api as Api
import           Cardano.Api.Shelley

import qualified Cardano.Ledger.Conway.Governance as Ledger

import           Prelude


foldBlocksFindLedgerEvent :: MonadIO m
  => (LedgerEvent -> Bool)
  -> FilePath
  -> FilePath
  -> m (Either FoldBlocksError (Maybe LedgerEvent))
foldBlocksFindLedgerEvent ledgerEventFilter configurationFile socketPath =
    runExceptT $ foldBlocks (File configurationFile)
                            (File socketPath)
                            FullValidation
                            Nothing -- Initial accumulator state
                            (go ledgerEventFilter)
  where
    go :: (LedgerEvent -> Bool) -- Predicate that ledger event must satisfy
       -> Env
       -> LedgerState
       -> [LedgerEvent]
       -> BlockInMode -- Block i
       -> Maybe LedgerEvent -- ^ Accumulator at block i - 1
       -> IO (Maybe LedgerEvent, FoldStatus) -- ^ Accumulator at block i and fold status
    go txFilter _ _ allEvents _ _ = do
      let foundTransactions = filter txFilter allEvents
      return $ case foundTransactions of
                  (foundTransaction:_) -> (Just foundTransaction, StopFold)
                  _ -> (Nothing, ContinueFold)

filterNewGovProposals :: TxId -> LedgerEvent -> Bool
filterNewGovProposals txid (NewGovernanceProposals eventTxId (AnyProposals props)) =
  let _govActionStates = Ledger.proposalsActionsMap props
  in fromShelleyTxId eventTxId == txid
filterNewGovProposals _ _ = False
