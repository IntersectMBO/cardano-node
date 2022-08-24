module  Cardano.TxGenerator.Types
        (module Cardano.TxGenerator.Types)
        where

import           Cardano.Api
import           Cardano.Prelude

import           Cardano.TxGenerator.Fund (Fund)


type FundSource m       = m (Either String [Fund])
type FundToStore m      = Fund -> m ()
type FundToStoreList m  = [Fund] -> m ()

data TxGenError =
    InsufficientFundsForRecipientTx !Lovelace !Lovelace
  -- ^ The calculated expenditure (second value) was not available as a single
  --   UTxO entry.  The first value is the largest single UTxO available.
  | TxFileError !(FileError TextEnvelopeError)
  | SplittingSubmissionError !Text
  | SuppliedUtxoTooSmall !Int !Int
  -- ^ The supplied UTxO size (second value) was less than the requested
  --   number of transactions to send (first value).
  | BadPayloadSize !Text
  deriving Show
