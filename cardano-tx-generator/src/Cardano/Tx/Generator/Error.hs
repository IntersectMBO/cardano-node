module Cardano.Tx.Generator.Error
  ( TxGenError (..)
  ) where

import           Cardano.Prelude

data TxGenError =
    CurrentlyCannotSendTxToRelayNode !FilePath
  -- ^ Relay nodes cannot currently be transaction recipients.
  | InsufficientFundsForRecipientTx
  -- ^ Error occurred while creating the target node address.
  | NeedMinimumThreeSigningKeyFiles ![FilePath]
  -- ^ Need at least 3 signing key files.
  | SecretKeyDeserialiseError !Text
  | SecretKeyReadError !Text
  deriving Show
