{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.CLI.Faucet.Types where

import Cardano.Api (AnyCardanoEra, TxBodyErrorAutoBalance, IsCardanoEra, TxIn, TxOut, CtxUTxO, NetworkId, TxInMode, CardanoMode, AnyConsensusMode, TxId, FileError)
import Cardano.CLI.Environment (EnvSocketError)
import Cardano.CLI.Shelley.Run.Address (SomeAddressVerificationKey)
import Cardano.CLI.Shelley.Run.Transaction (ShelleyTxCmdError, TxFeature, SomeWitness, renderShelleyTxCmdError)
import Cardano.Prelude
import Control.Concurrent.STM (TMVar, TQueue)
import Data.Aeson (ToJSON(..), object, (.=))
import Ouroboros.Consensus.Cardano.Block (EraMismatch)
import Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)
import Text.Parsec
import Cardano.CLI.Shelley.Key

data FaucetError = FaucetErrorInvalidAddress Text ParseError
  | FaucetErrorUtxoNotFound
  | FaucetErrorEraConversion
  | FaucetErrorTodo ShelleyTxCmdError
  | FaucetErrorSocketNotFound EnvSocketError
  | FaucetErrorEraMismatch EraMismatch
  | FaucetErrorAutoBalance TxBodyErrorAutoBalance
  | FaucetErrorFeatureMismatch AnyCardanoEra TxFeature
  | FaucetErrorAcquireFailure AcquireFailure
  | FaucetErrorConsensusModeMismatchTxBalance AnyConsensusMode AnyCardanoEra
  | FaucetErrorLoadingKey (FileError InputDecodeError)

data IsCardanoEra era => FaucetState era = FaucetState
  { utxoTMVar :: TMVar (Map TxIn (TxOut CtxUTxO era))
  , network :: NetworkId
  , queue :: TQueue (TxInMode CardanoMode, ByteString)
  , skey :: SomeWitness
  , vkey :: SomeAddressVerificationKey
  }

data SendMoneyReply = SendMoneyReply
  { txid :: TxId
  , txin :: TxIn
  }

instance ToJSON SendMoneyReply where
  toJSON (SendMoneyReply{txid,txin}) = object [ "txid" .= txid, "txin" .= txin ]

renderFaucetError :: FaucetError -> Text
renderFaucetError (FaucetErrorInvalidAddress a b) = show a <> show b
renderFaucetError (FaucetErrorEraConversion) = "unexpected error"
renderFaucetError (FaucetErrorTodo err) = renderShelleyTxCmdError err
renderFaucetError (FaucetErrorUtxoNotFound) = "no utxo of proper size found"
renderFaucetError (FaucetErrorSocketNotFound err) = show err
renderFaucetError (FaucetErrorEraMismatch err) = show err
renderFaucetError (FaucetErrorAutoBalance err) = show err
renderFaucetError (FaucetErrorFeatureMismatch a b) = show a <> " " <> show b
renderFaucetError (FaucetErrorAcquireFailure err) = show err
renderFaucetError (FaucetErrorConsensusModeMismatchTxBalance a b) = show a <> " " <> show b
