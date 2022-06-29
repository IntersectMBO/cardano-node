{-# LANGUAGE ImportQualifiedPost #-}

module Cardano.CLI.Faucet.Types where

import Cardano.Api (AnyCardanoEra, TxBodyErrorAutoBalance, IsCardanoEra, TxIn, TxOut, CtxUTxO, NetworkId, TxInMode, CardanoMode, AnyConsensusMode)
import Cardano.CLI.Environment (EnvSocketError)
import Cardano.CLI.Shelley.Run.Transaction
import Ouroboros.Consensus.Cardano.Block
import Text.Parsec
import Cardano.Prelude
import Control.Concurrent.STM (TMVar, TQueue)
import Cardano.CLI.Shelley.Run.Address
import Ouroboros.Network.Protocol.LocalStateQuery.Type

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

data IsCardanoEra era => FaucetState era = FaucetState
  { utxoTMVar :: TMVar (Map TxIn (TxOut CtxUTxO era))
  , network :: NetworkId
  , queue :: TQueue (TxInMode CardanoMode)
  , skey :: SomeWitness
  , vkey :: SomeAddressVerificationKey
  }

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
