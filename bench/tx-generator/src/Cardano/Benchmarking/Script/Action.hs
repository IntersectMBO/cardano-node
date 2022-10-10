{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Benchmarking.Script.Action(
          Action(AddFund, CancelBenchmark, DefineSigningKey, Delay,
                InitWallet, LogMsg, ReadSigningKey, Reserved, Set,
                SetProtocolParameters, StartProtocol, Submit,
                WaitBenchmark, WaitForEra)
        , action
        , setConst
) where

import           GHC.Generics

import           Control.Concurrent(threadDelay)
import           Control.Monad.Trans.Except
import           Data.Functor.Identity
import           Data.Dependent.Sum (DSum(..), (==>))
import qualified Data.Text as Text (Text, unpack)

import           Cardano.Api (AnyCardanoEra, AsType(..),
                   ExecutionUnits, FromSomeType(..), HasTextEnvelope,
                   Lovelace, ScriptData, ScriptRedeemer, TextEnvelope,
                   TextEnvelopeError, TxIn, castSigningKey,
                   deserialiseFromTextEnvelopeAnyOf)
import           Cardano.Benchmarking.OuroborosImports (PaymentKey, SigningKey, SigningKeyFile)
import           Cardano.Benchmarking.Script.Env
import           Cardano.Benchmarking.Script.NodeConfig (startProtocol)
import           qualified Cardano.Benchmarking.Script.Setters as Setters
import           Cardano.Benchmarking.Script.Types
import           Cardano.Benchmarking.Script.Store
import           Cardano.Benchmarking.Wallet
import           Cardano.TxGenerator.Types

type SetKeyVal = DSum Setters.Tag Identity

data Action where
  Set                :: !SetKeyVal -> Action
--  Declare            :: SetKeyVal   -> Action --declare (once): error if key was set before
  InitWallet         :: !WalletName -> Action
  StartProtocol      :: !FilePath -> !(Maybe FilePath) -> Action
  Delay              :: !Double -> Action
  ReadSigningKey     :: !KeyName -> !SigningKeyFile -> Action
  DefineSigningKey   :: !KeyName -> !TextEnvelope -> Action
  AddFund            :: !AnyCardanoEra -> !WalletName -> !TxIn -> !Lovelace -> !KeyName -> Action
  WaitBenchmark      :: !ThreadName -> Action
  Submit             :: !AnyCardanoEra -> !SubmitMode -> !TxGenTxParams -> !Generator -> Action
  CancelBenchmark    :: !ThreadName -> Action
  Reserved           :: [String] -> Action
  WaitForEra         :: !AnyCardanoEra -> Action
  SetProtocolParameters :: ProtocolParametersSource -> Action
  LogMsg             :: !Text.Text -> Action
  deriving (Show, Eq)
deriving instance Generic Action

action :: Action -> ActionM ()
action a = case a of
  Set (key :=> (Identity val)) -> setUser key val
  InitWallet name -> initWallet name
  SetProtocolParameters p -> setProtocolParameters p
  StartProtocol configFile cardanoTracerSocket -> startProtocol configFile cardanoTracerSocket
  ReadSigningKey name filePath -> readSigningKey name filePath
  DefineSigningKey name descr -> defineSigningKey name descr
  AddFund era wallet txIn lovelace keyName -> addFund era wallet txIn lovelace keyName
  Delay t -> delay t
  Submit era submitMode txParams generator -> submitAction era submitMode generator txParams
  WaitBenchmark thread -> waitBenchmark thread
  CancelBenchmark thread -> cancelBenchmark thread
  WaitForEra era -> waitForEra era
  LogMsg txt -> traceDebug $ Text.unpack txt
  Reserved options -> reserved options

setConst :: Setters.Tag v -> v -> Action
setConst key val = Set $ key ==> val

{-
This is for dirty hacking and testing and quick-fixes.
Its a function that can be called from the JSON scripts
and for which the JSON encoding is "reserved".
-}
reserved :: [String] -> ActionM ()
reserved _ = do
  throwE $ UserError "no dirty hack is implemented"

defineSigningKey :: KeyName -> TextEnvelope -> ActionM ()
defineSigningKey name descr
  = case parseSigningKey descr of
    Right key -> setName name key
    Left err -> liftTxGenError $ ApiError err

parseSigningKey :: TextEnvelope -> Either TextEnvelopeError (SigningKey PaymentKey)
parseSigningKey = deserialiseFromTextEnvelopeAnyOf types
  where
    types :: [FromSomeType HasTextEnvelope (SigningKey PaymentKey)]
    types =
      [ FromSomeType (AsSigningKey AsGenesisUTxOKey) castSigningKey
      , FromSomeType (AsSigningKey AsPaymentKey) id
      ]
