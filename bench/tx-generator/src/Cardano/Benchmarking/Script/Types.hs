{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module      : Cardano.Benchmarking.Script.Types
Description : Types used within transaction generator scripts.

'Action' is likely the most impactful type exported, as it represents
the individual steps to be executed by the transaction generator at
the system level; however, 'Generator' is much more of what one thinks
of transactions themselves being. The 'Generator' has to do with
combining streams of the transactions one typically might think of
doing with a wallet, where the 'Action' level largely sees those
transactions as interchangeable, and focuses more on the variety of
things one might do with the connexion.
 -}
module Cardano.Benchmarking.Script.Types (
          Action(..)
        , EndpointUri(..)
        , Generator(Cycle, NestedNtoM, NtoM, OneOf, PublishScripts, RoundRobin,
                SecureGenesis, Sequence, Split, SplitN, Take)
        , PayMode(PayToAddr, PayToScript, PayToScriptSet)
        , ProtocolParameterMode(..)
        , ProtocolParametersSource(QueryLocalNode, UseLocalProtocolFile)
        , ScriptBudget(AutoScript, MeasuredScript, StaticScriptBudget)
        , ScriptSetPayment(..)
        , ScriptSetSpec(..)
        , ScriptSource(..)
        , ScriptSpec(..)
        , SubmissionEndpoint(..)
        , SubmissionEndpointProtocol(..)
        , SubmitMode(..)
        , TargetNodes
        , TxList(..)
) where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L

import           Cardano.Benchmarking.OuroborosImports (SigningKeyFile)
import           Cardano.TxGenerator.ProtocolParameters (ProtocolParameters)
import           Cardano.TxGenerator.ScriptVariants (ScriptSizeSpec)
import           Cardano.TxGenerator.Setup.NixService (EndpointUri (..), NodeDescription,
                   SubmissionEndpoint (..), SubmissionEndpointProtocol (..))
import           Cardano.TxGenerator.Types

import           Prelude

import           Data.Function (on)
import           Data.List.NonEmpty
import           GHC.Generics


-- FIXME: temporary workaround instance until Action ADT is refactored
instance Eq (SigningKey PaymentKey) where
  (==) = (==) `on` serialiseToTextEnvelope Nothing

-- | 'Action' represents the individual actions to be executed by the
-- tx-generator. It gets translated to
-- 'Cardano.Benchmarking.Script.ActionM' using 'Env' as the
-- state, 'Ouroboros.Network.IOManager' as the reader, and 'IO' as the
-- monad, and further wrapped in an 'Control.Monad.Except.ExceptT' with
-- an 'Cardano.Benchmarking.Env.Error' as the exception.
data Action where
  -- | 'SetNetworkId' only entails changing a state variable in an 'Env'.
  SetNetworkId       :: !NetworkId -> Action
  -- | 'SetSocketPath' likewise only entails a state variable change.
  SetSocketPath      :: !FilePath -> Action
  -- | 'InitWallet' just uses the name in a state variable and creates a
  -- fresh 'Control.Concurrent.MVar' with an empty
  -- 'Cardano.TxGenerator.FundQueue.FundQueue' in it.
  InitWallet         :: !String -> Action
  -- | 'StartProtocol' sets state variables for protocol and genesis,
  -- but via 'Cardano.TxGenerator.Setup.NodeConfig.mkNodeConfig' and
  -- 'Cardano.Node.Protocol.mkConsensusProtocol'. The first unravels
  -- to reading wide a variety of config files in
  -- 'Cardano.Node.Configuration.POM.makeNodeConfiguration' and The
  -- second unravels to reading genesis content in helper functions
  -- spread across eras.
  StartProtocol      :: !FilePath -> !(Maybe FilePath) -> Action
  -- | 'Delay' translates to 'Control.Concurrent.threadDelay' via
  -- 'Cardano.Benchmarking.Script.delay'.
  Delay              :: !Double -> Action
  -- | 'ReadSigningKey' translates to a 'readFileTextEnvelopeAnyOf' from
  -- "Cardano.Api.SerialiseTextEnvelope" on the signing key file and then
  -- drops it into a state variable via
  -- 'Cardano.Benchmarking.Script.Env.setEnvKeys'.
  ReadSigningKey     :: !String -> !(SigningKeyFile In) -> Action
  -- | 'DefineSigningKey' is just a 'Map.insert' on the state variable.
  DefineSigningKey   :: !String -> !(SigningKey PaymentKey) -> Action
  -- | 'AddFund' is mostly a wrapper around
  -- 'Cardano.Benchmarking.Wallet.walletRefInsertFund' which in turn
  -- is just 'Control.Concurrent.modifyMVar' around
  -- 'Cardano.TxGenerator.FundQueue.insert'.
  AddFund            :: !AnyCardanoEra -> !String -> !TxIn -> !L.Coin -> !String -> Action
  -- | 'WaitBenchmark' signifies a 'Control.Concurrent.Async.waitCatch'
  -- on the 'Cardano.Benchmarking.GeneratorTx.AsyncBenchmarkControl'
  -- for the environment and also folds tracers into the completion.
  WaitBenchmark      :: Action
  -- | 'Submit' mostly wraps
  -- 'Cardano.Benchamrking.Script.Core.benchmarkTxStream'
  -- which in turn wraps
  -- 'Cardano.Benchmarking.GeneratorTx.walletBenchmark' which
  -- in turn wraps
  -- 'Cardano.Benchmarking.GeneratorTx.SubmissionClient.txSubmissionClient'
  -- and functions local to that like @requestTxs@.
  Submit             :: !AnyCardanoEra -> !SubmitMode -> !TxGenTxParams -> !Generator -> Action
  -- | 'CancelBenchmark' wraps a callback from the
  -- 'Cardano.Benchmarking.GeneratorTx.AsyncBenchmarkControl' type,
  -- which is a shutdown action.
  CancelBenchmark    :: Action
  -- | 'Reserved' just emits an error and is a placeholder that helps
  -- with testing and quick fixes.
  Reserved           :: [String] -> Action
  -- 'WaitForEra' loops doing delays/sleeps until the current era matches.
  WaitForEra         :: !AnyShelleyBasedEra -> Action
  -- | 'SetProtocolParameters' has one option to read from a file and
  -- another to pass directly and just sets a state variable for
  -- the @protoParams@ field of 'Cardano.Benchmarking.Script.Env.Env'.
  SetProtocolParameters :: ProtocolParametersSource -> Action
  -- | 'DefineScriptSet' builds the variants of a script, see
  -- "Cardano.TxGenerator.ScriptVariants", and keeps them in the 'Env'
  -- under a name, for 'PublishScripts' and 'PayToScriptSet' to use.
  DefineScriptSet    :: !String -> !ScriptSetSpec -> Action
  -- | 'LogMsg' logs its message calling
  -- 'Cardano.Benchmarking.GeneratorTx.traceDebug' i.e. via the tracer.
  LogMsg             :: !Text -> Action
  deriving (Show, Eq)
deriving instance Generic Action

-- | 'Generator' is interpreted by
-- 'Cardano.Bencmarking.Script.Core.evalGenerator' as a series of
-- transactions, albeit in the form of precursors to UTxO's.
data Generator where
  -- | 'SecureGenesis' gets funds from a genesis via
  -- 'Cardano.TxGenerator.Genesis.genesisSecureInitialFundForKey'.
  -- This is where streams of transactions start.
  SecureGenesis :: !String -> !String -> !String -> Generator -- 0 to N
  -- | 'Split' makes payments with change depending on the pay mode.
  -- The splitting is from potentially sending the change to a
  -- different place.
  Split :: !String -> !PayMode -> !PayMode -> [ L.Coin ] -> Generator
  -- | 'SplitN' divides the funds by N and divides them up into that
  -- many transactions in a finite sequence. The handling starts from
  -- a case in 'Cardano.Benchmarking.Script.Core.evalGenerator' and
  -- has some complexity to it.
  SplitN :: !String -> !PayMode -> !Int -> Generator            -- 1 to N
  -- 'NtoM' seems like it should issue a single N-to-M transaction,
  -- but it's difficult to tell what it's doing.
  NtoM  :: !String -> !PayMode -> !NumberOfInputsPerTx -> !NumberOfOutputsPerTx
        -> !(Maybe Int) -> Maybe String -> Generator
  -- | 'NestedNtoM' is an 'NtoM' transaction that also carries sub-transactions,
  -- which needs the Dijkstra era. The top-level transaction spends its N
  -- inputs into M outputs, pays the fee for the whole transaction and
  -- carries the metadata. Each sub-transaction spends its own inputs into its
  -- own outputs, balanced on its own. All the inputs come from the same
  -- wallet, and all the outputs are paid according to the same 'PayMode'.
  NestedNtoM :: !String -> !PayMode -> !NumberOfInputsPerTx -> !NumberOfOutputsPerTx
        -> !(Maybe Int) -> Maybe String -> !SubTxShape -> Generator
  -- | 'PublishScripts' publishes the variants of a script set as reference
  -- scripts: each variant in an output of its own, as many outputs per
  -- transaction as fit, at the address of the named key, which nothing spends
  -- from. Every transaction spends one fund of the source wallet and pays its
  -- change according to the 'PayMode'. The script set records each variant's
  -- reference output.
  PublishScripts :: !String -> !String -> !String -> !PayMode -> Generator
  -- | 'Sequence' represents sequentially issuing a series in the form
  -- of a list of transaction series represented by 'Generator' itself,
  -- but the nesting is done by first translating to
  -- 'Cardano.Benchmarking.Script.ActionM', then a difficult-to-understand
  -- part follows in the form of 'Streaming.Prelude.for' around
  -- 'Streaming.Prelude.each' @gList@ and is hard to interpret.
  Sequence :: [Generator] -> Generator
  -- | 'Cycle' infinitely repeats the series of transactions in
  -- its argument, through 'Cardano.Benchmarking.Script.Core.evalGenerator'
  -- as per 'Streaming.Prelude.cycle'.
  Cycle :: !Generator -> Generator
  -- | 'Take' is easily interpreted via 'Streaming.Prelude.take'.
  Take :: !Int -> !Generator -> Generator
  -- | 'RoundRobin' wants 'Streaming.interleaves' but
  -- just errors out unimplemented.
  RoundRobin :: [Generator] -> Generator
  -- 'OneOf' is also unimplemented. The intended effect at a
  -- practical level is unclear, though its name suggests something
  -- tough to reconcile with the constructor type.
  OneOf :: [(Generator, Double)] -> Generator
  deriving (Show, Eq)
deriving instance Generic Generator

data ProtocolParametersSource where
  QueryLocalNode :: ProtocolParametersSource
  UseLocalProtocolFile :: !FilePath -> ProtocolParametersSource
  deriving (Show, Eq)
deriving instance Generic ProtocolParametersSource

type TargetNodes = NonEmpty NodeDescription

data SubmitMode where
  LocalSocket :: SubmitMode
  Benchmark   :: !TargetNodes -> !TPSRate -> !NumberOfTxs -> SubmitMode
  DumpToFile  :: !FilePath -> SubmitMode
  DiscardTX   :: SubmitMode
  -- | Submit through an external service, with the 'SubmissionEndpoint'
  -- carrying both the backend protocol to speak and the endpoint it
  -- addresses.
  SubmitToEndpoint :: !SubmissionEndpoint -> SubmitMode
  WriteCBORList :: !FilePath -> SubmitMode
  deriving (Show, Eq)
deriving instance Generic SubmitMode

data PayMode where
  PayToAddr :: !String -> !String -> PayMode
  PayToScript :: !ScriptSpec -> !String -> PayMode
  -- | 'PayToScriptSet' locks outputs at the addresses of the variants of a
  -- script set, cycling through the variants, and stores each output in the
  -- wallet with the witness for spending it by its own variant.
  PayToScriptSet :: !ScriptSetPayment -> !String -> PayMode
  deriving (Show, Eq)
deriving instance Generic PayMode

-- | Where the transaction spending a script output finds the script.
data ScriptSource
  = ScriptInWitness
    -- ^ The script is included in the transaction's witnesses.
  | ScriptByReference
    -- ^ The transaction references the output that a 'PublishScripts' step
    -- created for the script.
  deriving (Show, Eq)
deriving instance Generic ScriptSource

-- | How 'PayToScriptSet' locks outputs at a script set's variants.
data ScriptSetPayment = ScriptSetPayment
  { scriptSetName       :: !String
  , scriptSetBudget     :: !ScriptBudget
    -- ^ Calibrated on the first variant; all variants cost the same.
  , scriptSetPlutusType :: !TxGenPlutusType
  , scriptSetSource     :: !ScriptSource
  , scriptSetGroupSize  :: !Int
    -- ^ The number of consecutive outputs locked at one variant: the number
    -- of inputs per transaction, for every transaction to spend one variant.
  }
  deriving (Show, Eq)
deriving instance Generic ScriptSetPayment

-- | The variants of one script that 'DefineScriptSet' builds.
data ScriptSetSpec = ScriptSetSpec
  { scriptSetBase  :: !(Either String FilePath)
    -- ^ The script, as for 'ScriptSpec': a bundled script's name, or a file.
  , scriptSetCount :: !Int
  , scriptSetSize  :: !ScriptSizeSpec
  }
  deriving (Show, Eq)
deriving instance Generic ScriptSetSpec

data ScriptBudget where
  StaticScriptBudget :: !FilePath -> !FilePath -> !ExecutionUnits -> !Bool -> ScriptBudget
  AutoScript :: !FilePath -> !Int -> ScriptBudget
  -- | 'MeasuredScript' spends with the redeemer in the file as it is, at the
  -- execution units of one run of the script, for transactions spending the
  -- given number of inputs locked by it.
  MeasuredScript :: !FilePath -> !Int -> ScriptBudget
  deriving (Show, Eq)
deriving instance Generic ScriptBudget

data ScriptSpec = ScriptSpec
  {
    scriptSpecFile :: !(Either String FilePath)
  , scriptSpecBudget :: !ScriptBudget
  , scriptSpecPlutusType :: !TxGenPlutusType
  }
  deriving (Show, Eq)
deriving instance Generic ScriptSpec

newtype TxList era = TxList [Tx era]

data ProtocolParameterMode where
  ProtocolParameterQuery :: ProtocolParameterMode
  ProtocolParameterLocal :: ProtocolParameters -> ProtocolParameterMode
