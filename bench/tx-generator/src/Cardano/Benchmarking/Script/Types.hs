{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-error=unused-top-binds #-}

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
        , Generator(..)
        , GeneratorDRepCredential (..)
        , GeneratorDRepCredentialBody
        , GeneratorDRepCredentialConstraints
        , GeneratorDRepCredentialScriptHash
        , GeneratorStakeCredential (..)
        , PayMode(PayToAddr, PayToScript)
        , ProtocolParameterMode(..)
        , ProtocolParametersSource(QueryLocalNode, UseLocalProtocolFile)
        , ScriptBudget(AutoScript, StaticScriptBudget)
        , ScriptSpec(..)
        , SubmitMode(Benchmark, DiscardTX, DumpToFile, LocalSocket,
                NodeToNode)
        , TargetNodes
        , TxList(..)
) where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L
import           Cardano.Api.Shelley

import qualified Cardano.Crypto.DSIGN as Crypto
import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Ledger.Crypto as Crypto
import qualified Cardano.Crypto.KES as Crypto
import qualified Cardano.Crypto.VRF as Crypto

import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Hashes as Ledger
import qualified Cardano.Ledger.Keys as Ledger

import           Cardano.Benchmarking.OuroborosImports (SigningKeyFile)
import           Cardano.Node.Configuration.NodeAddress (NodeIPv4Address)
import           Cardano.TxGenerator.Setup.NixService (NodeDescription)
import           Cardano.TxGenerator.Types

import           Prelude

import           Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.ByteString.Short as SBS (ShortByteString)
import           Data.Constraint (type (&), Constraint)
import           Data.Function (on)
import           Data.Kind (Type)
import           Data.List.NonEmpty
import           Data.Text (Text)
import           Data.Typeable
import           GHC.Generics
import           GHC.Show


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
  -- | 'ReadDRepKeys' expects the path to a node config file. This
  -- configuration is supposed to refer to a genesis which has
  -- been created with cardano-cli create-testnet-data, and from
  -- where DRep signing keys can be loaded.
  ReadDRepKeys       :: !FilePath -> Action
  -- | 'ReadStakeKeys' expects the path to a node config file. This
  -- configuration is supposed to refer to a genesis which has
  -- been created with cardano-cli create-testnet-data, and from
  -- where stake keys can be loaded.
  ReadStakeKeys      :: !FilePath -> Action
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
  WaitForEra         :: !AnyCardanoEra -> Action
  -- | 'SetProtocolParameters' has one option to read from a file and
  -- another to pass directly and just sets a state variable for
  -- the @protoParams@ field of 'Cardano.Benchmarking.Script.Env.Env'.
  SetProtocolParameters :: ProtocolParametersSource -> Action
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
  -- | 'Propose' represents submitting a governance action proposal as
  -- just a singleton.
  Propose :: !String
          -> !PayMode
          -> !L.Coin
          -> !GeneratorStakeCredential
          -> !(Ledger.Anchor L.StandardCrypto)
          -> Generator
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
  -- 'Vote issues a transaction to vote on a governance action proposal.
  Vote :: !String
       -> !PayMode
       -> !Vote
       -> !GeneratorDRepCredential
       -> Maybe (Ledger.Url, Text)
       -> Generator
  -- | 'EmptyStream' will yield an empty stream. For testing only.
  EmptyStream :: Generator
  deriving (Eq, Show)
deriving instance Generic Generator

deriving instance Generic Vote
deriving instance FromJSON Vote
deriving instance ToJSON Vote

newtype GeneratorStakeCredential = GeneratorStakeCredential
  { unGeneratorStakeCredential :: StakeCredential
  } deriving (Eq, Show, Generic, ToJSON)

instance FromJSON GeneratorStakeCredential where
  parseJSON value = do
    ledgerStakeCredential <- parseJSON value
    pure . GeneratorStakeCredential $
      fromShelleyStakeCredential ledgerStakeCredential

type GeneratorDRepCredentialConstraints :: Type -> Constraint
type GeneratorDRepCredentialConstraints ty =
  ( (L.Crypto ty)
  & (Typeable ty)
  & (Crypto.HashAlgorithm (Crypto.HASH ty))
  & (Crypto.HashAlgorithm (Crypto.ADDRHASH ty))
  & (Crypto.DSIGNAlgorithm (Crypto.DSIGN ty))
  & (Crypto.KESAlgorithm (Crypto.KES ty))
  & (Crypto.VRFAlgorithm (Crypto.VRF ty))
  & ((Crypto.ContextDSIGN (Crypto.DSIGN ty)) ~ ())
  & ((Crypto.ContextKES (Crypto.KES ty)) ~ ())
  & ((Crypto.ContextVRF (Crypto.VRF ty)) ~ ())
  )

type GeneratorDRepCredentialBody :: Type -> Type
type GeneratorDRepCredentialBody c = L.Credential 'L.DRepRole c

-- Somehow dropping the constraints made things build.
-- forall c . GeneratorDRepCredentialConstraints c =>
data GeneratorDRepCredential =
  GeneratorDRepCredential
    { unGeneratorDRepCredential :: GeneratorDRepCredentialBody L.StandardCrypto }

type GeneratorDRepCredentialKeyHash :: Type -> Type
type GeneratorDRepCredentialKeyHash c = L.KeyHash 'L.DRepRole c

type GeneratorDRepCredentialScriptHash :: Type -> Type
type GeneratorDRepCredentialScriptHash c = Ledger.ScriptHash c

instance Eq GeneratorDRepCredential where
  GeneratorDRepCredential            (leftDRep  :: GeneratorDRepCredentialBody c)
          == GeneratorDRepCredential (rightDRep :: GeneratorDRepCredentialBody c')
    | ( L.KeyHashObj (leftObj  :: GeneratorDRepCredentialKeyHash c)
      , L.KeyHashObj (rightObj :: GeneratorDRepCredentialKeyHash c'))
              <- (leftDRep, rightDRep)
    , ( Ledger.KeyHash (leftHash  :: Crypto.Hash Crypto.Blake2b_224 (Crypto.VerKeyDSIGN Crypto.Ed25519DSIGN))
    ,   Ledger.KeyHash (rightHash :: Crypto.Hash Crypto.Blake2b_224 (Crypto.VerKeyDSIGN Crypto.Ed25519DSIGN)))
              <- (leftObj, rightObj)
    , ( Crypto.UnsafeHash (leftBytes  :: SBS.ShortByteString)
    ,   Crypto.UnsafeHash (rightBytes :: SBS.ShortByteString))
              <- (leftHash, rightHash)
    = leftBytes == rightBytes
    | ( L.ScriptHashObj (leftObj  :: Ledger.ScriptHash Crypto.StandardCrypto)
      , L.ScriptHashObj (rightObj :: Ledger.ScriptHash Crypto.StandardCrypto))
              <- (leftDRep, rightDRep)
    , ( Ledger.ScriptHash (leftHash  :: Crypto.Hash Crypto.Blake2b_224 Ledger.EraIndependentScript)
    ,   Ledger.ScriptHash (rightHash :: Crypto.Hash Crypto.Blake2b_224 Ledger.EraIndependentScript))
              <- (leftObj, rightObj)
    , (Crypto.UnsafeHash (leftBytes :: SBS.ShortByteString), Crypto.UnsafeHash (rightBytes :: SBS.ShortByteString))
              <- (leftHash, rightHash)
    = leftBytes == rightBytes
    | otherwise = False

instance Show GeneratorDRepCredential where
  showsPrec n GeneratorDRepCredential {..} =
      header . showSpace . showParen True body where
    header :: ShowS
    header =  showString "GeneratorDRepCredential"
    body   :: ShowS
    body   =  showsPrec n unGeneratorDRepCredential

deriving instance Generic GeneratorDRepCredential

instance ToJSON GeneratorDRepCredential where
  toJSON GeneratorDRepCredential {..} = toJSON unGeneratorDRepCredential

instance FromJSON GeneratorDRepCredential where
  parseJSON value = do
    ledgerDRepCredential :: GeneratorDRepCredentialBody L.StandardCrypto
            <- parseJSON value
    pure $ GeneratorDRepCredential ledgerDRepCredential

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
  NodeToNode  :: NonEmpty NodeIPv4Address -> SubmitMode --deprecated
  deriving (Show, Eq)
deriving instance Generic SubmitMode

data PayMode where
  PayToAddr :: !String -> !String -> PayMode
  PayToScript :: !ScriptSpec -> !String -> PayMode
  deriving (Show, Eq)
deriving instance Generic PayMode

data ScriptBudget where
  StaticScriptBudget :: !FilePath -> !FilePath -> !ExecutionUnits -> !Bool -> ScriptBudget
  AutoScript :: !FilePath -> !Int -> ScriptBudget
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
