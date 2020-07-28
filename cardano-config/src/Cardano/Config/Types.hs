{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Config.Types
    ( CBORObject (..)
    , CertificateFile (..)
    , ConfigError (..)
    , DbFile (..)
    , GenesisFile (..)
    , KESMetricsData (..)
    , MaxKESEvolutions (..)
    , MaxConcurrencyBulkSync (..)
    , MaxConcurrencyDeadline (..)
    , OperationalCertStartKESPeriod (..)
    , HasKESMetricsData (..)
    , NodeAddress (..)
    , NodeHostAddress (..)
    , NodeProtocolMode (..)
    , SigningKeyFile (..)
    , ProtocolFilepaths (..)
    , TopologyFile (..)
    , TraceConstraints
    , SocketPath (..)
    , UpdateProposalFile (..)
    , ViewMode (..)
    , parseNodeHostAddress
    ) where

import           Cardano.Prelude hiding (show)
import           Prelude (show)

import           Data.Aeson
import           Data.IP (IP)
import           Data.String (String)
import qualified Data.Text as Text
import           Network.Socket (PortNumber)

import           Cardano.BM.Tracing (ToObject)

import qualified Cardano.Chain.Slotting as Byron
import           Cardano.Crypto.KES.Class (Period)

import           Ouroboros.Consensus.Block (BlockProtocol, ForgeState (..),
                     Header)
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import           Ouroboros.Consensus.HeaderValidation (OtherHeaderEnvelopeError)
import           Ouroboros.Consensus.Ledger.Abstract (LedgerError)
import           Ouroboros.Consensus.Ledger.Inspect (LedgerEvent)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr,
                     GenTxId, HasTxId, HasTxs (..))
import           Ouroboros.Consensus.Mock.Ledger.Block (SimpleBlock)
import           Ouroboros.Consensus.Protocol.Abstract (CannotLead,
                     ValidationErr)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Ledger.Mempool (GenTx, TxId)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto.HotKey
                     (HotKey (..))
import           Ouroboros.Consensus.Util.Condense (Condense (..))

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Unary

import           Ouroboros.Network.Block (HeaderHash)

import           Cardano.Config.LedgerQueries

import           Shelley.Spec.Ledger.OCert (KESPeriod (..))


-- | Errors for the cardano-config module.
data ConfigError
    = ConfigErrorFileNotFound !FilePath

-- | Instance for showing the @ConfigError@.
instance Show ConfigError where
    show (ConfigErrorFileNotFound fp)
        = "File '" <> fp <> "' not found!"

-- | Specify what the CBOR file is
-- i.e a block, a tx, etc
data CBORObject = CBORBlockByron Byron.EpochSlots
                | CBORDelegationCertificateByron
                | CBORTxByron
                | CBORUpdateProposalByron
                | CBORVoteByron
                deriving Show


--------------------------------------------------------------------------------
-- Cardano Configuration Data Structures
--------------------------------------------------------------------------------

-- | Mock protocols requires different parameters to real protocols.
-- Therefore we distinguish this at the top level on the command line.
data NodeProtocolMode = MockProtocolMode
                      | RealProtocolMode

data ProtocolFilepaths =
     ProtocolFilepaths {
       byronCertFile   :: !(Maybe FilePath)
     , byronKeyFile    :: !(Maybe FilePath)
     , shelleyKESFile  :: !(Maybe FilePath)
     , shelleyVRFFile  :: !(Maybe FilePath)
     , shelleyCertFile :: !(Maybe FilePath)
     }

newtype TopologyFile = TopologyFile
  { unTopology :: FilePath }
  deriving newtype Show

newtype DbFile = DbFile
  { unDB :: FilePath }
  deriving newtype Show

newtype GenesisFile = GenesisFile
  { unGenesisFile :: FilePath }
  deriving stock (Eq, Ord)
  deriving newtype (IsString, Show)

instance FromJSON GenesisFile where
  parseJSON (String genFp) = pure . GenesisFile $ Text.unpack genFp
  parseJSON invalid = panic $ "Parsing of GenesisFile failed due to type mismatch. "
                           <> "Encountered: " <> (Text.pack $ show invalid)

-- Encompasses stake certificates, stake pool certificates,
-- genesis delegate certificates and MIR certificates.
newtype CertificateFile = CertificateFile
  { unCertificateFile :: FilePath }
  deriving newtype (Eq, Show)

newtype UpdateProposalFile = UpdateProposalFile
  { unUpdateProposalFile :: FilePath }
  deriving newtype (Eq, Show)

newtype SocketPath = SocketPath
  { unSocketPath :: FilePath }
  deriving stock (Eq, Ord)
  deriving newtype (FromJSON, IsString, Show)

newtype SigningKeyFile = SigningKeyFile
  { unSigningKeyFile ::  FilePath }
  deriving stock (Eq, Ord)
  deriving newtype (IsString, Show)

-- Node can be run in two modes.
data ViewMode = LiveView    -- Live mode with TUI
              | SimpleView  -- Simple mode, just output text.
              deriving (Eq, Show)

instance FromJSON ViewMode where
  parseJSON (String str) = case str of
                            "LiveView" -> pure LiveView
                            "SimpleView" -> pure SimpleView
                            view -> panic $ "Parsing of ViewMode: "
                                          <> view <> " failed. "
                                          <> view <> " is not a valid view mode"
  parseJSON invalid = panic $ "Parsing of ViewMode failed due to type mismatch. "
                            <> "Encountered: " <> (Text.pack $ show invalid)

--------------------------------------------------------------------------------
-- Cardano Topology Related Data Structures
--------------------------------------------------------------------------------

-- | IPv4 address with a port number.
data NodeAddress = NodeAddress
  { naHostAddress :: !NodeHostAddress
  , naPort :: !PortNumber
  } deriving (Eq, Ord, Show)

instance Condense NodeAddress where
  condense (NodeAddress addr port) = show addr ++ ":" ++ show port

instance FromJSON NodeAddress where
  parseJSON = withObject "NodeAddress" $ \v -> do
    NodeAddress
      <$> v .: "addr"
      <*> ((fromIntegral :: Int -> PortNumber) <$> v .: "port")

instance ToJSON NodeAddress where
  toJSON na =
    object
      [ "addr" .= toJSON (naHostAddress na)
      , "port" .= (fromIntegral (naPort na) :: Int)
      ]

-- Embedding a Maybe inside a newtype is somewhat icky but this seems to work
-- and removing the Maybe breaks the functionality in a subtle way that is difficult
-- to diagnose.
newtype NodeHostAddress
  = NodeHostAddress { unNodeHostAddress :: Maybe IP }
  deriving newtype Show
  deriving (Eq, Ord)

instance FromJSON NodeHostAddress where
  parseJSON (String ipStr) =
    case readMaybe $ Text.unpack ipStr of
      Just ip -> pure $ NodeHostAddress (Just ip)
      Nothing -> panic $ "Parsing of IP failed: " <> ipStr
  parseJSON Null = pure $ NodeHostAddress Nothing
  parseJSON invalid = panic $ "Parsing of IP failed due to type mismatch. "
                            <> "Encountered: " <> (Text.pack $ show invalid) <> "\n"

parseNodeHostAddress :: String -> Either String NodeHostAddress
parseNodeHostAddress str =
   maybe (Left $ "Failed to parse: " ++ str) (Right . NodeHostAddress . Just) $ readMaybe str

instance ToJSON NodeHostAddress where
  toJSON mha =
    case unNodeHostAddress mha of
      Just ip -> String (Text.pack $ show ip)
      Nothing -> Null

--------------------------------------------------------------------------------
-- Protocol & Tracing Related
--------------------------------------------------------------------------------

--TODO: move all of these to cardano-node

-- | KES-related data to be traced as metrics.
data KESMetricsData
  = NoKESMetricsData
  -- ^ The current protocol does not support KES.
  | TPraosKESMetricsData
      !Period
      -- ^ The current KES period of the hot key, relative to the start KES
      -- period of the operational certificate.
      !MaxKESEvolutions
      -- ^ The configured max KES evolutions.
      !OperationalCertStartKESPeriod
      -- ^ The start KES period of the configured operational certificate.

-- | The maximum number of evolutions that a KES key can undergo before it is
-- considered expired.
newtype MaxKESEvolutions = MaxKESEvolutions Word64

-- | The start KES period of the configured operational certificate.
newtype OperationalCertStartKESPeriod = OperationalCertStartKESPeriod Period

class HasKESMetricsData blk where
  getKESMetricsData :: ForgeState blk -> KESMetricsData

  -- Default to 'NoKESMetricsData'
  getKESMetricsData _ = NoKESMetricsData

instance HasKESMetricsData (ShelleyBlock c) where
  getKESMetricsData forgeState =
      TPraosKESMetricsData currKesPeriod maxKesEvos oCertStartKesPeriod
    where
      HotKey
        { hkStart     = KESPeriod startKesPeriod
        , hkEvolution = currKesPeriod
        , hkEnd       = KESPeriod endKesPeriod
        } = chainIndepState forgeState

      maxKesEvos = MaxKESEvolutions $
          fromIntegral $ endKesPeriod - startKesPeriod

      oCertStartKesPeriod = OperationalCertStartKESPeriod startKesPeriod

instance HasKESMetricsData ByronBlock where

instance HasKESMetricsData (SimpleBlock a b) where

instance (HasKESMetricsData x, NoHardForks x)
      => HasKESMetricsData (HardForkBlock '[x]) where
  getKESMetricsData forgeState =
    getKESMetricsData (project forgeState)


newtype MaxConcurrencyBulkSync = MaxConcurrencyBulkSync
  { unMaxConcurrencyBulkSync :: Word }
  deriving stock (Eq, Ord)
  deriving newtype (FromJSON, Show)

newtype MaxConcurrencyDeadline = MaxConcurrencyDeadline
  { unMaxConcurrencyDeadline :: Word }
  deriving stock (Eq, Ord)
  deriving newtype (FromJSON, Show)


-- | Tracing-related constraints for monitoring purposes.
--
-- When you need a 'Show' or 'Condense' instance for more types, just add the
-- appropriate constraint here. There's no need to modify the consensus
-- code-base, unless the corresponding instance is missing. Note we are aiming to
-- remove all `Condense` constaints by defining the relevant 'ToObject' instance
-- in 'cardano-node'
type TraceConstraints blk =
    ( Condense blk
    , Condense (Header blk)
    , Condense (HeaderHash blk)
    , Condense (GenTx blk)
    , Condense (TxId (GenTx blk))
    , HasTxs blk
    , HasTxId (GenTx blk)
    , LedgerQueries blk
    , Show (ApplyTxErr blk)
    , Show (GenTx blk)
    , Show (GenTxId blk)
    , Show blk
    , Show (TxId (GenTx blk))
    , ToJSON   (TxId (GenTx blk))
    , ToObject (ApplyTxErr blk)
    , ToObject (GenTx blk)
    , ToObject (Header blk)
    , ToObject (LedgerError blk)
    , ToObject (LedgerEvent blk)
    , ToObject (OtherHeaderEnvelopeError blk)
    , ToObject (ValidationErr (BlockProtocol blk))
    , ToObject (CannotLead (BlockProtocol blk))
    )
