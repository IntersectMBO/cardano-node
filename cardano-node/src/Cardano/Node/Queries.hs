{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Node.Queries
  ( ConvertTxId (..)
  -- * KES
  , MaxKESEvolutions (..)
  , OperationalCertStartKESPeriod (..)
  , GetKESInfo(..)
  , HasKESInfo(..)
  , KESMetricsData (..)
  , HasKESMetricsData (..)
  -- * General ledger
  , LedgerQueries(..)
  -- * Node kernel
  , NodeKernelData(..)
  , nkQueryChain
  , nkQueryLedger
  , mapNodeKernelDataIO
  , setNodeKernel
  , mkNodeKernelData
  -- * Re-exports
  , NodeKernel (..)
  , LocalConnectionId
  , RemoteConnectionId
  , StrictMaybe(..)
  , fromSMaybe
  ) where

import qualified Cardano.Chain.Block as Byron
import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Crypto.Hashing as Byron.Crypto
import           Cardano.Crypto.KES.Class (Period)
import           Cardano.Ledger.BaseTypes (StrictMaybe (..), fromSMaybe)
import qualified Cardano.Ledger.Hashes as Ledger
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import qualified Cardano.Ledger.TxIn as Ledger
import qualified Cardano.Ledger.UMap as UM
import           Cardano.Protocol.TPraos.OCert (KESPeriod (..))
import           Ouroboros.Consensus.Block (ForgeStateInfo, ForgeStateUpdateError)
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import qualified Ouroboros.Consensus.Byron.Ledger.Block as Byron
import qualified Ouroboros.Consensus.Byron.Ledger.Ledger as Byron
import           Ouroboros.Consensus.Byron.Ledger.Mempool (TxId (..))
import qualified Ouroboros.Consensus.Cardano as Cardano
import qualified Ouroboros.Consensus.Cardano.Block as Cardano
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (OneEraForgeStateInfo (..),
                   OneEraForgeStateUpdateError (..))
import           Ouroboros.Consensus.HardFork.Combinator.Embed.Unary
import           Ouroboros.Consensus.Ledger.Abstract (EmptyMK)
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState)
import           Ouroboros.Consensus.Node (NodeKernel (..))
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Ledger.Mempool (TxId (..))
import           Ouroboros.Consensus.Shelley.Node ()
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util.Orphans ()
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.NodeToClient (LocalConnectionId)
import           Ouroboros.Network.NodeToNode (RemoteAddress, RemoteConnectionId)

import           Control.Monad.STM (atomically)
import           Data.ByteString (ByteString)
import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import           Data.SOP
import           Data.SOP.Functors
import           Data.Word (Word64)
import           Lens.Micro ((^.))

--
-- * TxId -> ByteString projection
--
-- | Convert a transaction ID to raw bytes.
class ConvertTxId blk where
  txIdToRawBytes :: TxId (GenTx blk) -> ByteString

instance ConvertTxId ByronBlock where
  txIdToRawBytes (ByronTxId txId) = Byron.Crypto.abstractHashToBytes txId
  txIdToRawBytes (ByronDlgId dlgId) = Byron.Crypto.abstractHashToBytes dlgId
  txIdToRawBytes (ByronUpdateProposalId upId) =
    Byron.Crypto.abstractHashToBytes upId
  txIdToRawBytes (ByronUpdateVoteId voteId) =
    Byron.Crypto.abstractHashToBytes voteId

instance ConvertTxId (ShelleyBlock protocol c) where
  txIdToRawBytes (ShelleyTxId txId) =
    Crypto.hashToBytes . Ledger.extractHash . Ledger.unTxId $ txId

instance All ConvertTxId xs
      => ConvertTxId (HardForkBlock xs) where
  txIdToRawBytes =
    hcollapse
      . hcmap (Proxy @ConvertTxId) (K . txIdToRawBytes . unwrapGenTxId)
      . getOneEraGenTxId
      . getHardForkGenTxId

--
-- * KES
--
-- | The maximum number of evolutions that a KES key can undergo before it is
-- considered expired.
newtype MaxKESEvolutions = MaxKESEvolutions Word64

-- | The start KES period of the configured operational certificate.
newtype OperationalCertStartKESPeriod = OperationalCertStartKESPeriod Period

--
-- * HasKESInfo
--
class HasKESInfo blk where
  getKESInfo :: Proxy blk -> ForgeStateUpdateError blk -> Maybe HotKey.KESInfo
  getKESInfo _ _ = Nothing

instance HasKESInfo (ShelleyBlock protocol era) where
  getKESInfo _ (HotKey.KESCouldNotEvolve ki _) = Just ki
  getKESInfo _ (HotKey.KESKeyAlreadyPoisoned ki _) = Just ki

instance HasKESInfo ByronBlock

instance All HasKESInfo xs => HasKESInfo (HardForkBlock xs) where
  getKESInfo _ =
      hcollapse
    . hcmap (Proxy @HasKESInfo) getOne
    . getOneEraForgeStateUpdateError
   where
    getOne :: forall blk. HasKESInfo blk
           => WrapForgeStateUpdateError blk
           -> K (Maybe HotKey.KESInfo) blk
    getOne = K . getKESInfo (Proxy @blk) . unwrapForgeStateUpdateError

--
-- * KESMetricsData
--
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

class HasKESMetricsData blk where
  -- Because 'ForgeStateInfo' is a type family, we need a Proxy argument to
  -- disambiguate.
  getKESMetricsData :: Proxy blk -> ForgeStateInfo blk -> KESMetricsData

  -- Default to 'NoKESMetricsData'
  getKESMetricsData _ _ = NoKESMetricsData

instance HasKESMetricsData (ShelleyBlock protocol c) where
  getKESMetricsData _ forgeStateInfo =
      TPraosKESMetricsData currKesPeriod maxKesEvos oCertStartKesPeriod
    where
      HotKey.KESInfo
        { kesStartPeriod = KESPeriod startKesPeriod
        , kesEvolution = currKesPeriod
        , kesEndPeriod = KESPeriod endKesPeriod
        } = forgeStateInfo

      maxKesEvos = MaxKESEvolutions $
          fromIntegral $ endKesPeriod - startKesPeriod

      oCertStartKesPeriod = OperationalCertStartKESPeriod startKesPeriod

instance HasKESMetricsData ByronBlock where

instance All HasKESMetricsData xs => HasKESMetricsData (HardForkBlock xs) where
  getKESMetricsData _ forgeStateInfo =
      case forgeStateInfo of
        CurrentEraLacksBlockForging _ -> NoKESMetricsData
        CurrentEraForgeStateUpdated currentEraForgeStateInfo ->
            hcollapse
          . hcmap (Proxy @HasKESMetricsData) getOne
          . getOneEraForgeStateInfo
          $ currentEraForgeStateInfo
    where
      getOne :: forall blk. HasKESMetricsData blk
             => WrapForgeStateInfo blk
             -> K KESMetricsData blk
      getOne = K . getKESMetricsData (Proxy @blk) . unwrapForgeStateInfo

--
-- * GetKESInfo
--
class GetKESInfo blk where
  getKESInfoFromStateInfo :: Proxy blk -> ForgeStateInfo blk -> Maybe HotKey.KESInfo
  getKESInfoFromStateInfo _ _ = Nothing

instance GetKESInfo (ShelleyBlock protocol era) where
  getKESInfoFromStateInfo _ = Just

instance GetKESInfo ByronBlock

instance All GetKESInfo xs => GetKESInfo (HardForkBlock xs) where
  getKESInfoFromStateInfo _ forgeStateInfo =
      case forgeStateInfo of
        CurrentEraLacksBlockForging _ -> Nothing
        CurrentEraForgeStateUpdated currentEraForgeStateInfo ->
            hcollapse
          . hcmap (Proxy @GetKESInfo) getOne
          . getOneEraForgeStateInfo
          $ currentEraForgeStateInfo
    where
      getOne :: forall blk. GetKESInfo blk
             => WrapForgeStateInfo blk
             -> K (Maybe HotKey.KESInfo) blk
      getOne = K . getKESInfoFromStateInfo (Proxy @blk) . unwrapForgeStateInfo

--
-- * General ledger
--
class LedgerQueries blk where
  ledgerUtxoSize     :: LedgerState blk EmptyMK -> Int
  ledgerDelegMapSize :: LedgerState blk EmptyMK -> Int
  ledgerDRepCount    :: LedgerState blk EmptyMK -> Int
  ledgerDRepMapSize  :: LedgerState blk EmptyMK -> Int

instance LedgerQueries Byron.ByronBlock where
  ledgerUtxoSize = Map.size . Byron.unUTxO . Byron.cvsUtxo . Byron.byronLedgerState
  ledgerDelegMapSize _ = 0
  ledgerDRepCount    _ = 0
  ledgerDRepMapSize  _ = 0

instance Shelley.EraCertState era => LedgerQueries (Shelley.ShelleyBlock protocol era) where
  ledgerUtxoSize =
      (\(Shelley.UTxO xs)-> Map.size xs)
    . Shelley.utxosUtxo
    . Shelley.lsUTxOState
    . Shelley.esLState
    . Shelley.nesEs
    . Shelley.shelleyLedgerState
  ledgerDelegMapSize =
      UM.size
    . UM.SPoolUView
    . Shelley.dsUnified
    . (^. Shelley.certDStateL)
    . Shelley.lsCertState
    . Shelley.esLState
    . Shelley.nesEs
    . Shelley.shelleyLedgerState
  ledgerDRepCount =
      Map.size
    . Shelley.vsDReps
    . (^. Shelley.certVStateL)
    . Shelley.lsCertState
    . Shelley.esLState
    . Shelley.nesEs
    . Shelley.shelleyLedgerState
  ledgerDRepMapSize =
      UM.size
    . UM.DRepUView
    . Shelley.dsUnified
    . (^. Shelley.certDStateL)
    . Shelley.lsCertState
    . Shelley.esLState
    . Shelley.nesEs
    . Shelley.shelleyLedgerState

instance (LedgerQueries x, NoHardForks x)
      => LedgerQueries (HardForkBlock '[x]) where
  ledgerUtxoSize     = ledgerUtxoSize     . unFlip . project . Flip
  ledgerDelegMapSize = ledgerDelegMapSize . unFlip . project . Flip
  ledgerDRepCount    = ledgerDRepCount    . unFlip . project . Flip
  ledgerDRepMapSize  = ledgerDRepMapSize  . unFlip . project . Flip

instance LedgerQueries (Cardano.CardanoBlock c) where
  ledgerUtxoSize = \case
    Cardano.LedgerStateByron   ledgerByron   -> ledgerUtxoSize ledgerByron
    Cardano.LedgerStateShelley ledgerShelley -> ledgerUtxoSize ledgerShelley
    Cardano.LedgerStateAllegra ledgerAllegra -> ledgerUtxoSize ledgerAllegra
    Cardano.LedgerStateMary    ledgerMary    -> ledgerUtxoSize ledgerMary
    Cardano.LedgerStateAlonzo  ledgerAlonzo  -> ledgerUtxoSize ledgerAlonzo
    Cardano.LedgerStateBabbage ledgerBabbage -> ledgerUtxoSize ledgerBabbage
    Cardano.LedgerStateConway  ledgerConway  -> ledgerUtxoSize ledgerConway
  ledgerDelegMapSize = \case
    Cardano.LedgerStateByron   ledgerByron   -> ledgerDelegMapSize ledgerByron
    Cardano.LedgerStateShelley ledgerShelley -> ledgerDelegMapSize ledgerShelley
    Cardano.LedgerStateAllegra ledgerAllegra -> ledgerDelegMapSize ledgerAllegra
    Cardano.LedgerStateMary    ledgerMary    -> ledgerDelegMapSize ledgerMary
    Cardano.LedgerStateAlonzo  ledgerAlonzo  -> ledgerDelegMapSize ledgerAlonzo
    Cardano.LedgerStateBabbage ledgerBabbage -> ledgerDelegMapSize ledgerBabbage
    Cardano.LedgerStateConway  ledgerConway  -> ledgerDelegMapSize ledgerConway
  ledgerDRepCount = \case
    Cardano.LedgerStateByron   ledgerByron   -> ledgerDRepCount ledgerByron
    Cardano.LedgerStateShelley ledgerShelley -> ledgerDRepCount ledgerShelley
    Cardano.LedgerStateAllegra ledgerAllegra -> ledgerDRepCount ledgerAllegra
    Cardano.LedgerStateMary    ledgerMary    -> ledgerDRepCount ledgerMary
    Cardano.LedgerStateAlonzo  ledgerAlonzo  -> ledgerDRepCount ledgerAlonzo
    Cardano.LedgerStateBabbage ledgerBabbage -> ledgerDRepCount ledgerBabbage
    Cardano.LedgerStateConway  ledgerConway  -> ledgerDRepCount ledgerConway
  ledgerDRepMapSize = \case
    Cardano.LedgerStateByron   ledgerByron   -> ledgerDRepMapSize ledgerByron
    Cardano.LedgerStateShelley ledgerShelley -> ledgerDRepMapSize ledgerShelley
    Cardano.LedgerStateAllegra ledgerAllegra -> ledgerDRepMapSize ledgerAllegra
    Cardano.LedgerStateMary    ledgerMary    -> ledgerDRepMapSize ledgerMary
    Cardano.LedgerStateAlonzo  ledgerAlonzo  -> ledgerDRepMapSize ledgerAlonzo
    Cardano.LedgerStateBabbage ledgerBabbage -> ledgerDRepMapSize ledgerBabbage
    Cardano.LedgerStateConway  ledgerConway  -> ledgerDRepMapSize ledgerConway

--
-- * Node kernel
--
newtype NodeKernelData blk =
  NodeKernelData
  { unNodeKernelData :: IORef (StrictMaybe (NodeKernel IO RemoteAddress LocalConnectionId blk))
  }

mkNodeKernelData :: IO (NodeKernelData blk)
mkNodeKernelData = NodeKernelData <$> newIORef SNothing

setNodeKernel :: NodeKernelData blk
              -> NodeKernel IO RemoteAddress LocalConnectionId blk
              -> IO ()
setNodeKernel (NodeKernelData ref) nodeKern =
  writeIORef ref $ SJust nodeKern

mapNodeKernelDataIO ::
  (NodeKernel IO RemoteAddress LocalConnectionId blk -> IO a)
  -> NodeKernelData blk
  -> IO (StrictMaybe a)
mapNodeKernelDataIO f (NodeKernelData ref) =
  readIORef ref >>= traverse f

nkQueryLedger ::
     (ExtLedgerState blk EmptyMK -> a)
  -> NodeKernel IO RemoteAddress LocalConnectionId blk
  -> IO a
nkQueryLedger f NodeKernel{getChainDB} =
  f <$> atomically (ChainDB.getCurrentLedger getChainDB)

nkQueryChain ::
     (AF.AnchoredFragment (Header blk) -> a)
  -> NodeKernel IO RemoteAddress LocalConnectionId blk
  -> IO a
nkQueryChain f NodeKernel{getChainDB} =
  f <$> atomically (ChainDB.getCurrentChain getChainDB)
