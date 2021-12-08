{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Node.Queries
  ( ConvertTxId (..)
  -- * KES
  , MaxKESEvolutions (..)
  , OperationalCertStartKESPeriod (..)
  , HasKESInfo(..)
  , KESMetricsData (..)
  , HasKESMetricsData (..)
  -- * General ledger
  , LedgerQueries(..)
  ) where

import Cardano.Prelude hiding (All, (:.:))

import Data.Map.Strict qualified as Map
import Data.SOP.Strict

import Cardano.Crypto.KES.Class (Period)
import Cardano.Protocol.TPraos.OCert (KESPeriod (..))
import Cardano.Chain.Block qualified as Byron
import Cardano.Chain.UTxO qualified as Byron
import Cardano.Crypto.Hash qualified as Crypto
import Cardano.Crypto.Hashing qualified as Byron.Crypto
import Cardano.Ledger.SafeHash qualified as Ledger
import Cardano.Ledger.Shelley.LedgerState qualified as Shelley
import Cardano.Ledger.Shelley.UTxO qualified as Shelley
import Cardano.Ledger.TxIn qualified as Ledger

import Ouroboros.Consensus.HardFork.Combinator.Embed.Unary
import Ouroboros.Consensus.Byron.Ledger.Block qualified as Byron
import Ouroboros.Consensus.Byron.Ledger.Ledger qualified as Byron
import Ouroboros.Consensus.Shelley.Ledger qualified as Shelley
import Ouroboros.Consensus.Cardano qualified as Cardano
import Ouroboros.Consensus.Cardano.Block qualified as Cardano
import Ouroboros.Consensus.Block (ForgeStateInfo, ForgeStateUpdateError)
import Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import Ouroboros.Consensus.Byron.Ledger.Mempool (TxId (..))
import Ouroboros.Consensus.HardFork.Combinator
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (OneEraForgeStateInfo (..), OneEraForgeStateUpdateError (..))
import Ouroboros.Consensus.Protocol.Ledger.HotKey qualified as HotKey
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import Ouroboros.Consensus.Shelley.Ledger.Mempool (TxId (..))
import Ouroboros.Consensus.Shelley.Node ()
import Ouroboros.Consensus.TypeFamilyWrappers


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

instance ConvertTxId (ShelleyBlock c) where
  txIdToRawBytes (ShelleyTxId txId) =
    Crypto.hashToBytes . Ledger.extractHash . Ledger._unTxId $ txId

instance All ConvertTxId xs
      => ConvertTxId (HardForkBlock xs) where
  txIdToRawBytes =
    hcollapse
      . hcmap (Proxy @ ConvertTxId) (K . txIdToRawBytes . unwrapGenTxId)
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

instance HasKESInfo (ShelleyBlock era) where
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

instance HasKESMetricsData (ShelleyBlock c) where
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
-- * General ledger
--
class LedgerQueries blk where
  ledgerUtxoSize     :: LedgerState blk -> Int
  ledgerDelegMapSize :: LedgerState blk -> Int

instance LedgerQueries Byron.ByronBlock where
  ledgerUtxoSize = Map.size . Byron.unUTxO . Byron.cvsUtxo . Byron.byronLedgerState
  ledgerDelegMapSize _ = 0

instance LedgerQueries (Shelley.ShelleyBlock era) where
  ledgerUtxoSize =
      (\(Shelley.UTxO xs)-> Map.size xs)
    . Shelley._utxo
    . Shelley._utxoState
    . Shelley.esLState
    . Shelley.nesEs
    . Shelley.shelleyLedgerState
  ledgerDelegMapSize =
      Map.size
    . Shelley._delegations
    . Shelley._dstate
    . Shelley._delegationState
    . Shelley.esLState
    . Shelley.nesEs
    . Shelley.shelleyLedgerState

instance (LedgerQueries x, NoHardForks x)
      => LedgerQueries (HardForkBlock '[x]) where
  ledgerUtxoSize = ledgerUtxoSize . project
  ledgerDelegMapSize = ledgerDelegMapSize . project

instance LedgerQueries (Cardano.CardanoBlock c) where
  ledgerUtxoSize = \case
    Cardano.LedgerStateByron   ledgerByron   -> ledgerUtxoSize ledgerByron
    Cardano.LedgerStateShelley ledgerShelley -> ledgerUtxoSize ledgerShelley
    Cardano.LedgerStateAllegra ledgerAllegra -> ledgerUtxoSize ledgerAllegra
    Cardano.LedgerStateMary    ledgerMary    -> ledgerUtxoSize ledgerMary
    Cardano.LedgerStateAlonzo  ledgerAlonzo  -> ledgerUtxoSize ledgerAlonzo
  ledgerDelegMapSize = \case
    Cardano.LedgerStateByron   ledgerByron   -> ledgerDelegMapSize ledgerByron
    Cardano.LedgerStateShelley ledgerShelley -> ledgerDelegMapSize ledgerShelley
    Cardano.LedgerStateAllegra ledgerAllegra -> ledgerDelegMapSize ledgerAllegra
    Cardano.LedgerStateMary    ledgerMary    -> ledgerDelegMapSize ledgerMary
    Cardano.LedgerStateAlonzo  ledgerAlonzo  -> ledgerDelegMapSize ledgerAlonzo
