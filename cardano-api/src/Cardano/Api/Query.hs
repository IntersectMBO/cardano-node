{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

-- The Shelley ledger uses promoted data kinds which we have to use, but we do
-- not export any from this API. We also use them unticked as nature intended.
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}


-- | Queries from local clients to the node.
--
module Cardano.Api.Query (

    -- * Queries
    QueryInMode(..),
    QueryInEra(..),
    QueryInShelleyBasedEra(..),
    QueryUTxOFilter(..),
    UTxO(..),
    UTxOInAnyEra(..),

    -- * Internal conversion functions
    toConsensusQuery,
    fromConsensusQueryResult,

    -- * Wrapper types used in queries
    SerialisedDebugLedgerState(..),
    ProtocolState(..),
    decodeProtocolState,

    DebugLedgerState(..),
    decodeDebugLedgerState,

    SerialisedCurrentEpochState(..),
    CurrentEpochState(..),
    decodeCurrentEpochState,

    SerialisedPoolState(..),
    PoolState(..),
    decodePoolState,

    EraHistory(..),
    SystemStart(..),

    SlotsInEpoch(..),
    SlotsToEpochEnd(..),

    slotToEpoch,

    LedgerState(..),

    getProgress,

    -- * Internal conversion functions
    toLedgerUTxO,
    fromLedgerUTxO,

  ) where

import           Control.Monad (forM)
import           Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.=))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (Parser)
import           Data.Bifunctor (bimap, first)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HMS
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Sharing (FromSharedCBOR, Interns, Share)
import           Data.SOP.Strict (SListI)
import           Data.Text (Text)
import           Data.Typeable
import           Prelude

import           Ouroboros.Network.Protocol.LocalStateQuery.Client (Some (..))

import qualified Ouroboros.Consensus.HardFork.Combinator as Consensus
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import qualified Ouroboros.Consensus.HardFork.Combinator.AcrossEras as Consensus
import qualified Ouroboros.Consensus.HardFork.Combinator.Degenerate as Consensus
import qualified Ouroboros.Consensus.HardFork.History as History
import qualified Ouroboros.Consensus.HardFork.History.Qry as Qry

import           Ouroboros.Consensus.BlockchainTime.WallClock.Types (RelativeTime, SlotLength)
import qualified Ouroboros.Consensus.Byron.Ledger as Consensus
import           Ouroboros.Consensus.Cardano.Block (LedgerState (..), StandardCrypto)
import qualified Ouroboros.Consensus.Cardano.Block as Consensus
import qualified Ouroboros.Consensus.Ledger.Query as Consensus
import qualified Ouroboros.Consensus.Protocol.Abstract as Consensus
import qualified Ouroboros.Consensus.Shelley.Ledger as Consensus
import           Ouroboros.Network.Block (Serialised (..))

import           Cardano.Binary
import           Cardano.Slotting.Slot (WithOrigin (..))
import           Cardano.Slotting.Time (SystemStart (..))

import qualified Cardano.Chain.Update.Validation.Interface as Byron.Update
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Era as Ledger
import qualified Control.State.Transition.Extended as Ledger

import qualified Cardano.Ledger.Shelley.API as Shelley
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley

import           Cardano.Api.Address
import           Cardano.Api.Block
import           Cardano.Api.Certificate
import           Cardano.Api.EraCast
import           Cardano.Api.Eras
import           Cardano.Api.GenesisParameters
import           Cardano.Api.KeysShelley
import           Cardano.Api.Modes
import           Cardano.Api.NetworkId
import           Cardano.Api.Orphans ()
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.TxBody
import           Cardano.Api.Value
import           Data.Word (Word64)

import qualified Data.Aeson.KeyMap as KeyMap
import Cardano.Ledger.SafeHash (HashAnnotated)

-- ----------------------------------------------------------------------------
-- Queries
--

data QueryInMode mode result where
  QueryCurrentEra
    :: ConsensusModeIsMultiEra mode
    -> QueryInMode mode AnyCardanoEra

  QueryInEra
    :: EraInMode era mode
    -> QueryInEra era result
    -> QueryInMode mode (Either EraMismatch result)

  QueryEraHistory
    :: ConsensusModeIsMultiEra mode
    -> QueryInMode mode (EraHistory mode)

  QuerySystemStart
    :: QueryInMode mode SystemStart

  QueryChainBlockNo
    :: QueryInMode mode (WithOrigin BlockNo)

  QueryChainPoint
    :: ConsensusMode mode
    -> QueryInMode mode ChainPoint

data EraHistory mode where
  EraHistory
    :: ConsensusBlockForMode mode ~ Consensus.HardForkBlock xs
    => ConsensusMode mode
    -> History.Interpreter xs
    -> EraHistory mode

getProgress :: SlotNo -> EraHistory mode -> Either Qry.PastHorizonException (RelativeTime, SlotLength)
getProgress slotNo (EraHistory _ interpreter) = Qry.interpretQuery interpreter (Qry.slotToWallclock slotNo)

--TODO: add support for these
--     QueryEraStart   :: ConsensusModeIsMultiEra mode
--                     -> EraInMode era mode
--                     -> QueryInMode mode (Maybe EraStart)

newtype SlotsInEpoch = SlotsInEpoch Word64

newtype SlotsToEpochEnd = SlotsToEpochEnd Word64

slotToEpoch :: SlotNo -> EraHistory mode -> Either Qry.PastHorizonException (EpochNo, SlotsInEpoch, SlotsToEpochEnd)
slotToEpoch slotNo (EraHistory _ interpreter) = case Qry.interpretQuery interpreter (Qry.slotToEpoch slotNo) of
  Right (epochNumber, slotsInEpoch, slotsToEpochEnd) -> Right (epochNumber, SlotsInEpoch slotsInEpoch, SlotsToEpochEnd slotsToEpochEnd)
  Left e -> Left e

deriving instance Show (QueryInMode mode result)

data QueryInEra era result where
     QueryByronUpdateState :: QueryInEra ByronEra ByronUpdateState

     QueryInShelleyBasedEra :: ShelleyBasedEra era
                            -> QueryInShelleyBasedEra era result
                            -> QueryInEra era result

deriving instance Show (QueryInEra era result)


data QueryInShelleyBasedEra era result where
  QueryEpoch
    :: QueryInShelleyBasedEra era EpochNo

  QueryGenesisParameters
    :: QueryInShelleyBasedEra era GenesisParameters

  QueryProtocolParameters
    :: QueryInShelleyBasedEra era ProtocolParameters

  QueryProtocolParametersUpdate
    :: QueryInShelleyBasedEra era
            (Map (Hash GenesisKey) ProtocolParametersUpdate)

  QueryStakeDistribution
    :: QueryInShelleyBasedEra era (Map (Hash StakePoolKey) Rational)

  QueryUTxO
    :: QueryUTxOFilter
    -> QueryInShelleyBasedEra era (UTxO era)

  QueryStakeAddresses
    :: Set StakeCredential
    -> NetworkId
    -> QueryInShelleyBasedEra era (Map StakeAddress Lovelace, Map StakeAddress PoolId)

  QueryStakePools
    :: QueryInShelleyBasedEra era (Set PoolId)

  QueryStakePoolParameters
    :: Set PoolId
    -> QueryInShelleyBasedEra era (Map PoolId StakePoolParameters)

     -- TODO: add support for RewardProvenance
     -- QueryPoolRanking
     --   :: QueryInShelleyBasedEra era RewardProvenance

  QueryDebugLedgerState
    :: QueryInShelleyBasedEra era (SerialisedDebugLedgerState era)

  QueryProtocolState
    :: QueryInShelleyBasedEra era (ProtocolState era)

  QueryCurrentEpochState
    :: QueryInShelleyBasedEra era (SerialisedCurrentEpochState era)

  QueryPoolState
    :: Maybe (Set PoolId)
    -> QueryInShelleyBasedEra era (SerialisedPoolState era)

deriving instance Show (QueryInShelleyBasedEra era result)


-- ----------------------------------------------------------------------------
-- Wrapper types used in queries
--

-- | Getting the /whole/ UTxO is obviously not efficient since the result can
-- be huge. Filtering by address is also not efficient because it requires a
-- linear search.
--
-- The 'QueryUTxOFilterByTxIn' is efficient since it fits with the structure of
-- the UTxO (which is indexed by 'TxIn').
--
data QueryUTxOFilter =
     -- | /O(n) time and space/ for utxo size n
     QueryUTxOWhole

     -- | /O(n) time, O(m) space/ for utxo size n, and address set size m
   | QueryUTxOByAddress (Set AddressAny)

     -- | /O(m log n) time, O(m) space/ for utxo size n, and address set size m
   | QueryUTxOByTxIn (Set TxIn)
  deriving (Eq, Show)

--TODO: provide appropriate instances for these types as needed, e.g. JSON

newtype ByronUpdateState = ByronUpdateState Byron.Update.State
  deriving Show

newtype UTxO era = UTxO { unUTxO :: Map TxIn (TxOut CtxUTxO era) }
  deriving (Eq, Show)

instance EraCast UTxO where
  eraCast toEra' (UTxO m) = UTxO <$> forM m (eraCast toEra')

data UTxOInAnyEra where
  UTxOInAnyEra :: CardanoEra era
               -> UTxO era
               -> UTxOInAnyEra

deriving instance Show UTxOInAnyEra

instance IsCardanoEra era => ToJSON (UTxO era) where
  toJSON (UTxO m) = toJSON m
  toEncoding (UTxO m) = toEncoding m

instance (IsShelleyBasedEra era, FromJSON (TxOut CtxUTxO era))
  => FromJSON (UTxO era) where
    parseJSON = withObject "UTxO" $ \hm -> do
      let l = HMS.toList $ KeyMap.toHashMapText hm
      res <- mapM toTxIn l
      pure . UTxO $ Map.fromList res
     where
      toTxIn :: (Text, Aeson.Value) -> Parser (TxIn, TxOut CtxUTxO era)
      toTxIn (txinText, txOutVal) = do
        (,) <$> parseJSON (Aeson.String txinText)
            <*> parseJSON txOutVal

newtype SerialisedDebugLedgerState era
  = SerialisedDebugLedgerState (Serialised (Shelley.NewEpochState (ShelleyLedgerEra era)))

decodeDebugLedgerState :: forall era. ()
  => FromCBOR (DebugLedgerState era)
  => SerialisedDebugLedgerState era
  -> Either LBS.ByteString (DebugLedgerState era)
decodeDebugLedgerState (SerialisedDebugLedgerState (Serialised ls)) =
  first (const ls) (decodeFull ls)

data DebugLedgerState era where
  DebugLedgerState :: ShelleyLedgerEra era ~ ledgerera => Shelley.NewEpochState ledgerera -> DebugLedgerState era

instance
    ( Typeable era
    , Ledger.Era (ShelleyLedgerEra era)
    , FromCBOR (Core.PParams (ShelleyLedgerEra era))
    , FromCBOR (Shelley.StashedAVVMAddresses (ShelleyLedgerEra era))
    , FromCBOR (Core.Value (ShelleyLedgerEra era))
    , FromCBOR (Ledger.State (Core.EraRule "PPUP" (ShelleyLedgerEra era)))
    , Share (Core.TxOut (ShelleyLedgerEra era)) ~ Interns (Shelley.Credential 'Shelley.Staking (Ledger.Crypto (ShelleyLedgerEra era)))
    , FromSharedCBOR (Core.TxOut (ShelleyLedgerEra era))
    , HashAnnotated (Core.TxBody (ShelleyLedgerEra era)) Core.EraIndependentTxBody (Ledger.Crypto (ShelleyLedgerEra era))
    ) => FromCBOR (DebugLedgerState era) where
  fromCBOR = DebugLedgerState <$> (fromCBOR :: Decoder s (Shelley.NewEpochState (ShelleyLedgerEra era)))

-- TODO: Shelley based era class!
instance ( IsShelleyBasedEra era
         , ShelleyLedgerEra era ~ ledgerera
         , Consensus.ShelleyBasedEra ledgerera
         , ToJSON (Core.PParams ledgerera)
         , ToJSON (Core.PParamsUpdate ledgerera)
         , ToJSON (Core.TxOut ledgerera)
         , Share (Core.TxOut (ShelleyLedgerEra era)) ~ Interns (Shelley.Credential 'Shelley.Staking (Ledger.Crypto (ShelleyLedgerEra era)))
         ) => ToJSON (DebugLedgerState era) where
  toJSON = object . toDebugLedgerStatePair
  toEncoding = Aeson.pairs . mconcat . toDebugLedgerStatePair

toDebugLedgerStatePair ::
  ( ShelleyLedgerEra era ~ ledgerera
  , Consensus.ShelleyBasedEra ledgerera
  , ToJSON (Core.PParams ledgerera)
  , ToJSON (Core.PParamsUpdate ledgerera)
  , ToJSON (Core.TxOut ledgerera)
  , Aeson.KeyValue a
  ) => DebugLedgerState era -> [a]
toDebugLedgerStatePair (DebugLedgerState newEpochS) =
    let !nesEL = Shelley.nesEL newEpochS
        !nesBprev = Shelley.nesBprev newEpochS
        !nesBcur = Shelley.nesBcur newEpochS
        !nesEs = Shelley.nesEs newEpochS
        !nesRu = Shelley.nesRu newEpochS
        !nesPd = Shelley.nesPd newEpochS
    in  [ "lastEpoch" .= nesEL
        , "blocksBefore" .= nesBprev
        , "blocksCurrent" .= nesBcur
        , "stateBefore" .= nesEs
        , "possibleRewardUpdate" .= nesRu
        , "stakeDistrib" .= nesPd
        ]

newtype ProtocolState era
  = ProtocolState (Serialised (Consensus.ChainDepState (ConsensusProtocol era)))

-- ChainDepState can use Praos or TPraos crypto
decodeProtocolState
  :: FromCBOR (Consensus.ChainDepState (ConsensusProtocol era))
  => ProtocolState era
  -> Either (LBS.ByteString, DecoderError) (Consensus.ChainDepState (ConsensusProtocol era))
decodeProtocolState (ProtocolState (Serialised pbs)) = first (pbs,) $ decodeFull pbs

newtype SerialisedCurrentEpochState era
  = SerialisedCurrentEpochState (Serialised (Shelley.EpochState (ShelleyLedgerEra era)))

newtype CurrentEpochState era = CurrentEpochState (Shelley.EpochState (ShelleyLedgerEra era))

decodeCurrentEpochState
  :: forall era. ( Ledger.Era (ShelleyLedgerEra era)
                 , HashAnnotated (Core.TxBody (ShelleyLedgerEra era)) Core.EraIndependentTxBody (Ledger.Crypto (ShelleyLedgerEra era))
                 )
  => FromSharedCBOR (Core.TxOut (ShelleyLedgerEra era))
  => Share (Core.TxOut (ShelleyLedgerEra era)) ~ Interns (Shelley.Credential 'Shelley.Staking (Ledger.Crypto (ShelleyLedgerEra era)))
  => FromCBOR (Core.PParams (ShelleyLedgerEra era))
  => FromCBOR (Core.Value (ShelleyLedgerEra era))
  => FromCBOR (Ledger.State (Core.EraRule "PPUP" (ShelleyLedgerEra era)))
  => SerialisedCurrentEpochState era
  -> Either DecoderError (CurrentEpochState era)
decodeCurrentEpochState (SerialisedCurrentEpochState (Serialised ls)) = CurrentEpochState <$> decodeFull ls

newtype SerialisedPoolState era
  = SerialisedPoolState (Serialised (Shelley.PState (Ledger.Crypto (ShelleyLedgerEra era))))

newtype PoolState era = PoolState (Shelley.PState (Ledger.Crypto (ShelleyLedgerEra era)))

decodePoolState
  :: forall era. ()
  => FromCBOR (Shelley.PState (Ledger.Crypto (ShelleyLedgerEra era)))
  => SerialisedPoolState era
  -> Either DecoderError (PoolState era)
decodePoolState (SerialisedPoolState (Serialised ls)) = PoolState <$> decodeFull ls

toShelleyAddrSet :: CardanoEra era
                 -> Set AddressAny
                 -> Set (Shelley.Addr Consensus.StandardCrypto)
toShelleyAddrSet era =
    Set.fromList
  . map toShelleyAddr
    -- Ignore any addresses that are not appropriate for the era,
    -- e.g. Shelley addresses in the Byron era, as these would not
    -- appear in the UTxO anyway.
  . mapMaybe (anyAddressInEra era)
  . Set.toList


toLedgerUTxO :: ShelleyLedgerEra era ~ ledgerera
             => Ledger.Crypto ledgerera ~ StandardCrypto
             => ShelleyBasedEra era
             -> UTxO era
             -> Shelley.UTxO ledgerera
toLedgerUTxO era (UTxO utxo) =
    Shelley.UTxO
  . Map.fromList
  . map (bimap toShelleyTxIn (toShelleyTxOut era))
  . Map.toList
  $ utxo

fromLedgerUTxO :: ShelleyLedgerEra era ~ ledgerera
               => Ledger.Crypto ledgerera ~ StandardCrypto
               => ShelleyBasedEra era
               -> Shelley.UTxO ledgerera
               -> UTxO era
fromLedgerUTxO era (Shelley.UTxO utxo) =
    UTxO
  . Map.fromList
  . map (bimap fromShelleyTxIn (fromShelleyTxOut era))
  . Map.toList
  $ utxo

fromShelleyPoolDistr :: Shelley.PoolDistr StandardCrypto
                     -> Map (Hash StakePoolKey) Rational
fromShelleyPoolDistr =
    --TODO: write an appropriate property to show it is safe to use
    -- Map.fromListAsc or to use Map.mapKeysMonotonic
    Map.fromList
  . map (bimap StakePoolKeyHash Shelley.individualPoolStake)
  . Map.toList
  . Shelley.unPoolDistr

fromShelleyDelegations :: Map (Shelley.Credential Shelley.Staking StandardCrypto)
                              (Shelley.KeyHash Shelley.StakePool StandardCrypto)
                       -> Map StakeCredential PoolId
fromShelleyDelegations =
    --TODO: write an appropriate property to show it is safe to use
    -- Map.fromListAsc or to use Map.mapKeysMonotonic
    -- In this case it may not be: the Ord instances for Shelley.Credential
    -- do not match the one for StakeCredential
    Map.fromList
  . map (bimap fromShelleyStakeCredential StakePoolKeyHash)
  . Map.toList

fromShelleyRewardAccounts :: Shelley.RewardAccounts Consensus.StandardCrypto
                          -> Map StakeCredential Lovelace
fromShelleyRewardAccounts =
    --TODO: write an appropriate property to show it is safe to use
    -- Map.fromListAsc or to use Map.mapKeysMonotonic
    Map.fromList
  . map (bimap fromShelleyStakeCredential fromShelleyLovelace)
  . Map.toList


-- ----------------------------------------------------------------------------
-- Conversions of queries into the consensus types.
--

toConsensusQuery :: forall mode block result.
                    ConsensusBlockForMode mode ~ block
                 => QueryInMode mode result
                 -> Some (Consensus.Query block)
toConsensusQuery (QueryCurrentEra CardanoModeIsMultiEra) =
    Some $ Consensus.BlockQuery $
      Consensus.QueryHardFork
        Consensus.GetCurrentEra

toConsensusQuery (QueryInEra ByronEraInByronMode QueryByronUpdateState) =
    Some $ Consensus.BlockQuery $
      Consensus.DegenQuery
        Consensus.GetUpdateInterfaceState

toConsensusQuery (QueryEraHistory CardanoModeIsMultiEra) =
    Some $ Consensus.BlockQuery $
      Consensus.QueryHardFork
        Consensus.GetInterpreter

toConsensusQuery QuerySystemStart = Some Consensus.GetSystemStart

toConsensusQuery QueryChainBlockNo = Some Consensus.GetChainBlockNo

toConsensusQuery (QueryChainPoint _) = Some Consensus.GetChainPoint

toConsensusQuery (QueryInEra ByronEraInCardanoMode QueryByronUpdateState) =
    Some $ Consensus.BlockQuery $
      Consensus.QueryIfCurrentByron
        Consensus.GetUpdateInterfaceState

toConsensusQuery (QueryInEra erainmode (QueryInShelleyBasedEra era q)) =
    case erainmode of
      ByronEraInByronMode     -> case era of {}
      ShelleyEraInShelleyMode -> toConsensusQueryShelleyBased erainmode q
      ByronEraInCardanoMode   -> case era of {}
      ShelleyEraInCardanoMode -> toConsensusQueryShelleyBased erainmode q
      AllegraEraInCardanoMode -> toConsensusQueryShelleyBased erainmode q
      MaryEraInCardanoMode    -> toConsensusQueryShelleyBased erainmode q
      AlonzoEraInCardanoMode  -> toConsensusQueryShelleyBased erainmode q
      BabbageEraInCardanoMode -> toConsensusQueryShelleyBased erainmode q


toConsensusQueryShelleyBased
  :: forall era ledgerera mode protocol block xs result.
     ConsensusBlockForEra era ~ Consensus.ShelleyBlock protocol ledgerera
  => Ledger.Crypto ledgerera ~ Consensus.StandardCrypto
  => ConsensusBlockForMode mode ~ block
  => block ~ Consensus.HardForkBlock xs
  => EraInMode era mode
  -> QueryInShelleyBasedEra era result
  -> Some (Consensus.Query block)
toConsensusQueryShelleyBased erainmode QueryEpoch =
    Some (consensusQueryInEraInMode erainmode Consensus.GetEpochNo)

toConsensusQueryShelleyBased erainmode QueryGenesisParameters =
    Some (consensusQueryInEraInMode erainmode Consensus.GetGenesisConfig)

toConsensusQueryShelleyBased erainmode QueryProtocolParameters =
    Some (consensusQueryInEraInMode erainmode Consensus.GetCurrentPParams)

toConsensusQueryShelleyBased erainmode QueryProtocolParametersUpdate =
    Some (consensusQueryInEraInMode erainmode Consensus.GetProposedPParamsUpdates)

toConsensusQueryShelleyBased erainmode QueryStakeDistribution =
    Some (consensusQueryInEraInMode erainmode Consensus.GetStakeDistribution)

toConsensusQueryShelleyBased erainmode (QueryUTxO QueryUTxOWhole) =
    Some (consensusQueryInEraInMode erainmode Consensus.GetUTxOWhole)

toConsensusQueryShelleyBased erainmode (QueryUTxO (QueryUTxOByAddress addrs)) =
    Some (consensusQueryInEraInMode erainmode (Consensus.GetUTxOByAddress addrs'))
  where
    addrs' :: Set (Shelley.Addr Consensus.StandardCrypto)
    addrs' = toShelleyAddrSet (eraInModeToEra erainmode) addrs

toConsensusQueryShelleyBased erainmode (QueryUTxO (QueryUTxOByTxIn txins)) =
    Some (consensusQueryInEraInMode erainmode (Consensus.GetUTxOByTxIn txins'))
  where
    txins' :: Set (Shelley.TxIn Consensus.StandardCrypto)
    txins' = Set.map toShelleyTxIn txins

toConsensusQueryShelleyBased erainmode (QueryStakeAddresses creds _nId) =
    Some (consensusQueryInEraInMode erainmode
            (Consensus.GetFilteredDelegationsAndRewardAccounts creds'))
  where
    creds' :: Set (Shelley.Credential Shelley.Staking StandardCrypto)
    creds' = Set.map toShelleyStakeCredential creds

toConsensusQueryShelleyBased erainmode QueryStakePools =
    Some (consensusQueryInEraInMode erainmode Consensus.GetStakePools)

toConsensusQueryShelleyBased erainmode (QueryStakePoolParameters poolids) =
    Some (consensusQueryInEraInMode erainmode (Consensus.GetStakePoolParams poolids'))
  where
    poolids' :: Set (Shelley.KeyHash Shelley.StakePool Consensus.StandardCrypto)
    poolids' = Set.map (\(StakePoolKeyHash kh) -> kh) poolids

toConsensusQueryShelleyBased erainmode QueryDebugLedgerState =
    Some (consensusQueryInEraInMode erainmode (Consensus.GetCBOR Consensus.DebugNewEpochState))

toConsensusQueryShelleyBased erainmode QueryProtocolState =
    Some (consensusQueryInEraInMode erainmode (Consensus.GetCBOR Consensus.DebugChainDepState))

toConsensusQueryShelleyBased erainmode QueryCurrentEpochState =
    Some (consensusQueryInEraInMode erainmode (Consensus.GetCBOR Consensus.DebugEpochState))

toConsensusQueryShelleyBased erainmode (QueryPoolState poolIds) =
    Some (consensusQueryInEraInMode erainmode (Consensus.GetCBOR (Consensus.GetPoolState (getPoolIds <$> poolIds))))
  where
    getPoolIds :: Set PoolId -> Set (Shelley.KeyHash Shelley.StakePool Consensus.StandardCrypto)
    getPoolIds = Set.map (\(StakePoolKeyHash kh) -> kh)

consensusQueryInEraInMode
  :: forall era mode erablock modeblock result result' xs.
     ConsensusBlockForEra era   ~ erablock
  => ConsensusBlockForMode mode ~ modeblock
  => modeblock ~ Consensus.HardForkBlock xs
  => Consensus.HardForkQueryResult xs result ~ result'
  => EraInMode era mode
  -> Consensus.BlockQuery erablock  result
  -> Consensus.Query modeblock result'
consensusQueryInEraInMode erainmode =
    Consensus.BlockQuery
  . case erainmode of
      ByronEraInByronMode     -> Consensus.DegenQuery
      ShelleyEraInShelleyMode -> Consensus.DegenQuery
      ByronEraInCardanoMode   -> Consensus.QueryIfCurrentByron
      ShelleyEraInCardanoMode -> Consensus.QueryIfCurrentShelley
      AllegraEraInCardanoMode -> Consensus.QueryIfCurrentAllegra
      MaryEraInCardanoMode    -> Consensus.QueryIfCurrentMary
      AlonzoEraInCardanoMode  -> Consensus.QueryIfCurrentAlonzo
      BabbageEraInCardanoMode -> Consensus.QueryIfCurrentBabbage

-- ----------------------------------------------------------------------------
-- Conversions of query results from the consensus types.
--

fromConsensusQueryResult :: forall mode block result result'. ConsensusBlockForMode mode ~ block
                         => QueryInMode mode result
                         -> Consensus.Query block result'
                         -> result'
                         -> result
fromConsensusQueryResult (QueryEraHistory CardanoModeIsMultiEra) q' r' =
    case q' of
      Consensus.BlockQuery (Consensus.QueryHardFork Consensus.GetInterpreter)
        -> EraHistory CardanoMode r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult QuerySystemStart q' r' =
    case q' of
      Consensus.GetSystemStart
        -> r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult QueryChainBlockNo q' r' =
    case q' of
      Consensus.GetChainBlockNo
        -> r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryChainPoint mode) q' r' =
    case q' of
      Consensus.GetChainPoint
        -> fromConsensusPointInMode mode r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryCurrentEra CardanoModeIsMultiEra) q' r' =
    case q' of
      Consensus.BlockQuery (Consensus.QueryHardFork Consensus.GetCurrentEra)
        -> anyEraInModeToAnyEra (fromConsensusEraIndex CardanoMode r')
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra ByronEraInByronMode
                                     QueryByronUpdateState) q' r' =
    case (q', r') of
      (Consensus.BlockQuery (Consensus.DegenQuery Consensus.GetUpdateInterfaceState),
       Consensus.DegenQueryResult r'')
        -> Right (ByronUpdateState r'')
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra ByronEraInCardanoMode
                                     QueryByronUpdateState) q' r' =
    case q' of
      Consensus.BlockQuery
        (Consensus.QueryIfCurrentByron Consensus.GetUpdateInterfaceState)
        -> bimap fromConsensusEraMismatch ByronUpdateState r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra ByronEraInByronMode
                                     (QueryInShelleyBasedEra era _)) _ _ =
    case era of {}

fromConsensusQueryResult (QueryInEra ShelleyEraInShelleyMode
                                     (QueryInShelleyBasedEra _era q)) q' r' =
    case (q', r') of
      (Consensus.BlockQuery (Consensus.DegenQuery q''),
       Consensus.DegenQueryResult r'')
        -> Right (fromConsensusQueryResultShelleyBased
                    ShelleyBasedEraShelley q q'' r'')
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra ByronEraInCardanoMode
                                     (QueryInShelleyBasedEra era _)) _ _ =
    case era of {}

fromConsensusQueryResult (QueryInEra ShelleyEraInCardanoMode
                                     (QueryInShelleyBasedEra _era q)) q' r' =
    case q' of
      Consensus.BlockQuery (Consensus.QueryIfCurrentShelley q'')
        -> bimap fromConsensusEraMismatch
                 (fromConsensusQueryResultShelleyBased
                    ShelleyBasedEraShelley q q'')
                 r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra AllegraEraInCardanoMode
                                     (QueryInShelleyBasedEra _era q)) q' r' =
    case q' of
      Consensus.BlockQuery (Consensus.QueryIfCurrentAllegra q'')
        -> bimap fromConsensusEraMismatch
                 (fromConsensusQueryResultShelleyBased
                    ShelleyBasedEraAllegra q q'')
                 r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra MaryEraInCardanoMode
                                     (QueryInShelleyBasedEra _era q)) q' r' =
    case q' of
      Consensus.BlockQuery (Consensus.QueryIfCurrentMary q'')
        -> bimap fromConsensusEraMismatch
                 (fromConsensusQueryResultShelleyBased
                    ShelleyBasedEraMary q q'')
                 r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra AlonzoEraInCardanoMode
                                     (QueryInShelleyBasedEra _era q)) q' r' =
    case q' of
      Consensus.BlockQuery (Consensus.QueryIfCurrentAlonzo q'')
        -> bimap fromConsensusEraMismatch
                 (fromConsensusQueryResultShelleyBased
                    ShelleyBasedEraAlonzo q q'')
                 r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra BabbageEraInCardanoMode
                                     (QueryInShelleyBasedEra _era q)) q' r' =
    case q' of
      Consensus.BlockQuery (Consensus.QueryIfCurrentBabbage q'')
        -> bimap fromConsensusEraMismatch
                 (fromConsensusQueryResultShelleyBased
                    ShelleyBasedEraBabbage q q'')
                 r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased
  :: forall era ledgerera protocol result result'.
     ShelleyLedgerEra era ~ ledgerera
  => Ledger.Crypto ledgerera ~ Consensus.StandardCrypto
  => ConsensusProtocol era ~ protocol
  => ShelleyBasedEra era
  -> QueryInShelleyBasedEra era result
  -> Consensus.BlockQuery (Consensus.ShelleyBlock protocol ledgerera) result'
  -> result'
  -> result
fromConsensusQueryResultShelleyBased _ QueryEpoch q' epoch =
    case q' of
      Consensus.GetEpochNo -> epoch
      _                    -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryGenesisParameters q' r' =
    case q' of
      Consensus.GetGenesisConfig -> fromShelleyGenesis
                                      (Consensus.getCompactGenesis r')
      _                          -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased era QueryProtocolParameters q' r' =
    case q' of
      Consensus.GetCurrentPParams -> fromLedgerPParams era r'
      _                           -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased era QueryProtocolParametersUpdate q' r' =
    case q' of
      Consensus.GetProposedPParamsUpdates -> fromLedgerProposedPPUpdates era r'
      _                                   -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryStakeDistribution q' r' =
    case q' of
      Consensus.GetStakeDistribution -> fromShelleyPoolDistr r'
      _                              -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased era (QueryUTxO QueryUTxOWhole) q' utxo' =
    case q' of
      Consensus.GetUTxOWhole -> fromLedgerUTxO era utxo'
      _                      -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased era (QueryUTxO QueryUTxOByAddress{}) q' utxo' =
    case q' of
      Consensus.GetUTxOByAddress{} -> fromLedgerUTxO era utxo'
      _                            -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased era (QueryUTxO QueryUTxOByTxIn{}) q' utxo' =
    case q' of
      Consensus.GetUTxOByTxIn{} -> fromLedgerUTxO era utxo'
      _                         -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ (QueryStakeAddresses _ nId) q' r' =
    case q' of
      Consensus.GetFilteredDelegationsAndRewardAccounts{}
        -> let (delegs, rwaccs) = r'
           in ( Map.mapKeys (makeStakeAddress nId) $ fromShelleyRewardAccounts rwaccs
              , Map.mapKeys (makeStakeAddress nId) $ fromShelleyDelegations delegs
              )
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryStakePools q' poolids' =
    case q' of
      Consensus.GetStakePools -> Set.map StakePoolKeyHash poolids'
      _                       -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryStakePoolParameters{} q' poolparams' =
    case q' of
      Consensus.GetStakePoolParams{} -> Map.map fromShelleyPoolParams
                                      . Map.mapKeysMonotonic StakePoolKeyHash
                                      $ poolparams'
      _                              -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryDebugLedgerState{} q' r' =
    case q' of
      Consensus.GetCBOR Consensus.DebugNewEpochState -> SerialisedDebugLedgerState r'
      _                                              -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryProtocolState q' r' =
    case q' of
      Consensus.GetCBOR Consensus.DebugChainDepState -> ProtocolState r'
      _                                              -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryCurrentEpochState q' r' =
  case q' of
    Consensus.GetCBOR Consensus.DebugEpochState -> SerialisedCurrentEpochState r'
    _                                           -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryPoolState{} q' r' =
  case q' of
    Consensus.GetCBOR Consensus.GetPoolState {} -> SerialisedPoolState r'
    _                                           -> fromConsensusQueryResultMismatch

-- | This should /only/ happen if we messed up the mapping in 'toConsensusQuery'
-- and 'fromConsensusQueryResult' so they are inconsistent with each other.
--
-- If we do encounter this error it means that 'toConsensusQuery' maps a
-- API query constructor to a certain consensus query constructor but that
-- 'fromConsensusQueryResult' apparently expects a different pairing.
--
-- For example, imagine if 'toConsensusQuery would (incorrectly) map
-- 'QueryChainPoint' to 'Consensus.GetEpochNo' but 'fromConsensusQueryResult'
-- (correctly) expected to find 'Consensus.GetLedgerTip'. This mismatch would
-- trigger this error.
--
-- Such mismatches should be preventable with an appropriate property test.
--
fromConsensusQueryResultMismatch :: a
fromConsensusQueryResultMismatch =
    error "fromConsensusQueryResult: internal query mismatch"


fromConsensusEraMismatch :: SListI xs
                         => Consensus.MismatchEraInfo xs -> EraMismatch
fromConsensusEraMismatch = Consensus.mkEraMismatch
