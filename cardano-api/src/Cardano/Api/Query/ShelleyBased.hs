{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Api.Query.ShelleyBased (
    -- * Query types
    QueryInEra(..),
    QueryShelleyBasedEra(..),
    QueryInShelleyBasedEra(..),
    ShelleyBasedQueryError(..),

    QueryUTxOFilter(..),

    -- * Wrapper types used in queries
    SerialisedDebugLedgerState(..),
    AnyProtocolState(..),
    ProtocolState(..),
    decodeProtocolState,

    SomeSerialisedDebugLedgerState(..),
    decodeSomeSerialisedDebugLedgerState,
    DebugLedgerState(..),
    decodeDebugLedgerState,

    SerialisedCurrentEpochState(..),
    CurrentEpochState(..),
    decodeCurrentEpochState,

    SerialisedPoolState(..),
    PoolState(..),
    decodePoolState,

    SerialisedPoolDistribution(..),
    PoolDistribution(..),
    decodePoolDistribution,

    SerialisedStakeSnapshots(..),
    SomeStakeSnapshot(..),
    StakeSnapshot(..),
    decodeStakeSnapshot,
    decodeSomeStakeSnapshot,

    AnyUTxO(..),
    UTxO(..),

    -- * Internal conversion functions
    fromConsensusQueryResultSbe,
    toConsensusQuerySbe,
    toLedgerUTxO,
    fromLedgerUTxO,
  ) where

import           Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.Aeson as Aeson
import           Data.Bifunctor (bimap, first)
import qualified Data.ByteString.Lazy as LBS
import           Data.Either.Combinators (rightToMaybe)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.SOP.Strict (SListI)
import           Data.Typeable

import           Ouroboros.Network.Protocol.LocalStateQuery.Client (Some (..))

import qualified Ouroboros.Consensus.HardFork.Combinator as Consensus
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import qualified Ouroboros.Consensus.HardFork.Combinator.AcrossEras as Consensus
import qualified Ouroboros.Consensus.HardFork.Combinator.Degenerate as Consensus

import qualified Ouroboros.Consensus.Byron.Ledger as Consensus
import           Ouroboros.Consensus.Cardano.Block (StandardCrypto)
import qualified Ouroboros.Consensus.Cardano.Block as Consensus
import qualified Ouroboros.Consensus.Ledger.Query as Consensus
import qualified Ouroboros.Consensus.Protocol.Abstract as Consensus
import qualified Ouroboros.Consensus.Shelley.Ledger as Consensus
import           Ouroboros.Network.Block (Serialised (..))
import           Ouroboros.Network.NodeToClient.Version (NodeToClientVersion (..))

import qualified Cardano.Chain.Update.Validation.Interface as Byron.Update

import           Cardano.Ledger.Binary
import qualified Cardano.Ledger.Binary.Plain as Plain
import           Cardano.Ledger.Crypto (Crypto)
import qualified Cardano.Ledger.Shelley.API as Shelley
import qualified Cardano.Ledger.Shelley.Core as Core
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley

import           Cardano.Api.Address
import           Cardano.Api.Block
import           Cardano.Api.Certificate
import           Cardano.Api.Eras
import           Cardano.Api.GenesisParameters
import           Cardano.Api.IPC.Version
import           Cardano.Api.Keys.Shelley
import           Cardano.Api.Modes
import           Cardano.Api.NetworkId
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.Query.Common
import           Cardano.Api.Query.Error
import           Cardano.Api.TxBody
import           Cardano.Api.Value

data QueryShelleyBasedEra mode result where
  QueryShelleyBasedEra
    :: EraInMode era mode
    -> QueryInEra era result
    -> QueryShelleyBasedEra mode (Either EraMismatch result)

-- This returns an Either EraMismatch
data QueryInEra era result where
     QueryByronUpdateState :: QueryInEra ByronEra ByronUpdateState

     QueryInShelleyBasedEra :: ShelleyBasedEra era
                            -> QueryInShelleyBasedEra era result
                            -> QueryInEra era result


instance NodeToClientVersionOf (QueryInEra era result) where
  nodeToClientVersionOf QueryByronUpdateState = NodeToClientV_9
  nodeToClientVersionOf (QueryInShelleyBasedEra _ q) = nodeToClientVersionOf q

deriving instance Show (QueryInEra era result)


instance NodeToClientVersionOf (QueryShelleyBasedEra mode result) where
  nodeToClientVersionOf (QueryShelleyBasedEra _ q) = nodeToClientVersionOf q

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

  QueryPoolDistribution
    :: Maybe (Set PoolId)
    -> QueryInShelleyBasedEra era (SerialisedPoolDistribution era)

  QueryStakeSnapshot
    :: Maybe (Set PoolId)
    -> QueryInShelleyBasedEra era (SerialisedStakeSnapshots era)

instance NodeToClientVersionOf (QueryInShelleyBasedEra era result) where
  nodeToClientVersionOf QueryEpoch = NodeToClientV_9
  nodeToClientVersionOf QueryGenesisParameters = NodeToClientV_9
  nodeToClientVersionOf QueryProtocolParameters = NodeToClientV_9
  nodeToClientVersionOf QueryProtocolParametersUpdate = NodeToClientV_9
  nodeToClientVersionOf QueryStakeDistribution = NodeToClientV_9
  nodeToClientVersionOf (QueryUTxO f) = nodeToClientVersionOf f
  nodeToClientVersionOf (QueryStakeAddresses _ _) = NodeToClientV_9
  nodeToClientVersionOf QueryStakePools = NodeToClientV_9
  nodeToClientVersionOf (QueryStakePoolParameters _) = NodeToClientV_9
  nodeToClientVersionOf QueryDebugLedgerState = NodeToClientV_9
  nodeToClientVersionOf QueryProtocolState = NodeToClientV_9
  nodeToClientVersionOf QueryCurrentEpochState = NodeToClientV_9
  nodeToClientVersionOf (QueryPoolState _) = NodeToClientV_14
  nodeToClientVersionOf (QueryPoolDistribution _) = NodeToClientV_14
  nodeToClientVersionOf (QueryStakeSnapshot _) = NodeToClientV_14

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

instance NodeToClientVersionOf QueryUTxOFilter where
  nodeToClientVersionOf QueryUTxOWhole = NodeToClientV_9
  nodeToClientVersionOf (QueryUTxOByAddress _) = NodeToClientV_9
  nodeToClientVersionOf (QueryUTxOByTxIn _) = NodeToClientV_9

newtype ByronUpdateState = ByronUpdateState Byron.Update.State
  deriving Show

data AnyUTxO where
  AnyUTxO :: IsCardanoEra era => CardanoEra era -> UTxO era -> AnyUTxO

data SomeSerialisedDebugLedgerState where
  SomeSerialisedDebugLedgerState
    :: ShelleyBasedEra era
    -> Either LBS.ByteString (DebugLedgerState era)
    -> SomeSerialisedDebugLedgerState

decodeSomeSerialisedDebugLedgerState
  :: ShelleyBasedEra era -> SerialisedDebugLedgerState era -> SomeSerialisedDebugLedgerState
decodeSomeSerialisedDebugLedgerState sbe sDebug =
  obtainLedgerEraClassConstraints sbe $ SomeSerialisedDebugLedgerState sbe $ decodeDebugLedgerState sDebug

obtainLedgerEraClassConstraints
  :: forall a era ledgerera. ShelleyLedgerEra era ~ ledgerera
  => ShelleyBasedEra era
  -> (( ToJSON (DebugLedgerState era)
      , FromCBOR (DebugLedgerState era)
      , Core.EraCrypto ledgerera ~ StandardCrypto
      , Core.Era (ShelleyLedgerEra era)
      ) => a) -> a
obtainLedgerEraClassConstraints ShelleyBasedEraShelley f = f
obtainLedgerEraClassConstraints ShelleyBasedEraAllegra f = f
obtainLedgerEraClassConstraints ShelleyBasedEraMary    f = f
obtainLedgerEraClassConstraints ShelleyBasedEraAlonzo  f = f
obtainLedgerEraClassConstraints ShelleyBasedEraBabbage f = f
obtainLedgerEraClassConstraints ShelleyBasedEraConway  f = f

newtype SerialisedDebugLedgerState era
  = SerialisedDebugLedgerState (Serialised (Shelley.NewEpochState (ShelleyLedgerEra era)))

decodeDebugLedgerState :: forall era. ()
  => FromCBOR (DebugLedgerState era)
  => SerialisedDebugLedgerState era
  -> Either LBS.ByteString (DebugLedgerState era)
decodeDebugLedgerState (SerialisedDebugLedgerState (Serialised ls)) =
  first (const ls) (Plain.decodeFull ls)

data DebugLedgerState era where
  DebugLedgerState :: ShelleyLedgerEra era ~ ledgerera => Shelley.NewEpochState ledgerera -> DebugLedgerState era

instance
    ( Typeable era
    , Core.EraTxOut (ShelleyLedgerEra era)
    , Core.EraGovernance (ShelleyLedgerEra era)
    , DecCBOR (Shelley.StashedAVVMAddresses (ShelleyLedgerEra era))
    ) => FromCBOR (DebugLedgerState era) where
  fromCBOR = DebugLedgerState <$>
    (fromCBOR :: Plain.Decoder s (Shelley.NewEpochState (ShelleyLedgerEra era)))

-- TODO: Shelley based era class!
instance ( IsShelleyBasedEra era
         , ShelleyLedgerEra era ~ ledgerera
         , Consensus.ShelleyBasedEra ledgerera
         ) => ToJSON (DebugLedgerState era) where
  toJSON = object . toDebugLedgerStatePair
  toEncoding = Aeson.pairs . mconcat . toDebugLedgerStatePair

toDebugLedgerStatePair ::
  ( ShelleyLedgerEra era ~ ledgerera
  , Consensus.ShelleyBasedEra ledgerera
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

data AnyProtocolState where
  AnyProtocolState
    :: ShelleyBasedEra era
    -> Either (LBS.ByteString, DecoderError) (Consensus.ChainDepState (ConsensusProtocol era))
    -> AnyProtocolState

-- ChainDepState can use Praos or TPraos crypto
decodeProtocolState
  :: FromCBOR (Consensus.ChainDepState (ConsensusProtocol era))
  => ProtocolState era
  -> Either (LBS.ByteString, DecoderError) (Consensus.ChainDepState (ConsensusProtocol era))
decodeProtocolState (ProtocolState (Serialised pbs)) = first (pbs,) $ Plain.decodeFull pbs

decodeCurrentEpochState
  :: ShelleyBasedEra era
  -> SerialisedCurrentEpochState era
  -> Either DecoderError (CurrentEpochState era)
decodeCurrentEpochState sbe (SerialisedCurrentEpochState (Serialised ls)) =
  CurrentEpochState <$>
    case sbe of
      ShelleyBasedEraShelley -> Plain.decodeFull ls
      ShelleyBasedEraAllegra -> Plain.decodeFull ls
      ShelleyBasedEraMary    -> Plain.decodeFull ls
      ShelleyBasedEraAlonzo  -> Plain.decodeFull ls
      ShelleyBasedEraBabbage -> Plain.decodeFull ls
      ShelleyBasedEraConway  -> Plain.decodeFull ls


newtype SerialisedPoolState era
  = SerialisedPoolState (Serialised (Shelley.PState (Core.EraCrypto (ShelleyLedgerEra era))))

newtype PoolState era = PoolState (Shelley.PState (Core.EraCrypto (ShelleyLedgerEra era)))

decodePoolState
  :: forall era. ()
  => Core.Era (ShelleyLedgerEra era)
  => DecCBOR (Shelley.PState (Core.EraCrypto (ShelleyLedgerEra era)))
  => SerialisedPoolState era
  -> Either DecoderError (PoolState era)
decodePoolState (SerialisedPoolState (Serialised ls)) =
  PoolState <$> decodeFull (Core.eraProtVerLow @(ShelleyLedgerEra era)) ls

decodePoolDistribution
  :: forall era. (Crypto (Core.EraCrypto (ShelleyLedgerEra era)))
  => ShelleyBasedEra era
  -> SerialisedPoolDistribution era
  -> Either DecoderError (PoolDistribution era)
decodePoolDistribution sbe (SerialisedPoolDistribution (Serialised ls)) =
  PoolDistribution <$> decodeFull (eraProtVerLow sbe) ls

newtype SerialisedStakeSnapshots era
  = SerialisedStakeSnapshots (Serialised (Consensus.StakeSnapshots (Core.EraCrypto (ShelleyLedgerEra era))))

newtype StakeSnapshot era = StakeSnapshot (Consensus.StakeSnapshots (Core.EraCrypto (ShelleyLedgerEra era)))

data SomeStakeSnapshot where
  SomeStakeSnapshot :: ShelleyBasedEra era -> StakeSnapshot era -> SomeStakeSnapshot


decodeStakeSnapshot
  :: forall era. ()
  => FromCBOR (Consensus.StakeSnapshots (Core.EraCrypto (ShelleyLedgerEra era)))
  => SerialisedStakeSnapshots era
  -> Either DecoderError (StakeSnapshot era)
decodeStakeSnapshot (SerialisedStakeSnapshots (Serialised ls)) = StakeSnapshot <$> Plain.decodeFull ls

decodeSomeStakeSnapshot
  :: ShelleyBasedEra era
  -> SerialisedStakeSnapshots era
  -> Either DecoderError SomeStakeSnapshot
decodeSomeStakeSnapshot sbe e =
  SomeStakeSnapshot sbe <$> obtainLedgerEraClassConstraints sbe (decodeStakeSnapshot e)

toShelleyAddrSet :: CardanoEra era
                 -> Set AddressAny
                 -> Set (Shelley.Addr Consensus.StandardCrypto)
toShelleyAddrSet era =
    Set.fromList
  . map toShelleyAddr
    -- Ignore any addresses that are not appropriate for the era,
    -- e.g. Shelley addresses in the Byron era, as these would not
    -- appear in the UTxO anyway.
  . mapMaybe (rightToMaybe . anyAddressInEra era)
  . Set.toList


toLedgerUTxO :: ShelleyLedgerEra era ~ ledgerera
             => Core.EraCrypto ledgerera ~ StandardCrypto
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
               => Core.EraCrypto ledgerera ~ StandardCrypto
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

toConsensusQuerySbe
  :: forall mode block result.
     ConsensusBlockForMode mode ~ block
  => QueryShelleyBasedEra mode result
  -> Some (Consensus.Query block)
toConsensusQuerySbe (QueryShelleyBasedEra ByronEraInByronMode QueryByronUpdateState) =
    Some $ Consensus.BlockQuery $
      Consensus.DegenQuery
        Consensus.GetUpdateInterfaceState

toConsensusQuerySbe (QueryShelleyBasedEra ByronEraInCardanoMode QueryByronUpdateState) =
    Some $ Consensus.BlockQuery $
      Consensus.QueryIfCurrentByron
        Consensus.GetUpdateInterfaceState

toConsensusQuerySbe (QueryShelleyBasedEra erainmode (QueryInShelleyBasedEra era q)) =
    case erainmode of
      ByronEraInByronMode     -> case era of {}
      ShelleyEraInShelleyMode -> toConsensusQueryShelleyBased erainmode q
      ByronEraInCardanoMode   -> case era of {}
      ShelleyEraInCardanoMode -> toConsensusQueryShelleyBased erainmode q
      AllegraEraInCardanoMode -> toConsensusQueryShelleyBased erainmode q
      MaryEraInCardanoMode    -> toConsensusQueryShelleyBased erainmode q
      AlonzoEraInCardanoMode  -> toConsensusQueryShelleyBased erainmode q
      BabbageEraInCardanoMode -> toConsensusQueryShelleyBased erainmode q
      ConwayEraInCardanoMode -> toConsensusQueryShelleyBased erainmode q



toConsensusQueryShelleyBased
  :: forall era ledgerera mode protocol block xs result.
     ConsensusBlockForEra era ~ Consensus.ShelleyBlock protocol ledgerera
  => Core.EraCrypto ledgerera ~ Consensus.StandardCrypto
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
    poolids' = Set.map unStakePoolKeyHash poolids

toConsensusQueryShelleyBased erainmode QueryDebugLedgerState =
    Some (consensusQueryInEraInMode erainmode (Consensus.GetCBOR Consensus.DebugNewEpochState))

toConsensusQueryShelleyBased erainmode QueryProtocolState =
    Some (consensusQueryInEraInMode erainmode (Consensus.GetCBOR Consensus.DebugChainDepState))

toConsensusQueryShelleyBased erainmode QueryCurrentEpochState =
    Some (consensusQueryInEraInMode erainmode (Consensus.GetCBOR Consensus.DebugEpochState))

toConsensusQueryShelleyBased erainmode (QueryPoolState poolIds) =
    Some (consensusQueryInEraInMode erainmode (Consensus.GetCBOR (Consensus.GetPoolState (Set.map unStakePoolKeyHash <$> poolIds))))

toConsensusQueryShelleyBased erainmode (QueryStakeSnapshot mPoolIds) =
    Some (consensusQueryInEraInMode erainmode (Consensus.GetCBOR (Consensus.GetStakeSnapshots (fmap (Set.map unStakePoolKeyHash) mPoolIds))))

toConsensusQueryShelleyBased erainmode (QueryPoolDistribution poolIds) =
    Some (consensusQueryInEraInMode erainmode (Consensus.GetCBOR (Consensus.GetPoolDistr (getPoolIds <$> poolIds))))
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
      ConwayEraInCardanoMode -> Consensus.QueryIfCurrentConway

-- ----------------------------------------------------------------------------
-- Conversions of query results from the consensus types.
--

fromConsensusQueryResultSbe
  :: forall mode block result result'. ConsensusBlockForMode mode ~ block
   => QueryShelleyBasedEra mode result
   -> Consensus.Query block result'
   -> result'
   -> result
fromConsensusQueryResultSbe (QueryShelleyBasedEra ByronEraInByronMode
                                     QueryByronUpdateState) q' r' =
    case (q', r') of
      (Consensus.BlockQuery (Consensus.DegenQuery Consensus.GetUpdateInterfaceState),
       Consensus.DegenQueryResult r'')
        -> Right (ByronUpdateState r'')
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResultSbe (QueryShelleyBasedEra ByronEraInCardanoMode
                                     QueryByronUpdateState) q' r' =
    case q' of
      Consensus.BlockQuery
        (Consensus.QueryIfCurrentByron Consensus.GetUpdateInterfaceState)
        -> bimap fromConsensusEraMismatch ByronUpdateState r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResultSbe (QueryShelleyBasedEra ByronEraInByronMode
                                     (QueryInShelleyBasedEra era _)) _ _ =
    case era of {}

fromConsensusQueryResultSbe (QueryShelleyBasedEra ShelleyEraInShelleyMode
                                     (QueryInShelleyBasedEra _era q)) q' r' =
    case (q', r') of
      (Consensus.BlockQuery (Consensus.DegenQuery q''),
       Consensus.DegenQueryResult r'')
        -> Right (fromConsensusQueryResultShelleyBased
                    ShelleyBasedEraShelley q q'' r'')
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResultSbe (QueryShelleyBasedEra ByronEraInCardanoMode
                                     (QueryInShelleyBasedEra era _)) _ _ =
    case era of {}

fromConsensusQueryResultSbe (QueryShelleyBasedEra ShelleyEraInCardanoMode
                                     (QueryInShelleyBasedEra _era q)) q' r' =
    case q' of
      Consensus.BlockQuery (Consensus.QueryIfCurrentShelley q'')
        -> bimap fromConsensusEraMismatch
                 (fromConsensusQueryResultShelleyBased
                    ShelleyBasedEraShelley q q'')
                 r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResultSbe (QueryShelleyBasedEra AllegraEraInCardanoMode
                                     (QueryInShelleyBasedEra _era q)) q' r' =
    case q' of
      Consensus.BlockQuery (Consensus.QueryIfCurrentAllegra q'')
        -> bimap fromConsensusEraMismatch
                 (fromConsensusQueryResultShelleyBased
                    ShelleyBasedEraAllegra q q'')
                 r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResultSbe (QueryShelleyBasedEra MaryEraInCardanoMode
                                     (QueryInShelleyBasedEra _era q)) q' r' =
    case q' of
      Consensus.BlockQuery (Consensus.QueryIfCurrentMary q'')
        -> bimap fromConsensusEraMismatch
                 (fromConsensusQueryResultShelleyBased
                    ShelleyBasedEraMary q q'')
                 r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResultSbe (QueryShelleyBasedEra AlonzoEraInCardanoMode
                                     (QueryInShelleyBasedEra _era q)) q' r' =
    case q' of
      Consensus.BlockQuery (Consensus.QueryIfCurrentAlonzo q'')
        -> bimap fromConsensusEraMismatch
                 (fromConsensusQueryResultShelleyBased
                    ShelleyBasedEraAlonzo q q'')
                 r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResultSbe (QueryShelleyBasedEra BabbageEraInCardanoMode
                                     (QueryInShelleyBasedEra _era q)) q' r' =
    case q' of
      Consensus.BlockQuery (Consensus.QueryIfCurrentBabbage q'')
        -> bimap fromConsensusEraMismatch
                 (fromConsensusQueryResultShelleyBased
                    ShelleyBasedEraBabbage q q'')
                 r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResultSbe (QueryShelleyBasedEra ConwayEraInCardanoMode
                                     (QueryInShelleyBasedEra _era q)) q' r' =
    case q' of
      Consensus.BlockQuery (Consensus.QueryIfCurrentConway q'')
        -> bimap fromConsensusEraMismatch
                 (fromConsensusQueryResultShelleyBased
                    ShelleyBasedEraConway q q'')
                 r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusEraMismatch :: SListI xs
                         => Consensus.MismatchEraInfo xs -> EraMismatch
fromConsensusEraMismatch = Consensus.mkEraMismatch

fromConsensusQueryResultShelleyBased
  :: forall era ledgerera protocol result result'.
     ShelleyLedgerEra era ~ ledgerera
  => Core.EraCrypto ledgerera ~ Consensus.StandardCrypto
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

fromConsensusQueryResultShelleyBased _ QueryPoolDistribution{} q' r' =
  case q' of
    Consensus.GetCBOR Consensus.GetPoolDistr {} -> SerialisedPoolDistribution r'
    _                                           -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryStakeSnapshot{} q' r' =
  case q' of
    Consensus.GetCBOR Consensus.GetStakeSnapshots {} -> SerialisedStakeSnapshots r'
    _                                                -> fromConsensusQueryResultMismatch
