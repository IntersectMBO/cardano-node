{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
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

import           Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.=))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (Parser)
import           Data.Bifunctor (bimap, first)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HMS
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
import           Data.SOP.Strict (SListI)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Sharing (FromSharedCBOR, Interns, Share)
import           Data.Text (Text)
import           Data.Typeable
import           Prelude

import           Ouroboros.Network.Protocol.LocalStateQuery.Type (FootprintL (..), QueryWithSomeResult (..))

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
import           Cardano.Api.Eras
import           Cardano.Api.GenesisParameters
import           Cardano.Api.KeysShelley
import           Cardano.Api.Modes
import           Cardano.Api.NetworkId
import           Cardano.Api.Orphans ()
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.TxBody
import           Cardano.Api.Value

import qualified Data.Aeson.KeyMap as KeyMap
import           Data.Word (Word64)

-- ----------------------------------------------------------------------------
-- Queries
--

data QueryInMode mode fp result where
  QueryCurrentEra
    :: ConsensusModeIsMultiEra mode
    -> QueryInMode mode SmallL AnyCardanoEra

  QueryInEra
    :: EraInMode era mode
    -> QueryInEra era fp result
    -> QueryInMode mode fp (Either EraMismatch result)

  QueryEraHistory
    :: ConsensusModeIsMultiEra mode
    -> QueryInMode mode SmallL (EraHistory mode)

  QuerySystemStart
    :: QueryInMode mode SmallL SystemStart

  QueryChainBlockNo
    :: QueryInMode mode SmallL (WithOrigin BlockNo)

  QueryChainPoint
    :: ConsensusMode mode
    -> QueryInMode mode SmallL ChainPoint

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

deriving instance Show (QueryInMode mode fp result)

data QueryInEra era fp result where
     QueryByronUpdateState :: QueryInEra ByronEra SmallL ByronUpdateState

     QueryInShelleyBasedEra :: ShelleyBasedEra era
                            -> QueryInShelleyBasedEra era fp result
                            -> QueryInEra era fp result

deriving instance Show (QueryInEra era fp result)


data QueryInShelleyBasedEra era fp result where
     QueryEpoch
       :: QueryInShelleyBasedEra era SmallL EpochNo

     QueryGenesisParameters
       :: QueryInShelleyBasedEra era SmallL GenesisParameters

     QueryProtocolParameters
       :: QueryInShelleyBasedEra era SmallL ProtocolParameters

     QueryProtocolParametersUpdate
       :: QueryInShelleyBasedEra era SmallL
            (Map (Hash GenesisKey) ProtocolParametersUpdate)

     QueryStakeDistribution
       :: QueryInShelleyBasedEra era SmallL (Map (Hash StakePoolKey) Rational)

     QueryUTxO
       :: QueryUTxOFilter fp
       -> QueryInShelleyBasedEra era fp (UTxO era)

     QueryStakeAddresses
       :: Set StakeCredential
       -> NetworkId
       -> QueryInShelleyBasedEra era SmallL (Map StakeAddress Lovelace,
                                             Map StakeAddress PoolId)

     QueryStakePools
       :: QueryInShelleyBasedEra era SmallL (Set PoolId)

     QueryStakePoolParameters
       :: Set PoolId
       -> QueryInShelleyBasedEra era SmallL (Map PoolId StakePoolParameters)

     -- TODO: add support for RewardProvenance
     -- QueryPoolRanking
     --   :: QueryInShelleyBasedEra era RewardProvenance

     QueryDebugLedgerState
       :: QueryInShelleyBasedEra era SmallL (SerialisedDebugLedgerState era)

     QueryProtocolState
       :: QueryInShelleyBasedEra era SmallL (ProtocolState era)

     QueryCurrentEpochState
       :: QueryInShelleyBasedEra era SmallL (SerialisedCurrentEpochState era)

deriving instance Show (QueryInShelleyBasedEra era fp result)


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
data QueryUTxOFilter fp where
     -- | /O(n) time and space/ for utxo size n
     QueryUTxOWhole :: QueryUTxOFilter WholeL

     -- | /O(n) time, O(m) space/ for utxo size n, and address set size m
     QueryUTxOByAddress :: Set AddressAny -> QueryUTxOFilter WholeL

     -- | /O(m log n) time, O(m) space/ for utxo size n, and address set size m
     QueryUTxOByTxIn :: Set TxIn -> QueryUTxOFilter LargeL

instance Show (QueryUTxOFilter fp) where
  showsPrec p = \case
     QueryUTxOWhole           ->                      showString "QueryUTxOWhole"
     QueryUTxOByAddress addrs -> showParen (p > 11) $ showString "QueryUTxOByAddress " . shows addrs
     QueryUTxOByTxIn    txins -> showParen (p > 11) $ showString "QueryUTxOByTxIn "    . shows txins

--TODO: provide appropriate instances for these types as needed, e.g. JSON

newtype ByronUpdateState = ByronUpdateState Byron.Update.State
  deriving Show

newtype UTxO era = UTxO { unUTxO :: Map TxIn (TxOut CtxUTxO era) }
  deriving (Eq, Show)

data UTxOInAnyEra where
  UTxOInAnyEra :: CardanoEra era
               -> UTxO era
               -> UTxOInAnyEra

deriving instance Show UTxOInAnyEra

instance IsCardanoEra era => ToJSON (UTxO era) where
  toJSON (UTxO m) = toJSON m

instance (IsCardanoEra era, IsShelleyBasedEra era, FromJSON (TxOut CtxUTxO era))
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
    ) => FromCBOR (DebugLedgerState era) where
  fromCBOR = DebugLedgerState <$> (fromCBOR :: Decoder s (Shelley.NewEpochState (ShelleyLedgerEra era)))

-- TODO: Shelley based era class!
instance ( IsShelleyBasedEra era
         , ShelleyLedgerEra era ~ ledgerera
         , Consensus.ShelleyBasedEra ledgerera
         , ToJSON (Core.PParams ledgerera)
         , ToJSON (Core.PParamsDelta ledgerera)
         , ToJSON (Core.TxOut ledgerera)
         , Share (Core.TxOut (ShelleyLedgerEra era)) ~ Interns (Shelley.Credential 'Shelley.Staking (Ledger.Crypto (ShelleyLedgerEra era)))
         ) => ToJSON (DebugLedgerState era) where
  toJSON (DebugLedgerState newEpochS) = object [ "lastEpoch" .= Shelley.nesEL newEpochS
                                          , "blocksBefore" .= Shelley.nesBprev newEpochS
                                          , "blocksCurrent" .= Shelley.nesBcur newEpochS
                                          , "stateBefore" .= Shelley.nesEs newEpochS
                                          , "possibleRewardUpdate" .= Shelley.nesRu newEpochS
                                          , "stakeDistrib" .= Shelley.nesPd newEpochS
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
  :: forall era. Ledger.Era (ShelleyLedgerEra era)
  => Share (Core.TxOut (ShelleyLedgerEra era)) ~ Interns (Shelley.Credential 'Shelley.Staking (Ledger.Crypto (ShelleyLedgerEra era)))
  => FromSharedCBOR (Core.TxOut (ShelleyLedgerEra era))
  => Share (Core.TxOut (ShelleyLedgerEra era)) ~ Interns (Shelley.Credential 'Shelley.Staking (Ledger.Crypto (ShelleyLedgerEra era)))
  => FromCBOR (Core.PParams (ShelleyLedgerEra era))
  => FromCBOR (Core.Value (ShelleyLedgerEra era))
  => FromCBOR (Ledger.State (Core.EraRule "PPUP" (ShelleyLedgerEra era)))
  => SerialisedCurrentEpochState era
  -> Either DecoderError (CurrentEpochState era)
decodeCurrentEpochState (SerialisedCurrentEpochState (Serialised ls)) = CurrentEpochState <$> decodeFull ls

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

toConsensusQuery :: forall mode block fp result.
                    ConsensusBlockForMode mode ~ block
                 => QueryInMode mode fp result
                 -> Consensus.QueryWithSomeResult (Consensus.Query block) fp
toConsensusQuery (QueryCurrentEra CardanoModeIsMultiEra) =
    QueryWithSomeResult $ Consensus.BlockQuery $
      Consensus.QueryHardFork
        Consensus.GetCurrentEra

toConsensusQuery (QueryInEra ByronEraInByronMode QueryByronUpdateState) =
    QueryWithSomeResult $ Consensus.BlockQuery $
      Consensus.DegenQuery
        Consensus.GetUpdateInterfaceState

toConsensusQuery (QueryEraHistory CardanoModeIsMultiEra) =
    QueryWithSomeResult $ Consensus.BlockQuery $
      Consensus.QueryHardFork
        Consensus.GetInterpreter

toConsensusQuery QuerySystemStart = QueryWithSomeResult Consensus.GetSystemStart

toConsensusQuery QueryChainBlockNo = QueryWithSomeResult Consensus.GetChainBlockNo

toConsensusQuery (QueryChainPoint _) = QueryWithSomeResult Consensus.GetChainPoint

toConsensusQuery (QueryInEra ByronEraInCardanoMode QueryByronUpdateState) =
    QueryWithSomeResult $ Consensus.BlockQuery $
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
  :: forall era ledgerera mode protocol block xs fp result.
     ConsensusBlockForEra era ~ Consensus.ShelleyBlock protocol ledgerera
  => Ledger.Crypto ledgerera ~ Consensus.StandardCrypto
  => ConsensusBlockForMode mode ~ block
  => block ~ Consensus.HardForkBlock xs
  => EraInMode era mode
  -> QueryInShelleyBasedEra era fp result
  -> QueryWithSomeResult (Consensus.Query block) fp
toConsensusQueryShelleyBased erainmode QueryEpoch =
    QueryWithSomeResult (consensusQueryInEraInMode erainmode Consensus.GetEpochNo)

toConsensusQueryShelleyBased erainmode QueryGenesisParameters =
    QueryWithSomeResult (consensusQueryInEraInMode erainmode Consensus.GetGenesisConfig)

toConsensusQueryShelleyBased erainmode QueryProtocolParameters =
    QueryWithSomeResult (consensusQueryInEraInMode erainmode Consensus.GetCurrentPParams)

toConsensusQueryShelleyBased erainmode QueryProtocolParametersUpdate =
    QueryWithSomeResult (consensusQueryInEraInMode erainmode Consensus.GetProposedPParamsUpdates)

toConsensusQueryShelleyBased erainmode QueryStakeDistribution =
    QueryWithSomeResult (consensusQueryInEraInMode erainmode Consensus.GetStakeDistribution)

toConsensusQueryShelleyBased erainmode (QueryUTxO QueryUTxOWhole) =
    QueryWithSomeResult (consensusQueryInEraInMode erainmode Consensus.GetUTxOWhole)

toConsensusQueryShelleyBased erainmode (QueryUTxO (QueryUTxOByAddress addrs)) =
    QueryWithSomeResult (consensusQueryInEraInMode erainmode (Consensus.GetUTxOByAddress addrs'))
  where
    addrs' :: Set (Shelley.Addr Consensus.StandardCrypto)
    addrs' = toShelleyAddrSet (eraInModeToEra erainmode) addrs

toConsensusQueryShelleyBased erainmode (QueryUTxO (QueryUTxOByTxIn txins)) =
    QueryWithSomeResult (consensusQueryInEraInMode erainmode (Consensus.GetUTxOByTxIn txins'))
  where
    txins' :: Set (Shelley.TxIn Consensus.StandardCrypto)
    txins' = Set.map toShelleyTxIn txins

toConsensusQueryShelleyBased erainmode (QueryStakeAddresses creds _nId) =
    QueryWithSomeResult (consensusQueryInEraInMode erainmode
                           (Consensus.GetFilteredDelegationsAndRewardAccounts creds'))
  where
    creds' :: Set (Shelley.Credential Shelley.Staking StandardCrypto)
    creds' = Set.map toShelleyStakeCredential creds

toConsensusQueryShelleyBased erainmode QueryStakePools =
    QueryWithSomeResult (consensusQueryInEraInMode erainmode Consensus.GetStakePools)

toConsensusQueryShelleyBased erainmode (QueryStakePoolParameters poolids) =
    QueryWithSomeResult (consensusQueryInEraInMode erainmode (Consensus.GetStakePoolParams poolids'))
  where
    poolids' :: Set (Shelley.KeyHash Shelley.StakePool Consensus.StandardCrypto)
    poolids' = Set.map (\(StakePoolKeyHash kh) -> kh) poolids

toConsensusQueryShelleyBased erainmode QueryDebugLedgerState =
    QueryWithSomeResult (consensusQueryInEraInMode erainmode (Consensus.GetCBOR Consensus.DebugNewEpochState))

toConsensusQueryShelleyBased erainmode QueryProtocolState =
    QueryWithSomeResult (consensusQueryInEraInMode erainmode (Consensus.GetCBOR Consensus.DebugChainDepState))

toConsensusQueryShelleyBased erainmode QueryCurrentEpochState =
    QueryWithSomeResult (consensusQueryInEraInMode erainmode (Consensus.GetCBOR Consensus.DebugEpochState))


consensusQueryInEraInMode
  :: forall era mode erablock modeblock fp result result' xs.
     ConsensusBlockForEra era   ~ erablock
  => ConsensusBlockForMode mode ~ modeblock
  => modeblock ~ Consensus.HardForkBlock xs
  => Consensus.HardForkQueryResult xs result ~ result'
  => EraInMode era mode
  -> Consensus.BlockQuery erablock fp result
  -> Consensus.Query modeblock fp result'
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

fromConsensusQueryResult :: forall mode block fp result result'. ConsensusBlockForMode mode ~ block
                         => QueryInMode mode fp result
                         -> Consensus.Query block fp result'
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
  :: forall era ledgerera fp protocol result result'.
     ShelleyLedgerEra era ~ ledgerera
  => Ledger.Crypto ledgerera ~ Consensus.StandardCrypto
  => ConsensusProtocol era ~ protocol
  => ShelleyBasedEra era
  -> QueryInShelleyBasedEra era fp result
  -> Consensus.BlockQuery (Consensus.ShelleyBlock protocol ledgerera) fp result'
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
