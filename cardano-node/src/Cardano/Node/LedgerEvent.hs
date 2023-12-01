{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Local representation for display purpose of cardano-ledger events.
--
-- Shamelessly stolen and adapted from db-sync.
module Cardano.Node.LedgerEvent (
    -- * Ledger Events
    AnchoredEvents (..)
  , LedgerEvent (..)
  , LedgerNewEpochEvent (..)
  , LedgerRewardUpdateEvent (..)
  , Versioned (..)
  , ledgerEventName

    -- ** Using Ledger events
  , StandardLedgerEventHandler
  , withLedgerEventsServerStream
  , foldEvent
  , withLedgerEventsChan
  , mkLedgerEventHandler
  , LedgerEventsWriter
  , LedgerEventsReader

    -- * Type-level plumbing
  , ConvertLedgerEvent (..)
  , eventCodecVersion

    -- * Re-Exports
  , Coin (..)
  , Credential (..)
  , DeltaCoin (..)
  , EpochNo (..)
  , KeyHash (..)
  , KeyRole (..)
  , Reward (..)
  , ScriptHash (..)
  , SlotNo (..)
  , BlockNo (..)
  , StandardCrypto
  , WithOrigin (..)
  , serialize'
  ) where

import           Cardano.Prelude hiding (All, Sum)

import           Control.Arrow ((&&&))
import           Control.Monad.Fail (MonadFail(..))
import           Cardano.Ledger.Binary (DecCBOR(..), EncCBOR(..), Version,
                   decodeFull', fromCBOR, serialize', toCBOR)
import           Cardano.Ledger.Binary.Coders (Decode(..), Encode (..), encode, (!>),
                   (<!), decode)
import           Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import           Cardano.Ledger.Credential (Credential (..))
import           Cardano.Ledger.Rewards (Reward(..))
import qualified Cardano.Ledger.Core as Ledger
import           Cardano.Ledger.Core (eraProtVerLow)
import           Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import           Cardano.Ledger.Keys (KeyRole (..))
import           Cardano.Ledger.Shelley.API (InstantaneousRewards (..), KeyHash (..), ScriptHash (..))
import           Cardano.Ledger.Shelley.Core (EraCrypto)
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import           Cardano.Ledger.Shelley.Rules (RupdEvent (..),
                     ShelleyEpochEvent (..), ShelleyMirEvent (..),
                     ShelleyNewEpochEvent, ShelleyPoolreapEvent (..),
                     ShelleyTickEvent (..))
import           Cardano.Slotting.Slot (SlotNo (..), EpochNo (..), WithOrigin (..))
import           Cardano.Slotting.Block (BlockNo(..))
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import           Control.Concurrent.STM (newTChanIO, readTChan, writeTChan)
import           Control.State.Transition (Event)
import           Data.ByteString.Short(ShortByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString  as BS
import qualified Data.List.NonEmpty as NE
import           Data.SOP (All, K (..))
import           Data.SOP.Strict (NS(..), hcmap, hcollapse)
import qualified Data.Set as Set
import           Data.String (String)
import           Network.Socket(PortNumber, defaultProtocol, listen, accept,
                  bind, close, socket, socketToHandle, withSocketsDo,
                  SockAddr(..), SocketType(Stream), Family(AF_INET))
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import           Ouroboros.Consensus.Cardano.Block (AllegraEra, AlonzoEra,
                     BabbageEra, CardanoEras, ConwayEra, HardForkBlock,
                     MaryEra, ShelleyEra)
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
                     (OneEraLedgerEvent(..), getOneEraHash, getOneEraLedgerEvent)
import           Ouroboros.Consensus.Ledger.Abstract (AuxLedgerEvent,
                     LedgerEventHandler(..))
import qualified Ouroboros.Consensus.Ledger.Abstract as Abstract
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState)
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock,
                     ShelleyLedgerEvent (..))
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Prelude (type (~))
import           System.IO(hIsEOF)
import Cardano.Ledger.Conway.Rules (ConwayNewEpochEvent, ConwayEpochEvent)
import qualified Cardano.Ledger.Conway.Rules as Conway
import qualified Cardano.Ledger.Shelley.API as ShelleyAPI
import Cardano.Ledger.Alonzo.Rules (AlonzoBbodyEvent (ShelleyInAlonzoEvent), AlonzoUtxowEvent (WrappedShelleyEraEvent), AlonzoUtxoEvent (UtxosEvent), AlonzoUtxosEvent)
import GHC.IO.Exception (IOException(IOError, ioe_type), IOErrorType (ResourceVanished))
import Ouroboros.Network.Block (ChainHash(GenesisHash, BlockHash), HeaderHash)

type LedgerState crypto =
  ExtLedgerState (HardForkBlock (CardanoEras crypto))

data LedgerEvent crypto
  = LedgerNewEpochEvent !(LedgerNewEpochEvent crypto)
  | LedgerRewardUpdateEvent !(LedgerRewardUpdateEvent crypto)
  -- TODO complete those vvv
  -- | LedgerUtxoTotalDeposits
  -- | LedgerNewEpoch
  -- | LedgerRegisterPool
  -- | LedgerReRegisterPool
  | LedgerBody
  deriving (Eq, Show)

-- TODO: Review encoding & make future-proof (i.e. favor records over lists/tuples)
instance Crypto crypto => EncCBOR (LedgerEvent crypto) where
  encCBOR = encode . \case
    LedgerNewEpochEvent e ->
      Sum LedgerNewEpochEvent 0
        !> To e
    LedgerRewardUpdateEvent e ->
      Sum LedgerRewardUpdateEvent 1
        !> To e
    LedgerBody ->
      Sum LedgerBody 2

instance Crypto crypto => DecCBOR (LedgerEvent crypto) where
  decCBOR = decode (Summands "LedgerEvent" decRaw)
    where
      decRaw 0 =
        SumD LedgerNewEpochEvent
          <! From
      decRaw 1 =
        SumD LedgerRewardUpdateEvent
          <! From
      decRaw 2 =
        SumD LedgerBody
      decRaw n = Invalid n


-- TODO(KtorZ): Discuss that design choice; I believe we should favor a more
-- 'flat' structure for events instead of preserving whatever the ledger imposes
-- on us.
data LedgerNewEpochEvent crypto
  = LedgerMirDist
      !(Map (Credential 'Staking crypto) Coin)
        -- ^ Rewards paid from the __Reserve__ into stake credentials
      !(Map (Credential 'Staking crypto) Coin)
        -- ^ Rewards paid from the __Treasury__ to stake credentials
      !DeltaCoin
        -- ^ Transfer from the __Reserve__ into the __Treasury__
      !DeltaCoin
        -- ^ Transfer from the __Treasury__ into the __Reserve__
  | LedgerPoolReaping
      !EpochNo
        -- ^ Epoch for which the event is emitted
      !(Map (Credential 'Staking crypto) (Map (KeyHash 'StakePool crypto) Coin))
        -- ^ Stake pools refunds after retirement
      !(Map (Credential 'Staking crypto) (Map (KeyHash 'StakePool crypto) Coin))
        -- ^ Unclaimed deposit after retirement, for stake credentials that no longer exist.
  | LedgerStakeDistEvent
      !(Map (Credential 'Staking crypto) (Coin, KeyHash 'StakePool crypto))
        -- ^ Stake controlled and delegated by registered credentials at an epoch boundary
  | LedgerRestrainedRewards
      !EpochNo
        -- ^ Epoch when the event is calculated
      !(Map (Credential 'Staking crypto) (Set (Reward crypto)))
        -- ^ A map of rewards that are ignored due to a ledger bug in Shelley
      !(Set (Credential 'Staking crypto))
        -- ^ A set of unregistered credentials which won't receive rewards
  | LedgerTotalRewards
      !EpochNo
        -- ^ The epoch number of the new epoch
      !(Map (Credential 'Staking crypto) (Set (Reward crypto)))
        -- ^ Total rewards distributed at the epoch boundary
  | LedgerTotalAdaPots
      !Coin
        -- ^ Treasury Ada pot
      !Coin
        -- ^ Reserves Ada pot
      !Coin
        -- ^ Rewards Ada pot
      !Coin
        -- ^ Utxo Ada pot
      !Coin
        -- ^ Key deposit Ada pot
      !Coin
        -- ^ Pool deposit Ada pot
      !Coin
        -- ^ Deposits Ada pot
      !Coin
        -- ^ Fees Ada pot
  deriving (Eq, Show)

instance Crypto crypto => EncCBOR (LedgerNewEpochEvent crypto) where
  encCBOR = encode . \case
    LedgerMirDist fromReserve fromTreasury deltaReserve deltaTreasury ->
      Sum LedgerMirDist 0
        !> To fromReserve
        !> To fromTreasury
        !> To deltaReserve
        !> To deltaTreasury
    LedgerPoolReaping epoch refunded unclaimed ->
      Sum LedgerPoolReaping 1
        !> To epoch
        !> To refunded
        !> To unclaimed
    LedgerStakeDistEvent stakeDist ->
      Sum LedgerStakeDistEvent 2
        !> To stakeDist
    LedgerRestrainedRewards epoch ignored unregistered ->
      Sum (\a0 a1 -> LedgerRestrainedRewards a0 (Set.fromList <$> a1) . Set.fromList) 3
        !> To epoch
        !> To (Set.toList <$> ignored)
        !> To (Set.toList unregistered)
    LedgerTotalRewards epoch rewards ->
      Sum (\a0 -> LedgerTotalRewards a0 . fmap Set.fromList) 4
        !> To epoch
        !> To (Set.toList <$> rewards)
    LedgerTotalAdaPots treasury reserves rewards utxo keyDeposit poolDeposit deposits fees ->
      Sum LedgerTotalAdaPots 5
        !> To treasury
        !> To reserves
        !> To rewards
        !> To utxo
        !> To keyDeposit
        !> To poolDeposit
        !> To deposits
        !> To fees

instance Crypto crypto => DecCBOR (LedgerNewEpochEvent crypto) where
  decCBOR = decode (Summands "LedgerNewEpochEvent" decRaw)
    where
      decRaw 0 =
        SumD LedgerMirDist
          <! From
          <! From
          <! From
          <! From
      decRaw 1 =
        SumD LedgerPoolReaping
          <! From
          <! From
          <! From
      decRaw 2 =
        SumD LedgerStakeDistEvent
          <! From
      decRaw 3 =
        SumD (\a0 a1 -> LedgerRestrainedRewards a0 (Set.fromList <$> a1) . Set.fromList)
          <! From
          <! From
          <! From
      decRaw 4 =
        SumD (\a0 -> LedgerTotalRewards a0 . fmap Set.fromList)
          <! From
          <! From
      decRaw 5 =
        SumD LedgerTotalAdaPots
          <! From
          <! From
          <! From
          <! From
          <! From
          <! From
          <! From
          <! From
      decRaw n =
        Invalid n

data LedgerRewardUpdateEvent crypto
  = LedgerIncrementalRewards
      !EpochNo
        -- ^ Epoch at which those rewards will become available.
      !(Map (Credential 'Staking crypto) (Set (Reward crypto)))
        -- ^ Reward increment
  deriving (Eq, Show)

instance Crypto crypto => EncCBOR (LedgerRewardUpdateEvent crypto) where
  encCBOR = encode . \case
    LedgerIncrementalRewards epoch rewards ->
      Sum (\a0 -> LedgerIncrementalRewards a0 . fmap Set.fromList) 0
        !> To epoch
        !> To (Set.toList <$> rewards)

instance Crypto crypto => DecCBOR (LedgerRewardUpdateEvent crypto) where
  decCBOR = decode (Summands "LedgerRewardUpdateEvent" decRaw)
    where
      decRaw 0 = SumD (\a0 -> LedgerIncrementalRewards a0 . fmap Set.fromList)
        <! From
        <! From
      decRaw n =
        Invalid n

instance Ord (LedgerEvent crypto) where
  a <= b = toOrdering a <= toOrdering b

-- TODO: Review order once we're done with the type modeling
toOrdering :: LedgerEvent crypto -> Int
toOrdering = \case
  LedgerNewEpochEvent LedgerMirDist {}                -> 0
  LedgerNewEpochEvent LedgerPoolReaping {}            -> 1
  LedgerNewEpochEvent LedgerStakeDistEvent {}         -> 2
  LedgerRewardUpdateEvent LedgerIncrementalRewards {} -> 3
  LedgerNewEpochEvent LedgerRestrainedRewards {}      -> 5
  LedgerNewEpochEvent LedgerTotalRewards {}           -> 6
  LedgerNewEpochEvent LedgerTotalAdaPots {}           -> 8
  LedgerBody                                          -> 9

ledgerEventName :: LedgerEvent crypto -> Text
ledgerEventName = \case
  LedgerNewEpochEvent e       -> ledgerNewEpochEventName e
  LedgerRewardUpdateEvent e   -> ledgerRewardUpdateEventName e
  LedgerBody {}               -> "LedgerBody"

ledgerNewEpochEventName :: LedgerNewEpochEvent crypto -> Text
ledgerNewEpochEventName = \case
  LedgerMirDist {}            -> "LedgerMirDist"
  LedgerPoolReaping {}        -> "LedgerPoolReaping"
  LedgerStakeDistEvent {}     -> "LedgerStakeDistEvent"
  LedgerRestrainedRewards {}  -> "LedgerRestrainedRewards"
  LedgerTotalRewards {}       -> "LedgerTotalRewards"
  LedgerTotalAdaPots {}       -> "LedgerTotalAdaPots"

ledgerRewardUpdateEventName :: LedgerRewardUpdateEvent crypto -> Text
ledgerRewardUpdateEventName = \case
  LedgerIncrementalRewards {} -> "LedgerIncrementalRewards"

fromAuxLedgerEvent
  :: forall xs. (All ConvertLedgerEvent xs)
  => AuxLedgerEvent (Abstract.LedgerState (HardForkBlock xs))
  -> Maybe (LedgerEvent StandardCrypto)
fromAuxLedgerEvent =
  toLedgerEvent . WrapLedgerEvent @(HardForkBlock xs)

class ConvertLedgerEvent blk where
  toLedgerEvent :: WrapLedgerEvent blk -> Maybe (LedgerEvent StandardCrypto)

instance All ConvertLedgerEvent xs => ConvertLedgerEvent (HardForkBlock xs) where
  toLedgerEvent =
    hcollapse
      . hcmap (Proxy @ConvertLedgerEvent) (K . toLedgerEvent)
      . getOneEraLedgerEvent
      . unwrapLedgerEvent

instance ConvertLedgerEvent ByronBlock where
  toLedgerEvent _ = Nothing

toLedgerEventShelley
  :: forall era proto.
     ( EraCrypto era ~ StandardCrypto
     , Event (Ledger.EraRule "TICK" era) ~ ShelleyTickEvent era
     , Event (Ledger.EraRule "NEWEPOCH" era) ~ ShelleyNewEpochEvent era
     , Event (Ledger.EraRule "MIR" era) ~ ShelleyMirEvent era
     , Event (Ledger.EraRule "EPOCH" era) ~ ShelleyEpochEvent era
     , Event (Ledger.EraRule "POOLREAP" era) ~ ShelleyPoolreapEvent era
     , Event (Ledger.EraRule "RUPD" era) ~ RupdEvent (EraCrypto era)
     , Event (Ledger.EraRule "SNAP" era) ~ Shelley.SnapEvent era
     , Event (Ledger.EraRule "BBODY" era) ~ Shelley.ShelleyBbodyEvent era
     , Event (Ledger.EraRule "LEDGERS" era) ~ Shelley.ShelleyLedgersEvent era
     , Event (Ledger.EraRule "LEDGER" era) ~ Shelley.ShelleyLedgerEvent era
     , Event (Ledger.EraRule "UTXOW" era) ~ Shelley.ShelleyUtxowEvent era
     , Event (Ledger.EraRule "UTXO" era) ~ Shelley.UtxoEvent era
     , Event (Ledger.EraRule "PPUP" era) ~ Shelley.PpupEvent era
     , Event (Ledger.EraRule "DELEGS" era) ~ Shelley.ShelleyDelegsEvent era
     )
  => WrapLedgerEvent (ShelleyBlock proto era)
  -> Maybe (LedgerEvent (EraCrypto era))
toLedgerEventShelley evt =
  case unwrapLedgerEvent evt of
    ShelleyLedgerEventTICK tickEvent ->
      case tickEvent of
        TickNewEpochEvent newEpochEvent ->
          case newEpochEvent of
            Shelley.TotalRewardEvent epoch rewards ->
              liftNewEpoch $ LedgerTotalRewards epoch rewards

            Shelley.RestrainedRewards epoch rewards credentials ->
              liftNewEpoch $ LedgerRestrainedRewards epoch rewards credentials

            Shelley.MirEvent transfer ->
              case transfer of
                MirTransfer (InstantaneousRewards fromReserve fromTreasury deltaReserve deltaTreasury) ->
                  liftNewEpoch $ LedgerMirDist fromReserve fromTreasury deltaReserve deltaTreasury
                NoMirTransfer{} -> -- FIXME: create an event for this
                  Nothing

            Shelley.EpochEvent (Shelley.PoolReapEvent (RetiredPools refunded unclaimed epoch)) ->
               liftNewEpoch $ LedgerPoolReaping epoch refunded unclaimed

            Shelley.EpochEvent (Shelley.SnapEvent (Shelley.StakeDistEvent stakeDist)) ->
              liftNewEpoch $ LedgerStakeDistEvent stakeDist

            Shelley.EpochEvent (Shelley.UpecEvent _) ->
              -- There isn't any data associated with UpecEvent: Event (EraRule "UPEC" era) ~ Void
              Nothing

            Shelley.TotalAdaPotsEvent adaPots ->
              liftNewEpoch $ LedgerTotalAdaPots
                (ShelleyAPI.treasuryAdaPot adaPots)
                (ShelleyAPI.reservesAdaPot adaPots)
                (ShelleyAPI.rewardsAdaPot adaPots)
                (ShelleyAPI.utxoAdaPot adaPots)
                (ShelleyAPI.keyDepositAdaPot adaPots)
                (ShelleyAPI.poolDepositAdaPot adaPots)
                (ShelleyAPI.depositsAdaPot adaPots)
                (ShelleyAPI.feesAdaPot adaPots)

            Shelley.DeltaRewardEvent (RupdEvent epoch rewards) ->
              liftRewardUpdate $ LedgerIncrementalRewards epoch rewards

        TickRupdEvent (RupdEvent epoch rewards) ->
          liftRewardUpdate $ LedgerIncrementalRewards epoch rewards

    ShelleyLedgerEventBBODY (Shelley.LedgersEvent (Shelley.LedgerEvent (Shelley.UtxowEvent (Shelley.UtxoEvent (Shelley.TotalDeposits _txBodyHash _coin))))) ->
      Just LedgerBody

    ShelleyLedgerEventBBODY (Shelley.LedgersEvent (Shelley.LedgerEvent (Shelley.UtxowEvent (Shelley.UtxoEvent (Shelley.UpdateEvent (Shelley.NewEpoch _epochNo)))))) ->
      Just LedgerBody

    ShelleyLedgerEventBBODY (Shelley.LedgersEvent (Shelley.LedgerEvent (Shelley.DelegsEvent (Shelley.DelplEvent _)))) ->
      -- TODO Constructors of ShelleyDelplEvent (PoolEvent and DelegEvent) not exposed by
      -- cardano-ledger.
      Just LedgerBody
 where
  liftNewEpoch = Just . LedgerNewEpochEvent
  liftRewardUpdate = Just . LedgerRewardUpdateEvent

toAllegraEventShelley
  :: forall era proto.
     ( EraCrypto era ~ StandardCrypto
     , Event (Ledger.EraRule "TICK" era) ~ ShelleyTickEvent era
     , Event (Ledger.EraRule "NEWEPOCH" era) ~ ShelleyNewEpochEvent era
     , Event (Ledger.EraRule "MIR" era) ~ ShelleyMirEvent era
     , Event (Ledger.EraRule "EPOCH" era) ~ ShelleyEpochEvent era
     , Event (Ledger.EraRule "POOLREAP" era) ~ ShelleyPoolreapEvent era
     , Event (Ledger.EraRule "RUPD" era) ~ RupdEvent (EraCrypto era)
     , Event (Ledger.EraRule "SNAP" era) ~ Shelley.SnapEvent era
     , Event (Ledger.EraRule "BBODY" era) ~ Shelley.ShelleyBbodyEvent era
     , Event (Ledger.EraRule "LEDGERS" era) ~ Shelley.ShelleyLedgersEvent era
     , Event (Ledger.EraRule "LEDGER" era) ~ Shelley.ShelleyLedgerEvent era
     , Event (Ledger.EraRule "UTXOW" era) ~ Shelley.ShelleyUtxowEvent era
     -- , Event (Ledger.EraRule "UTXO" era) ~ Allegra.AllegraUtxoEvent era -- TODO Not exported
     -- , Event (Ledger.EraRule "PPUP" era) ~ Shelley.PpupEvent era
     , Event (Ledger.EraRule "DELEGS" era) ~ Shelley.ShelleyDelegsEvent era
     )
  => WrapLedgerEvent (ShelleyBlock proto era)
  -> Maybe (LedgerEvent (EraCrypto era))
toAllegraEventShelley evt =
  case unwrapLedgerEvent evt of
    ShelleyLedgerEventTICK tickEvent ->
      case tickEvent of
        TickNewEpochEvent newEpochEvent ->
          case newEpochEvent of
            Shelley.TotalRewardEvent epoch rewards ->
              liftNewEpoch $ LedgerTotalRewards epoch rewards

            Shelley.RestrainedRewards epoch rewards credentials ->
              liftNewEpoch $ LedgerRestrainedRewards epoch rewards credentials

            Shelley.MirEvent transfer ->
              case transfer of
                MirTransfer (InstantaneousRewards fromReserve fromTreasury deltaReserve deltaTreasury) ->
                  liftNewEpoch $ LedgerMirDist fromReserve fromTreasury deltaReserve deltaTreasury
                NoMirTransfer{} -> -- FIXME: create an event for this
                  Nothing

            Shelley.EpochEvent (Shelley.PoolReapEvent (RetiredPools refunded unclaimed epoch)) ->
               liftNewEpoch $ LedgerPoolReaping epoch refunded unclaimed

            Shelley.EpochEvent (Shelley.SnapEvent (Shelley.StakeDistEvent stakeDist)) ->
              liftNewEpoch $ LedgerStakeDistEvent stakeDist

            Shelley.EpochEvent (Shelley.UpecEvent _) ->
              -- There isn't any data associated with UpecEvent: Event (EraRule "UPEC" era) ~ Void
              Nothing

            Shelley.TotalAdaPotsEvent adaPots ->
              liftNewEpoch $ LedgerTotalAdaPots
                (ShelleyAPI.treasuryAdaPot adaPots)
                (ShelleyAPI.reservesAdaPot adaPots)
                (ShelleyAPI.rewardsAdaPot adaPots)
                (ShelleyAPI.utxoAdaPot adaPots)
                (ShelleyAPI.keyDepositAdaPot adaPots)
                (ShelleyAPI.poolDepositAdaPot adaPots)
                (ShelleyAPI.depositsAdaPot adaPots)
                (ShelleyAPI.feesAdaPot adaPots)

            Shelley.DeltaRewardEvent (RupdEvent epoch rewards) ->
              liftRewardUpdate $ LedgerIncrementalRewards epoch rewards

        TickRupdEvent (RupdEvent epoch rewards) ->
          liftRewardUpdate $ LedgerIncrementalRewards epoch rewards

    -- TODO Constructors not exported by current cardano-ledger version
    ShelleyLedgerEventBBODY (Shelley.LedgersEvent (Shelley.LedgerEvent (Shelley.UtxowEvent (Shelley.UtxoEvent _)))) ->
      Just LedgerBody

    ShelleyLedgerEventBBODY (Shelley.LedgersEvent (Shelley.LedgerEvent (Shelley.DelegsEvent (Shelley.DelplEvent _)))) ->
      -- TODO Constructors of ShelleyDelplEvent (PoolEvent and DelegEvent) not exposed by
      -- cardano-ledger.
      Just LedgerBody
 where
  liftNewEpoch = Just . LedgerNewEpochEvent
  liftRewardUpdate = Just . LedgerRewardUpdateEvent

toAlonzoEventShelley
  :: forall era proto.
     ( EraCrypto era ~ StandardCrypto
     , Event (Ledger.EraRule "TICK" era) ~ ShelleyTickEvent era
     , Event (Ledger.EraRule "NEWEPOCH" era) ~ ShelleyNewEpochEvent era
     , Event (Ledger.EraRule "MIR" era) ~ ShelleyMirEvent era
     , Event (Ledger.EraRule "EPOCH" era) ~ ShelleyEpochEvent era
     , Event (Ledger.EraRule "POOLREAP" era) ~ ShelleyPoolreapEvent era
     , Event (Ledger.EraRule "RUPD" era) ~ RupdEvent (EraCrypto era)
     , Event (Ledger.EraRule "SNAP" era) ~ Shelley.SnapEvent era
     , Event (Ledger.EraRule "BBODY" era) ~ AlonzoBbodyEvent era
     , Event (Ledger.EraRule "LEDGERS" era) ~ Shelley.ShelleyLedgersEvent era
     , Event (Ledger.EraRule "LEDGER" era) ~ Shelley.ShelleyLedgerEvent era
     , Event (Ledger.EraRule "UTXOW" era) ~ AlonzoUtxowEvent era
     , Event (Ledger.EraRule "UTXOS" era) ~ AlonzoUtxosEvent era
     , Event (Ledger.EraRule "UTXO" era) ~ AlonzoUtxoEvent era
     , Event (Ledger.EraRule "PPUP" era) ~ Shelley.PpupEvent era
     , Event (Ledger.EraRule "DELEGS" era) ~ Shelley.ShelleyDelegsEvent era
     )
  => WrapLedgerEvent (ShelleyBlock proto era)
  -> Maybe (LedgerEvent (EraCrypto era))
toAlonzoEventShelley evt =
  case unwrapLedgerEvent evt of
    ShelleyLedgerEventTICK tickEvent ->
      case tickEvent of
        TickNewEpochEvent newEpochEvent ->
          case newEpochEvent of
            Shelley.TotalRewardEvent epoch rewards ->
              liftNewEpoch $ LedgerTotalRewards epoch rewards

            Shelley.RestrainedRewards epoch rewards credentials ->
              liftNewEpoch $ LedgerRestrainedRewards epoch rewards credentials

            Shelley.MirEvent transfer ->
              case transfer of
                MirTransfer (InstantaneousRewards fromReserve fromTreasury deltaReserve deltaTreasury) ->
                  liftNewEpoch $ LedgerMirDist fromReserve fromTreasury deltaReserve deltaTreasury
                NoMirTransfer{} -> -- FIXME: create an event for this
                  Nothing

            Shelley.EpochEvent (Shelley.PoolReapEvent (RetiredPools refunded unclaimed epoch)) ->
               liftNewEpoch $ LedgerPoolReaping epoch refunded unclaimed

            Shelley.EpochEvent (Shelley.SnapEvent (Shelley.StakeDistEvent stakeDist)) ->
              liftNewEpoch $ LedgerStakeDistEvent stakeDist

            Shelley.EpochEvent (Shelley.UpecEvent _) ->
              -- There isn't any data associated with UpecEvent: Event (EraRule "UPEC" era) ~ Void
              Nothing

            Shelley.TotalAdaPotsEvent adaPots ->
              liftNewEpoch $ LedgerTotalAdaPots
                (ShelleyAPI.treasuryAdaPot adaPots)
                (ShelleyAPI.reservesAdaPot adaPots)
                (ShelleyAPI.rewardsAdaPot adaPots)
                (ShelleyAPI.utxoAdaPot adaPots)
                (ShelleyAPI.keyDepositAdaPot adaPots)
                (ShelleyAPI.poolDepositAdaPot adaPots)
                (ShelleyAPI.depositsAdaPot adaPots)
                (ShelleyAPI.feesAdaPot adaPots)

            Shelley.DeltaRewardEvent (RupdEvent epoch rewards) ->
              liftRewardUpdate $ LedgerIncrementalRewards epoch rewards

        TickRupdEvent (RupdEvent epoch rewards) ->
          liftRewardUpdate $ LedgerIncrementalRewards epoch rewards

    ShelleyLedgerEventBBODY (ShelleyInAlonzoEvent (Shelley.LedgersEvent (Shelley.LedgerEvent (Shelley.UtxowEvent (WrappedShelleyEraEvent (Shelley.UtxoEvent (UtxosEvent (Alonzo.AlonzoPpupToUtxosEvent (Shelley.NewEpoch _epochNo))))))))) ->
      Just LedgerBody
    ShelleyLedgerEventBBODY (ShelleyInAlonzoEvent (Shelley.LedgersEvent (Shelley.LedgerEvent (Shelley.UtxowEvent (WrappedShelleyEraEvent (Shelley.UtxoEvent (UtxosEvent (Alonzo.SuccessfulPlutusScriptsEvent _plutusDebugList)))))))) ->
      Just LedgerBody
    ShelleyLedgerEventBBODY (ShelleyInAlonzoEvent (Shelley.LedgersEvent (Shelley.LedgerEvent (Shelley.UtxowEvent (WrappedShelleyEraEvent (Shelley.UtxoEvent (UtxosEvent (Alonzo.FailedPlutusScriptsEvent _plutusDebugList)))))))) ->
      Just LedgerBody
    ShelleyLedgerEventBBODY (ShelleyInAlonzoEvent (Shelley.LedgersEvent (Shelley.LedgerEvent (Shelley.UtxowEvent (WrappedShelleyEraEvent (Shelley.UtxoEvent (UtxosEvent (Alonzo.TotalDeposits _txBodyHash _coin)))))))) ->
      Just LedgerBody

    ShelleyLedgerEventBBODY (ShelleyInAlonzoEvent (Shelley.LedgersEvent (Shelley.LedgerEvent (Shelley.DelegsEvent (Shelley.DelplEvent _))))) ->
      -- TODO Constructors of ShelleyDelplEvent (PoolEvent and DelegEvent) not exposed by
      -- cardano-ledger.
      Just LedgerBody
 where
  liftNewEpoch = Just . LedgerNewEpochEvent
  liftRewardUpdate = Just . LedgerRewardUpdateEvent

toConwayEventShelley
  :: forall era proto.
     ( EraCrypto era ~ StandardCrypto
     , Event (Ledger.EraRule "TICK" era) ~ ShelleyTickEvent era
     , Event (Ledger.EraRule "NEWEPOCH" era) ~ ConwayNewEpochEvent era
     , Event (Ledger.EraRule "EPOCH" era) ~ ConwayEpochEvent era
     , Event (Ledger.EraRule "POOLREAP" era) ~ ShelleyPoolreapEvent era
     , Event (Ledger.EraRule "RUPD" era) ~ RupdEvent (EraCrypto era)
     , Event (Ledger.EraRule "SNAP" era) ~ Shelley.SnapEvent era
     , Event (Ledger.EraRule "BBODY" era) ~ AlonzoBbodyEvent era
     , Event (Ledger.EraRule "LEDGERS" era) ~ Shelley.ShelleyLedgersEvent era
     , Event (Ledger.EraRule "LEDGER" era) ~ Conway.ConwayLedgerEvent era
     , Event (Ledger.EraRule "UTXOW" era) ~ AlonzoUtxowEvent era
     , Event (Ledger.EraRule "UTXO" era) ~ AlonzoUtxoEvent era
     , Event (Ledger.EraRule "UTXOS" era) ~ AlonzoUtxosEvent era
     -- , Event (Ledger.EraRule "PPUP" era) ~ Shelley.PpupEvent era
     -- , Event (Ledger.EraRule "DELEGS" era) ~ Conway.ConwayDelegsEvent era
     -- , Event (Ledger.EraRule "GOV" era) ~ ()
     )
  => WrapLedgerEvent (ShelleyBlock proto era)
  -> Maybe (LedgerEvent (EraCrypto era))
toConwayEventShelley evt =
  case unwrapLedgerEvent evt of
    ShelleyLedgerEventTICK (TickNewEpochEvent (Conway.TotalRewardEvent epoch rewards)) ->
      liftNewEpoch $ LedgerTotalRewards epoch rewards
    ShelleyLedgerEventTICK (TickNewEpochEvent (Conway.RestrainedRewards epoch rewards credentials)) ->
      liftNewEpoch  $ LedgerRestrainedRewards epoch rewards credentials
    ShelleyLedgerEventTICK (TickNewEpochEvent (Conway.DeltaRewardEvent (RupdEvent epoch rewards))) ->
      liftRewardUpdate $ LedgerIncrementalRewards epoch rewards
    ShelleyLedgerEventTICK (TickNewEpochEvent (Conway.EpochEvent (Conway.PoolReapEvent (RetiredPools refunded unclaimed epoch)))) ->
      liftNewEpoch $ LedgerPoolReaping epoch refunded unclaimed
    ShelleyLedgerEventTICK (TickNewEpochEvent (Conway.EpochEvent (Conway.SnapEvent (Shelley.StakeDistEvent stakeDist)))) ->
      liftNewEpoch $ LedgerStakeDistEvent stakeDist
    ShelleyLedgerEventTICK (TickNewEpochEvent (Conway.TotalAdaPotsEvent adaPots)) ->
      liftNewEpoch
        $ LedgerTotalAdaPots
            (ShelleyAPI.treasuryAdaPot adaPots)
            (ShelleyAPI.reservesAdaPot adaPots)
            (ShelleyAPI.rewardsAdaPot adaPots)
            (ShelleyAPI.utxoAdaPot adaPots)
            (ShelleyAPI.keyDepositAdaPot adaPots)
            (ShelleyAPI.poolDepositAdaPot adaPots)
            (ShelleyAPI.depositsAdaPot adaPots)
            (ShelleyAPI.feesAdaPot adaPots)
    ShelleyLedgerEventTICK (TickRupdEvent (RupdEvent epoch rewards)) ->
      liftRewardUpdate $ LedgerIncrementalRewards epoch rewards

    ShelleyLedgerEventBBODY (ShelleyInAlonzoEvent (Shelley.LedgersEvent (Shelley.LedgerEvent (Conway.UtxowEvent (WrappedShelleyEraEvent (Shelley.UtxoEvent (UtxosEvent (Alonzo.AlonzoPpupToUtxosEvent _)))))))) ->
      -- TODO Constructor missing from current cardano-ledger version
      Just LedgerBody
    ShelleyLedgerEventBBODY (ShelleyInAlonzoEvent (Shelley.LedgersEvent (Shelley.LedgerEvent (Conway.UtxowEvent (WrappedShelleyEraEvent (Shelley.UtxoEvent (UtxosEvent (Alonzo.SuccessfulPlutusScriptsEvent _plutusDebugList)))))))) ->
      Just LedgerBody
    ShelleyLedgerEventBBODY (ShelleyInAlonzoEvent (Shelley.LedgersEvent (Shelley.LedgerEvent (Conway.UtxowEvent (WrappedShelleyEraEvent (Shelley.UtxoEvent (UtxosEvent (Alonzo.FailedPlutusScriptsEvent _plutusDebugList)))))))) ->
      Just LedgerBody
    ShelleyLedgerEventBBODY (ShelleyInAlonzoEvent (Shelley.LedgersEvent (Shelley.LedgerEvent (Conway.UtxowEvent (WrappedShelleyEraEvent (Shelley.UtxoEvent (UtxosEvent (Alonzo.TotalDeposits _ _)))))))) ->
      Just LedgerBody
    ShelleyLedgerEventBBODY (ShelleyInAlonzoEvent (Shelley.LedgersEvent (Shelley.LedgerEvent (Conway.GovEvent _)))) ->
      Nothing
    ShelleyLedgerEventBBODY (ShelleyInAlonzoEvent (Shelley.LedgersEvent (Shelley.LedgerEvent (Conway.CertsEvent _)))) ->
      -- TODO Constructors of ShelleyDelplEvent (PoolEvent and CertsEvent) not exposed by cardano-ledger.
      Just LedgerBody
 where
  liftNewEpoch = Just . LedgerNewEpochEvent
  liftRewardUpdate = Just . LedgerRewardUpdateEvent

instance ConvertLedgerEvent (ShelleyBlock proto (ShelleyEra StandardCrypto)) where
  toLedgerEvent = toLedgerEventShelley

instance ConvertLedgerEvent (ShelleyBlock proto (AllegraEra StandardCrypto)) where
  toLedgerEvent = toAllegraEventShelley

instance ConvertLedgerEvent (ShelleyBlock proto (MaryEra StandardCrypto)) where
  toLedgerEvent = toAllegraEventShelley

instance ConvertLedgerEvent (ShelleyBlock proto (AlonzoEra StandardCrypto)) where
  toLedgerEvent = toAlonzoEventShelley

instance ConvertLedgerEvent (ShelleyBlock proto (BabbageEra StandardCrypto)) where
  toLedgerEvent = toAlonzoEventShelley

instance ConvertLedgerEvent (ShelleyBlock proto (ConwayEra StandardCrypto)) where
  toLedgerEvent = toConwayEventShelley

eventCodecVersion ::
     forall crypto. Crypto crypto
  => OneEraLedgerEvent (CardanoEras crypto)
  -> Version
eventCodecVersion = \case
  OneEraLedgerEvent (          S(Z{})     ) -> eraProtVerLow @(ShelleyEra crypto)
  OneEraLedgerEvent (        S(S(Z{}))    ) -> eraProtVerLow @(AllegraEra crypto)
  OneEraLedgerEvent (      S(S(S(Z{})))   ) -> eraProtVerLow @(MaryEra crypto)
  OneEraLedgerEvent (    S(S(S(S(Z{}))))  ) -> eraProtVerLow @(AlonzoEra crypto)
  OneEraLedgerEvent (  S(S(S(S(S(Z{}))))) ) -> eraProtVerLow @(BabbageEra crypto)
  OneEraLedgerEvent (S(S(S(S(S(S(Z{}))))))) -> eraProtVerLow @(ConwayEra crypto)

data AnchoredEvents =
  AnchoredEvents
    { prevBlockHeaderHash :: !(WithOrigin ShortByteString)
    , blockHeaderHash :: !ShortByteString
    , slotNo :: !SlotNo
    , blockNo :: !BlockNo
    , ledgerEvents :: !(NonEmpty (LedgerEvent StandardCrypto))
    }
  deriving (Eq, Show)

instance EncCBOR AnchoredEvents where
  encCBOR AnchoredEvents{prevBlockHeaderHash, blockHeaderHash , slotNo, blockNo, ledgerEvents} =
    encode $ Rec AnchoredEvents
      !> To prevBlockHeaderHash
      !> To blockHeaderHash
      !> To slotNo
      !> To blockNo
      !> To ledgerEvents

instance DecCBOR AnchoredEvents where
  decCBOR =
    decode $ RecD AnchoredEvents
      <! From
      <! From
      <! From
      <! From
      <! From

data Versioned a = Versioned Version a

serializeVersioned :: EncCBOR a => Versioned a -> ByteString
serializeVersioned (Versioned version x) =
  CBOR.toStrictByteString encoding
 where
  encoding =
    CBOR.encodeListLen 2
    <>
    toCBOR version
    <>
    CBOR.encodeBytes (serialize' version x)

deserializeVersioned
  :: DecCBOR a
  => LBS.ByteString
  -> Either CBOR.DeserialiseFailure (LBS.ByteString, Versioned a)
deserializeVersioned =
  CBOR.deserialiseFromBytes decoder
 where
   decoder = do
    -- TODO: ensure map len is 2
    _ <- CBOR.decodeListLen
    version <- fromCBOR
    bytes <- CBOR.decodeBytes
    either (fail . show) (pure . Versioned version) $ decodeFull' version bytes

-- IO action to read ledger events in binary form
foldEvent
  :: Handle
  -> a
  -> (a -> AnchoredEvents -> IO a)
  -> IO a
foldEvent h st0 fn =
  LBS.hGetContents h >>= go st0
  where
    go st bytes = do
      eof <- hIsEOF h
      if eof then
        pure st
      else do
        (events, Versioned _ event) <- either (panic . show) pure $ deserializeVersioned bytes
        st' <- fn st event
        go st' events

type StandardLedgerEventHandler = LedgerEventHandler IO (LedgerState StandardCrypto) (HardForkBlock (CardanoEras StandardCrypto))

withLedgerEventsServerStream
  :: PortNumber
  -> (StandardLedgerEventHandler -> IO ())
  -> IO ()
withLedgerEventsServerStream port action = do
  withSocketsDo $ do
    bracket open closeSockets go
 where
  go s = do
    h <- socketToHandle s WriteMode
    action $ mkLedgerEventHandler (writeAnchoredEvents h)

  open = do
    sock <- socket AF_INET Stream defaultProtocol
    bind sock (SockAddrInet port 0)
    listen sock 1
    putStrLn ("Waiting for client to connect to socket..." :: String)
    (clientSock, _) <- accept sock
    pure clientSock

  closeSockets = close

  writeAnchoredEvents h = handle errHandler . BS.hPut h . serializeVersioned
    where
      errHandler = \case
        -- If the client closes the socket, we continue running the node, but ignore the events.
        IOError { ioe_type = ResourceVanished } -> do
          pure ()
        err -> do
          print err
          throwIO err

withLedgerEventsChan
  :: (LedgerEventsWriter -> LedgerEventsReader -> IO a)
  -> IO a
withLedgerEventsChan action = do
  chan <- newTChanIO
  action (atomically . writeTChan chan) (atomically $ readTChan chan)

type LedgerEventsWriter = Versioned AnchoredEvents -> IO ()
type LedgerEventsReader = IO (Versioned AnchoredEvents)

mkLedgerEventHandler
  :: LedgerEventsWriter
  -> StandardLedgerEventHandler
mkLedgerEventHandler writer =
  LedgerEventHandler $ \p h s b -> traverse_ writer . mkVersionedAnchoredEvents p h s b

mkVersionedAnchoredEvents
  :: ChainHash (HardForkBlock (CardanoEras StandardCrypto))
  -> HeaderHash (HardForkBlock (CardanoEras StandardCrypto))
  -> SlotNo
  -> BlockNo
  -> [AuxLedgerEvent (LedgerState StandardCrypto)]
  -> [Versioned AnchoredEvents]
mkVersionedAnchoredEvents prevHash headerHash slotNo blockNo auxEvents =
  [ Versioned version $
      AnchoredEvents
        (getOneEraHash <$> chainHashToOriginHash prevHash)
        (getOneEraHash headerHash)
        slotNo
        blockNo
        ledgerEvents
  | (version, ledgerEvents) <- versionedGroups
  ]
  where
    chainHashToOriginHash :: ChainHash b -> WithOrigin (HeaderHash b)
    chainHashToOriginHash GenesisHash = Origin
    chainHashToOriginHash (BlockHash bh) = At bh
    versionedEvents = mapMaybe (sequence . (eventCodecVersion &&& fromAuxLedgerEvent)) auxEvents
    versionedGroups = map (first NE.head . NE.unzip) . NE.groupBy ((==) `on` fst) $ versionedEvents
