{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DisambiguateRecordFields   #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.Api.Orphans () where

import           Cardano.Ledger.Alonzo.Rules.Bbody (AlonzoBbodyPredFail)
import           Cardano.Ledger.Alonzo.Rules.Utxow (UtxowPredicateFail (..))
import           Cardano.Ledger.BaseTypes (strictMaybeToMaybe)
import           Cardano.Ledger.Chain
import           Cardano.Ledger.Compactible (Compactible (fromCompact))
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Ledger.Shelley.API hiding (ShelleyBasedEra)
import           Cardano.Ledger.Shelley.PParams (PParamsUpdate)
import           Cardano.Ledger.Shelley.Rules.Bbody
import           Cardano.Ledger.Shelley.Rules.Deleg
import           Cardano.Ledger.Shelley.Rules.Delegs
import           Cardano.Ledger.Shelley.Rules.Delpl
import           Cardano.Ledger.Shelley.Rules.Epoch
import           Cardano.Ledger.Shelley.Rules.Ledger
import           Cardano.Ledger.Shelley.Rules.Ledgers
import           Cardano.Ledger.Shelley.Rules.Mir
import           Cardano.Ledger.Shelley.Rules.NewEpoch
import           Cardano.Ledger.Shelley.Rules.Newpp
import           Cardano.Ledger.Shelley.Rules.Pool
import           Cardano.Ledger.Shelley.Rules.PoolReap
import           Cardano.Ledger.Shelley.Rules.Ppup
import           Cardano.Ledger.Shelley.Rules.Rupd
import           Cardano.Ledger.Shelley.Rules.Snap
import           Cardano.Ledger.Shelley.Rules.Tick
import           Cardano.Ledger.Shelley.Rules.Upec
import           Cardano.Ledger.Shelley.Rules.Utxo
import           Cardano.Ledger.Shelley.Rules.Utxow
import           Cardano.Ledger.UnifiedMap (UnifiedMap)
import           Cardano.Prelude
import           Cardano.Protocol.TPraos.API (ChainTransitionError (ChainTransitionError))
import           Cardano.Protocol.TPraos.BHeader (LastAppliedBlock, labBlockNo)
import           Cardano.Protocol.TPraos.OCert (KESPeriod (KESPeriod))
import           Cardano.Protocol.TPraos.Rules.OCert
import           Cardano.Protocol.TPraos.Rules.Overlay
import           Cardano.Protocol.TPraos.Rules.Prtcl
import           Cardano.Protocol.TPraos.Rules.Tickn
import           Cardano.Protocol.TPraos.Rules.Updn
import           Cardano.Slotting.Block (BlockNo (..))
import           Cardano.Slotting.Slot (SlotNo (..))
import           Cardano.Slotting.Time (SystemStart (..))
import           Data.Aeson (FromJSON (..), ToJSON (..), Value(..), object, (.=))
import           Data.Aeson.Types (Pair, ToJSONKey (..), toJSONKeyText)
import           Data.BiMap (BiMap (..), Bimap)
import           Data.Compact.VMap (VB, VMap, VP)
import           Data.UMap (Trip (Triple), UMap (UnifiedMap))
import           Ouroboros.Consensus.Ledger.SupportsMempool (txId)
import           Ouroboros.Consensus.Protocol.TPraos (TPraosCannotForge (..))
import           Ouroboros.Consensus.Shelley.Eras as Consensus (StandardAlonzo)
import           Ouroboros.Consensus.Shelley.Ledger hiding (TxId)
import           Ouroboros.Consensus.Shelley.Ledger.Inspect
import           Ouroboros.Consensus.Util.Condense (condense)
import           Ouroboros.Network.Block (blockHash, blockNo, blockSlot)
import           Ouroboros.Network.Point (WithOrigin, withOriginToMaybe)
import           Prelude hiding ((.), map, show)

import qualified Cardano.Api.Address as Api
import qualified Cardano.Api.Certificate as Api
import qualified Cardano.Api.Script as Api
import qualified Cardano.Api.SerialiseRaw as Api
import qualified Cardano.Api.SerialiseTextEnvelope as Api
import qualified Cardano.Api.TxBody as Api
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Crypto.VRF.Class as Crypto
import qualified Cardano.Ledger.Alonzo as Alonzo
import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.PlutusScriptApi as Alonzo
import qualified Cardano.Ledger.Alonzo.Rules.Utxo as Alonzo
import qualified Cardano.Ledger.Alonzo.Rules.Utxos as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxInfo as Alonzo
import qualified Cardano.Ledger.AuxiliaryData as Core
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Coin as Shelley
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Crypto as Core
import qualified Cardano.Ledger.Crypto as Crypto
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.Hashes as Ledger
import qualified Cardano.Ledger.Mary.Value as Mary
import qualified Cardano.Ledger.PoolDistr as Ledger
import qualified Cardano.Ledger.SafeHash as SafeHash
import qualified Cardano.Ledger.Shelley.API as Shelley
import qualified Cardano.Ledger.Shelley.Constraints as Shelley
import qualified Cardano.Ledger.Shelley.EpochBoundary as ShelleyEpoch
import qualified Cardano.Ledger.Shelley.LedgerState as ShelleyLedger
import qualified Cardano.Ledger.Shelley.PoolRank as Shelley
import qualified Cardano.Ledger.Shelley.Rewards as Shelley
import qualified Cardano.Ledger.Shelley.RewardUpdate as Shelley
import qualified Cardano.Ledger.ShelleyMA.Rules.Utxo as MA
import qualified Cardano.Ledger.ShelleyMA.Timelocks as MA
import qualified Cardano.Prelude as CP
import qualified Cardano.Protocol.TPraos.BHeader as Protocol
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Compact.VMap as VMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as Consensus
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as SupportsMempool
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
import qualified Ouroboros.Consensus.Shelley.Eras as Consensus
import qualified Plutus.V1.Ledger.Api as Plutus
import qualified Plutus.V1.Ledger.Api as PV1
import qualified PlutusCore
import qualified PlutusCore.Core as Plutus
import qualified PlutusCore.DeBruijn
import qualified PlutusCore.Evaluation.Machine.ExBudget as Cek
import qualified PlutusCore.Evaluation.Machine.Exception
import qualified UntypedPlutusCore.Core.Type
import qualified UntypedPlutusCore.Evaluation.Machine.Cek.Internal as Cek

-- Orphan instances involved in the JSON output of the API queries.
-- We will remove/replace these as we provide more API wrapper types

-- deriving newtype instance ToJSON (Core.AuxiliaryDataHash StandardCrypto)
deriving newtype instance Core.Crypto crypto => ToJSON (Core.AuxiliaryDataHash crypto)

instance ToJSON (Mary.Value era) where
  toJSON (Mary.Value l ps) =
    object
      [ "lovelace" .= toJSON l
      , "policies" .= toJSON ps
      ]

instance ToJSONKey Mary.AssetName where
  toJSONKey = toJSONKeyText render
    where
      render = Text.decodeLatin1 . B16.encode . Mary.assetName

instance ToJSON (Mary.PolicyID era) where
  toJSON (Mary.PolicyID (Shelley.ScriptHash h)) = Aeson.String (hashToText h)

instance ToJSONKey (Mary.PolicyID era) where
  toJSONKey = toJSONKeyText render
    where
      render (Mary.PolicyID (Shelley.ScriptHash h)) = hashToText h

instance ToJSON Mary.AssetName where
  toJSON = Aeson.String . Text.decodeLatin1 . B16.encode . Mary.assetName

instance ToJSON Shelley.AccountState where
  toJSON (Shelley.AccountState tr rs) = object [ "treasury" .= tr
                                               , "reserves" .= rs
                                               ]

instance ( Consensus.ShelleyBasedEra era
         , ToJSON (Core.TxOut era)
         , ToJSON (Core.PParams era)
         , ToJSON (Core.PParamsDelta era)
         ) => ToJSON (Shelley.EpochState era) where
  toJSON eState = object [ "esAccountState" .= Shelley.esAccountState eState
                         , "esSnapshots" .= Shelley.esSnapshots eState
                         , "esLState" .= Shelley.esLState eState
                         , "esPrevPp" .= Shelley.esPrevPp eState
                         , "esPp" .= Shelley.esPp eState
                         , "esNonMyopic" .= Shelley.esNonMyopic eState
                         ]

instance ( Consensus.ShelleyBasedEra era
         , ToJSON (Core.TxOut era)
         , ToJSON (Core.PParamsDelta era)
         ) => ToJSON (Shelley.LedgerState era) where
  toJSON lState = object [ "utxoState" .= Shelley.lsUTxOState lState
                         , "delegationState" .= Shelley.lsDPState lState
                         ]

instance Crypto.Crypto crypto => ToJSON (ShelleyLedger.IncrementalStake crypto) where
  toJSON iStake = object [ "credentials" .= Map.toList (ShelleyLedger.credMap iStake)
                         , "pointers" .= Map.toList (ShelleyLedger.ptrMap iStake)
                         ]

instance ( Consensus.ShelleyBasedEra era
         , ToJSON (Core.TxOut era)
         , ToJSON (Core.PParamsDelta era)
         ) => ToJSON (Shelley.UTxOState era) where
  toJSON utxoState = object [ "utxo" .= Shelley._utxo utxoState
                            , "deposited" .= Shelley._deposited utxoState
                            , "fees" .= Shelley._fees utxoState
                            , "ppups" .= Shelley._ppups utxoState
                            , "stake" .= Shelley._stakeDistro utxoState
                            ]

instance ( ToJSON (Core.PParamsDelta era)
         , Shelley.UsesPParams era
         ) => ToJSON (Shelley.PPUPState era) where
  toJSON ppUpState = object [ "proposals" .= Shelley.proposals ppUpState
                            , "futureProposals" .= Shelley.futureProposals ppUpState
                            ]

instance ( ToJSON (Core.PParamsDelta era)
         , Shelley.UsesPParams era
         ) => ToJSON (Shelley.ProposedPPUpdates era) where
  toJSON (Shelley.ProposedPPUpdates ppUpdates) = toJSON $ Map.toList ppUpdates

instance ToJSON (PParamsUpdate era) where
  toJSON pp =
    Aeson.object $
        [ "minFeeA"               .= x | x <- mbfield (Shelley._minfeeA pp) ]
     ++ [ "minFeeB"               .= x | x <- mbfield (Shelley._minfeeB pp) ]
     ++ [ "maxBlockBodySize"      .= x | x <- mbfield (Shelley._maxBBSize pp) ]
     ++ [ "maxTxSize"             .= x | x <- mbfield (Shelley._maxTxSize pp) ]
     ++ [ "maxBlockHeaderSize"    .= x | x <- mbfield (Shelley._maxBHSize pp) ]
     ++ [ "keyDeposit"            .= x | x <- mbfield (Shelley._keyDeposit pp) ]
     ++ [ "poolDeposit"           .= x | x <- mbfield (Shelley._poolDeposit pp) ]
     ++ [ "eMax"                  .= x | x <- mbfield (Shelley._eMax pp) ]
     ++ [ "nOpt"                  .= x | x <- mbfield (Shelley._nOpt pp) ]
     ++ [ "a0"                    .= x | x <- mbfield (Shelley._a0 pp) ]
     ++ [ "rho"                   .= x | x <- mbfield (Shelley._rho pp) ]
     ++ [ "tau"                   .= x | x <- mbfield (Shelley._tau pp) ]
     ++ [ "decentralisationParam" .= x | x <- mbfield (Shelley._d pp) ]
     ++ [ "extraEntropy"          .= x | x <- mbfield (Shelley._extraEntropy pp) ]
     ++ [ "protocolVersion"       .= x | x <- mbfield (Shelley._protocolVersion pp) ]
     ++ [ "minUTxOValue"          .= x | x <- mbfield (Shelley._minUTxOValue pp) ]
     ++ [ "minPoolCost"           .= x | x <- mbfield (Shelley._minPoolCost pp) ]
    where
      mbfield SNothing  = []
      mbfield (SJust x) = [x]

instance Crypto.Crypto crypto => ToJSON (Shelley.DPState crypto) where
  toJSON dpState = object [ "dstate" .= Shelley.dpsDState dpState
                          , "pstate" .= Shelley.dpsPState dpState
                          ]

instance (ToJSON coin, ToJSON ptr, ToJSON pool) => ToJSON (Trip coin ptr pool) where
  toJSON (Triple coin ptr pool) = object
    [ "coin" .= coin
    , "ptr" .= ptr
    , "pool" .= pool
    ]
instance Crypto.Crypto crypto => ToJSON (UnifiedMap crypto) where
  toJSON (UnifiedMap m1 m2) = object
    [ "credentials" .= m1
    , "pointers" .= m2
    ]

instance Crypto.Crypto crypto => ToJSON (Shelley.DState crypto) where
  toJSON dState = object [ "unifiedRewards" .= Shelley._unified dState
                         , "fGenDelegs" .= Map.toList (Shelley._fGenDelegs dState)
                         , "genDelegs" .= Shelley._genDelegs dState
                         , "irwd" .= Shelley._irwd dState
                         ]

instance Crypto.Crypto crypto => ToJSON (ShelleyLedger.FutureGenDeleg crypto) where
  toJSON fGenDeleg =
    object [ "fGenDelegSlot" .= ShelleyLedger.fGenDelegSlot fGenDeleg
           , "fGenDelegGenKeyHash" .= ShelleyLedger.fGenDelegGenKeyHash fGenDeleg
           ]

instance Crypto.Crypto crypto => ToJSON (Shelley.GenDelegs crypto) where
  toJSON (Shelley.GenDelegs delegs) = toJSON delegs

instance Crypto.Crypto crypto => ToJSON (Shelley.InstantaneousRewards crypto) where
  toJSON iRwds = object [ "iRReserves" .= Shelley.iRReserves iRwds
                        , "iRTreasury" .= Shelley.iRTreasury iRwds
                        ]

instance
  Crypto.Crypto crypto =>
  ToJSON (Bimap Shelley.Ptr (Shelley.Credential Shelley.Staking crypto))
  where
  toJSON (MkBiMap ptsStakeM stakePtrSetM) =
    object [ "stakedCreds" .= Map.toList ptsStakeM
           , "credPtrR" .= toJSON stakePtrSetM
           ]

deriving newtype instance ToJSON Shelley.CertIx
deriving newtype instance ToJSON Shelley.TxIx

instance ToJSON Shelley.Ptr where
  toJSON (Shelley.Ptr slotNo txIndex certIndex) =
    object [ "slot" .= unSlotNo slotNo
           , "txIndex" .= txIndex
           , "certIndex" .= certIndex
           ]
instance ToJSONKey Shelley.Ptr


instance Crypto.Crypto crypto => ToJSON (Shelley.PState crypto) where
  toJSON pState = object [ "pParams pState" .= Shelley._pParams pState
                         , "fPParams pState" .= Shelley._fPParams pState
                         , "retiring pState" .= Shelley._retiring pState
                         ]

instance ( Consensus.ShelleyBasedEra era
         , ToJSON (Core.TxOut era)
         ) => ToJSON (Shelley.UTxO era) where
  toJSON (Shelley.UTxO utxo) = toJSON utxo

instance ( Consensus.ShelleyBasedEra era
         , ToJSON (Core.Value era)
         ) => ToJSON (Shelley.TxOut era) where
  toJSON (Shelley.TxOut addr amount) =
    object
      [ "address" .= addr
      , "amount" .= amount
      ]

instance Crypto.Crypto crypto => ToJSON (Shelley.TxIn crypto) where
  toJSON = toJSON . txInToText

instance Crypto.Crypto crypto => ToJSONKey (Shelley.TxIn crypto) where
  toJSONKey = toJSONKeyText txInToText

txInToText :: Shelley.TxIn crypto -> Text
txInToText (Shelley.TxIn (Shelley.TxId txidHash) ix) =
  hashToText (SafeHash.extractHash txidHash)
    <> Text.pack "#"
    <> Text.pack (show ix)

hashToText :: Crypto.Hash crypto a -> Text
hashToText = Text.decodeLatin1 . Crypto.hashToBytesAsHex

instance Crypto.Crypto crypto => ToJSON (Shelley.NonMyopic crypto) where
  toJSON nonMy = object [ "likelihoodsNM" .= Shelley.likelihoodsNM nonMy
                        , "rewardPotNM" .= Shelley.rewardPotNM nonMy
                        ]

instance ToJSON Shelley.Likelihood where
  toJSON (Shelley.Likelihood llhd) =
    toJSON $ fmap (\(Shelley.LogWeight f) -> exp $ realToFrac f :: Double) llhd

instance Crypto.Crypto crypto => ToJSON (Shelley.SnapShots crypto) where
  toJSON ss = object [ "pstakeMark" .= Shelley._pstakeMark ss
                     , "pstakeSet" .= Shelley._pstakeSet ss
                     , "pstakeGo" .= Shelley._pstakeGo ss
                     , "feeSS" .= Shelley._feeSS ss
                     ]

instance Crypto.Crypto crypto => ToJSON (Shelley.SnapShot crypto) where
  toJSON ss = object [ "stake" .= Shelley._stake ss
                     , "delegations" .= ShelleyEpoch._delegations ss
                     , "poolParams" .= Shelley._poolParams ss
                     ]

instance Crypto.Crypto crypto => ToJSON (Shelley.Stake crypto) where
  toJSON (Shelley.Stake s) = toJSON s

instance Crypto.Crypto crypto => ToJSON (Shelley.RewardUpdate crypto) where
  toJSON rUpdate = object [ "deltaT" .= Shelley.deltaT rUpdate
                          , "deltaR" .= Shelley.deltaR rUpdate
                          , "rs" .= Shelley.rs rUpdate
                          , "deltaF" .= Shelley.deltaF rUpdate
                          , "nonMyopic" .= Shelley.nonMyopic rUpdate
                          ]

instance Crypto.Crypto crypto => ToJSON (Shelley.PulsingRewUpdate crypto) where
  toJSON (Shelley.Pulsing _ _) = Aeson.Null
  toJSON (Shelley.Complete ru) = toJSON ru

instance ToJSON Shelley.DeltaCoin where
  toJSON (Shelley.DeltaCoin i) = toJSON i

instance Crypto.Crypto crypto => ToJSON (Ledger.PoolDistr crypto) where
  toJSON (Ledger.PoolDistr m) = toJSON m

instance Crypto.Crypto crypto => ToJSON (Ledger.IndividualPoolStake crypto) where
  toJSON indivPoolStake =
    object [ "individualPoolStake" .= Ledger.individualPoolStake indivPoolStake
           , "individualPoolStakeVrf" .= Ledger.individualPoolStakeVrf indivPoolStake
           ]

instance Crypto.Crypto crypto => ToJSON (Shelley.Reward crypto) where
  toJSON reward =
     object [ "rewardType" .= Shelley.rewardType reward
            , "rewardPool" .= Shelley.rewardPool reward
            , "rewardAmount" .= Shelley.rewardAmount reward
            ]

instance ToJSON Shelley.RewardType where
  toJSON Shelley.MemberReward = "MemberReward"
  toJSON Shelley.LeaderReward = "LeaderReward"

instance Crypto.Crypto c => ToJSON (SafeHash.SafeHash c a) where
  toJSON = toJSON . SafeHash.extractHash

-----

deriving newtype instance ToJSON SystemStart
deriving newtype instance FromJSON SystemStart


instance Crypto.Crypto crypto => ToJSON (VMap VB VB (Shelley.Credential 'Shelley.Staking crypto) (Shelley.KeyHash 'Shelley.StakePool crypto)) where
  toJSON = toJSON . VMap.toMap

instance Crypto.Crypto crypto => ToJSON (VMap VB VB (Shelley.KeyHash    'Shelley.StakePool crypto) (Shelley.PoolParams crypto)) where
  toJSON = toJSON . VMap.toMap

instance Crypto.Crypto crypto => ToJSON (VMap VB VP (Shelley.Credential 'Shelley.Staking   crypto) (Shelley.CompactForm Shelley.Coin)) where
  toJSON = toJSON . fmap fromCompact . VMap.toMap

-----

instance ToJSON (PredicateFailure (Core.EraRule "LEDGER" era)) => ToJSON (ApplyTxError era) where
  toJSON (ApplyTxError es) = toJSON es

instance
  ( ShelleyBasedEra era
  , ToJSON (Core.Tx era)
  , ToJSON (TxId (Ledger.Crypto era))
  ) => ToJSON (GenTx (ShelleyBlock era)) where
  toJSON tx = object [ "txid" .= Text.take 8 (renderTxId (txId tx)) ]

instance ToJSON (SupportsMempool.TxId (GenTx (ShelleyBlock era))) where
  toJSON = String . Text.take 8 . renderTxId

instance
  ( ShelleyBasedEra era
  , ToJSON (ShelleyHash (Ledger.Crypto era))
  , ToJSON (Protocol.BHeader (Ledger.Crypto era))
  ) => ToJSON (Header (ShelleyBlock era)) where
  toJSON b = object
    [ "kind"      .= String "ShelleyBlock"
    , "hash"      .= condense (blockHash b)
    , "slotNo"    .= condense (blockSlot b)
    , "blockNo"   .= condense (blockNo b)
    -- , "delegate"  .= condense (headerSignerVk h)
    ]

instance Core.Crypto crypto => ToJSON (TPraosCannotForge crypto) where
  toJSON (TPraosCannotForgeKeyNotUsableYet wallClockPeriod keyStartPeriod) = object
    [ "kind"      .= String "TPraosCannotForgeKeyNotUsableYet"
    , "keyStart"  .= keyStartPeriod
    , "wallClock" .= wallClockPeriod
    ]
  toJSON (TPraosCannotForgeWrongVRF genDlgVRFHash coreNodeVRFHash) = object
    [ "kind"      .= String "TPraosCannotLeadWrongVRF"
    , "expected"  .= genDlgVRFHash
    , "actual"    .= coreNodeVRFHash
    ]

deriving newtype instance ToJSON KESPeriod

instance ToJSON HotKey.KESInfo where
  toJSON HotKey.KESInfo { kesStartPeriod, kesEndPeriod, kesEvolution } = object
    [ "kind"        .= String "KESInfo"
    , "startPeriod" .= kesStartPeriod
    , "endPeriod"   .= kesEndPeriod
    , "evolution"   .= kesEvolution
    ]

instance ToJSON HotKey.KESEvolutionError where
  toJSON (HotKey.KESCouldNotEvolve kesInfo targetPeriod) = object
    [ "kind"          .= String "KESCouldNotEvolve"
    , "kesInfo"       .= toJSON kesInfo
    , "targetPeriod"  .= targetPeriod
    ]
  toJSON (HotKey.KESKeyAlreadyPoisoned kesInfo targetPeriod) = object
    [ "kind"          .= String "KESKeyAlreadyPoisoned"
    , "kesInfo"       .= toJSON kesInfo
    , "targetPeriod"  .= targetPeriod
    ]

instance
  ( ShelleyBasedEra era
  , ToJSON (PredicateFailure (UTXO era))
  , ToJSON (PredicateFailure (UTXOW era))
  , ToJSON (PredicateFailure (Core.EraRule "BBODY" era))
  , ToJSON (BlockTransitionError era)
  ) => ToJSON (ShelleyLedgerError era) where
  toJSON (BBodyError (BlockTransitionError fs)) = object
    [ "kind"      .= String "BBodyError"
    , "failures"  .= map toJSON fs
    ]

instance
  ( ShelleyBasedEra era
  , ToJSON (Ledger.PParamsDelta era)
  , ToJSON (Crypto.OutputVRF (Core.VRF (Ledger.Crypto era)))
  ) => ToJSON (ShelleyLedgerUpdate era) where
  toJSON (ShelleyUpdatedProtocolUpdates updates) = object
    [ "kind"    .= String "ShelleyUpdatedProtocolUpdates"
    , "updates" .= map toJSON updates
    ]

instance
  ( Ledger.Era era, ToJSON (Ledger.PParamsDelta era)
  , ToJSON (Crypto.OutputVRF (Core.VRF (Ledger.Crypto era)))
  ) => ToJSON (ProtocolUpdate era) where
  toJSON ProtocolUpdate{protocolUpdateProposal, protocolUpdateState} = object
    [ "proposal" .= toJSON protocolUpdateProposal
    , "state"    .= toJSON protocolUpdateState
    ]

instance ToJSON (Ledger.PParamsDelta era)
         => ToJSON (UpdateProposal era) where
  toJSON UpdateProposal{proposalParams, proposalVersion, proposalEpoch} = object
    [ "params"  .= proposalParams
    , "version" .= proposalVersion
    , "epoch"   .= proposalEpoch
    ]

instance
  ( Core.Crypto crypto
  , ToJSON (Crypto.OutputVRF (Core.VRF crypto))
  ) => ToJSON (UpdateState crypto) where
  toJSON UpdateState{proposalVotes, proposalReachedQuorum} = object
    [ "proposal"      .= proposalVotes
    , "reachedQuorum" .= proposalReachedQuorum
    ]

instance
  ( Core.Crypto crypto
  , ToJSON (Crypto.CertifiedVRF (Core.VRF crypto) Nonce)
  , ToJSON (Crypto.OutputVRF (Core.VRF crypto))
  , ToJSON Ledger.ActiveSlotCoeff
  ) => ToJSON (ChainTransitionError crypto) where
  toJSON (ChainTransitionError fs) = object
    [ "kind"      .= String "ChainTransitionError"
    , "failures"  .= map toJSON fs
    ]

instance ToJSON ChainPredicateFailure where
  toJSON (HeaderSizeTooLargeCHAIN hdrSz maxHdrSz) = object
    [ "kind"          .= String "HeaderSizeTooLarge"
    , "headerSize"    .= hdrSz
    , "maxHeaderSize" .= maxHdrSz
    ]
  toJSON (BlockSizeTooLargeCHAIN blkSz maxBlkSz) = object
    [ "kind"          .= String "BlockSizeTooLarge"
    , "blockSize"     .= blkSz
    , "maxBlockSize"  .= maxBlkSz
    ]
  toJSON (ObsoleteNodeCHAIN currentPtcl supportedPtcl) = object
    [ "kind"              .= String "ObsoleteNode"
    , "explanation"       .= String explanation
    , "currentProtocol"   .= currentPtcl
    , "supportedProtocol" .= supportedPtcl
    ]
      where
        explanation = "A scheduled major protocol version change (hard fork) \
                      \has taken place on the chain, but this node does not \
                      \understand the new major protocol version. This node \
                      \must be upgraded before it can continue with the new \
                      \protocol version."

instance
  ( ToJSON (Protocol.PrevHash crypto)
  , ToJSON (WithOrigin (LastAppliedBlock crypto))
  , ToJSON BlockNo
  ) => ToJSON (PrtlSeqFailure crypto) where
  toJSON (WrongSlotIntervalPrtclSeq (SlotNo lastSlot) (SlotNo currSlot)) = object
    [ "kind"        .= String "WrongSlotInterval"
    , "lastSlot"    .= lastSlot
    , "currentSlot" .= currSlot
    ]
  toJSON (WrongBlockNoPrtclSeq lab currentBlockNo) = object
    [ "kind"                .= String "WrongBlockNo"
    , "lastAppliedBlockNo"  .= showLastAppBlockNo lab
    , "currentBlockNo"      .= (String . textShow $ unBlockNo currentBlockNo)
    ]
  toJSON (WrongBlockSequencePrtclSeq lastAppliedHash currentHash) = object
    [ "kind"                  .= String "WrongBlockSequence"
    , "lastAppliedBlockHash"  .= String (textShow lastAppliedHash)
    , "currentBlockHash"      .= String (textShow currentHash)
    ]

instance
  ( ShelleyBasedEra era
  , ToJSON (PredicateFailure (UTXO era))
  , ToJSON (PredicateFailure (UTXOW era))
  , ToJSON (PredicateFailure (Core.EraRule "LEDGER" era))
  , ToJSON (PredicateFailure (Core.EraRule "LEDGERS" era))
  ) => ToJSON (BbodyPredicateFailure era) where
  toJSON (WrongBlockBodySizeBBODY actualBodySz claimedBodySz) = object
    [ "kind"                  .= String "WrongBlockBodySizeBBODY"
    , "actualBlockBodySize"   .= actualBodySz
    , "claimedBlockBodySize"  .= claimedBodySz
    ]
  toJSON (InvalidBodyHashBBODY actualHash claimedHash) = object
    [ "kind"            .= String "InvalidBodyHashBBODY"
    , "actualBodyHash"  .= textShow actualHash
    , "claimedBodyHash" .= textShow claimedHash
    ]
  toJSON (LedgersFailure f) = toJSON f


instance
  ( ShelleyBasedEra era
  , ToJSON (PredicateFailure (UTXO era))
  , ToJSON (PredicateFailure (UTXOW era))
  , ToJSON (PredicateFailure (Core.EraRule "LEDGER" era))
  ) => ToJSON (LedgersPredicateFailure era) where
  toJSON (LedgerFailure f) = object
    [ "kind"  .= String "LedgerFailure"
    , "value" .= toJSON f
    ]

instance
  ( ShelleyBasedEra era
  , ToJSON (PredicateFailure (UTXO era))
  , ToJSON (PredicateFailure (UTXOW era))
  , ToJSON (PredicateFailure (Core.EraRule "DELEGS" era))
  , ToJSON (PredicateFailure (Core.EraRule "UTXOW" era))
  ) => ToJSON (LedgerPredicateFailure era) where
  toJSON (UtxowFailure f) = object
    [ "kind"  .= String "UtxowFailure"
    , "value" .= toJSON f
    ]
  toJSON (DelegsFailure f) = object
    [ "kind"  .= String "UtxowFailure"
    , "value" .= toJSON f
    ]

instance
  ( ToJSON (Core.AuxiliaryDataHash StandardCrypto)
  ) => ToJSON (UtxowPredicateFail (Alonzo.AlonzoEra StandardCrypto)) where
  toJSON (WrappedShelleyEraFailure utxoPredFail) = toJSON utxoPredFail
  toJSON (MissingRedeemers scripts) = object
    [ "kind"    .= String "MissingRedeemers"
    , "scripts" .= renderMissingRedeemers scripts
    ]
  toJSON (MissingRequiredDatums required received) = object
    [ "kind"      .= String "MissingRequiredDatums"
    , "required"  .= map (Crypto.hashToTextAsHex . SafeHash.extractHash) (Set.toList required)
    , "received"  .= map (Crypto.hashToTextAsHex . SafeHash.extractHash) (Set.toList received)
    ]
  toJSON (PPViewHashesDontMatch ppHashInTxBody ppHashFromPParams) = object
    [ "kind"        .= String "PPViewHashesDontMatch"
    , "fromTxBody"  .= renderScriptIntegrityHash (strictMaybeToMaybe ppHashInTxBody)
    , "fromPParams" .= renderScriptIntegrityHash (strictMaybeToMaybe ppHashFromPParams)
    ]
  toJSON (MissingRequiredSigners missingKeyWitnesses) = object
    [ "kind"      .= String "MissingRequiredSigners"
    , "witnesses" .= Set.toList missingKeyWitnesses
    ]
  toJSON (UnspendableUTxONoDatumHash txins) = object
    [ "kind"  .= String "MissingRequiredSigners"
    , "txins" .= Set.toList txins
    ]
  toJSON (NonOutputSupplimentaryDatums disallowed acceptable) = object
    [ "kind"        .= String "NonOutputSupplimentaryDatums"
    , "disallowed"  .= Set.toList disallowed
    , "acceptable"  .= Set.toList acceptable
    ]
  toJSON (ExtraRedeemers rdmrs) = object
    [ "kind"  .= String "ExtraRedeemers"
    , "rdmrs" .= map (Api.renderScriptWitnessIndex . Api.fromAlonzoRdmrPtr) rdmrs
    ]

instance
  ( ShelleyBasedEra era
  , ToJSON (PredicateFailure (UTXO era))
  , ToJSON (PredicateFailure (Core.EraRule "UTXO" era))
  , ToJSON (Core.AuxiliaryDataHash (Ledger.Crypto era))
  ) => ToJSON (UtxowPredicateFailure era) where
  toJSON (ExtraneousScriptWitnessesUTXOW extraneousScripts) = object
    [ "kind"              .= String "InvalidWitnessesUTXOW"
    , "extraneousScripts" .= extraneousScripts
    ]
  toJSON (InvalidWitnessesUTXOW wits') = object
    [ "kind"              .= String "InvalidWitnessesUTXOW"
    , "invalidWitnesses"  .= map textShow wits'
    ]
  toJSON (MissingVKeyWitnessesUTXOW (WitHashes wits')) = object
    [ "kind"              .= String "MissingVKeyWitnessesUTXOW"
    , "missingWitnesses"  .= wits'
    ]
  toJSON (MissingScriptWitnessesUTXOW missingScripts) = object
    [ "kind"            .= String "MissingScriptWitnessesUTXOW"
    , "missingScripts"  .= missingScripts
    ]
  toJSON (ScriptWitnessNotValidatingUTXOW failedScripts) = object
    [ "kind"          .= String "ScriptWitnessNotValidatingUTXOW"
    , "failedScripts" .= failedScripts
    ]
  toJSON (UtxoFailure f) = toJSON f
  toJSON (MIRInsufficientGenesisSigsUTXOW genesisSigs) = object
    [ "kind"        .= String "MIRInsufficientGenesisSigsUTXOW"
    , "genesisSigs" .= genesisSigs
    ]
  toJSON (MissingTxBodyMetadataHash metadataHash) = object
    [ "kind"          .= String "MissingTxBodyMetadataHash"
    , "metadataHash"  .= metadataHash
    ]
  toJSON (MissingTxMetadata txBodyMetadataHash) = object
    [ "kind"                .= String "MissingTxMetadata"
    , "txBodyMetadataHash"  .= txBodyMetadataHash
    ]
  toJSON (ConflictingMetadataHash txBodyMetadataHash fullMetadataHash) = object
    [ "kind"                .= String "ConflictingMetadataHash"
    , "txBodyMetadataHash"  .= txBodyMetadataHash
    , "fullMetadataHash"    .= fullMetadataHash
    ]
  toJSON InvalidMetadata = object
    [ "kind"  .= String "InvalidMetadata"
    ]

instance
  ( ShelleyBasedEra era
  , ToJSON (Core.Value era)
  , ToJSON (Core.TxOut era)
  , ToJSON (PredicateFailure (Core.EraRule "PPUP" era))
  ) => ToJSON (UtxoPredicateFailure era) where
  toJSON (BadInputsUTxO badInputs) = object
    [ "kind"      .= String "BadInputsUTxO"
    , "badInputs" .= badInputs
    , "error"     .= renderBadInputsUTxOErr badInputs
    ]
  toJSON (ExpiredUTxO ttl slot) = object
    [ "kind" .= String "ExpiredUTxO"
    , "ttl"  .= ttl
    , "slot" .= slot
    ]
  toJSON (MaxTxSizeUTxO txsize maxtxsize) = object
    [ "kind"    .= String "MaxTxSizeUTxO"
    , "size"    .= txsize
    , "maxSize" .= maxtxsize
    ]
  -- TODO: Add the minimum allowed UTxO value to OutputTooSmallUTxO
  toJSON (OutputTooSmallUTxO badOutputs) = object
    [ "kind"    .= String "OutputTooSmallUTxO"
    , "outputs" .= badOutputs
    , "error"   .= String "The output is smaller than the allow minimum UTxO value defined in the protocol parameters"
    ]
  toJSON (OutputBootAddrAttrsTooBig badOutputs) = object
    [ "kind"    .= String "OutputBootAddrAttrsTooBig"
    , "outputs" .= badOutputs
    , "error"   .= String "The Byron address attributes are too big"
    ]
  toJSON InputSetEmptyUTxO = object
    [ "kind" .= String "InputSetEmptyUTxO"
    ]
  toJSON (FeeTooSmallUTxO minfee txfee) = object
    [ "kind"    .= String "FeeTooSmallUTxO"
    , "minimum" .= minfee
    , "fee"     .= txfee
    ]
  toJSON (ValueNotConservedUTxO consumed produced) = object
    [ "kind"      .= String "ValueNotConservedUTxO"
    , "consumed"  .= consumed
    , "produced"  .= produced
    , "error"     .= renderValueNotConservedErr consumed produced
    ]
  toJSON (UpdateFailure f) = object
    [ "kind"  .= String "UpdateFailure"
    , "value" .= toJSON f
    ]
  toJSON (WrongNetwork network addrs) = object
    [ "kind"    .= String "WrongNetwork"
    , "network" .= network
    , "addrs"   .= addrs
    ]
  toJSON (WrongNetworkWithdrawal network addrs) = object
    [ "kind"    .= String "WrongNetworkWithdrawal"
    , "network" .= network
    , "addrs"   .= addrs
    ]

instance ToJSON MA.ValidityInterval where
  toJSON vi = object $
        [ "invalidBefore"    .= x | x <- mbfield (MA.invalidBefore    vi) ]
     ++ [ "invalidHereafter" .= x | x <- mbfield (MA.invalidHereafter vi) ]
    where mbfield SNothing  = []
          mbfield (SJust x) = [x]

instance ( ShelleyBasedEra era
         , ToJSON (Core.Value era)
         , ToJSON (Core.TxOut era)
         , ToJSON (PredicateFailure (Core.EraRule "PPUP" era))
         ) => ToJSON (MA.UtxoPredicateFailure era) where
  toJSON (MA.BadInputsUTxO badInputs) = object
    [ "kind"      .= String "BadInputsUTxO"
    , "badInputs" .= badInputs
    , "error"     .= renderBadInputsUTxOErr badInputs
    ]
  toJSON (MA.OutsideValidityIntervalUTxO validityInterval slot) = object
    [ "kind"              .= String "ExpiredUTxO"
    , "validityInterval"  .= validityInterval
    , "slot"              .= slot
    ]
  toJSON (MA.MaxTxSizeUTxO txsize maxtxsize) = object
    [ "kind"    .= String "MaxTxSizeUTxO"
    , "size"    .= txsize
    , "maxSize" .= maxtxsize
    ]
  toJSON MA.InputSetEmptyUTxO = object
    [ "kind"  .= String "InputSetEmptyUTxO"
    ]
  toJSON (MA.FeeTooSmallUTxO minfee txfee) = object
    [ "kind"    .= String "FeeTooSmallUTxO"
    , "minimum" .= minfee
    , "fee"     .= txfee
    ]
  toJSON (MA.ValueNotConservedUTxO consumed produced) = object
    [ "kind"      .= String "ValueNotConservedUTxO"
    , "consumed"  .= consumed
    , "produced"  .= produced
    , "error"     .= renderValueNotConservedErr consumed produced
    ]
  toJSON (MA.WrongNetwork network addrs) = object
    [ "kind"    .= String "WrongNetwork"
    , "network" .= network
    , "addrs"   .= addrs
    ]
  toJSON (MA.WrongNetworkWithdrawal network addrs) = object
    [ "kind"    .= String "WrongNetworkWithdrawal"
    , "network" .= network
    , "addrs"   .= addrs
    ]
  -- TODO: Add the minimum allowed UTxO value to OutputTooSmallUTxO
  toJSON (MA.OutputTooSmallUTxO badOutputs) = object
    [ "kind"    .= String "OutputTooSmallUTxO"
    , "outputs" .= badOutputs
    , "error"   .= String "The output is smaller than the allow minimum UTxO value defined in the protocol parameters"
    ]
  toJSON (MA.UpdateFailure f) = toJSON f
  toJSON (MA.OutputBootAddrAttrsTooBig badOutputs) = object
    [ "kind"    .= String "OutputBootAddrAttrsTooBig"
    , "outputs" .= badOutputs
    , "error"   .= String "The Byron address attributes are too big"
    ]
  toJSON MA.TriesToForgeADA = object
    [ "kind"  .= String "TriesToForgeADA"
    ]
  toJSON (MA.OutputTooBigUTxO badOutputs) = object
    [ "kind"    .= String "OutputTooBigUTxO"
    , "outputs" .= badOutputs
    , "error"   .= String "Too many asset ids in the tx output"
    ]

instance
  ( Ledger.Era era
  ) => ToJSON (PpupPredicateFailure era) where
  toJSON (NonGenesisUpdatePPUP proposalKeys genesisKeys) = object
    [ "kind"  .= String "NonGenesisUpdatePPUP"
    , "keys"  .= proposalKeys Set.\\ genesisKeys
    ]
  toJSON (PPUpdateWrongEpoch currEpoch intendedEpoch votingPeriod) = object
    [ "kind"          .= String "PPUpdateWrongEpoch"
    , "currentEpoch"  .= currEpoch
    , "intendedEpoch" .= intendedEpoch
    , "votingPeriod"  .= String (show votingPeriod)
    ]
  toJSON (PVCannotFollowPPUP badPv) = object
    [ "kind"                .= String "PVCannotFollowPPUP"
    , "badProtocolVersion" .= badPv
    ]

instance ( ShelleyBasedEra era
         , ToJSON (PredicateFailure (Core.EraRule "DELPL" era))
         ) => ToJSON (DelegsPredicateFailure era) where
  toJSON (DelegateeNotRegisteredDELEG targetPool) = object
    [ "kind"        .= String "DelegateeNotRegisteredDELEG"
    , "targetPool"  .= targetPool
    ]
  toJSON (WithdrawalsNotInRewardsDELEGS incorrectWithdrawals) = object
    [ "kind"                  .= String "WithdrawalsNotInRewardsDELEGS"
    , "incorrectWithdrawals"  .= incorrectWithdrawals
    ]
  toJSON (DelplFailure f) = toJSON f

instance ( ToJSON (PredicateFailure (Core.EraRule "POOL" era))
         , ToJSON (PredicateFailure (Core.EraRule "DELEG" era))
         ) => ToJSON (DelplPredicateFailure era) where
  toJSON (PoolFailure   f) = toJSON f
  toJSON (DelegFailure  f) = toJSON f

instance
  ( Ledger.Era era
  ) => ToJSON (DelegPredicateFailure era) where
  toJSON (StakeKeyAlreadyRegisteredDELEG alreadyRegistered) = object
    [ "kind"        .= String "StakeKeyAlreadyRegisteredDELEG"
    , "credential"  .= String (textShow alreadyRegistered)
    , "error"       .= String "Staking credential already registered"
    ]
  toJSON (StakeKeyInRewardsDELEG alreadyRegistered) = object
    [ "kind"        .= String "StakeKeyInRewardsDELEG"
    , "credential"  .= String (textShow alreadyRegistered)
    , "error"       .= String "Staking credential registered in rewards map"
    ]
  toJSON (StakeKeyNotRegisteredDELEG notRegistered) = object
    [ "kind"        .= String "StakeKeyNotRegisteredDELEG"
    , "credential"  .= String (textShow notRegistered)
    , "error"       .= String "Staking credential not registered"
    ]
  toJSON (StakeKeyNonZeroAccountBalanceDELEG remBalance) = object
    [ "kind"              .= String "StakeKeyNonZeroAccountBalanceDELEG"
    , "remainingBalance"  .= remBalance
    ]
  toJSON (StakeDelegationImpossibleDELEG unregistered) = object
    [ "kind"        .= String "StakeDelegationImpossibleDELEG"
    , "credential"  .= String (textShow unregistered)
    , "error"       .= String "Cannot delegate this stake credential because it is not registered"
    ]
  toJSON WrongCertificateTypeDELEG = object
    [ "kind" .= String "WrongCertificateTypeDELEG"
    ]
  toJSON (GenesisKeyNotInMappingDELEG (KeyHash genesisKeyHash)) = object
    [ "kind"            .= String "GenesisKeyNotInMappingDELEG"
    , "unknownKeyHash"  .= String (textShow genesisKeyHash)
    , "error"           .= String "This genesis key is not in the delegation mapping"
    ]
  toJSON (DuplicateGenesisDelegateDELEG (KeyHash genesisKeyHash)) = object
    [ "kind"              .= String "DuplicateGenesisDelegateDELEG"
    , "duplicateKeyHash"  .= String (textShow genesisKeyHash)
    , "error"             .= String "This genesis key has already been delegated to"
    ]
  toJSON (InsufficientForInstantaneousRewardsDELEG mirpot neededMirAmount reserves) = object
    [ "kind"          .= String "InsufficientForInstantaneousRewardsDELEG"
    , "pot"           .= String potText
    , "neededAmount"  .= neededMirAmount
    , "reserves"      .= reserves
    ]
    where potText = case mirpot of
            ReservesMIR -> "Reserves"
            TreasuryMIR -> "Treasury"
  toJSON (MIRCertificateTooLateinEpochDELEG currSlot boundSlotNo) = object
    [ "kind"                        .= String "MIRCertificateTooLateinEpochDELEG"
    , "currentSlotNo"               .= currSlot
    , "mustBeSubmittedBeforeSlotNo" .= boundSlotNo
    ]
  toJSON (DuplicateGenesisVRFDELEG vrfKeyHash) = object
    [ "kind"    .= String "DuplicateGenesisVRFDELEG"
    , "keyHash" .= vrfKeyHash
    ]
  toJSON MIRTransferNotCurrentlyAllowed = object
    [ "kind" .= String "MIRTransferNotCurrentlyAllowed"
    ]
  toJSON MIRNegativesNotCurrentlyAllowed = object
    [ "kind" .= String "MIRNegativesNotCurrentlyAllowed"
    ]
  toJSON (InsufficientForTransferDELEG mirpot attempted available) = object
    [ "kind"      .= String "DuplicateGenesisVRFDELEG"
    , "pot"       .= String potText
    , "attempted" .= attempted
    , "available" .= available
    ]
    where potText = case mirpot of
            ReservesMIR -> "Reserves"
            TreasuryMIR -> "Treasury"
  toJSON MIRProducesNegativeUpdate = object
    [ "kind" .= String "MIRProducesNegativeUpdate"
    ]
  toJSON (MIRNegativeTransfer pot coin) = object
    [ "kind"    .= String "MIRNegativeTransfer"
    , "error"   .= String "Attempt to transfer a negative amount from a pot."
    , "pot"     .= String potText
    , "amount"  .= coin
    ]
    where potText = case pot of
            ReservesMIR -> "Reserves"
            TreasuryMIR -> "Treasury"

instance
  ( Core.Crypto (Ledger.Crypto era)
  ) => ToJSON (PoolPredicateFailure era) where
  toJSON (StakePoolNotRegisteredOnKeyPOOL (KeyHash unregStakePool)) = object
    [ "kind"                .= String "StakePoolNotRegisteredOnKeyPOOL"
    , "unregisteredKeyHash" .= String (textShow unregStakePool)
    , "error"               .= String "This stake pool key hash is unregistered"
    ]
  toJSON (StakePoolRetirementWrongEpochPOOL currentEpoch intendedRetireEpoch maxRetireEpoch) = object
    [ "kind"                    .= String "StakePoolRetirementWrongEpochPOOL"
    , "currentEpoch"            .= String (textShow currentEpoch)
    , "intendedRetirementEpoch" .= String (textShow intendedRetireEpoch)
    , "maxEpochForRetirement"   .= String (textShow maxRetireEpoch)
    ]
  toJSON (StakePoolCostTooLowPOOL certCost protCost) = object
    [ "kind"              .= String "StakePoolCostTooLowPOOL"
    , "certificateCost"   .= String (textShow certCost)
    , "protocolParCost"   .= String (textShow protCost)
    , "error"             .= String "The stake pool cost is too low"
    ]
  toJSON (PoolMedataHashTooBig poolID hashSize) = object
    [ "kind"      .= String "PoolMedataHashTooBig"
    , "poolID"    .= String (textShow poolID)
    , "hashSize"  .= String (textShow hashSize)
    , "error"     .= String "The stake pool metadata hash is too large"
    ]

-- Apparently this should never happen according to the Shelley exec spec
  toJSON (WrongCertificateTypePOOL index) =
    case index of
      0 -> object
        [ "kind"  .= String "WrongCertificateTypePOOL"
        , "error" .= String "Wrong certificate type: Delegation certificate"
        ]
      1 -> object
        [ "kind"  .= String "WrongCertificateTypePOOL"
        , "error" .= String "Wrong certificate type: MIR certificate"
        ]
      2 -> object
        [ "kind"  .= String "WrongCertificateTypePOOL"
        , "error" .= String "Wrong certificate type: Genesis certificate"
        ]
      k -> object
        [ "kind"            .= String "WrongCertificateTypePOOL"
        , "certificateType" .= k
        , "error"           .= String "Wrong certificate type: Unknown certificate type"
        ]

  toJSON (WrongNetworkPOOL networkId listedNetworkId poolId) = object
    [ "kind"            .= String "WrongNetworkPOOL"
    , "networkId"       .= String (textShow networkId)
    , "listedNetworkId" .= String (textShow listedNetworkId)
    , "poolId"          .= String (textShow poolId)
    , "error"           .= String "Wrong network ID in pool registration certificate"
    ]

instance ( ToJSON (PredicateFailure (Core.EraRule "NEWEPOCH" era))
         , ToJSON (PredicateFailure (Core.EraRule "RUPD" era))
         ) => ToJSON (TickPredicateFailure era) where
  toJSON (NewEpochFailure f) = object
    [ "kind"  .= String "NewEpochFailure"
    , "value" .= toJSON f
    ]
  toJSON (RupdFailure f) = object
    [ "kind"  .= String "RupdFailure"
    , "value" .= toJSON f
    ]

instance ToJSON TicknPredicateFailure where
  toJSON x = case x of {} -- no constructors

instance ( ToJSON (PredicateFailure (Core.EraRule "EPOCH" era))
         , ToJSON (PredicateFailure (Core.EraRule "MIR" era))
         , Core.Crypto (Ledger.Crypto era)
         ) => ToJSON (NewEpochPredicateFailure era) where
  toJSON (EpochFailure f) = object
    [ "kind"    .= String "EpochFailure"
    , "update"  .= toJSON f
    ]
  toJSON (MirFailure f) = object
    [ "kind"    .= String "MirFailure"
    , "update"  .= toJSON f
    ]
  toJSON (CorruptRewardUpdate update) = object
    [ "kind"    .= String "CorruptRewardUpdate"
    , "update"  .= String (show update)
    ]

instance ( ToJSON (PredicateFailure (Core.EraRule "POOLREAP" era))
         , ToJSON (PredicateFailure (Core.EraRule "SNAP" era))
         , ToJSON (PredicateFailure (Core.EraRule "UPEC" era))
         ) => ToJSON (EpochPredicateFailure era) where
  toJSON (PoolReapFailure f) = object
    [ "kind"    .= String "PoolReapFailure"
    , "update"  .= toJSON f
    ]
  toJSON (SnapFailure f) = object
    [ "kind"    .= String "SnapFailure"
    , "update"  .= toJSON f
    ]
  toJSON (UpecFailure f) = object
    [ "kind"    .= String "UpecFailure"
    , "update"  .= toJSON f
    ]

instance ToJSON (PoolreapPredicateFailure era) where
  toJSON x = case x of {} -- no constructors

instance ToJSON (SnapPredicateFailure era) where
  toJSON x = case x of {} -- no constructors

-- TODO: Need to elaborate more on this error
instance ToJSON (NewppPredicateFailure era) where
  toJSON (UnexpectedDepositPot outstandingDeposits depositPot) = object
    [ "kind"                .= String "UnexpectedDepositPot"
    , "outstandingDeposits" .= String (textShow outstandingDeposits)
    , "depositPot"          .= String (textShow depositPot)
    ]

instance ToJSON (MirPredicateFailure era) where
  toJSON x = case x of {} -- no constructors


instance ToJSON (RupdPredicateFailure era) where
  toJSON x = case x of {} -- no constructors


instance
  ( Core.Crypto crypto
  , ToJSON Ledger.ActiveSlotCoeff
  , ToJSON (Crypto.OutputVRF (Core.VRF crypto))
  , ToJSON (Crypto.CertifiedVRF (Core.VRF crypto) Nonce)
  ) => ToJSON (PrtclPredicateFailure crypto) where
  toJSON (OverlayFailure f) = object
    [ "kind"    .= String "OverlayFailure"
    , "update"  .= toJSON f
    ]
  toJSON (UpdnFailure f) = object
    [ "kind"    .= String "UpdnFailure"
    , "update"  .= toJSON f
    ]

instance
  ( Core.Crypto crypto
  , ToJSON (Crypto.OutputVRF (Core.VRF crypto))
  , ToJSON (Crypto.CertifiedVRF (Core.VRF crypto) Nonce)
  , ToJSON Ledger.ActiveSlotCoeff
  ) => ToJSON (OverlayPredicateFailure crypto) where
  toJSON (UnknownGenesisKeyOVERLAY (KeyHash genKeyHash)) = object
    [ "kind"            .= String "UnknownGenesisKeyOVERLAY"
    , "unknownKeyHash"  .= String (textShow genKeyHash)
    ]
  toJSON (VRFKeyBadLeaderValue seedNonce (SlotNo currSlotNo) prevHashNonce leaderElecVal) = object
    [ "kind"                .= String "VRFKeyBadLeaderValueOVERLAY"
    , "seedNonce"           .= String (textShow seedNonce)
    , "currentSlot"         .= String (textShow currSlotNo)
    , "previousHashAsNonce" .= String (textShow prevHashNonce)
    , "leaderElectionValue" .= String (textShow leaderElecVal)
    ]
  toJSON (VRFKeyBadNonce seedNonce (SlotNo currSlotNo) prevHashNonce blockNonce) = object
    [ "kind"                .= String "VRFKeyBadNonceOVERLAY"
    , "seedNonce"           .= String (textShow seedNonce)
    , "currentSlot"         .= String (textShow currSlotNo)
    , "previousHashAsNonce" .= String (textShow prevHashNonce)
    , "blockNonce"          .= String (textShow blockNonce)
    ]
  toJSON (VRFKeyWrongVRFKey issuerHash regVRFKeyHash unregVRFKeyHash) = object
    [ "kind"                    .= String "VRFKeyWrongVRFKeyOVERLAY"
    , "poolHash"                .= textShow issuerHash
    , "registeredVRFKeHash"     .= textShow regVRFKeyHash
    , "unregisteredVRFKeyHash"  .= textShow unregVRFKeyHash
    ]
  toJSON (VRFKeyUnknown (KeyHash kHash)) = object
    [ "kind"    .= String "VRFKeyUnknownOVERLAY"
    , "keyHash" .= String (textShow kHash)
    ]
  toJSON (VRFLeaderValueTooBig leadElecVal weightOfDelegPool actSlotCoefff) = object
    [ "kind"                  .= String "VRFLeaderValueTooBigOVERLAY"
    , "leaderElectionValue"   .= String (textShow leadElecVal)
    , "delegationPoolWeight"  .= String (textShow weightOfDelegPool)
    , "activeSlotCoefficient" .= String (textShow actSlotCoefff)
    ]
  toJSON (NotActiveSlotOVERLAY notActiveSlotNo) = object
    [ "kind" .= String "NotActiveSlotOVERLAY"
    , "slot" .= String (textShow notActiveSlotNo)
    ]
  toJSON (WrongGenesisColdKeyOVERLAY actual expected) = object
    [ "kind"      .= String "WrongGenesisColdKeyOVERLAY"
    , "actual"    .= actual
    , "expected"  .= expected
    ]
  toJSON (WrongGenesisVRFKeyOVERLAY issuer actual expected) = object
    [ "kind"      .= String "WrongGenesisVRFKeyOVERLAY"
    , "issuer"    .= issuer
    , "actual"    .= actual
    , "expected"  .= expected
    ]
  toJSON (OcertFailure f) = toJSON f

instance
  ( Core.Crypto crypto
  ) => ToJSON (OcertPredicateFailure crypto) where
  toJSON (KESBeforeStartOCERT (KESPeriod oCertstart) (KESPeriod current)) = object
    [ "kind"                  .= String "KESBeforeStartOCERT"
    , "opCertKESStartPeriod"  .= String (textShow oCertstart)
    , "currentKESPeriod"      .= String (textShow current)
    , "error"                 .= String "Your operational certificate's KES start period is before the KES current period."
    ]
  toJSON (KESAfterEndOCERT (KESPeriod current) (KESPeriod oCertstart) maxKESEvolutions) = object
    [ "kind"                  .= String "KESAfterEndOCERT"
    , "currentKESPeriod"      .= String (textShow current)
    , "opCertKESStartPeriod"  .= String (textShow oCertstart)
    , "maxKESEvolutions"      .= String  (textShow maxKESEvolutions)
    , "error"                 .= String "The operational certificate's KES start period is greater than the max number of KES + the KES current period"
    ]
  toJSON (CounterTooSmallOCERT lastKEScounterUsed currentKESCounter) = object
    [ "kind"              .= String "CounterTooSmallOCert"
    , "currentKESCounter" .= String (textShow currentKESCounter)
    , "lastKESCounter"    .= String (textShow lastKEScounterUsed)
    , "error"             .= String "The operational certificate's last KES counter is greater than the current KES counter."
    ]
  toJSON (InvalidSignatureOCERT oCertCounter oCertKESStartPeriod) = object
    [ "kind"                  .= String "InvalidSignatureOCERT"
    , "opCertKESStartPeriod"  .= String (textShow oCertKESStartPeriod)
    , "opCertCounter"         .= String (textShow oCertCounter)
    ]
  toJSON (InvalidKesSignatureOCERT currKESPeriod startKESPeriod expectedKESEvolutions err) = object
    [ "kind"                        .= String "InvalidKesSignatureOCERT"
    , "opCertKESStartPeriod"        .= String (textShow startKESPeriod)
    , "opCertKESCurrentPeriod"      .= String (textShow currKESPeriod)
    , "opCertExpectedKESEvolutions" .= String (textShow expectedKESEvolutions)
    , "error"                       .= err
    ]
  toJSON (NoCounterForKeyHashOCERT (KeyHash stakePoolKeyHash)) = object
    [ "kind" .= String "NoCounterForKeyHashOCERT"
    , "stakePoolKeyHash" .= String (textShow stakePoolKeyHash)
    , "error" .= String "A counter was not found for this stake pool key hash"
    ]

instance ToJSON (UpdnPredicateFailure crypto) where
  toJSON x = case x of {} -- no constructors

instance ToJSON (UpecPredicateFailure era) where
  toJSON (NewPpFailure (UnexpectedDepositPot totalOutstanding depositPot)) = object
    [ "kind"              .= String "UnexpectedDepositPot"
    , "totalOutstanding"  .= String (textShow totalOutstanding)
    , "depositPot"        .= String (textShow depositPot)
    ]


--------------------------------------------------------------------------------
-- Alonzo related
--------------------------------------------------------------------------------


instance ToJSON (Alonzo.UtxoPredicateFailure (Alonzo.AlonzoEra StandardCrypto)) where
  toJSON (Alonzo.BadInputsUTxO badInputs) = object
    [ "kind"      .= String "BadInputsUTxO"
    , "badInputs" .= badInputs
    , "error"     .= renderBadInputsUTxOErr badInputs
    ]
  toJSON (Alonzo.OutsideValidityIntervalUTxO validtyInterval slot) = object
    [ "kind"              .= String "ExpiredUTxO"
    , "validityInterval"  .= validtyInterval
    , "slot"              .= slot
    ]
  toJSON (Alonzo.MaxTxSizeUTxO txsize maxtxsize) = object
    [ "kind"    .= String "MaxTxSizeUTxO"
    , "size"    .= txsize
    , "maxSize" .= maxtxsize
    ]
  toJSON Alonzo.InputSetEmptyUTxO = object
    [ "kind" .= String "InputSetEmptyUTxO"
    ]
  toJSON (Alonzo.FeeTooSmallUTxO minfee currentFee) = object
    [ "kind"    .= String "FeeTooSmallUTxO"
    , "minimum" .= minfee
    , "fee"     .= currentFee
    ]
  toJSON (Alonzo.ValueNotConservedUTxO consumed produced) = object
    [ "kind"      .= String "ValueNotConservedUTxO"
    , "consumed"  .= consumed
    , "produced"  .= produced
    , "error"     .= renderValueNotConservedErr consumed produced
    ]
  toJSON (Alonzo.WrongNetwork network addrs) = object
    [ "kind"    .= String "WrongNetwork"
    , "network" .= network
    , "addrs"   .= addrs
    ]
  toJSON (Alonzo.WrongNetworkWithdrawal network addrs) = object
    [ "kind"    .= String "WrongNetworkWithdrawal"
    , "network" .= network
    , "addrs"   .= addrs
    ]
  toJSON (Alonzo.OutputTooSmallUTxO badOutputs) = object
    [ "kind"    .= String "OutputTooSmallUTxO"
    , "outputs" .= badOutputs
    , "error"   .= String "The output is smaller than the allow minimum UTxO value defined in the protocol parameters"
    ]
  toJSON (Alonzo.UtxosFailure predFailure) = object
    [ "kind"    .= String "UtxosFailure"
    , "error"   .= toJSON predFailure
    ]
  toJSON (Alonzo.OutputBootAddrAttrsTooBig txouts) = object
    [ "kind"    .= String "OutputBootAddrAttrsTooBig"
    , "outputs" .= txouts
    , "error"   .= String "The Byron address attributes are too big"
    ]
  toJSON Alonzo.TriesToForgeADA = object
    [ "kind"  .= String "TriesToForgeADA"
    ]
  toJSON (Alonzo.OutputTooBigUTxO badOutputs) = object
    [ "kind"    .= String "OutputTooBigUTxO"
    , "outputs" .= badOutputs
    , "error"   .= String "Too many asset ids in the tx output"
    ]
  toJSON (Alonzo.InsufficientCollateral computedBalance suppliedFee) = object
    [ "kind"    .= String "InsufficientCollateral"
    , "balance" .= computedBalance
    , "txfee"   .= suppliedFee
    ]
  toJSON (Alonzo.ScriptsNotPaidUTxO utxos) = object
    [ "kind"  .= String "ScriptsNotPaidUTxO"
    , "utxos" .= utxos
    ]
  toJSON (Alonzo.ExUnitsTooBigUTxO pParamsMaxExUnits suppliedExUnits) = object
    [ "kind"        .= String "ExUnitsTooBigUTxO"
    , "maxexunits"  .= pParamsMaxExUnits
    , "exunits"     .= suppliedExUnits
    ]
  toJSON (Alonzo.CollateralContainsNonADA inputs) = object
    [ "kind"    .= String "CollateralContainsNonADA"
    , "inputs"  .= inputs
    ]
  toJSON (Alonzo.WrongNetworkInTxBody actualNetworkId netIdInTxBody) = object
    [ "kind"            .= String "WrongNetworkInTxBody"
    , "networkid"       .= actualNetworkId
    , "txbodyNetworkId" .= netIdInTxBody
    ]
  toJSON (Alonzo.OutsideForecast slotNum) = object
    [ "kind" .= String "OutsideForecast"
    , "slot" .= slotNum
    ]
  toJSON (Alonzo.TooManyCollateralInputs maxCollateralInputs numberCollateralInputs) = object
    [ "kind"    .= String "TooManyCollateralInputs"
    , "max"     .= maxCollateralInputs
    , "inputs"  .= numberCollateralInputs
    ]
  toJSON Alonzo.NoCollateralInputs = object
    [ "kind"  .= String "NoCollateralInputs"
    ]

instance ToJSON (Alonzo.UtxosPredicateFailure (Alonzo.AlonzoEra StandardCrypto)) where
  toJSON (Alonzo.ValidationTagMismatch isValidating reason) = object
    [ "kind"          .= String "ValidationTagMismatch"
    , "isvalidating"  .= isValidating
    , "reason"        .= reason
    ]
  toJSON (Alonzo.CollectErrors errors) = object
    [ "kind"    .= String "CollectErrors"
    , "errors"  .= errors
    ]
  toJSON (Alonzo.UpdateFailure pFailure) = toJSON pFailure

deriving newtype instance ToJSON Alonzo.IsValid

instance ToJSON (Alonzo.CollectError StandardCrypto) where
  toJSON cError = case cError of
    Alonzo.NoRedeemer sPurpose -> object
      [ "kind"          .= String "CollectError"
      , "error"         .= String "NoRedeemer"
      , "scriptpurpose" .= renderScriptPurpose sPurpose
      ]
    Alonzo.NoWitness sHash -> object
      [ "kind"        .= String "CollectError"
      , "error"       .= String "NoWitness"
      , "scripthash"  .= toJSON sHash
      ]
    Alonzo.NoCostModel lang -> object
      [ "kind"      .= String "CollectError"
      , "error"     .= String "NoCostModel"
      , "language"  .= toJSON lang
      ]
    Alonzo.BadTranslation err -> object
      [ "kind"  .= String "PlutusTranslationError"
      , "error" .= errMsg
      ]
      where errMsg = case err of
              Alonzo.ByronInputInContext              -> String "Byron input in the presence of a plutus script"
              Alonzo.ByronOutputInContext             -> String "Byron output in the presence of a plutus script"
              Alonzo.TranslationLogicErrorInput       -> String "Logic error translating inputs"
              Alonzo.TranslationLogicErrorRedeemer    -> String "Logic error translating redeemers"
              Alonzo.TranslationLogicErrorDoubleDatum -> String "Logic error double datum"
              Alonzo.LanguageNotSupported             -> String "Language not supported"
              Alonzo.InlineDatumsNotSupported         -> String "Inline datums not supported"
              Alonzo.ReferenceScriptsNotSupported     -> String "Reference scripts not supported"
              Alonzo.ReferenceInputsNotSupported      -> String "Reference inputs not supported"

instance ToJSON Alonzo.TagMismatchDescription where
  toJSON tmd = case tmd of
    Alonzo.PassedUnexpectedly -> object
      [ "kind"  .= String "TagMismatchDescription"
      , "error" .= String "PassedUnexpectedly"
      ]
    Alonzo.FailedUnexpectedly forReasons -> object
      [ "kind"            .= String "TagMismatchDescription"
      , "error"           .= String "FailedUnexpectedly"
      , "reconstruction"  .= forReasons
      ]

instance ToJSON Alonzo.FailureDescription where
  toJSON = \case
    Alonzo.OnePhaseFailure t -> object
      [ "kind"        .= String "FailureDescription"
      , "error"       .= String "OnePhaseFailure"
      , "description" .= t
      ]
    Alonzo.PlutusFailure t bs -> object
      [ "kind"                  .= String "FailureDescription"
      , "error"                 .= String "PlutusFailure"
      , "description"           .= t
      , "reconstructionDetail"  .= Alonzo.debugPlutus (BSU.toString bs)
      ]

instance
  ( ToJSON (Core.AuxiliaryDataHash StandardCrypto)
  ) => ToJSON (AlonzoBbodyPredFail (Alonzo.AlonzoEra StandardCrypto)) where
  toJSON err = object
    [ "kind"  .= String "AlonzoBbodyPredFail"
    , "error" .= String (show err)
    ]

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

instance ToJSON Alonzo.PlutusDebugInfo where
  toJSON = \case
    Alonzo.DebugSuccess budget -> object
      [ "kind"    .= String "DebugSuccess"
      , "budget"  .= budget
      ]
    Alonzo.DebugCannotDecode msg -> object
      [ "kind"    .= String "DebugCannotDecode"
      , "message" .= toJSON msg
      ]
    Alonzo.DebugInfo texts e d -> object
      [ "kind"  .= String "DebugInfo"
      , "texts" .= texts
      , "error" .= e
      , "debug" .= d
      ]
    Alonzo.DebugBadHex msg -> object
      [ "kind"    .= String "DebugBadHex"
      , "message" .= toJSON msg
      ]


instance ToJSON Alonzo.PlutusError where
  toJSON = \case
    Alonzo.PlutusErrorV1 evaluationError -> toJSON evaluationError
    Alonzo.PlutusErrorV2 evaluationError -> toJSON evaluationError

instance ToJSON Alonzo.PlutusDebug where
  toJSON = \case
    Alonzo.PlutusDebugV1 costModel exUnits sbs ds protVer -> object
      [ "costModel"   .= costModel
      , "exUnits"     .= exUnits
      , "sbs"         .= toJSON (Text.decodeLatin1 (B16.encode (SBS.fromShort sbs)))
      , "scriptHash"  .= scriptHashOf Alonzo.PlutusV1 sbs
      , "ds"          .= toJSON ds
      , "dsSummary"   .= plutusDataToDsSummary ds
      , "protVer"     .= protVer
      ]
    Alonzo.PlutusDebugV2 costModel exUnits sbs ds protVer -> object
      [ "costModel"   .= costModel
      , "exUnits"     .= exUnits
      , "sbs"         .= toJSON (Text.decodeLatin1 (B16.encode (SBS.fromShort sbs)))
      , "scriptHash"  .= scriptHashOf Alonzo.PlutusV2 sbs
      , "ds"          .= toJSON ds
      , "dsSummary"   .= plutusDataToDsSummary ds
      , "protVer"     .= protVer
      ]

plutusDataToDsSummary :: [Plutus.Data] -> Aeson.Value
plutusDataToDsSummary [dat, redeemer, info] = Aeson.object
  [ "data"      .= toJSON dat
  , "redeemer"  .= toJSON redeemer
  , "info"      .= plutusInfoDataToDsSummary info
  ]
plutusDataToDsSummary [dat, info] = Aeson.object
  [ "data"      .= toJSON dat
  , "info"      .= plutusInfoDataToDsSummary info
  ]
plutusDataToDsSummary _ = Aeson.Null

plutusInfoDataToDsSummary :: Plutus.Data -> Aeson.Value
plutusInfoDataToDsSummary info = case PV1.fromData info of
  Nothing -> String "no-info"
  Just PV1.ScriptContext { PV1.scriptContextTxInfo, PV1.scriptContextPurpose} -> object
    [ "scriptContextTxInfo" .= txInfoToJson scriptContextTxInfo
    , "scriptContextPurpose" .= scriptPurposeToJson scriptContextPurpose
    ]

txInfoToJson :: PV1.TxInfo -> Value
txInfoToJson txInfo = Aeson.object
  [ "txInfoInputs"      .= toJSON (PV1.toData (PV1.txInfoInputs txInfo))
  , "txInfoOutputs"     .= toJSON (PV1.toData (PV1.txInfoOutputs txInfo))
  , "txInfoFee"         .= toJSON (PV1.toData (PV1.txInfoFee txInfo))
  , "txInfoMint"        .= toJSON (PV1.toData (PV1.txInfoMint txInfo))
  , "txInfoDCert"       .= toJSON (PV1.toData (PV1.txInfoDCert txInfo))
  , "txInfoWdrl"        .= toJSON (PV1.toData (PV1.txInfoWdrl txInfo))
  , "txInfoValidRange"  .= toJSON (PV1.toData (PV1.txInfoValidRange txInfo))
  , "txInfoSignatories" .= toJSON (PV1.toData (PV1.txInfoSignatories txInfo))
  , "txInfoData"        .= toJSON (PV1.toData (PV1.txInfoData txInfo))
  , "txInfoId"          .= toJSON (PV1.toData (PV1.txInfoId txInfo))
  ]

-- toData :: (ToData a) => a -> PLC.Data
-- toData a = builtinDataToData (toBuiltinData a)

instance ToJSON PV1.Datum where
  toJSON v = toJSON (PV1.toData (PV1.getDatum v))

instance ToJSON PV1.DatumHash where
  toJSON v = toJSON (PV1.toData v)

instance ToJSON PV1.DCert where
  toJSON v = toJSON (PV1.toData v)

instance ToJSON PV1.POSIXTimeRange where
  toJSON v = toJSON (PV1.toData v)

instance ToJSON PV1.PubKeyHash where
  toJSON v = toJSON (PV1.toData v)

instance ToJSON PV1.StakingCredential where
  toJSON v = toJSON (PV1.toData v)

instance ToJSON PV1.TxId where
  toJSON v = toJSON (PV1.toData v)

instance ToJSON PV1.TxInInfo where
  toJSON v = toJSON (PV1.toData v)

instance ToJSON PV1.TxOut where
  toJSON v = toJSON (PV1.toData v)

instance ToJSON PV1.Value where
  toJSON v = toJSON (PV1.toData v)

scriptPurposeToJson :: PV1.ScriptPurpose -> Value
scriptPurposeToJson = \case
  PV1.Minting currencySymbol -> Aeson.object
    [ "kind" .= String "Minting"
    , "value" .= toJSON (PV1.toData currencySymbol)
    ]
  PV1.Spending txOutRef -> Aeson.object
    [ "kind" .= String "Spending"
    , "value" .= toJSON (PV1.toData txOutRef)
    ]
  PV1.Rewarding stakingCredential -> Aeson.object
    [ "kind" .= String "Rewarding"
    , "value" .= toJSON (PV1.toData stakingCredential)
    ]
  PV1.Certifying dCert -> Aeson.object
    [ "kind" .= String "Certifying"
    , "value" .= toJSON (PV1.toData dCert)
    ]

scriptHashOf :: Alonzo.Language -> SBS.ShortByteString -> Text
scriptHashOf lang sbs = Text.pack $ Hash.hashToStringAsHex h
  where Ledger.ScriptHash h = case lang of
          Alonzo.PlutusV1 -> Ledger.hashScript @Consensus.StandardAlonzo (Ledger.PlutusScript lang sbs)
          Alonzo.PlutusV2 -> error "not implemented"

instance ToJSON Plutus.EvaluationError where
  toJSON = \case
    Plutus.CekError e -> object
      [ "kind"  .= String "CekError"
      , "error" .= toJSON @Text (show e)
      , "value" .= toJSON e
      ]
    Plutus.DeBruijnError e -> object
      [ "kind"  .= String "DeBruijnError"
      , "error" .= toJSON @Text (show e)
      ]
    Plutus.CodecError e -> object
      [ "kind"  .= String "CodecError"
      , "error" .= toJSON @Text (show e)
      ]
    Plutus.IncompatibleVersionError actual -> object
      [ "kind"    .= String "IncompatibleVersionError"
      , "actual"  .= toJSON actual
      ]
    Plutus.CostModelParameterMismatch -> object
      [ "kind"  .= String "CostModelParameterMismatch"
      ]

instance ToJSON (Plutus.Version ann) where
  toJSON (Plutus.Version _ i j k) = object
    [ "i" .= toJSON i
    , "j" .= toJSON j
    , "k" .= toJSON k
    ]

instance ToJSON Plutus.Data where
  toJSON = \case
    Plutus.Constr t as -> object
      [ "kind"      .= String "Constr"
      , "tag"       .= toJSON t
      , "arguments" .= fmap toJSON as
      ]
    Plutus.Map es -> object
      [ "kind"    .= String "Map"
      , "entries" .= fmap dataEntryToJson es
      ]
    Plutus.List es -> object
      [ "kind"      .= String "List"
      , "elements"  .= fmap toJSON es
      ]
    Plutus.I n -> object
      [ "kind"  .= String "I"
      , "value" .= toJSON n
      ]
    Plutus.B bs -> object
      [ "kind"  .= String "B"
      , "value" .= toJSON (Text.decodeLatin1 (B16.encode bs))
      ]

dataEntryToJson :: (Plutus.Data, Plutus.Data) -> Value
dataEntryToJson (k, v) = toJSON [toJSON k, toJSON v]

instance ToJSON Cek.CekUserError where
  toJSON = \case
    Cek.CekOutOfExError (Cek.ExRestrictingBudget res) -> object
      [ "kind"    .= String "CekOutOfExError"
      , "budget"  .= toJSON res
      ]
    Cek.CekEvaluationFailure -> object
      [ "kind"  .= String "CekEvaluationFailure"
      ]

instance (ToJSON name, ToJSON fun) => ToJSON (Cek.CekEvaluationException name uni fun) where

instance (ToJSON name, ToJSON fun) => ToJSON (UntypedPlutusCore.Core.Type.Term name uni fun ann) where
  toJSON = \case
    UntypedPlutusCore.Core.Type.Var {} -> Aeson.object
      [ "kind" .= String "Var"
      ]
    UntypedPlutusCore.Core.Type.LamAbs {} -> Aeson.object
      [ "kind" .= String "LamAbs"
      ]
    UntypedPlutusCore.Core.Type.Apply {} -> Aeson.object
      [ "kind" .= String "Apply"
      ]
    UntypedPlutusCore.Core.Type.Force {} -> Aeson.object
      [ "kind" .= String "Force"
      ]
    UntypedPlutusCore.Core.Type.Delay {} -> Aeson.object
      [ "kind" .= String "Delay"
      ]
    UntypedPlutusCore.Core.Type.Constant {} -> Aeson.object
      [ "kind" .= String "Constant"
      ]
    UntypedPlutusCore.Core.Type.Builtin {} -> Aeson.object
      [ "kind" .= String "Builtin"
      ]
    UntypedPlutusCore.Core.Type.Error {} -> Aeson.object
      [ "kind" .= String "Error"
      ]

instance ToJSON fun => ToJSON (Cek.EvaluationError Cek.CekUserError (PlutusCore.Evaluation.Machine.Exception.MachineError fun)) where

instance ToJSON PlutusCore.NamedDeBruijn where

instance ToJSON PlutusCore.DeBruijn.Index where

instance ToJSON PlutusCore.DefaultFun where

instance (forall a. ToJSON (f a)) => ToJSON (PlutusCore.Some f) where
  toJSON (PlutusCore.Some a) = object
    [ "kind"  .= String "Some"
    , "value" .= toJSON a
    ]

instance (ToJSON (uni (PlutusCore.Esc a)), ToJSON a) => ToJSON (PlutusCore.ValueOf uni a) where
  toJSON (PlutusCore.ValueOf u a) = object
    [ "kind" .= String "ValueOf"
    , "uni"   .= toJSON u
    , "a"     .= toJSON a
    ]

instance ToJSON fun => ToJSON (PlutusCore.Evaluation.Machine.Exception.MachineError fun) where

instance ToJSON PlutusCore.Evaluation.Machine.Exception.UnliftingError where
  toJSON _ = "UnliftingError"

renderScriptIntegrityHash :: Maybe (Alonzo.ScriptIntegrityHash StandardCrypto) -> Value
renderScriptIntegrityHash (Just witPPDataHash) =
  String . Crypto.hashToTextAsHex $ SafeHash.extractHash witPPDataHash
renderScriptIntegrityHash Nothing = Aeson.Null


renderScriptHash :: ScriptHash StandardCrypto -> Text
renderScriptHash = Api.serialiseToRawBytesHexText . Api.fromShelleyScriptHash

renderMissingRedeemers :: [(Alonzo.ScriptPurpose StandardCrypto, ScriptHash StandardCrypto)] -> Value
renderMissingRedeemers scripts = object $ map renderTuple  scripts
 where
  renderTuple :: (Alonzo.ScriptPurpose StandardCrypto, ScriptHash StandardCrypto) -> Pair
  renderTuple (scriptPurpose, sHash) =
    Aeson.fromText (renderScriptHash sHash) .= renderScriptPurpose scriptPurpose

renderScriptPurpose :: Alonzo.ScriptPurpose StandardCrypto -> Value
renderScriptPurpose (Alonzo.Minting pid) = object
  [ "minting" .= toJSON pid
  ]
renderScriptPurpose (Alonzo.Spending txin) = object
  [ "spending" .= Api.fromShelleyTxIn txin
  ]
renderScriptPurpose (Alonzo.Rewarding rwdAcct) = object
  [ "rewarding" .= String (Api.serialiseAddress $ Api.fromShelleyStakeAddr rwdAcct)
  ]
renderScriptPurpose (Alonzo.Certifying cert) = object
  [ "certifying" .= toJSON (Api.textEnvelopeDefaultDescr $ Api.fromShelleyCertificate cert)
  ]

renderBadInputsUTxOErr ::  Set (TxIn era) -> Value
renderBadInputsUTxOErr txIns
  | Set.null txIns = String "The transaction contains no inputs."
  | otherwise = String "The transaction contains inputs that do not exist in the UTxO set."

renderValueNotConservedErr :: Show val => val -> val -> Value
renderValueNotConservedErr consumed produced = String $
  "This transaction consumed " <> show consumed <> " but produced " <> show produced

renderTxId :: Consensus.TxId (GenTx (ShelleyBlock era)) -> Text
renderTxId = error "TODO implement"

textShow :: Show a => a -> Text
textShow = Text.pack . CP.show

showLastAppBlockNo :: WithOrigin (LastAppliedBlock crypto) -> Text
showLastAppBlockNo wOblk =  case withOriginToMaybe wOblk of
                     Nothing -> "Genesis Block"
                     Just blk -> textShow . unBlockNo $ labBlockNo blk
