{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.Api.Orphans () where

import           Data.Aeson (FromJSON (..), ToJSON (..), object, pairs, (.=))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (ToJSONKey (..), toJSONKeyText)
import           Data.BiMap (BiMap (..), Bimap)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Short as Short
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.UMap (Trip (Triple), UMap (UnifiedMap))
import           Data.VMap (VB, VMap, VP)
import qualified Data.VMap as VMap

import qualified Cardano.Ledger.Babbage as Babbage
import           Cardano.Ledger.BaseTypes (StrictMaybe (..))
import qualified Cardano.Ledger.BaseTypes as Ledger
import           Cardano.Ledger.Compactible (Compactible (fromCompact))
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.Shelley.PoolRank as Shelley
import           Cardano.Ledger.UnifiedMap (UnifiedMap)
import           Cardano.Slotting.Slot (SlotNo (..))
import           Cardano.Slotting.Time (SystemStart (..))
import           Control.State.Transition (STS (State))

import           Cardano.Api.Script
import qualified Cardano.Binary as CBOR
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Cardano.Ledger.Babbage.PParams as Babbage
import           Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..))
import qualified Cardano.Ledger.Babbage.TxBody as Babbage
import qualified Cardano.Ledger.Coin as Shelley
import           Cardano.Ledger.Core (EraTxOut)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as Crypto
import qualified Cardano.Ledger.PoolDistr as Ledger
import qualified Cardano.Ledger.SafeHash as SafeHash
import           Cardano.Ledger.Shelley.API (ShelleyTxOut (..))
import qualified Cardano.Ledger.Shelley.API as Shelley
import qualified Cardano.Ledger.Mary.Value as Mary
import qualified Cardano.Ledger.Shelley.EpochBoundary as ShelleyEpoch
import qualified Cardano.Ledger.Shelley.LedgerState as ShelleyLedger
import           Cardano.Ledger.Shelley.PParams (ShelleyPParamsUpdate)
import qualified Cardano.Ledger.Shelley.Rewards as Shelley
import qualified Cardano.Ledger.Shelley.RewardUpdate as Shelley
import           Cardano.Ledger.Val (Val)
import qualified Ouroboros.Consensus.Shelley.Eras as Consensus
import qualified Ouroboros.Consensus.Shelley.Ledger.Query as Consensus

import           Cardano.Api.Script
import Cardano.Ledger.Mary.Value (MaryValue(..))
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut(..))
import Cardano.Ledger.Shelley.API (ShelleyTxOut(..))
import Cardano.Ledger.Val (Val)
import Cardano.Ledger.Core (EraTxOut)
import Cardano.Ledger.Babbage.PParams (BabbagePParamsUpdate, BabbagePParams)
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript)

-- Orphan instances involved in the JSON output of the API queries.
-- We will remove/replace these as we provide more API wrapper types

instance ToJSON (MaryValue era) where
  toJSON = object . toMaryValuePairs
  toEncoding = Aeson.pairs . mconcat . toMaryValuePairs

toMaryValuePairs :: Aeson.KeyValue a => MaryValue crypto -> [a]
toMaryValuePairs (MaryValue !l !ps) =
  [ "lovelace" .= l
  , "policies" .= ps
  ]

instance ToJSONKey Mary.AssetName where
  toJSONKey = toJSONKeyText render
    where
      render = Text.decodeLatin1 . B16.encode . Short.fromShort . Mary.assetName

instance ToJSON (Mary.PolicyID era) where
  toJSON (Mary.PolicyID (Shelley.ScriptHash h)) = Aeson.String (hashToText h)

instance ToJSONKey (Mary.PolicyID era) where
  toJSONKey = toJSONKeyText render
    where
      render (Mary.PolicyID (Shelley.ScriptHash h)) = hashToText h

instance ToJSON Mary.AssetName where
  toJSON = Aeson.String . Text.decodeLatin1 . B16.encode . Short.fromShort . Mary.assetName

instance ToJSON Shelley.AccountState where
  toJSON = object . toAccountStatePairs
  toEncoding = Aeson.pairs . mconcat . toAccountStatePairs

toAccountStatePairs :: Aeson.KeyValue a => ShelleyLedger.AccountState -> [a]
toAccountStatePairs (Shelley.AccountState !tr !rs) =
  [ "treasury" .= tr
  , "reserves" .= rs
  ]

instance forall era.
         ( Consensus.ShelleyBasedEra era
         , ToJSON (Core.TxOut era)
         , ToJSON (Core.PParams era)
         , ToJSON (Core.PParamsUpdate era)
         ) => ToJSON (Shelley.EpochState era) where
  toJSON = object . toEpochStatePairs
  toEncoding = Aeson.pairs . mconcat . toEpochStatePairs

toEpochStatePairs ::
  ( Consensus.ShelleyBasedEra era
  , ToJSON (Core.TxOut era)
  , ToJSON (Core.PParamsUpdate era)
  , ToJSON (Core.PParams era)
  , Aeson.KeyValue a
  )
  => ShelleyLedger.EpochState era
  -> [a]
toEpochStatePairs eState =
  let !esAccountState = Shelley.esAccountState eState
      !esSnapshots = Shelley.esSnapshots eState
      !esLState = Shelley.esLState eState
      !esPrevPp = Shelley.esPrevPp eState
      !esPp = Shelley.esPp eState
      !esNonMyopic = Shelley.esNonMyopic eState
  in  [ "esAccountState" .= esAccountState
      , "esSnapshots" .= esSnapshots
      , "esLState" .= esLState
      , "esPrevPp" .= esPrevPp
      , "esPp" .= esPp
      , "esNonMyopic" .= esNonMyopic
      ]


instance ( Consensus.ShelleyBasedEra era
         , ToJSON (Core.TxOut era)
         , ToJSON (Core.PParamsUpdate era)
         ) => ToJSON (Shelley.LedgerState era) where
  toJSON = object . toLedgerStatePairs
  toEncoding = Aeson.pairs . mconcat . toLedgerStatePairs

toLedgerStatePairs ::
  ( Consensus.ShelleyBasedEra era
  , ToJSON (Core.TxOut era)
  , ToJSON (Core.PParamsUpdate era)
  , Aeson.KeyValue a
  ) => ShelleyLedger.LedgerState era -> [a]
toLedgerStatePairs lState =
  let !lsUTxOState = Shelley.lsUTxOState lState
      !lsDPState = Shelley.lsDPState lState
  in  [ "utxoState" .= lsUTxOState
      , "delegationState" .= lsDPState
      ]

instance Crypto.Crypto crypto => ToJSON (ShelleyLedger.IncrementalStake crypto) where
  toJSON = object . toIncrementalStakePairs
  toEncoding = Aeson.pairs . mconcat . toIncrementalStakePairs

toIncrementalStakePairs ::
  ( Aeson.KeyValue a
  , Crypto.Crypto crypto
  ) => ShelleyLedger.IncrementalStake crypto -> [a]
toIncrementalStakePairs iStake =
  let !credentials = Map.toList (ShelleyLedger.credMap iStake)
      !pointers = Map.toList (ShelleyLedger.ptrMap iStake)
  in  [ "credentials" .= credentials
      , "pointers" .= pointers
      ]

instance ( Consensus.ShelleyBasedEra era
         , ToJSON (Core.TxOut era)
         , ToJSON (Core.PParamsUpdate era)
         ) => ToJSON (Shelley.UTxOState era) where
  toJSON = object . toUtxoStatePairs
  toEncoding = Aeson.pairs . mconcat . toUtxoStatePairs

toUtxoStatePairs ::
  ( Aeson.KeyValue a
  , Consensus.ShelleyBasedEra era
  , ToJSON (Core.TxOut era)
  , ToJSON (State (Core.EraRule "PPUP" era))
  ) => ShelleyLedger.UTxOState era -> [a]
toUtxoStatePairs utxoState =
  let !utxo = Shelley._utxo utxoState
      !deposited = Shelley._deposited utxoState
      !fees = Shelley._fees utxoState
      !ppups = Shelley._ppups utxoState
      !stakeDistro = Shelley._stakeDistro utxoState
  in  [ "utxo" .= utxo
      , "deposited" .= deposited
      , "fees" .= fees
      , "ppups" .= ppups
      , "stake" .= stakeDistro
      ]

instance ( ToJSON (Core.PParamsUpdate era)
         , Core.Era era
         ) => ToJSON (Shelley.PPUPState era) where
  toJSON = object . toPpupStatePairs
  toEncoding = Aeson.pairs . mconcat . toPpupStatePairs

toPpupStatePairs ::
  ( Aeson.KeyValue a
  , ToJSON (Core.PParamsUpdate era)
  , Core.Era era) => ShelleyLedger.PPUPState era -> [a]
toPpupStatePairs ppUpState =
  let !proposals = Shelley.proposals ppUpState
      !futureProposals = Shelley.futureProposals ppUpState
  in  [ "proposals" .= proposals
      , "futureProposals" .= futureProposals
      ]

instance ( ToJSON (Core.PParamsUpdate era)
         , Core.Era era
         ) => ToJSON (Shelley.ProposedPPUpdates era) where
  toJSON (Shelley.ProposedPPUpdates ppUpdates) = toJSON $ Map.toList ppUpdates
  toEncoding (Shelley.ProposedPPUpdates ppUpdates) = toEncoding $ Map.toList ppUpdates

instance ToJSON (ShelleyPParamsUpdate era) where
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

instance ToJSON (BabbagePParamsUpdate era) where
  toJSON pp =
    Aeson.object $
        [ "minFeeA"               .= x | x <- mbfield (Babbage._minfeeA pp) ]
     ++ [ "minFeeB"               .= x | x <- mbfield (Babbage._minfeeB pp) ]
     ++ [ "maxBlockBodySize"      .= x | x <- mbfield (Babbage._maxBBSize pp) ]
     ++ [ "maxTxSize"             .= x | x <- mbfield (Babbage._maxTxSize pp) ]
     ++ [ "maxBlockHeaderSize"    .= x | x <- mbfield (Babbage._maxBHSize pp) ]
     ++ [ "keyDeposit"            .= x | x <- mbfield (Babbage._keyDeposit pp) ]
     ++ [ "poolDeposit"           .= x | x <- mbfield (Babbage._poolDeposit pp) ]
     ++ [ "eMax"                  .= x | x <- mbfield (Babbage._eMax pp) ]
     ++ [ "nOpt"                  .= x | x <- mbfield (Babbage._nOpt pp) ]
     ++ [ "a0"                    .= x | x <- mbfield (Babbage._a0 pp) ]
     ++ [ "rho"                   .= x | x <- mbfield (Babbage._rho pp) ]
     ++ [ "tau"                   .= x | x <- mbfield (Babbage._tau pp) ]
     ++ [ "protocolVersion"       .= x | x <- mbfield (Babbage._protocolVersion pp) ]
     ++ [ "minPoolCost"           .= x | x <- mbfield (Babbage._minPoolCost pp) ]
     ++ [ "coinsPerUTxOByte"      .= x | x <- mbfield (Babbage._coinsPerUTxOByte pp) ]
     ++ [ "costmdls"              .= x | x <- mbfield (Babbage._costmdls pp) ]
     ++ [ "prices"                .= x | x <- mbfield (Babbage._prices pp) ]
     ++ [ "maxTxExUnits"          .= x | x <- mbfield (Babbage._maxTxExUnits pp) ]
     ++ [ "maxBlockExUnits"       .= x | x <- mbfield (Babbage._maxBlockExUnits pp) ]
     ++ [ "maxValSize"            .= x | x <- mbfield (Babbage._maxValSize pp) ]
     ++ [ "collateralPercentage"  .= x | x <- mbfield (Babbage._collateralPercentage pp) ]
     ++ [ "maxCollateralInputs"   .= x | x <- mbfield (Babbage._maxCollateralInputs pp) ]

instance ToJSON (BabbagePParams (Babbage.BabbageEra Consensus.StandardCrypto)) where
  toJSON pp =
    Aeson.object
      [ "minFeeA" .= Babbage._minfeeA pp
      , "minFeeB" .= Babbage._minfeeB pp
      , "maxBlockBodySize" .= Babbage._maxBBSize pp
      , "maxTxSize" .= Babbage._maxTxSize pp
      , "maxBlockHeaderSize" .= Babbage._maxBHSize pp
      , "keyDeposit" .= Babbage._keyDeposit pp
      , "poolDeposit" .= Babbage._poolDeposit pp
      , "eMax" .= Babbage._eMax pp
      , "nOpt" .= Babbage._nOpt pp
      , "a0" .= Babbage._a0 pp
      , "rho" .= Babbage._rho pp
      , "tau" .= Babbage._tau pp
      , "protocolVersion" .= Babbage._protocolVersion pp
      , "minPoolCost" .= Babbage._minPoolCost pp
      , "coinsPerUTxOByte" .= Babbage._coinsPerUTxOByte pp
      , "costmdls" .= Babbage._costmdls pp
      , "prices" .= Babbage._prices pp
      , "maxTxExUnits" .= Babbage._maxTxExUnits pp
      , "maxBlockExUnits" .= Babbage._maxBlockExUnits pp
      , "maxValSize" .= Babbage._maxValSize pp
      , "collateralPercentage" .= Babbage._collateralPercentage pp
      , "maxCollateralInputs" .= Babbage._maxCollateralInputs pp
      ]

mbfield :: StrictMaybe a -> [a]
mbfield SNothing  = []
mbfield (SJust x) = [x]

instance ( Ledger.Era era
         , ToJSON (Core.Value era)
         , ToJSON (Babbage.Datum era)
         , ToJSON (Core.Script era)
         , Ledger.Crypto era ~ Consensus.StandardCrypto
         , Val (Core.Value era)
         ) => ToJSON (BabbageTxOut era) where
  toJSON = object . toBabbageTxOutPairs
  toEncoding = Aeson.pairs . mconcat . toBabbageTxOutPairs

toBabbageTxOutPairs ::
  ( Aeson.KeyValue a
  , Ledger.Era era
  , ToJSON (Core.Value era)
  , ToJSON (Core.Script era)
  , Ledger.Crypto era ~ Consensus.StandardCrypto
  , Val (Core.Value era)
  ) => BabbageTxOut era -> [a]
toBabbageTxOutPairs (BabbageTxOut !addr !val !dat !mRefScript) =
  [ "address" .= addr
  , "value" .= val
  , "datum" .= dat
  , "referenceScript" .= mRefScript
  ]

instance ( Ledger.Era era
         , Ledger.Crypto era ~ Consensus.StandardCrypto
         ) => ToJSON (Babbage.Datum era) where
  toJSON d =
    case Alonzo.datumDataHash d of
      SNothing -> Aeson.Null
      SJust dH -> toJSON $ ScriptDataHash dH
  toEncoding d =
    case Alonzo.datumDataHash d of
      SNothing -> toEncoding Aeson.Null
      SJust dH -> toEncoding $ ScriptDataHash dH



instance ToJSON (AlonzoScript (Babbage.BabbageEra Consensus.StandardCrypto)) where
  toJSON = Aeson.String . Text.decodeUtf8 . B16.encode . CBOR.serialize'

instance Crypto.Crypto crypto => ToJSON (Shelley.DPState crypto) where
  toJSON = object . toDpStatePairs
  toEncoding = Aeson.pairs . mconcat . toDpStatePairs

toDpStatePairs ::
  ( Aeson.KeyValue a
  , Crypto.Crypto crypto
  ) => ShelleyLedger.DPState crypto -> [a]
toDpStatePairs dpState =
  let !dstate = Shelley.dpsDState dpState
      !pstate = Shelley.dpsPState dpState
  in  [ "dstate" .= dstate
      , "pstate" .= pstate
      ]

instance (ToJSON coin, ToJSON ptr, ToJSON pool) => ToJSON (Trip coin ptr pool) where
  toJSON = object . toTripPair
  toEncoding = Aeson.pairs . mconcat . toTripPair

toTripPair ::
  ( Aeson.KeyValue a
  , ToJSON coin
  , ToJSON ptr
  , ToJSON pool
  ) => Trip coin ptr pool -> [a]
toTripPair (Triple !coin !ptr !pool) =
  [ "coin" .= coin
  , "ptr" .= ptr
  , "pool" .= pool
  ]

instance Crypto.Crypto crypto => ToJSON (UnifiedMap crypto) where
  toJSON = object . toUnifiedMapPair
  toEncoding = Aeson.pairs . mconcat . toUnifiedMapPair

toUnifiedMapPair ::
  ( Aeson.KeyValue a
  , ToJSON coin
  , ToJSON ptr
  , ToJSON pool
  , ToJSON cred
  , ToJSONKey cred
  , ToJSONKey ptr
  ) => UMap coin cred pool ptr -> [a]
toUnifiedMapPair (UnifiedMap !m1 !m2) =
  [ "credentials" .= m1
  , "pointers" .= m2
  ]

instance Crypto.Crypto crypto => ToJSON (Shelley.DState crypto) where
  toJSON = object . toDStatePair
  toEncoding = Aeson.pairs . mconcat . toDStatePair

toDStatePair ::
  ( Aeson.KeyValue a
  , Crypto.Crypto crypto
  ) => ShelleyLedger.DState crypto -> [a]
toDStatePair dState =
  let !unifiedRewards = Shelley._unified dState
      !fGenDelegs = Map.toList (Shelley._fGenDelegs dState)
      !genDelegs = Shelley._genDelegs dState
      !irwd = Shelley._irwd dState
  in  [ "unifiedRewards" .= unifiedRewards
      , "fGenDelegs" .= fGenDelegs
      , "genDelegs" .= genDelegs
      , "irwd" .= irwd
      ]

instance Crypto.Crypto crypto => ToJSON (ShelleyLedger.FutureGenDeleg crypto) where
  toJSON fGenDeleg =
    object [ "fGenDelegSlot" .= ShelleyLedger.fGenDelegSlot fGenDeleg
           , "fGenDelegGenKeyHash" .= ShelleyLedger.fGenDelegGenKeyHash fGenDeleg
           ]

instance Crypto.Crypto crypto => ToJSON (Shelley.GenDelegs crypto) where
  toJSON (Shelley.GenDelegs delegs) = toJSON delegs
  toEncoding (Shelley.GenDelegs delegs) = toEncoding delegs

instance Crypto.Crypto crypto => ToJSON (Shelley.InstantaneousRewards crypto) where
  toJSON = object . toInstantaneousRewardsPair
  toEncoding = Aeson.pairs . mconcat . toInstantaneousRewardsPair

toInstantaneousRewardsPair ::
  ( Aeson.KeyValue a
  , Crypto.Crypto crypto
  ) => ShelleyLedger.InstantaneousRewards crypto -> [a]
toInstantaneousRewardsPair iRwds =
  let !iRReserves = Shelley.iRReserves iRwds
      !iRTreasury = Shelley.iRTreasury iRwds
  in  [ "iRReserves" .= iRReserves
      , "iRTreasury" .= iRTreasury
      ]

instance
  Crypto.Crypto crypto =>
  ToJSON (Bimap Shelley.Ptr (Shelley.Credential Shelley.Staking crypto))
  where
  toJSON = object . toPtrCredentialStakingPair
  toEncoding = Aeson.pairs . mconcat . toPtrCredentialStakingPair

toPtrCredentialStakingPair ::
  ( Aeson.KeyValue a
  , Crypto.Crypto crypto
  ) => Bimap Shelley.Ptr (Shelley.Credential Shelley.Staking crypto) -> [a]
toPtrCredentialStakingPair (MkBiMap ptsStakeM stakePtrSetM) =
  let !stakedCreds = Map.toList ptsStakeM
      !credPtrR = stakePtrSetM
  in  [ "stakedCreds" .= stakedCreds
      , "credPtrR" .= credPtrR
      ]

deriving newtype instance ToJSON Shelley.CertIx
deriving newtype instance ToJSON Shelley.TxIx

instance ToJSON Shelley.Ptr where
  toJSON = object . toPtrPair
  toEncoding = Aeson.pairs . mconcat . toPtrPair

instance ToJSONKey Shelley.Ptr

toPtrPair :: Aeson.KeyValue a => Shelley.Ptr -> [a]
toPtrPair (Shelley.Ptr !slotNo !txIndex !certIndex) =
  [ "slot" .= unSlotNo slotNo
  , "txIndex" .= txIndex
  , "certIndex" .= certIndex
  ]


instance Crypto.Crypto crypto => ToJSON (Shelley.PState crypto) where
  toJSON = object . toPStatePair
  toEncoding = Aeson.pairs . mconcat . toPStatePair

toPStatePair ::
  ( Aeson.KeyValue a
  , Crypto.Crypto crypto
  ) => ShelleyLedger.PState crypto -> [a]
toPStatePair pState =
  let !pParams = Shelley._pParams pState
      !fPParams = Shelley._fPParams pState
      !retiring = Shelley._retiring pState
  in  [ "pParams pState" .= pParams
      , "fPParams pState" .= fPParams
      , "retiring pState" .= retiring
      ]

instance ( Consensus.ShelleyBasedEra era
         , ToJSON (Core.TxOut era)
         ) => ToJSON (Shelley.UTxO era) where
  toJSON (Shelley.UTxO utxo) = toJSON utxo
  toEncoding (Shelley.UTxO utxo) = toEncoding utxo

instance ( Consensus.ShelleyBasedEra era
         , ToJSON (Core.Value era)
         ) => ToJSON (ShelleyTxOut era) where
  toJSON = object . toTxOutPair
  toEncoding = Aeson.pairs . mconcat . toTxOutPair

toTxOutPair ::
  ( Aeson.KeyValue a
  , ToJSON (Core.Value era)
  , EraTxOut era)
  => ShelleyTxOut era -> [a]
toTxOutPair (ShelleyTxOut !addr !amount) =
  [ "address" .= addr
  , "amount" .= amount
  ]

instance Crypto.Crypto crypto => ToJSON (Shelley.TxIn crypto) where
  toJSON = toJSON . txInToText
  toEncoding = toEncoding . txInToText

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
  toJSON = object . toNonMyopicPair
  toEncoding = Aeson.pairs . mconcat . toNonMyopicPair

toNonMyopicPair ::
  ( Aeson.KeyValue a
  , Crypto.Crypto crypto
  ) => Shelley.NonMyopic crypto -> [a]
toNonMyopicPair nonMy =
  let !likelihoodsNM = Shelley.likelihoodsNM nonMy
      !rewardPotNM = Shelley.rewardPotNM nonMy
  in  [ "likelihoodsNM" .= likelihoodsNM
      , "rewardPotNM" .= rewardPotNM
      ]

instance ToJSON Shelley.Likelihood where
  toJSON (Shelley.Likelihood llhd) =
    toJSON $ fmap (\(Shelley.LogWeight f) -> exp $ realToFrac f :: Double) llhd
  toEncoding (Shelley.Likelihood llhd) =
    toEncoding $ fmap (\(Shelley.LogWeight f) -> exp $ realToFrac f :: Double) llhd

instance Crypto.Crypto crypto => ToJSON (Shelley.SnapShots crypto) where
  toJSON = object . toSnapShotsPair
  toEncoding = Aeson.pairs . mconcat . toSnapShotsPair

toSnapShotsPair ::
  ( Aeson.KeyValue a
  , Crypto.Crypto crypto
  ) => ShelleyEpoch.SnapShots crypto -> [a]
toSnapShotsPair ss =
  let !pstakeMark = Shelley._pstakeMark ss
      !pstakeSet = Shelley._pstakeSet ss
      !pstakeGo = Shelley._pstakeGo ss
      !feeSS = Shelley._feeSS ss
  in  [ "pstakeMark" .= pstakeMark
      , "pstakeSet" .= pstakeSet
      , "pstakeGo" .= pstakeGo
      , "feeSS" .= feeSS
      ]

instance Crypto.Crypto crypto => ToJSON (Shelley.SnapShot crypto) where
  toJSON = object . toSnapShotPair
  toEncoding = Aeson.pairs . mconcat . toSnapShotPair

toSnapShotPair ::
  ( Aeson.KeyValue a
  , Crypto.Crypto crypto
  ) => ShelleyEpoch.SnapShot crypto -> [a]
toSnapShotPair ss =
  let !stake = Shelley._stake ss
      !delegations = ShelleyEpoch._delegations ss
      !poolParams = Shelley._poolParams ss
  in  [ "stake" .= stake
      , "delegations" .= delegations
      , "poolParams" .= poolParams
      ]

instance Crypto.Crypto crypto => ToJSON (Shelley.Stake crypto) where
  toJSON (Shelley.Stake s) = toJSON s
  toEncoding (Shelley.Stake s) = toEncoding s

instance Crypto.Crypto crypto => ToJSON (Shelley.RewardUpdate crypto) where
  toJSON = object . toRewardUpdatePair
  toEncoding = Aeson.pairs . mconcat . toRewardUpdatePair

toRewardUpdatePair ::
  ( Aeson.KeyValue a
  , Crypto.Crypto crypto
  ) => Shelley.RewardUpdate crypto -> [a]
toRewardUpdatePair rUpdate =
  let !deltaT = Shelley.deltaT rUpdate
      !deltaR = Shelley.deltaR rUpdate
      !rs = Shelley.rs rUpdate
      !deltaF = Shelley.deltaF rUpdate
      !nonMyopic = Shelley.nonMyopic rUpdate
  in  [ "deltaT" .= deltaT
      , "deltaR" .= deltaR
      , "rs" .= rs
      , "deltaF" .= deltaF
      , "nonMyopic" .= nonMyopic
      ]

instance Crypto.Crypto crypto => ToJSON (Shelley.PulsingRewUpdate crypto) where
  toJSON  = \case
    Shelley.Pulsing _ _ -> Aeson.Null
    Shelley.Complete ru -> toJSON ru
  toEncoding  = \case
    Shelley.Pulsing _ _ -> toEncoding Aeson.Null
    Shelley.Complete ru -> toEncoding ru

instance ToJSON Shelley.DeltaCoin where
  toJSON (Shelley.DeltaCoin i) = toJSON i
  toEncoding (Shelley.DeltaCoin i) = toEncoding i

instance Crypto.Crypto crypto => ToJSON (Ledger.PoolDistr crypto) where
  toJSON (Ledger.PoolDistr m) = toJSON m
  toEncoding (Ledger.PoolDistr m) = toEncoding m

instance Crypto.Crypto crypto => ToJSON (Ledger.IndividualPoolStake crypto) where
  toJSON = object . toIndividualPoolStakePair
  toEncoding = Aeson.pairs . mconcat . toIndividualPoolStakePair

toIndividualPoolStakePair ::
  ( Aeson.KeyValue a
  , Crypto.HashAlgorithm (Crypto.HASH crypto)
  ) => Ledger.IndividualPoolStake crypto -> [a]
toIndividualPoolStakePair indivPoolStake =
  let !individualPoolStake = Ledger.individualPoolStake indivPoolStake
      !individualPoolStakeVrf = Ledger.individualPoolStakeVrf indivPoolStake
  in  [ "individualPoolStake" .= individualPoolStake
      , "individualPoolStakeVrf" .= individualPoolStakeVrf
      ]

instance Crypto.Crypto crypto => ToJSON (Shelley.Reward crypto) where
  toJSON = object . toRewardPair
  toEncoding = Aeson.pairs . mconcat . toRewardPair

toRewardPair ::
  ( Aeson.KeyValue a
  , Crypto.Crypto crypto
  ) => Shelley.Reward crypto -> [a]
toRewardPair reward =
  let !rewardType = Shelley.rewardType reward
      !rewardPool = Shelley.rewardPool reward
      !rewardAmount = Shelley.rewardAmount reward
  in  [ "rewardType" .= rewardType
      , "rewardPool" .= rewardPool
      , "rewardAmount" .= rewardAmount
      ]

instance ToJSON Shelley.RewardType where
  toJSON Shelley.MemberReward = "MemberReward"
  toJSON Shelley.LeaderReward = "LeaderReward"

instance Crypto.Crypto c => ToJSON (SafeHash.SafeHash c a) where
  toJSON = toJSON . SafeHash.extractHash
  toEncoding = toEncoding . SafeHash.extractHash

-----

deriving newtype instance ToJSON SystemStart
deriving newtype instance FromJSON SystemStart


instance Crypto.Crypto crypto => ToJSON (VMap VB VB (Shelley.Credential 'Shelley.Staking crypto) (Shelley.KeyHash 'Shelley.StakePool crypto)) where
  toJSON = toJSON . VMap.toMap
  toEncoding = toEncoding . VMap.toMap

instance Crypto.Crypto crypto => ToJSON (VMap VB VB (Shelley.KeyHash    'Shelley.StakePool crypto) (Shelley.PoolParams crypto)) where
  toJSON = toJSON . VMap.toMap
  toEncoding = toEncoding . VMap.toMap

instance Crypto.Crypto crypto => ToJSON (VMap VB VP (Shelley.Credential 'Shelley.Staking   crypto) (Shelley.CompactForm Shelley.Coin)) where
  toJSON = toJSON . fmap fromCompact . VMap.toMap
  toEncoding = toEncoding . fmap fromCompact . VMap.toMap

instance Crypto.Crypto crypto => ToJSON (Consensus.StakeSnapshots crypto) where
  toJSON = object . stakeSnapshotsToPair
  toEncoding = pairs . mconcat . stakeSnapshotsToPair

stakeSnapshotsToPair :: (Aeson.KeyValue a, Crypto.Crypto crypto) => Consensus.StakeSnapshots crypto -> [a]
stakeSnapshotsToPair Consensus.StakeSnapshots
    { Consensus.ssStakeSnapshots
    , Consensus.ssMarkTotal
    , Consensus.ssSetTotal
    , Consensus.ssGoTotal
    } =
    [ "pools" .= ssStakeSnapshots
    , "total" .= object
      [ "stakeMark" .= ssMarkTotal
      , "stakeSet" .= ssSetTotal
      , "stakeGo" .= ssGoTotal
      ]
    ]

instance ToJSON (Consensus.StakeSnapshot crypto) where
  toJSON = object . stakeSnapshotToPair
  toEncoding = pairs . mconcat . stakeSnapshotToPair

stakeSnapshotToPair :: Aeson.KeyValue a => Consensus.StakeSnapshot crypto -> [a]
stakeSnapshotToPair Consensus.StakeSnapshot
    { Consensus.ssMarkPool
    , Consensus.ssSetPool
    , Consensus.ssGoPool
    } =
    [ "stakeMark" .= ssMarkPool
    , "stakeSet" .= ssSetPool
    , "stakeGo" .= ssGoPool
    ]
