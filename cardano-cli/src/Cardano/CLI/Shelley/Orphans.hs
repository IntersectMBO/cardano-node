{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.CLI.Shelley.Orphans () where

import           Cardano.Prelude

import           Control.SetAlgebra as SetAlgebra
import           Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Short as SBS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Cardano.Crypto.Hash.Class as Crypto

import           Ouroboros.Consensus.Byron.Ledger.Block (ByronHash (..))
import           Ouroboros.Consensus.HardFork.Combinator (OneEraHash (..))
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyHash (..))
import           Ouroboros.Consensus.Shelley.Eras
                   (ShelleyBasedEra, StandardCrypto,
                    StandardShelley, StandardAllegra, StandardMary)
import           Ouroboros.Network.Block (BlockNo (..), HeaderHash, Tip (..))

import           Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash (..))
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.Core as Core

import qualified Shelley.Spec.Ledger.API.Protocol as Ledger
import           Shelley.Spec.Ledger.BaseTypes (StrictMaybe)
import           Shelley.Spec.Ledger.BlockChain (HashHeader (..))
import           Shelley.Spec.Ledger.Coin (DeltaCoin (..))
import qualified Shelley.Spec.Ledger.Credential as Ledger
import qualified Shelley.Spec.Ledger.Delegation.Certificates as Ledger
import qualified Shelley.Spec.Ledger.EpochBoundary as Ledger
import qualified Shelley.Spec.Ledger.Keys as Ledger
import qualified Shelley.Spec.Ledger.LedgerState as Ledger
import qualified Shelley.Spec.Ledger.PParams as Ledger
import qualified Shelley.Spec.Ledger.Rewards as Ledger
import qualified Shelley.Spec.Ledger.STS.Prtcl as Ledger
import qualified Shelley.Spec.Ledger.STS.Tickn as Ledger
import           Shelley.Spec.Ledger.TxBody (TxId (..), TxIn (..), TxOut (..))
import           Shelley.Spec.Ledger.UTxO (UTxO (..))

import qualified Cardano.Ledger.Mary.Value as Ledger.Mary

instance ToJSONKey (TxIn StandardCrypto) where
  toJSONKey = ToJSONKeyText txInToText (Aeson.text . txInToText)

txInToText :: TxIn StandardCrypto -> Text
txInToText (TxIn (TxId txidHash) ix) =
  hashToText txidHash
    <> Text.pack "#"
    <> Text.pack (show ix)

hashToText :: Hash crypto a -> Text
hashToText = Text.decodeLatin1 . Crypto.hashToBytesAsHex

deriving instance ToJSON (TxIn StandardCrypto)

instance (ShelleyBasedEra era, ToJSON (Core.Value era)) => ToJSON (TxOut era) where
  toJSON (TxOut addr amount) =
    Aeson.object
      [ "address" .= addr
      , "amount" .= amount
      ]

instance ToJSON (OneEraHash xs) where
  toJSON = toJSON
         . Text.decodeLatin1
         . Base16.encode
         . SBS.fromShort
         . getOneEraHash

deriving newtype instance ToJSON ByronHash

-- This instance is temporarily duplicated in cardano-config

instance ToJSON (HeaderHash blk) => ToJSON (Tip blk) where
  toJSON TipGenesis = object [ "genesis" .= True ]
  toJSON (Tip slotNo headerHash blockNo) =
    object
      [ "slotNo"     .= slotNo
      , "headerHash" .= headerHash
      , "blockNo"    .= blockNo
      ]

-- This instance is temporarily duplicated in cardano-config
deriving newtype instance ToJSON BlockNo

--
-- Simple newtype wrappers JSON conversion
--

deriving newtype instance ToJSON (TxId era)

deriving newtype instance ( ShelleyBasedEra era
                          , ToJSON (Core.Value era)
                          , Ledger.Crypto era ~ StandardCrypto
                          ) => ToJSON (UTxO era)

deriving newtype instance ToJSON (ShelleyHash era)
deriving newtype instance ToJSON (HashHeader era)

deriving newtype instance ToJSON (AuxiliaryDataHash StandardCrypto)
deriving newtype instance ToJSON Ledger.LogWeight
deriving newtype instance ToJSON Ledger.Likelihood
deriving newtype instance ToJSON (Ledger.PoolDistr StandardCrypto)
deriving newtype instance ToJSON DeltaCoin

deriving newtype instance ToJSON (Ledger.Stake StandardCrypto)

deriving anyclass instance ToJSON (Ledger.GenDelegs StandardCrypto)
deriving anyclass instance ToJSON (Ledger.IndividualPoolStake StandardCrypto)

deriving anyclass instance ToJSON (Ledger.ProposedPPUpdates StandardShelley)
deriving anyclass instance ToJSON (Ledger.PPUPState StandardShelley)

deriving anyclass instance ToJSON (Ledger.ProposedPPUpdates StandardAllegra)
deriving anyclass instance ToJSON (Ledger.PPUPState StandardAllegra)

deriving anyclass instance ToJSON (Ledger.ProposedPPUpdates StandardMary)
deriving anyclass instance ToJSON (Ledger.PPUPState StandardMary)

deriving instance ToJSON Ledger.Ptr
deriving instance ToJSON Ledger.AccountState

deriving instance ToJSON (Ledger.DPState StandardCrypto)
deriving instance ToJSON (Ledger.DState StandardCrypto)
deriving instance ToJSON (Ledger.InstantaneousRewards StandardCrypto)
deriving instance ToJSON (Ledger.NonMyopic StandardCrypto)
deriving instance ToJSON (Ledger.PState StandardCrypto)
deriving instance ToJSON (Ledger.RewardUpdate StandardCrypto)
deriving instance ToJSON (Ledger.SnapShot StandardCrypto)
deriving instance ToJSON (Ledger.SnapShots StandardCrypto)
deriving instance ToJSON (Ledger.StakeReference StandardCrypto)

deriving instance ToJSON (Ledger.LedgerState StandardShelley)
deriving instance ToJSON (Ledger.EpochState StandardShelley)
deriving instance ToJSON (Ledger.NewEpochState StandardShelley)
deriving instance ToJSON (Ledger.PParams' StrictMaybe StandardShelley)
deriving instance ToJSON (Ledger.UTxOState StandardShelley)

deriving instance ToJSON (Ledger.LedgerState StandardAllegra)
deriving instance ToJSON (Ledger.EpochState StandardAllegra)
deriving instance ToJSON (Ledger.NewEpochState StandardAllegra)
deriving instance ToJSON (Ledger.PParams' StrictMaybe StandardAllegra)
deriving instance ToJSON (Ledger.UTxOState StandardAllegra)

deriving instance ToJSON (Ledger.LedgerState StandardMary)
deriving instance ToJSON (Ledger.EpochState StandardMary)
deriving instance ToJSON (Ledger.NewEpochState StandardMary)
deriving instance ToJSON (Ledger.PParams' StrictMaybe StandardMary)
deriving instance ToJSON (Ledger.UTxOState StandardMary)

deriving instance ToJSON (Ledger.FutureGenDeleg StandardCrypto)
deriving instance ToJSON (Ledger.PrtclState StandardCrypto)
deriving instance ToJSON Ledger.TicknState
deriving instance ToJSON (Ledger.ChainDepState StandardCrypto)

deriving instance ToJSONKey Ledger.Ptr
deriving instance ToJSONKey (Ledger.FutureGenDeleg StandardCrypto)

deriving anyclass instance ToJSON    (Ledger.Mary.Value StandardCrypto)
deriving newtype  instance ToJSON    (Ledger.Mary.PolicyID StandardCrypto)
deriving anyclass instance ToJSONKey (Ledger.Mary.PolicyID StandardCrypto)
deriving anyclass instance ToJSONKey  Ledger.Mary.AssetName

instance ToJSON Ledger.Mary.AssetName where
  toJSON (Ledger.Mary.AssetName bs) = toJSON (Text.decodeLatin1 bs)

instance (ToJSONKey k, ToJSON v) => ToJSON (SetAlgebra.BiMap v k v) where
  toJSON = toJSON . SetAlgebra.forwards -- to normal Map
