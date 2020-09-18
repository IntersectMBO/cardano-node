{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.CLI.Shelley.Orphans () where

import           Cardano.Prelude

import           Control.Iterate.SetAlgebra as SetAlgebra
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
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardShelley)
import           Ouroboros.Network.Block (BlockNo (..), HeaderHash, Tip (..))

import           Cardano.Ledger.Era (Era)

import           Shelley.Spec.Ledger.BaseTypes (StrictMaybe)
import           Shelley.Spec.Ledger.BlockChain (HashHeader (..))
import qualified Shelley.Spec.Ledger.Credential as Ledger
import qualified Shelley.Spec.Ledger.Delegation.Certificates as Ledger
import qualified Shelley.Spec.Ledger.EpochBoundary as Ledger
import qualified Shelley.Spec.Ledger.Keys as Ledger
import qualified Shelley.Spec.Ledger.LedgerState as Ledger
import           Shelley.Spec.Ledger.MetaData (MetaDataHash (..))
import qualified Shelley.Spec.Ledger.PParams as Ledger
import qualified Shelley.Spec.Ledger.Rewards as Ledger
import           Shelley.Spec.Ledger.TxBody (TxId (..), TxIn (..), TxOut (..))
import           Shelley.Spec.Ledger.UTxO (UTxO (..))

instance Era era => ToJSONKey (TxIn era) where
  toJSONKey = ToJSONKeyText txInToText (Aeson.text . txInToText)

txInToText :: Era era => TxIn era -> Text
txInToText (TxIn (TxId txidHash) ix) =
  hashToText txidHash
    <> Text.pack "#"
    <> Text.pack (show ix)

hashToText :: Hash crypto a -> Text
hashToText = Text.decodeLatin1 . Crypto.hashToBytesAsHex

deriving instance Era era => ToJSON (TxIn era)

instance Era era => ToJSON (TxOut era) where
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

deriving newtype instance Era era => ToJSON (UTxO era)

deriving newtype instance ToJSON (ShelleyHash era)
deriving newtype instance ToJSON (HashHeader era)

deriving newtype instance ToJSON (MetaDataHash era)
deriving newtype instance ToJSON Ledger.LogWeight
deriving newtype instance ToJSON Ledger.Likelihood
deriving newtype instance ToJSON (Ledger.Stake StandardShelley)

deriving anyclass instance ToJSON (Ledger.GenDelegs StandardShelley)
deriving anyclass instance ToJSON (Ledger.IndividualPoolStake StandardShelley)
deriving anyclass instance ToJSON (Ledger.ProposedPPUpdates StandardShelley)
deriving anyclass instance ToJSON (Ledger.PPUPState StandardShelley)

deriving instance ToJSON Ledger.Ptr
deriving instance ToJSON Ledger.AccountState

deriving instance ToJSON (Ledger.DPState StandardShelley)
deriving instance ToJSON (Ledger.DState StandardShelley)
deriving instance ToJSON (Ledger.FutureGenDeleg StandardShelley)
deriving instance ToJSON (Ledger.InstantaneousRewards StandardShelley)
deriving instance ToJSON (Ledger.SnapShot StandardShelley)
deriving instance ToJSON (Ledger.SnapShots StandardShelley)
deriving instance ToJSON (Ledger.NonMyopic StandardShelley)
deriving instance ToJSON (Ledger.LedgerState StandardShelley)
deriving instance ToJSON (Ledger.EpochState StandardShelley)
deriving instance ToJSON (Ledger.PParams' StrictMaybe StandardShelley)
deriving instance ToJSON (Ledger.PState StandardShelley)
deriving instance ToJSON (Ledger.StakeReference StandardShelley)
deriving instance ToJSON (Ledger.UTxOState StandardShelley)

deriving instance ToJSONKey Ledger.Ptr
deriving instance ToJSONKey (Ledger.FutureGenDeleg StandardShelley)

instance (ToJSONKey k, ToJSON v) => ToJSON (SetAlgebra.BiMap v k v) where
  toJSON = toJSON . SetAlgebra.forwards -- to normal Map
