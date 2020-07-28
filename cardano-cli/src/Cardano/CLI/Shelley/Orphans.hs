{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.CLI.Shelley.Orphans () where

import           Cardano.Prelude

import           Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Control.Iterate.SetAlgebra as SetAlgebra

import           Cardano.Crypto.Hash.Class as Crypto

import           Ouroboros.Consensus.Byron.Ledger.Block (ByronHash(..))
import           Ouroboros.Consensus.HardFork.Combinator (OneEraHash(..))
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (TPraosStandardCrypto)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyHash(..))
import           Ouroboros.Network.Block (BlockNo(..), HeaderHash, Tip (..))

import           Shelley.Spec.Ledger.BaseTypes (StrictMaybe)
import           Shelley.Spec.Ledger.BlockChain (HashHeader(..))
import qualified Shelley.Spec.Ledger.Credential as Ledger
import           Shelley.Spec.Ledger.Crypto (Crypto)
import qualified Shelley.Spec.Ledger.EpochBoundary as Ledger
import qualified Shelley.Spec.Ledger.Keys as Ledger
import qualified Shelley.Spec.Ledger.LedgerState as Ledger
import qualified Shelley.Spec.Ledger.Rewards as Ledger
import           Shelley.Spec.Ledger.MetaData (MetaDataHash(..))
import           Shelley.Spec.Ledger.PParams (PParams' (..))
import qualified Shelley.Spec.Ledger.PParams as Ledger
import           Shelley.Spec.Ledger.TxData (TxId(..), TxIn(..), TxOut(..))
import           Shelley.Spec.Ledger.UTxO (UTxO(..))

instance Crypto c => ToJSONKey (TxIn c) where
  toJSONKey = ToJSONKeyText txInToText (Aeson.text . txInToText)

txInToText :: Crypto c => TxIn c -> Text
txInToText (TxIn (TxId txidHash) ix) =
  hashToText txidHash
    <> Text.pack "#"
    <> Text.pack (show ix)

hashToText :: Hash crypto a -> Text
hashToText = Text.decodeLatin1 . Crypto.hashToBytesAsHex

deriving instance Crypto c => ToJSON (TxIn c)

instance Crypto c => ToJSON (TxOut c) where
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

deriving newtype instance ToJSON (TxId c)

deriving newtype instance Crypto c => ToJSON (UTxO c)

deriving newtype instance ToJSON (ShelleyHash c)
deriving newtype instance ToJSON (HashHeader c)

deriving newtype instance ToJSON (MetaDataHash c)
deriving newtype instance ToJSON Ledger.LogWeight
deriving newtype instance ToJSON Ledger.Likelihood
deriving newtype instance ToJSON (Ledger.Stake TPraosStandardCrypto)

deriving anyclass instance ToJSON (Ledger.GenDelegs TPraosStandardCrypto)
deriving anyclass instance ToJSON (Ledger.ProposedPPUpdates TPraosStandardCrypto)
deriving anyclass instance ToJSON (Ledger.PPUPState TPraosStandardCrypto)

deriving instance ToJSON Ledger.Ptr
deriving instance ToJSON Ledger.AccountState

deriving instance ToJSON (Ledger.DPState TPraosStandardCrypto)
deriving instance ToJSON (Ledger.DState TPraosStandardCrypto)
deriving instance ToJSON (Ledger.FutureGenDeleg TPraosStandardCrypto)
deriving instance ToJSON (Ledger.InstantaneousRewards TPraosStandardCrypto)
deriving instance ToJSON (Ledger.SnapShot TPraosStandardCrypto)
deriving instance ToJSON (Ledger.SnapShots TPraosStandardCrypto)
deriving instance ToJSON (Ledger.NonMyopic TPraosStandardCrypto)
deriving instance ToJSON (Ledger.LedgerState TPraosStandardCrypto)
deriving instance ToJSON (Ledger.EpochState TPraosStandardCrypto)
deriving instance ToJSON (Ledger.PParams' StrictMaybe)
deriving instance ToJSON (Ledger.PState TPraosStandardCrypto)
deriving instance ToJSON (Ledger.StakeReference TPraosStandardCrypto)
deriving instance ToJSON (Ledger.UTxOState TPraosStandardCrypto)

deriving instance ToJSONKey Ledger.Ptr
deriving instance ToJSONKey (Ledger.FutureGenDeleg TPraosStandardCrypto)

instance (ToJSONKey k, ToJSON v) => ToJSON (SetAlgebra.BiMap v k v) where
  toJSON = toJSON . SetAlgebra.forwards -- to normal Map
