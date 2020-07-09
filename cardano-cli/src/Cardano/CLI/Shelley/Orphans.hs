{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.CLI.Shelley.Orphans () where

import           Cardano.Prelude

import           Data.Aeson (ToJSON (..), ToJSONKey (..),
                   ToJSONKeyFunction (..), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Cardano.Crypto.Hash.Class as Crypto
import           Cardano.TracingOrphanInstances.Common ()

import           Ouroboros.Consensus.Shelley.Protocol.Crypto (TPraosStandardCrypto)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyHash(..))

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
import qualified Shelley.Spec.Ledger.TxData as Ledger
import           Shelley.Spec.Ledger.UTxO (UTxO(..))

instance Crypto c => ToJSONKey (TxIn c) where
  toJSONKey = ToJSONKeyText txInToText (Aeson.text . txInToText)

txInToText :: TxIn c -> Text
txInToText (TxIn (TxId txidHash) ix) =
  hashToText txidHash
    <> Text.pack "#"
    <> Text.pack (show ix)

hashToText :: Hash crypto a -> Text
hashToText = Text.decodeLatin1 . Crypto.getHashBytesAsHex

deriving instance Crypto c => ToJSON (TxIn c)

instance Crypto c => ToJSON (TxOut c) where
  toJSON (TxOut addr amount) =
    Aeson.object
      [ "address" .= addr
      , "amount" .= amount
      ]


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
deriving anyclass instance ToJSON (Ledger.StakePools TPraosStandardCrypto)

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
