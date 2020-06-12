{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Config.Shelley.Orphans () where

import           Cardano.Prelude

import           Data.Aeson (ToJSON (..), ToJSONKey (..),
                   ToJSONKeyFunction (..), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import           Data.ByteString.Char8 as BS
import           Data.Char (isPrint)
import           Data.IP (IPv4, IPv6)
import           Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Cardano.Crypto.Hash.Class as Crypto
import           Cardano.TracingOrphanInstances.Common ()

import           Ouroboros.Consensus.Shelley.Protocol.Crypto (TPraosStandardCrypto)

import           Shelley.Spec.Ledger.BaseTypes (DnsName, Port, StrictMaybe)
import           Shelley.Spec.Ledger.Credential (StakeCredential, Credential (..))
import qualified Shelley.Spec.Ledger.Credential as Ledger
import           Shelley.Spec.Ledger.Crypto (Crypto)
import qualified Shelley.Spec.Ledger.Keys as Ledger
import qualified Shelley.Spec.Ledger.LedgerState as Ledger
import           Shelley.Spec.Ledger.MetaData (MetaDataHash(..))
import           Shelley.Spec.Ledger.PParams (PParams' (..))
import qualified Shelley.Spec.Ledger.PParams as Ledger
import           Shelley.Spec.Ledger.Scripts (ScriptHash (..))
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

deriving instance Crypto c => ToJSON (TxIn c)

instance Crypto c => ToJSON (TxOut c) where
  toJSON (TxOut addr amount) =
    Aeson.object
      [ "address" .= addr
      , "amount" .= amount
      ]

deriving instance ToJSONKey (StakeCredential c)

deriving instance ToJSON (StakeCredential c)

--
-- Simple newtype wrappers JSON conversion
--

deriving newtype instance ToJSON (TxId c)

deriving newtype instance Crypto c => ToJSON (UTxO c)

deriving anyclass instance ToJSONKey (Ledger.RewardAcnt c)

deriving anyclass instance ToJSON (Ledger.RewardAcnt c)

deriving newtype instance ToJSON (ScriptHash c)

deriving newtype instance ToJSON (MetaDataHash c)

hashToText :: Hash crypto a -> Text
hashToText = Text.decodeLatin1 . Crypto.getHashBytesAsHex

--
-- Addresses JSON conversion, including as map keys
--

-- We are deriving ToJSON instances for all of these types mainly so we can dump
-- a JSON representation for the purposes of debug. Therefore ByteString that are
-- not printable should be hex encoded for readability.
instance ToJSON ByteString where
  toJSON bs =
    toJSON $
      if BS.all isPrint bs
        then bs
        else Base16.encode bs

instance ToJSON a => ToJSON (StrictSeq a) where
  toJSON ss =
    toJSON $ toList (getSeq ss)

deriving instance ToJSON a => ToJSON (StrictMaybe a)

deriving anyclass instance ToJSON DnsName
deriving anyclass instance ToJSON IPv4
deriving anyclass instance ToJSON IPv6
deriving anyclass instance ToJSON Ledger.Url
deriving anyclass instance ToJSON Port

deriving anyclass instance ToJSON (Ledger.GenDelegs TPraosStandardCrypto)
deriving anyclass instance ToJSON (Ledger.ProposedPPUpdates TPraosStandardCrypto)
deriving anyclass instance ToJSON (Ledger.StakeCreds TPraosStandardCrypto)
deriving anyclass instance ToJSON (Ledger.StakePools TPraosStandardCrypto)

deriving instance ToJSON Ledger.PoolMetaData
deriving instance ToJSON Ledger.Ptr
deriving instance ToJSON Ledger.StakePoolRelay

deriving instance ToJSON (Ledger.DPState TPraosStandardCrypto)
deriving instance ToJSON (Ledger.DState TPraosStandardCrypto)
deriving instance ToJSON (Ledger.FutureGenDeleg TPraosStandardCrypto)
deriving instance ToJSON (Ledger.InstantaneousRewards TPraosStandardCrypto)
deriving instance ToJSON (Ledger.LedgerState TPraosStandardCrypto)
deriving instance ToJSON (Ledger.PoolParams TPraosStandardCrypto)
deriving instance ToJSON (Ledger.PParams' StrictMaybe)
deriving instance ToJSON (Ledger.PState TPraosStandardCrypto)
deriving instance ToJSON (Ledger.StakeReference TPraosStandardCrypto)
deriving instance ToJSON (Ledger.UTxOState TPraosStandardCrypto)

deriving instance ToJSONKey Ledger.Ptr
deriving instance ToJSONKey (Ledger.FutureGenDeleg TPraosStandardCrypto)
