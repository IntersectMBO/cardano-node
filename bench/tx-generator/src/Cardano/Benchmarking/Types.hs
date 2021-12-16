{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Benchmarking.Types
  ( Ack(..)
  , Acked(..)
  , InitCooldown(..)   
  , NodeIPv4Address
  , NumberOfInputsPerTx(..)
  , NumberOfOutputsPerTx(..)
  , NumberOfTxs(..)
  , Req(..)
  , Sent(..)
  , SubmissionErrorPolicy(..)
  , ToAnnce(..)
  , TxAdditionalSize(..)
  , TPSRate(..)
  , UnAcked(..)
  , Unav(..)
  ) where


import           Prelude
import           Data.Word
import           GHC.Generics
import           Data.Aeson

import           Cardano.Node.Types (NodeIPv4Address)

myJsonOptions :: Options
myJsonOptions = defaultOptions {
  unwrapUnaryRecords = True
  }

-- | How long wait before starting the main submission phase,
--   after the init Tx batch was submitted.
newtype InitCooldown =
  InitCooldown Int
  deriving newtype (Eq, Ord, Num, Show)
deriving stock instance Generic InitCooldown
instance ToJSON InitCooldown where
  toJSON     = genericToJSON myJsonOptions
  toEncoding = genericToEncoding myJsonOptions
instance FromJSON InitCooldown where parseJSON = genericParseJSON myJsonOptions

newtype NumberOfInputsPerTx =
  NumberOfInputsPerTx Int
  deriving newtype (Eq, Ord, Enum, Real, Num, Integral, Show)
deriving stock instance Generic NumberOfInputsPerTx
instance ToJSON NumberOfInputsPerTx where
  toJSON     = genericToJSON myJsonOptions
  toEncoding = genericToEncoding myJsonOptions

instance FromJSON NumberOfInputsPerTx where parseJSON = genericParseJSON myJsonOptions

newtype NumberOfOutputsPerTx =
  NumberOfOutputsPerTx Int
  deriving newtype (Eq, Ord, Num, Show)
deriving stock instance Generic NumberOfOutputsPerTx
instance ToJSON NumberOfOutputsPerTx where
  toJSON     = genericToJSON myJsonOptions
  toEncoding = genericToEncoding myJsonOptions
instance FromJSON NumberOfOutputsPerTx where parseJSON = genericParseJSON myJsonOptions

newtype NumberOfTxs =
  NumberOfTxs { unNumberOfTxs :: Word64 }
  deriving newtype (Eq, Ord, Num, Show)
deriving stock instance Generic NumberOfTxs
instance ToJSON NumberOfTxs where
  toJSON     = genericToJSON myJsonOptions
  toEncoding = genericToEncoding myJsonOptions
instance FromJSON NumberOfTxs where parseJSON = genericParseJSON myJsonOptions

newtype TPSRate=
  TPSRate Double
  deriving newtype (Eq, Ord, Num, Show)
deriving stock instance Generic TPSRate
instance ToJSON TPSRate where
  toJSON     = genericToJSON myJsonOptions
  toEncoding = genericToEncoding myJsonOptions
instance FromJSON TPSRate where parseJSON = genericParseJSON myJsonOptions

-- | This parameter specifies additional size (in bytes) of transaction.
--   Since 1 transaction is ([input] + [output] + attributes), its size
--   is defined by its inputs and outputs. We want to have an ability to
--   increase transaction's size without increasing the number of inputs/
--   outputs. Such a big transaction will give us more real-world results
--   of benchmarking.
--   Technically this parameter specifies the size of attribute we'll
--   add to transaction (by default attributes are empty, so if this
--   parameter is skipped, attributes will remain empty).
newtype TxAdditionalSize =
  TxAdditionalSize { unTxAdditionalSize :: Int }
  deriving newtype (Eq, Ord, Num, Show)
deriving stock instance Generic TxAdditionalSize
instance ToJSON TxAdditionalSize where
  toJSON     = genericToJSON myJsonOptions
  toEncoding = genericToEncoding myJsonOptions
instance FromJSON TxAdditionalSize where parseJSON = genericParseJSON myJsonOptions

-- | Transactions we decided to announce now.
newtype ToAnnce tx = ToAnnce [tx]

-- | Transactions announced, yet unacked by peer.
newtype UnAcked tx = UnAcked [tx]

-- | Transactions acked by peer.
newtype Acked tx = Acked [tx]

-- | Peer acknowledged this many txids of the outstanding window.
newtype Ack = Ack Int deriving newtype (Enum, Eq, Integral, Num, Ord, Real)

-- | Peer requested this many txids to add to the outstanding window.
newtype Req = Req Int deriving newtype (Enum, Eq, Integral, Num, Ord, Real, Show)

-- | This many Txs sent to peer.
newtype Sent = Sent Int deriving newtype (Enum, Eq, Integral, Num, Ord, Real, Show)
deriving stock instance Generic Sent

-- | This many Txs requested by the peer, but not available for sending.
newtype Unav = Unav Int deriving newtype (Enum, Eq, Integral, Num, Ord, Real, Show)
deriving stock instance Generic Unav

data SubmissionErrorPolicy
  = FailOnError
  | LogErrors
  deriving stock (Eq, Show)

instance ToJSON Sent
instance ToJSON Unav
