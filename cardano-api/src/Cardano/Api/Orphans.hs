{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Api.Orphans () where

import           Data.Aeson (ToJSON (..), object, pairs, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base16 as Base16
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text

import qualified Cardano.Ledger.Crypto as Crypto

import           Cardano.Ledger.Shelley.API (MIRPot (..))
import qualified Cardano.Ledger.Shelley.API as Ledger (KeyRole (..), WitVKey)

import           Cardano.Api.SerialiseCBOR (ToCBOR (..))
import           Cardano.Binary (encodeListLen, serialize')
import           Codec.CBOR.Encoding (encodeWord)
import qualified Ouroboros.Consensus.Shelley.Ledger.Query as Consensus

-- Orphan instances involved in the JSON output of the API queries.
-- We will remove/replace these as we provide more API wrapper types

-----

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

instance ToJSON MIRPot where
  toJSON pot = toJSON @Text $
    case pot of
      ReservesMIR -> "reserves"
      TreasuryMIR -> "treasury"

instance Crypto.Crypto crypto => ToJSON (Ledger.WitVKey 'Ledger.Witness crypto) where
  toJSON = toJSON . Text.decodeLatin1 . Base16.encode . serialize' . prefixWithTag
   where
    prefixWithTag wit = encodeListLen 2 <> encodeWord 0 <> toCBOR wit
