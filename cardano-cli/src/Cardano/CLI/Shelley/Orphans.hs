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
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Short as SBS
import qualified Data.Text.Encoding as Text

import           Cardano.Api.Orphans ()

import           Ouroboros.Consensus.Byron.Ledger.Block (ByronHash (..))
import           Ouroboros.Consensus.HardFork.Combinator (OneEraHash (..))
import           Ouroboros.Consensus.Shelley.Eras (StandardCrypto)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyHash (..))
import           Ouroboros.Network.Block (BlockNo (..), HeaderHash, Tip (..))

import           Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash (..))
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import           Cardano.Ledger.PoolDistr (PoolDistr (..))
import           Cardano.Protocol.TPraos.BHeader (HashHeader (..))

import qualified Cardano.Ledger.Credential as Ledger
import qualified Cardano.Ledger.Shelley.API.Protocol as Ledger
import qualified Cardano.Ledger.Shelley.EpochBoundary as Ledger
import qualified Cardano.Ledger.Shelley.Rewards as Ledger
import qualified Cardano.Protocol.TPraos.Rules.Prtcl as Ledger
import qualified Cardano.Protocol.TPraos.Rules.Tickn as Ledger
import           Cardano.Ledger.TxIn (TxId (..))

import qualified Cardano.Ledger.Mary.Value as Ledger.Mary

import qualified Data.Compact.VMap as VMap

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

deriving newtype instance CC.Crypto crypto => ToJSON (TxId crypto)

deriving newtype instance CC.Crypto crypto => ToJSON (ShelleyHash crypto)
deriving newtype instance CC.Crypto crypto => ToJSON (HashHeader crypto)

deriving newtype instance ToJSON (AuxiliaryDataHash StandardCrypto)
deriving newtype instance ToJSON Ledger.LogWeight
deriving newtype instance ToJSON (PoolDistr StandardCrypto)

deriving newtype instance ToJSON (Ledger.Stake StandardCrypto)

deriving instance ToJSON (Ledger.StakeReference StandardCrypto)

deriving instance ToJSON (Ledger.PrtclState StandardCrypto)
deriving instance ToJSON Ledger.TicknState
deriving instance ToJSON (Ledger.ChainDepState StandardCrypto)

deriving instance ToJSONKey Ledger.Ptr

deriving newtype  instance ToJSON    (Ledger.Mary.PolicyID StandardCrypto)

instance (ToJSONKey k, ToJSON v) => ToJSON (SetAlgebra.BiMap v k v) where
  toJSON = toJSON . SetAlgebra.forwards -- to normal Map
