{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.CLI.Shelley.Orphans () where

import           Cardano.Api.Orphans ()
import qualified Cardano.Ledger.AuxiliaryData as Ledger
import qualified Cardano.Ledger.Credential as Ledger
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import qualified Cardano.Ledger.Mary.Value as Ledger.Mary
import qualified Cardano.Ledger.PoolDistr as Ledger
import qualified Cardano.Ledger.Shelley.EpochBoundary as Ledger
import qualified Cardano.Ledger.Shelley.PoolRank as Ledger
import           Cardano.Ledger.TxIn (TxId (..))
import qualified Cardano.Protocol.TPraos.API as Ledger
import           Cardano.Protocol.TPraos.BHeader (HashHeader (..))
import qualified Cardano.Protocol.TPraos.Rules.Prtcl as Ledger
import qualified Cardano.Protocol.TPraos.Rules.Tickn as Ledger
import qualified Cardano.Slotting.Slot as Cardano
import qualified Control.SetAlgebra as SetAlgebra (BiMap, forwards)
import           Data.Aeson (FromJSON (..), KeyValue ((.=)), ToJSON (..), ToJSONKey)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Short as SBS
import qualified Data.Text.Encoding as Text
import qualified Data.VMap as VMap
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronHash (..))
import           Ouroboros.Consensus.HardFork.Combinator (OneEraHash (..))
import           Ouroboros.Consensus.Protocol.Praos (PraosState)
import qualified Ouroboros.Consensus.Protocol.Praos as Consensus
import           Ouroboros.Consensus.Protocol.TPraos (TPraosState)
import qualified Ouroboros.Consensus.Protocol.TPraos as Consensus
import           Ouroboros.Consensus.Shelley.Eras (StandardCrypto)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyHash (..))
import           Ouroboros.Network.Block (BlockNo (..), HeaderHash, Tip (..))

instance ToJSON (OneEraHash xs) where
  toJSON = toJSON
         . Text.decodeLatin1
         . Base16.encode
         . SBS.fromShort
         . getOneEraHash

deriving newtype instance ToJSON ByronHash

-- This instance is temporarily duplicated in cardano-config

instance ToJSON (HeaderHash blk) => ToJSON (Tip blk) where
  toJSON TipGenesis = Aeson.object [ "genesis" .= True ]
  toJSON (Tip slotNo headerHash blockNo) =
    Aeson.object
      [ "slotNo"     .= slotNo
      , "headerHash" .= headerHash
      , "blockNo"    .= blockNo
      ]

deriving newtype instance ToJSON BlockNo
deriving newtype instance FromJSON BlockNo

--
-- Simple newtype wrappers JSON conversion
--

deriving newtype instance CC.Crypto crypto => ToJSON (TxId crypto)

deriving newtype instance CC.Crypto crypto => ToJSON (ShelleyHash crypto)
deriving newtype instance CC.Crypto crypto => ToJSON (HashHeader crypto)

deriving newtype instance ToJSON (Ledger.AuxiliaryDataHash StandardCrypto)
deriving newtype instance ToJSON Ledger.LogWeight
deriving newtype instance ToJSON (Ledger.PoolDistr StandardCrypto)

deriving newtype instance ToJSON (Ledger.Stake StandardCrypto)

deriving instance ToJSON (Ledger.StakeReference StandardCrypto)

deriving instance ToJSON (Ledger.PrtclState StandardCrypto)
deriving instance ToJSON Ledger.TicknState
deriving instance ToJSON (Ledger.ChainDepState StandardCrypto)

deriving newtype  instance ToJSON    (Ledger.Mary.PolicyID StandardCrypto)

instance (ToJSONKey k, ToJSON v) => ToJSON (SetAlgebra.BiMap v k v) where
  toJSON = toJSON . SetAlgebra.forwards -- to normal Map

instance ToJSON (TPraosState StandardCrypto) where
  toJSON s = Aeson.object
    [ "lastSlot" .= Consensus.tpraosStateLastSlot s
    , "chainDepState" .= Consensus.tpraosStateChainDepState s
    ]

instance ToJSON (PraosState StandardCrypto) where
  toJSON s = Aeson.object
    [ "lastSlot" .= Consensus.praosStateLastSlot s
    , "oCertCounters" .= Consensus.praosStateOCertCounters s
    , "evolvingNonce" .= Consensus.praosStateEvolvingNonce s
    , "candidateNonce" .= Consensus.praosStateCandidateNonce s
    , "epochNonce" .= Consensus.praosStateEpochNonce s
    , "labNonce" .= Consensus.praosStateLabNonce s
    , "lastEpochBlockNonce" .= Consensus.praosStateLastEpochBlockNonce s
    ]


instance ToJSON (Cardano.WithOrigin Cardano.SlotNo) where
  toJSON = \case
    Cardano.Origin -> Aeson.String "origin"
    Cardano.At (Cardano.SlotNo n) -> toJSON n
