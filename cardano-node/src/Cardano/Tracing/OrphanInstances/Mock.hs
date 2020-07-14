{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Tracing.OrphanInstances.Mock () where

import           Cardano.Prelude

import           Data.Text (pack)

import           Cardano.Tracing.OrphanInstances.Common
import           Cardano.Tracing.OrphanInstances.Consensus ()

import           Data.Aeson (Value (..), ToJSON, toJSON, (.=))

import           Cardano.Crypto.Hash.Class (Hash)
import           Cardano.Crypto.KES.Class
                   (VerKeyKES, deriveVerKeyKES, hashVerKeyKES)

import           Ouroboros.Consensus.Block (Header)
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTx, TxId, txId)
import qualified Ouroboros.Consensus.Mock.Ledger as Mock
import qualified Ouroboros.Consensus.Mock.Protocol.Praos as Praos
import           Ouroboros.Consensus.Util.Condense

import           Ouroboros.Network.Block


--
-- | instances of @ToObject@
--
-- NOTE: this list is sorted by the unqualified name of the outermost type.

instance ( Mock.SimpleCrypto c
         , Typeable a)
 => ToObject (Header (Mock.SimpleBlock c a)) where
  toObject _verb b =
    mkObject $
        [ "kind" .= String "SimpleBlockHeader"
        , "hash" .= condense (blockHash b)
        , "slotNo" .= condense (blockSlot b)
        , "blockNo" .= condense (blockNo b) ]


instance ToObject (Praos.HotKey Praos.PraosMockCrypto) where
  toObject _verb (Praos.HotKey signKey) =
    mkObject
      [ "kind" .= String "HotKey"
      , "vkey" .= (hashVerKeyKES (deriveVerKeyKES signKey)
                     :: Hash (Praos.PraosHash Praos.PraosMockCrypto)
                             (VerKeyKES (Praos.PraosKES Praos.PraosMockCrypto)))
      ]

instance StandardHash blk
 => ToObject (Mock.MockError blk) where
  toObject _verb (Mock.MockUtxoError e) =
    mkObject
      [ "kind" .= String "MockUtxoError"
      , "error" .= String (show e)
      ]
  toObject _verb (Mock.MockInvalidHash expect act) =
    mkObject
      [ "kind" .= String "MockInvalidHash"
      , "expected" .= String (show expect)
      , "actual" .= String (show act)
      ]
  toObject _verb (Mock.MockExpired expiredSlot validatedSlot) =
      mkObject
        [ "kind" .= String "MockExpired"
        , "error" .= String (pack msg)
        ]
    where
      msg =
        "transaction expired in slot " <> condense expiredSlot <>
        ", current slot " <> condense validatedSlot


instance ToObject (Praos.PraosValidationError c) where
  toObject _verb (Praos.PraosInvalidSlot expect act) =
    mkObject
      [ "kind" .= String "PraosInvalidSlot"
      , "expected" .= String (show expect)
      , "actual" .= String (show act)
      ]
  toObject _verb (Praos.PraosUnknownCoreId cid) =
    mkObject
      [ "kind" .= String "PraosUnknownCoreId"
      , "error" .= String (show cid)
      ]
  toObject _verb (Praos.PraosInvalidSig str _ _ _) =
    mkObject
      [ "kind" .= String "PraosInvalidSig"
      , "error" .= String (pack str)
      ]
  toObject _verb (Praos.PraosInvalidCert _vkvrf y nat _vrf) =
    mkObject
      [ "kind" .= String "PraosInvalidCert"
      , "y" .= String (show y)
      , "nat" .= String (show nat)
      ]
  toObject _verb (Praos.PraosInsufficientStake t y) =
    mkObject
      [ "kind" .= String "PraosInsufficientStake"
      , "t" .= String (show t)
      , "y" .= String (show y)
      ]


instance ToObject (GenTx (Mock.SimpleBlock c ext)) where
  toObject verb tx =
    mkObject $
        [ "txid" .= txId tx ]
     ++ [ "tx"   .= condense tx | verb == MaximalVerbosity ]


instance ToJSON (TxId (GenTx (Mock.SimpleBlock c ext))) where
  toJSON txid = toJSON (condense txid)
