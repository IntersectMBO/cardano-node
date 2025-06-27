{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Rpc.Proto.Api.UtxoRpc.Query
  ( module Proto.Utxorpc.V1alpha.Query.Query
  , module Proto.Utxorpc.V1alpha.Query.Query_Fields
  , module Proto.Utxorpc.V1alpha.Cardano.Cardano
  , module Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields
  )
where

import           Network.GRPC.Common
import           Network.GRPC.Common.Protobuf

import           Proto.Utxorpc.V1alpha.Cardano.Cardano
import           Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields hiding (hash, index, items, key, slot,
                   tx, values, vec'items, vec'values)
import           Proto.Utxorpc.V1alpha.Query.Query
import           Proto.Utxorpc.V1alpha.Query.Query_Fields

type instance RequestMetadata (Protobuf QueryService meth) = NoMetadata

type instance ResponseInitialMetadata (Protobuf QueryService meth) = NoMetadata

type instance ResponseTrailingMetadata (Protobuf QueryService meth) = NoMetadata
