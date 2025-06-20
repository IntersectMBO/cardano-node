{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Rpc.Proto.Api.UtxoRpc.Query
  ( module Proto.Utxorpc.V1alpha.Query.Query
  , module Proto.Utxorpc.V1alpha.Cardano.Cardano
  )
where

import           Network.GRPC.Common
import           Network.GRPC.Common.Protobuf

import           Proto.Utxorpc.V1alpha.Cardano.Cardano
import           Proto.Utxorpc.V1alpha.Query.Query

type instance RequestMetadata (Protobuf QueryService meth) = NoMetadata

type instance ResponseInitialMetadata (Protobuf QueryService meth) = NoMetadata

type instance ResponseTrailingMetadata (Protobuf QueryService meth) = NoMetadata
