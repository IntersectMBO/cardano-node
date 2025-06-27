{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Rpc.Proto.Api.Node
  ( module Proto.Cardano.Rpc.Node
  , module Proto.Cardano.Rpc.Node_Fields
  )
where

import           Network.GRPC.Common
import           Network.GRPC.Common.Protobuf

import           Proto.Cardano.Rpc.Node
import           Proto.Cardano.Rpc.Node_Fields

type instance RequestMetadata (Protobuf Node meth) = NoMetadata

type instance ResponseInitialMetadata (Protobuf Node meth) = NoMetadata

type instance ResponseTrailingMetadata (Protobuf Node meth) = NoMetadata
