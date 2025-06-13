{-# LANGUAGE TypeFamilies #-}

module Cardano.Node.Rpc.Proto.Api.NodeMetadata (
  ) where

import           Network.GRPC.Common
import           Network.GRPC.Common.Protobuf

import           Proto.Cardano.Node.Rpc.Node

type instance RequestMetadata          (Protobuf Node meth) = NoMetadata
type instance ResponseInitialMetadata  (Protobuf Node meth) = NoMetadata
type instance ResponseTrailingMetadata (Protobuf Node meth) = NoMetadata
