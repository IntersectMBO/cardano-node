{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Node.Rpc.Client
  ( exampleCurrentEra

  )
  where 
import qualified Cardano.Node.Rpc.Proto.Api.Node  as ProtoGen
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Field (field)
import Lens.Micro

exampleCurrentEra :: ProtoGen.CurrentEra 
exampleCurrentEra = defMessage  & field @"era" .~ ProtoGen.Byron
