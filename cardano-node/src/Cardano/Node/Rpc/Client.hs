{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Node.Rpc.Client
  ( exampleCurrentEra

  )
  where
import           Data.ProtoLens (defMessage)
import           Data.ProtoLens.Field (field)
import           Lens.Micro

import qualified Proto.Node as ProtoGen

exampleCurrentEra :: ProtoGen.CurrentEra
exampleCurrentEra = defMessage  & field @"era" .~ ProtoGen.Byron
