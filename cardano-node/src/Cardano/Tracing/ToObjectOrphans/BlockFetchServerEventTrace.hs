{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Tracing.ToObjectOrphans.BlockFetchServerEventTrace () where

import           Cardano.Prelude

import           Data.Aeson hiding (Error)

import           Ouroboros.Consensus.BlockFetchServer (TraceBlockFetchServerEvent)
import           Cardano.BM.Data.Tracer ( mkObject, trStructured)
import           Cardano.BM.Tracing ( DefinePrivacyAnnotation
                                    , DefineSeverity(..), ToObject(..)
                                    , Severity(..),  Transformable(..)
                                    )

instance DefinePrivacyAnnotation (TraceBlockFetchServerEvent blk)
instance DefineSeverity (TraceBlockFetchServerEvent blk) where
  defineSeverity _ = Info

-- transform @BlockFetchServerEvent@
instance Transformable Text IO (TraceBlockFetchServerEvent blk) where
  trTransformer _ verb tr = trStructured verb tr
instance ToObject (TraceBlockFetchServerEvent blk) where
  toObject _verb _ =
    mkObject [ "kind" .= String "TraceBlockFetchServerEvent" ]
