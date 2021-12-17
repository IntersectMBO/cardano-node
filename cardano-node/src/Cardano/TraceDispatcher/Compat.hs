{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE LambdaCase            #-}

module Cardano.TraceDispatcher.Compat
  ( toDetailLevel
  ) where

import Cardano.BM.Data.Tracer qualified as IOMF
import Cardano.Logging.Types  qualified as TD

toDetailLevel :: IOMF.TracingVerbosity -> TD.DetailLevel
toDetailLevel = \case
  IOMF.MinimalVerbosity -> TD.DMinimal
  IOMF.NormalVerbosity  -> TD.DNormal
  IOMF.MaximalVerbosity -> TD.DMaximum
