{-# LANGUAGE LambdaCase #-}

module Cardano.Node.Tracing.Compat
  ( toDetailLevel
  , fromDetailLevel
  ) where

import qualified Cardano.BM.Data.Tracer as IOMF
import qualified Cardano.Logging.Types as TD

toDetailLevel :: IOMF.TracingVerbosity -> TD.DetailLevel
toDetailLevel = \case
  IOMF.MinimalVerbosity -> TD.DMinimal
  IOMF.NormalVerbosity  -> TD.DNormal
  IOMF.MaximalVerbosity -> TD.DMaximum

fromDetailLevel :: TD.DetailLevel -> IOMF.TracingVerbosity
fromDetailLevel = \case
  TD.DMinimal  -> IOMF.MinimalVerbosity
  TD.DNormal   -> IOMF.NormalVerbosity
  TD.DDetailed -> IOMF.NormalVerbosity
  TD.DMaximum  -> IOMF.MaximalVerbosity
