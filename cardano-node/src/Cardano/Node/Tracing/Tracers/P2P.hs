{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Node.Tracing.Tracers.P2P
  () where

import           Cardano.Logging
import           Cardano.Network.Diffusion.Types
import           Cardano.Node.Configuration.TopologyP2P ()
import           Cardano.Node.Tracing.Tracers.NodeToNode ()
import           Cardano.Tracing.OrphanInstances.Network ()
import           Ouroboros.Network.OrphanInstances ()
import           Ouroboros.Network.PeerSelection.Types ()
import           Ouroboros.Network.Tracing ()

import           Data.Aeson (Value (..), (.=))
import           Data.Text (pack)

--------------------------------------------------------------------------------
-- ChurnMode Tracer
--------------------------------------------------------------------------------

instance LogFormatting TraceChurnMode where
  forMachine _dtal (TraceChurnMode mode) =
    mconcat [ "kind" .= String "ChurnMode"
            , "churnMode" .= String (pack . show $ mode)
            ]

instance MetaTrace TraceChurnMode where
  namespaceFor TraceChurnMode {} =
    Namespace [] ["PeerSelection", "ChurnMode"]
  severityFor _ (Just TraceChurnMode {}) = Just Info
  severityFor _ Nothing = Nothing

  documentFor (Namespace _ ["PeerSelection", "ChurnMode"]) = Just $ mconcat
    ["Affects churning strategy. For a synced node or operating in GenesisMode "
    , " consensus mode, the default strategy is used. Otherwise for a syncing PraosMode"
    , " node, the legacy bulk sync churning intervals are used whose durations"
    , " depend on whether bootstrap peers are enabled."
    ]
  documentFor _ = Nothing

  allNamespaces = [
      Namespace [] ["PeerSelection", "ChurnMode"]
    ]
