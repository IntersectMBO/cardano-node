module Cardano.Tracer.Handlers.RTView.SupportedNodes
  ( supportedNodesVersions
  ) where

-- | RTView works correctly with these versions of the nodes __only__.
supportedNodesVersions :: [String]
supportedNodesVersions =
  [ "1.27.0"
  , "1.27.1"
  ]
