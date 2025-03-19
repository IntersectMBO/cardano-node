{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Node.Tracing.NodeStartupInfo
  ( NodeStartupInfo (..)
  ) where

import           Cardano.Logging.Types.NodeStartupInfo (NodeStartupInfo (..))
import           Cardano.Logging.Types (MetaTrace(..), Namespace (..), SeverityS (..))

instance MetaTrace NodeStartupInfo where
  namespaceFor NodeStartupInfo {}  =
    Namespace [] ["NodeStartupInfo"]
  severityFor  (Namespace _ ["NodeStartupInfo"]) _ =
    Just Info
  severityFor _ns _ =
    Nothing
  documentFor  (Namespace _ ["NodeStartupInfo"]) = Just
    "Startup information about this node, required for RTView\
        \\n\
        \\n _suiEra_: Name of the current era. \
        \\n _suiSlotLength_: Slot length, in seconds. \
        \\n _suiEpochLength_: Epoch length, in slots. \
        \\n _suiSlotsPerKESPeriod_: KES period length, in slots."
  documentFor _ns =
     Nothing
  allNamespaces = [ Namespace [] ["NodeStartupInfo"]]
