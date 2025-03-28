{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Node.Tracing.NodeInfo
  ( NodeInfo (..)
  ) where

import           Cardano.Logging.Types.NodeInfo (NodeInfo (..))
import           Cardano.Logging.Types (MetaTrace(..), Namespace (..), SeverityS (..))

instance MetaTrace NodeInfo where
  namespaceFor NodeInfo {}  =
    Namespace [] ["NodeInfo"]
  severityFor  (Namespace _ ["NodeInfo"]) _ =
    Just Info
  severityFor _ns _ =
    Nothing
  documentFor  (Namespace _ ["NodeInfo"]) = Just
    "Basic information about this node collected at startup\
        \\n\
        \\n _niName_: Name of the node. \
        \\n _niProtocol_: Protocol which this nodes uses. \
        \\n _niVersion_: Software version which this node is using. \
        \\n _niStartTime_: Start time of this node. \
        \\n _niSystemStartTime_: How long did the start of the node took."
  documentFor _ns =
     Nothing
  allNamespaces = [ Namespace [] ["NodeInfo"]]
