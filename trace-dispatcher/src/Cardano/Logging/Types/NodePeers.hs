{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Cardano.Logging.Types.NodePeers
  ( NodePeers (..)
  , PeerInfoPP
  )
  where

import           Control.DeepSeq (NFData)
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Text (Text)
import           GHC.Generics (Generic)

type PeerInfoPP = Text -- The result of 'ppPeer' function.

-- | This type contains an information about current peers of the node.
--   It will be asked by external applications as a DataPoint.
newtype NodePeers = NodePeers [PeerInfoPP]
  deriving stock    Generic
  deriving anyclass (NFData, ToJSON, FromJSON)
