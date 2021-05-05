{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.TraceDispatcher.Common.Formatting
  (
  ) where

import           Cardano.Prelude ()
import           Data.Aeson (Value (String), toJSON, (.=))
import           Text.Show

import           Cardano.Logging (LogFormatting (..), mkObject)
import           Cardano.Prelude hiding (Show, show)

import           Cardano.TraceDispatcher.Render (renderHeaderHashForDetails,
                     showT)

import           Ouroboros.Consensus.Block (ConvertRawHash (..))
import           Ouroboros.Network.Block
import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch


instance
  (  Show peer
  ,  LogFormatting a)
  => LogFormatting (BlockFetch.TraceLabelPeer peer a) where
  forMachine dtal (BlockFetch.TraceLabelPeer peerid a) =
    mkObject [ "peer" .= showT peerid ] <> forMachine dtal a

  forHuman (BlockFetch.TraceLabelPeer peerid m) = "Peer is " <> showT peerid <> ". " <> forHuman m

  asMetrics (BlockFetch.TraceLabelPeer _peerid m) = asMetrics m

-- | A bit of a weird one, but needed because some of the very general
-- consensus interfaces are sometimes instantiated to 'Void', when there are
-- no cases needed.
--
instance LogFormatting Void where
  forMachine _dtal _x = mempty

instance LogFormatting () where
  forMachine _dtal _x = mempty


instance LogFormatting SlotNo where
  forMachine _dtal slot =
    mkObject [ "kind" .= String "SlotNo"
               , "slot" .= toJSON (unSlotNo slot) ]

instance forall blk. ConvertRawHash blk
      => LogFormatting (Point blk) where
  forMachine _dtal GenesisPoint =
    mkObject
      [ "kind" .= String "GenesisPoint" ]
  forMachine dtal (BlockPoint slot h) =
    mkObject
      [ "kind" .= String "BlockPoint"
      , "slot" .= toJSON (unSlotNo slot)
      , "headerHash" .= renderHeaderHashForDetails (Proxy @blk) dtal h
      ]
