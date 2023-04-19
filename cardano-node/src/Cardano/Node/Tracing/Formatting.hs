{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Node.Tracing.Formatting
  (
  ) where

import           Data.Aeson (Value (String), toJSON, (.=))
import           Data.Proxy (Proxy (..))
import           Data.Void (Void)

import           Cardano.Logging (LogFormatting (..))

import           Cardano.Node.Tracing.Render (renderHeaderHashForDetails)

import           Ouroboros.Consensus.Block (ConvertRawHash (..), RealPoint, realPointHash,
                   realPointSlot)
import           Ouroboros.Network.Block

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
    mconcat [ "kind" .= String "SlotNo"
             , "slot" .= toJSON (unSlotNo slot) ]

instance forall blk. ConvertRawHash blk
      => LogFormatting (Point blk) where
  forMachine _dtal GenesisPoint =
    mconcat
      [ "kind" .= String "GenesisPoint" ]
  forMachine dtal (BlockPoint slot h) =
    mconcat
      [ "kind" .= String "BlockPoint"
      , "slot" .= toJSON (unSlotNo slot)
      , "headerHash" .= renderHeaderHashForDetails (Proxy @blk) dtal h
      ]

instance ConvertRawHash blk
      => LogFormatting (RealPoint blk) where
  forMachine dtal p = mconcat
        [ "kind" .= String "Point"
        , "slot" .= unSlotNo (realPointSlot p)
        , "hash" .= renderHeaderHashForDetails (Proxy @blk) dtal (realPointHash p)
        ]
