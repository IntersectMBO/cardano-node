{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.TraceDispatcher.Common.Formatting
  (
  ) where

import           Cardano.Prelude ()
import           Data.Aeson (Value (String), toJSON, (.=))

import           Cardano.Logging (LogFormatting (..), mkObject)
import           Cardano.Prelude hiding (Show, show)

import           Cardano.TraceDispatcher.Render (renderHeaderHashForDetails)

import           Ouroboros.Consensus.Block (ConvertRawHash (..))
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
