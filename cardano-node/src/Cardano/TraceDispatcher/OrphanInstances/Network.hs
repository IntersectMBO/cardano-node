{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}



{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.TraceDispatcher.OrphanInstances.Network (
  ) where

import           Data.Aeson (Value (String), toJSON, (.=))

import           Cardano.Logging (LogFormatting (forMachine), mkObject)
import           Cardano.Prelude ()
import           Cardano.TraceDispatcher.Render (renderHeaderHashForDetails)

import           Ouroboros.Consensus.Block (ConvertRawHash (..), Proxy(..))
import           Ouroboros.Network.Block


instance LogFormatting SlotNo where
  forMachine _dtal slot =
    mkObject [ "kind" .= String "SlotNo"
               , "slot" .= toJSON (unSlotNo slot) ]

instance ConvertRawHash blk
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
