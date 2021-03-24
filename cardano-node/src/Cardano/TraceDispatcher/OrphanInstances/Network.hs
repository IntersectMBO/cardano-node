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
import           Cardano.Slotting.Slot (SlotNo (..))
import           Cardano.TraceDispatcher.Render ()


instance LogFormatting SlotNo where
  forMachine _dtal slot =
    mkObject [ "kind" .= String "SlotNo"
               , "slot" .= toJSON (unSlotNo slot) ]
