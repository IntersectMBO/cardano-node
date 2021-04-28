{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.TraceDispatcher.Network.Formatting
  (
  ) where

import           Data.Aeson (ToJSON, Value (String), toJSON, (.=))
import qualified Data.Text as Text
import           Text.Show

import           Cardano.Logging
import           Cardano.Prelude hiding (Show, show)

import           Ouroboros.Network.Block (Serialised, Tip, Point)
import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import           Ouroboros.Network.Driver.Simple (TraceSendRecv (..))
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)

instance LogFormatting (TraceSendRecv
  (ChainSync (Serialised blk) (Point blk) (Tip blk))) where
    forMachine _ _ = mempty
