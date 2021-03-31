{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}



{-# OPTIONS_GHC -Wno-orphans  #-}
{-# OPTIONS_GHC -Wno-unused-imports  #-}

module Cardano.TraceDispatcher.OrphanInstances.Consensus (
  ) where

import           Data.Aeson (Value (String), toJSON, (.=))
import qualified Data.Aeson as A
import           Data.HashMap.Strict (insertWith)
import qualified Data.Text as Text

import           Cardano.Logging
import           Cardano.Prelude
import           Cardano.TraceDispatcher.OrphanInstances.Network ()
import           Cardano.TraceDispatcher.Render (condenseT,
                     renderHeaderHashForDetails, renderPoint,
                     renderPointAsPhrase, renderPointForDetails,
                     renderRealPoint, renderRealPointAsPhrase, showT)

import           Ouroboros.Consensus.Block (BlockNo (BlockNo), ConvertRawHash,
                     HasHeader, Header, HeaderHash, Point, RealPoint,
                     SlotNo (unSlotNo), StandardHash, headerPoint, pointSlot,
                     realPointHash, realPointSlot)
import           Ouroboros.Consensus.Block.RealPoint
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock,
                     ByronHash (..))
import qualified Ouroboros.Consensus.Cardano as PBFT
import           Ouroboros.Consensus.Ledger.Inspect (InspectLedger,
                     LedgerEvent (..), LedgerUpdate, LedgerWarning)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol)
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.OnDisk as LedgerDB
import qualified Ouroboros.Network.AnchoredFragment as AF



-- instance LogFormatting (Header blk) where

instance ConvertRawHash blk
      => LogFormatting (RealPoint blk) where
  forMachine dtal p = mkObject
        [ "kind" .= String "Point"
        , "slot" .= unSlotNo (realPointSlot p)
        , "hash" .= renderHeaderHashForDetails (Proxy @blk) dtal (realPointHash p) ]

instance (LogFormatting (LedgerUpdate blk), LogFormatting (LedgerWarning blk))
      => LogFormatting (LedgerEvent blk) where
  forMachine dtal = \case
    LedgerUpdate  update  -> forMachine dtal update
    LedgerWarning warning -> forMachine dtal warning

instance LogFormatting LedgerDB.DiskSnapshot where
  forMachine DDetailed snap =
    mkObject [ "kind" .= String "snapshot"
             , "snapshot" .= String (Text.pack $ show snap) ]
  forMachine _ _snap = mkObject [ "kind" .= String "snapshot" ]
