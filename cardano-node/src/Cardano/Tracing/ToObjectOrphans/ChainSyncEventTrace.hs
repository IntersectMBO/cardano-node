{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Tracing.ToObjectOrphans.ChainSyncEventTrace () where

import           Cardano.Prelude hiding (show)
import           Prelude (show)

import           Data.Aeson hiding (Error)

import           Cardano.Tracing.ToObjectOrphans.ConsensusToObjectOrphans

import           Cardano.BM.Data.Tracer ( mkObject, trStructured)
import           Cardano.BM.Tracing ( DefinePrivacyAnnotation
                                    , DefineSeverity(..), ToObject(..)
                                    , Severity(..), Transformable(..)
                                    )
import           Ouroboros.Consensus.Block (SupportedBlock, headerPoint)
import           Ouroboros.Consensus.ChainSyncClient (TraceChainSyncClientEvent (..))
import           Ouroboros.Consensus.ChainSyncServer (TraceChainSyncServerEvent(..))
import           Ouroboros.Consensus.Ledger.Abstract (ProtocolLedgerView(..))
import           Ouroboros.Consensus.Util.Condense (Condense(..))
import           Ouroboros.Network.Block (ChainUpdate(..), HeaderHash, tipPoint)


instance DefinePrivacyAnnotation (TraceChainSyncClientEvent blk)
instance DefineSeverity (TraceChainSyncClientEvent blk) where
  defineSeverity (TraceDownloadedHeader _) = Info
  defineSeverity (TraceFoundIntersection _ _ _) = Info
  defineSeverity (TraceRolledBack _) = Notice
  defineSeverity (TraceException _) = Warning

-- transform @ChainSyncClient@
instance (Condense (HeaderHash blk), ProtocolLedgerView blk, SupportedBlock blk)
          => Transformable Text IO (TraceChainSyncClientEvent blk) where
  trTransformer _ verb tr = trStructured verb tr

instance (Condense (HeaderHash blk), ProtocolLedgerView blk, SupportedBlock blk)
          => ToObject (TraceChainSyncClientEvent blk) where
  toObject verb ev = case ev of
    TraceDownloadedHeader pt ->
      mkObject [ "kind" .= String "ChainSyncClientEvent.TraceDownloadedHeader"
               , "block" .= toObject verb (headerPoint pt) ]
    TraceRolledBack tip ->
      mkObject [ "kind" .= String "ChainSyncClientEvent.TraceRolledBack"
               , "tip" .= toObject verb tip ]
    TraceException exc ->
      mkObject [ "kind" .= String "ChainSyncClientEvent.TraceException"
               , "exception" .= String (toS $ show exc) ]
    TraceFoundIntersection _ _ _ ->
      mkObject [ "kind" .= String "ChainSyncClientEvent.TraceFoundIntersection" ]

instance DefinePrivacyAnnotation (TraceChainSyncServerEvent blk b)
instance DefineSeverity (TraceChainSyncServerEvent blk b) where
  defineSeverity _ = Info

-- transform @ChainSyncServer@
instance Condense (HeaderHash blk) => Transformable Text IO (TraceChainSyncServerEvent blk b) where
  trTransformer _ verb tr = trStructured verb tr

instance Condense (HeaderHash blk) => ToObject (TraceChainSyncServerEvent blk b) where
    toObject verb ev = case ev of
      TraceChainSyncServerRead tip (AddBlock hdr) ->
        mkObject [ "kind" .= String "ChainSyncServerEvent.TraceChainSyncServerRead.AddBlock"
                 , "tip" .= (String (toS . showTip verb $ tipPoint tip))
                 , "addedBlock" .= (String (toS $ condense hdr)) ]
      TraceChainSyncServerRead tip (RollBack pt) ->
        mkObject [ "kind" .= String "ChainSyncServerEvent.TraceChainSyncServerRead.RollBack"
                 , "tip" .= (String (toS . showTip verb $ tipPoint tip))
                 , "rolledBackBlock" .= (String (toS $ showTip verb pt)) ]
      TraceChainSyncServerReadBlocked tip (AddBlock hdr) ->
        mkObject [ "kind" .= String "ChainSyncServerEvent.TraceChainSyncServerReadBlocked.AddBlock"
                 , "tip" .= (String (toS . showTip verb $ tipPoint tip))
                 , "addedBlock" .= (String (toS $ condense hdr)) ]
      TraceChainSyncServerReadBlocked tip (RollBack pt) ->
        mkObject [ "kind" .= String "ChainSyncServerEvent.TraceChainSyncServerReadBlocked.RollBack"
                 , "tip" .= (String (toS . showTip verb $ tipPoint tip))
                 , "rolledBackBlock" .= (String (toS $ showTip verb pt)) ]
