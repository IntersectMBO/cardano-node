{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Tracing.ToObjectOrphans.LabelPeerTrace () where

import           Cardano.Prelude hiding (show)
import           Prelude (show)

import           Data.Aeson hiding (Error)

import           Cardano.BM.Data.Tracer ( TracingVerbosity(..), emptyObject
                                        , mkObject, trStructured)
import           Cardano.BM.Tracing ( DefinePrivacyAnnotation
                                    , DefineSeverity(..), ToObject(..)
                                    , Severity(..), Transformable(..)
                                    )
import           Ouroboros.Network.Block (Point)
import           Ouroboros.Network.BlockFetch.ClientState (TraceFetchClientState (..)
                                                          , TraceLabelPeer (..))
import           Ouroboros.Network.BlockFetch.Decision (FetchDecision)


instance DefinePrivacyAnnotation [TraceLabelPeer peer
                                  (FetchDecision [Point header])]
instance DefineSeverity [TraceLabelPeer peer(FetchDecision [Point header])] where
  defineSeverity [] = Debug
  defineSeverity _ = Info


instance DefinePrivacyAnnotation (TraceLabelPeer peer
                                  (TraceFetchClientState header))
instance DefineSeverity (TraceLabelPeer peer
                         (TraceFetchClientState header)) where
  defineSeverity _ = Info

-- transform @BlockFetchDecision@
instance Show peer => Transformable Text IO [TraceLabelPeer peer
                                (FetchDecision [Point header])] where
  trTransformer _ verb tr = trStructured verb tr

-- transform @BlockFetchDecision@
instance Show peer => Transformable Text IO (TraceLabelPeer peer
                                (TraceFetchClientState header)) where
  trTransformer _ verb tr = trStructured verb tr

instance Show peer => ToObject [TraceLabelPeer peer
                        (FetchDecision [Point header])] where
  toObject MinimalVerbosity _ = emptyObject
  toObject NormalVerbosity lbls = mkObject [ "kind" .= String "TraceLabelPeer"
                                           , "length" .= String (toS $ show $ length lbls) ]
  toObject MaximalVerbosity [] = emptyObject
  toObject MaximalVerbosity (lbl : r) = toObject MaximalVerbosity lbl <>
                                        toObject MaximalVerbosity r

instance Show peer => ToObject (TraceLabelPeer peer
                        (FetchDecision [Point header])) where
  toObject verb (TraceLabelPeer peerid a) =
    mkObject [ "kind" .= String "FetchDecision"
             , "peer" .= show peerid
             , "decision" .= toObject verb a ]

instance Show peer => ToObject (TraceLabelPeer peer
                        (TraceFetchClientState header)) where
  toObject verb (TraceLabelPeer peerid a) =
    mkObject [ "kind" .= String "TraceFetchClientState"
             , "peer" .= show peerid
             , "state" .= toObject verb a ]

instance ToObject (TraceFetchClientState header) where
  toObject _verb (AddedFetchRequest {}) =
    mkObject [ "kind" .= String "AddedFetchRequest" ]
  toObject _verb (AcknowledgedFetchRequest {}) =
    mkObject [ "kind" .= String "AcknowledgedFetchRequest" ]
  toObject _verb (CompletedBlockFetch {}) =
    mkObject [ "kind" .= String "CompletedBlockFetch" ]
  toObject _verb (CompletedFetchBatch {}) =
    mkObject [ "kind" .= String "CompletedFetchBatch" ]
  toObject _verb (StartedFetchBatch {}) =
    mkObject [ "kind" .= String "StartedFetchBatch" ]
  toObject _verb (RejectedFetchBatch {}) =
    mkObject [ "kind" .= String "RejectedFetchBatch" ]

instance ToObject (FetchDecision [Point header]) where
  toObject _verb (Left decline) =
    mkObject [ "kind" .= String "FetchDecision declined"
             , "declined" .= String (toS $ show $ decline) ]
  toObject _verb (Right results) =
    mkObject [ "kind" .= String "FetchDecision results"
             , "length" .= String (toS $ show $ length results) ]
