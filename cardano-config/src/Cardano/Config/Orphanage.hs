{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Config.Orphanage
  ( ConsensusTraceOptions
  , ProtocolTraceOptions
  ) where

import           Cardano.Prelude

import           Cardano.BM.Data.Tracer (TracingVerbosity(..))
import qualified Ouroboros.Consensus.BlockchainTime as Consensus
import qualified Ouroboros.Consensus.Node.Tracers as ConsensusTracers
import           Ouroboros.Consensus.NodeNetwork (ProtocolTracers'(..))


deriving instance Eq Consensus.SlotLength
deriving instance Num Consensus.SlotLength

deriving instance Show TracingVerbosity

type ConsensusTraceOptions = ConsensusTracers.Tracers' () ()    () (Const Bool)
deriving instance Eq ConsensusTraceOptions
deriving instance Show ConsensusTraceOptions

type ProtocolTraceOptions  = ProtocolTracers'   () () ()    (Const Bool)
deriving instance Eq ProtocolTraceOptions
deriving instance Show ProtocolTraceOptions
