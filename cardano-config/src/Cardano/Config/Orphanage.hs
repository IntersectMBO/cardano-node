{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Config.Orphanage
  ( ConsensusTraceOptions
  , ProtocolTraceOptions
  ) where

import           Cardano.Prelude
import qualified Prelude

import           Data.Aeson (FromJSON, parseJSON, Value(..))
import           Network.Socket (PortNumber)
import           Data.Scientific (coefficient)
import qualified Data.Text as T

import           Cardano.BM.Data.Tracer (TracingVerbosity(..))
import qualified Ouroboros.Consensus.BlockchainTime as Consensus
import qualified Ouroboros.Consensus.Node.Tracers as ConsensusTracers
import           Ouroboros.Consensus.NodeNetwork (ProtocolTracers'(..))


deriving instance Eq Consensus.SlotLength
deriving instance Num Consensus.SlotLength

deriving instance Show TracingVerbosity

type ConsensusTraceOptions = ConsensusTracers.Tracers' () ()    (Const Bool)
deriving instance Eq ConsensusTraceOptions
deriving instance Show ConsensusTraceOptions

type ProtocolTraceOptions  = ProtocolTracers'   () () ()    (Const Bool)
deriving instance Eq ProtocolTraceOptions
deriving instance Show ProtocolTraceOptions


instance FromJSON PortNumber where
  parseJSON (Number portNum) = case readMaybe . show $ coefficient portNum of
                                 Just port -> pure port
                                 Nothing -> panic $ (show portNum)
                                                  <> " is not a valid port number."
  parseJSON invalid  = panic $ "Parsing of port number failed due to type mismatch. "
                             <> "Encountered: " <> (T.pack $ Prelude.show invalid)
