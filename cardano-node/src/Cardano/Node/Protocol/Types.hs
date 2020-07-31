{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Node.Protocol.Types
  ( MockProtocol(..)
  , Protocol(..)
  , SomeConsensusProtocol(..)
  , SomeConsensusProtocolConstraints
  ) where

import           Cardano.Prelude

import           Control.Monad.Fail (fail)
import           Data.Aeson

import           Cardano.BM.Tracing (Transformable)
import           Ouroboros.Consensus.Block (BlockProtocol, ForgeState (..))
import qualified Ouroboros.Consensus.Cardano as Consensus (Protocol)
import           Ouroboros.Consensus.Node.Run (RunNode)

import           Cardano.Tracing.Constraints (TraceConstraints)
import           Cardano.Tracing.Metrics (HasKESMetricsData)

data Protocol = MockProtocol !MockProtocol
              | ByronProtocol
              | ShelleyProtocol
              | CardanoProtocol
  deriving (Eq, Show, Generic)

deriving instance NFData Protocol
deriving instance NoUnexpectedThunks Protocol

instance FromJSON Protocol where
  parseJSON =
    withText "Protocol" $ \str -> case str of

      -- The new names
      "MockBFT" -> pure (MockProtocol MockBFT)
      "MockPBFT" -> pure (MockProtocol MockPBFT)
      "MockPraos" -> pure (MockProtocol MockPraos)
      "Byron" -> pure ByronProtocol
      "Shelley" -> pure ShelleyProtocol
      "Cardano" -> pure CardanoProtocol

      -- The old names
      "BFT" -> pure (MockProtocol MockBFT)
    --"MockPBFT" -- same as new name
      "Praos" -> pure (MockProtocol MockPraos)
      "RealPBFT" -> pure ByronProtocol
      "TPraos" -> pure ShelleyProtocol

      _ -> fail $ "Parsing of Protocol failed. "
                <> show str <> " is not a valid protocol"

data MockProtocol = MockBFT | MockPBFT | MockPraos
  deriving (Eq, Show, Generic)

deriving instance NFData MockProtocol
deriving instance NoUnexpectedThunks MockProtocol

type SomeConsensusProtocolConstraints blk =
     ( HasKESMetricsData blk
     , RunNode blk
     , TraceConstraints blk
     , Transformable Text IO (ForgeState blk)
     )


data SomeConsensusProtocol where

     SomeConsensusProtocol :: SomeConsensusProtocolConstraints blk
                           => Consensus.Protocol IO blk (BlockProtocol blk)
                           -> SomeConsensusProtocol
