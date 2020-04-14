{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Config.Protocol
  ( Protocol(..)
  , ProtocolInstantiationError(..)
  , SomeConsensusProtocol(..)
  , TraceConstraints
  , mkConsensusProtocol
  , renderProtocolInstantiationError
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)

import           Cardano.Tracing.ToObjectOrphans ()

import           Ouroboros.Consensus.Block (BlockProtocol)
import qualified Ouroboros.Consensus.Cardano as Consensus

import           Ouroboros.Consensus.Node.Run (RunNode)

import           Cardano.Config.Types
                   (NodeConfiguration(..), Protocol (..),
                    MiscellaneousFilepaths(..))
import           Cardano.Tracing.Constraints (TraceConstraints)

import           Cardano.Config.Protocol.Mock
import           Cardano.Config.Protocol.Byron


------------------------------------------------------------------------------
-- Conversions from configuration into specific protocols and their params
--


data SomeConsensusProtocol where
  SomeConsensusProtocol :: (RunNode blk, TraceConstraints blk)
                        => Consensus.Protocol blk (BlockProtocol blk)
                        -> SomeConsensusProtocol

mkConsensusProtocol
  :: NodeConfiguration
  -> Maybe MiscellaneousFilepaths
  -> ExceptT ProtocolInstantiationError IO SomeConsensusProtocol
mkConsensusProtocol config@NodeConfiguration{ncProtocol} files =
    case ncProtocol of
      -- Mock protocols
      BFT      -> firstExceptT MockProtocolInstantiationError $
                    SomeConsensusProtocol <$> mkConsensusProtocolBFT   config

      MockPBFT -> firstExceptT MockProtocolInstantiationError $
                    SomeConsensusProtocol <$> mkConsensusProtocolPBFT  config

      Praos    -> firstExceptT MockProtocolInstantiationError $
                    SomeConsensusProtocol <$> mkConsensusProtocolPraos config

      -- Real protocols
      RealPBFT -> firstExceptT ByronProtocolInstantiationError $
                    SomeConsensusProtocol <$> mkConsensusProtocolRealPBFT config files



------------------------------------------------------------------------------
-- Errors
--

data ProtocolInstantiationError =
    ByronProtocolInstantiationError ByronProtocolInstantiationError
  | MockProtocolInstantiationError MockProtocolInstantiationError
  deriving Show


renderProtocolInstantiationError :: ProtocolInstantiationError -> Text
renderProtocolInstantiationError pie =
  case pie of
    ByronProtocolInstantiationError bpie ->
      renderByronProtocolInstantiationError bpie

    MockProtocolInstantiationError mpie ->
      renderMockProtocolInstantiationError mpie

