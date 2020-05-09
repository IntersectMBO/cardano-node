{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.Config.Protocol
  ( Protocol(..)
  , mkConsensusProtocol
  , SomeConsensusProtocol(..)
  , TraceConstraints
  , ProtocolInstantiationError(..)
  , renderProtocolInstantiationError
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)

import           Cardano.Config.Types
                   (NodeConfiguration(..), Protocol (..),
                    ProtocolFilepaths(..), SomeConsensusProtocol(..),
                    TraceConstraints)

import           Cardano.Config.Byron.Protocol
import           Cardano.Config.Mock.Protocol
import           Cardano.Config.Shelley.Protocol


------------------------------------------------------------------------------
-- Conversions from configuration into specific protocols and their params
--

mkConsensusProtocol
  :: NodeConfiguration
  -> Maybe ProtocolFilepaths
  -> ExceptT ProtocolInstantiationError IO SomeConsensusProtocol
mkConsensusProtocol config@NodeConfiguration{ncProtocol} files =
    case ncProtocol of
      -- Mock protocols
      BFT      -> firstExceptT MockProtocolInstantiationError $
                    mkSomeConsensusProtocolBFT   config

      MockPBFT -> firstExceptT MockProtocolInstantiationError $
                    mkSomeConsensusProtocolPBFT  config

      Praos    -> firstExceptT MockProtocolInstantiationError $
                    mkSomeConsensusProtocolPraos config

      -- Real protocols
      RealPBFT -> firstExceptT ByronProtocolInstantiationError $
                    mkSomeConsensusProtocolRealPBFT config files

      TPraos   -> firstExceptT ShelleyProtocolInstantiationError $
                    mkSomeConsensusProtocolTPraos config files


------------------------------------------------------------------------------
-- Errors
--

data ProtocolInstantiationError =
    ByronProtocolInstantiationError   ByronProtocolInstantiationError
  | ShelleyProtocolInstantiationError ShelleyProtocolInstantiationError
  | MockProtocolInstantiationError    MockProtocolInstantiationError


renderProtocolInstantiationError :: ProtocolInstantiationError -> Text
renderProtocolInstantiationError pie =
  case pie of
    ByronProtocolInstantiationError bpie ->
      renderByronProtocolInstantiationError bpie

    ShelleyProtocolInstantiationError spie ->
      renderShelleyProtocolInstantiationError spie

    MockProtocolInstantiationError mpie ->
      renderMockProtocolInstantiationError mpie

