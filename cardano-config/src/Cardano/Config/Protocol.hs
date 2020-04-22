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

import           Cardano.Config.Types
                   (NodeConfiguration(..), Protocol (..),
                    ProtocolFilepaths(..))

import           Cardano.Config.Protocol.Types
import           Cardano.Config.Protocol.Mock
import           Cardano.Config.Protocol.Byron
import           Cardano.Config.Protocol.Shelley


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
                    mkConsensusProtocolBFT   config

      MockPBFT -> firstExceptT MockProtocolInstantiationError $
                    mkConsensusProtocolPBFT  config

      Praos    -> firstExceptT MockProtocolInstantiationError $
                    mkConsensusProtocolPraos config

      -- Real protocols
      RealPBFT -> firstExceptT ByronProtocolInstantiationError $
                    mkConsensusProtocolRealPBFT config files

      TPraos   -> firstExceptT ShelleyProtocolInstantiationError $
                    SomeConsensusProtocol <$> mkConsensusProtocolTPraos config files


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

