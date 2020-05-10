{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.Config.Protocol
  (
    -- * The enumeration of supported protocols
    Protocol(..)

    -- * Node support
    -- | Support for the context needed to run a node with a protocol
  , SomeConsensusProtocol(..)
  , TraceConstraints
  , mkConsensusProtocol
  , ProtocolInstantiationError(..)
  , renderProtocolInstantiationError

    -- * Client support
    -- | Support for the context needed to run a client of a node that is using
    -- a protocol.
  , SomeNodeClientProtocol(..)
  , mkNodeClientProtocol
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)

import           Cardano.Config.Types
                   (NodeConfiguration(..), Protocol (..), ProtocolFilepaths(..),
                    SomeConsensusProtocol(..), SomeNodeClientProtocol(..),
                    TraceConstraints)
import           Cardano.Chain.Slotting (EpochSlots(..))

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


mkNodeClientProtocol :: Protocol -> SomeNodeClientProtocol
mkNodeClientProtocol protocol =
    case protocol of
{-
      --TODO
      -- Mock protocols
      BFT      -> firstExceptT MockProtocolInstantiationError $
                    mkNodeClientProtocolBFT

      MockPBFT -> firstExceptT MockProtocolInstantiationError $
                    mkNodeClientProtocolPBFT

      Praos    -> firstExceptT MockProtocolInstantiationError $
                    mkNodeClientProtocolPraos
-}

      -- Real protocols
      RealPBFT -> mkSomeNodeClientProtocolRealPBFT
                    --TODO: this is only the correct value for mainnet
                    -- not for Byron testnets. This value is needed because
                    -- to decode legacy EBBs one needs to know how many
                    -- slots there are per-epoch. This info comes from
                    -- the genesis file, but we don't have that in the
                    -- client case.
                    (EpochSlots 21600)

      TPraos   -> mkSomeNodeClientProtocolTPraos

      _        -> panic ("mkNodeClientProtocol TODO: " <> show protocol)


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

