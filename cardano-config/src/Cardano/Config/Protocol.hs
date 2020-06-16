{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  , CardanoEra(..)
  , SomeNodeClientProtocol(..)
  , mkNodeClientProtocol

    -- * Errors
  , RealPBFTError(..)
  , renderRealPBFTError
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)
import qualified Data.Text as Text

import           Cardano.Config.Types
                   (NodeConfiguration(..), NodeProtocolConfiguration(..),
                    NodeMockProtocolConfiguration(..), ProtocolFilepaths(..),
                    Protocol(..), MockProtocol(..), SomeConsensusProtocol(..),
                    SomeNodeClientProtocol(..), TraceConstraints)
import           Cardano.Chain.Slotting (EpochSlots(..))

import           Cardano.Config.Byron.Protocol
import           Cardano.Config.Mock.Protocol
import           Cardano.Config.Shelley.Protocol

import qualified Ouroboros.Consensus.Cardano as Consensus


------------------------------------------------------------------------------
-- Conversions from configuration into specific protocols and their params
--

mkConsensusProtocol
  :: NodeConfiguration
  -> Maybe ProtocolFilepaths
  -> ExceptT ProtocolInstantiationError IO SomeConsensusProtocol
mkConsensusProtocol NodeConfiguration{ncProtocolConfig} files =
    case ncProtocolConfig of

      -- Mock protocols
      NodeProtocolConfigurationMock config ->
        case npcMockProtocol config of
          MockBFT   -> pure $ mkSomeConsensusProtocolBFT   config
          MockPBFT  -> pure $ mkSomeConsensusProtocolPBFT  config
          MockPraos -> pure $ mkSomeConsensusProtocolPraos config

      -- Real protocols
      NodeProtocolConfigurationByron config ->
        firstExceptT ByronProtocolInstantiationError $
          mkSomeConsensusProtocolRealPBFT config files

      NodeProtocolConfigurationShelley config ->
        firstExceptT ShelleyProtocolInstantiationError $
          mkSomeConsensusProtocolTPraos config files


mkNodeClientProtocol :: Protocol -> SomeNodeClientProtocol
mkNodeClientProtocol protocol =
    case protocol of
{-
      --TODO
      -- Mock protocols
      NodeProtocolConfigurationMock config ->
        case npcMockProtocol config of
          BFT      -> mkNodeClientProtocolBFT
          MockPBFT -> mkNodeClientProtocolPBFT
          Praos    -> mkNodeClientProtocolPraos
-}
      MockProtocol _ ->
        panic "TODO: mkNodeClientProtocol NodeProtocolConfigurationMock"

      -- Real protocols
      ByronProtocol ->
        mkSomeNodeClientProtocolRealPBFT
          --TODO: this is only the correct value for mainnet
          -- not for Byron testnets. This value is needed because
          -- to decode legacy EBBs one needs to know how many
          -- slots there are per-epoch. This info comes from
          -- the genesis file, but we don't have that in the
          -- client case.
          (EpochSlots 21600)
          (Consensus.SecurityParam 2160)

      ShelleyProtocol ->
        mkSomeNodeClientProtocolTPraos


-- | Many commands have variants or file formats that depend on the era.
--

data CardanoEra = ByronEraLegacy | ByronEra | ShelleyEra
  deriving Show


------------------------------------------------------------------------------
-- Errors
--

data ProtocolInstantiationError =
    ByronProtocolInstantiationError   ByronProtocolInstantiationError
  | ShelleyProtocolInstantiationError ShelleyProtocolInstantiationError
  deriving Show


renderProtocolInstantiationError :: ProtocolInstantiationError -> Text
renderProtocolInstantiationError pie =
  case pie of
    ByronProtocolInstantiationError bpie ->
      renderByronProtocolInstantiationError bpie

    ShelleyProtocolInstantiationError spie ->
      renderShelleyProtocolInstantiationError spie


data RealPBFTError
  = IncorrectProtocolSpecified !Protocol
  | FromProtocolError !ProtocolInstantiationError
  | InvariantViolation !Text
  | TransactionTypeNotHandledYet !Text
  deriving Show

renderRealPBFTError :: RealPBFTError -> Text
renderRealPBFTError err =
  case err of
    IncorrectProtocolSpecified ptcl -> "Incorrect protocol specified: " <> (Text.pack $ show ptcl)
    FromProtocolError ptclInstErr -> renderProtocolInstantiationError ptclInstErr
    InvariantViolation invErr -> "Invariant violation: " <> invErr
    TransactionTypeNotHandledYet err' -> "Transaction type not handled yet: " <> err'
