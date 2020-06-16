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
                   (NodeConfiguration(..), Protocol (..), ProtocolFilepaths(..),
                    SomeConsensusProtocol(..), SomeNodeClientProtocol(..),
                    TraceConstraints)
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
                    (Consensus.SecurityParam 2160)

      TPraos   -> mkSomeNodeClientProtocolTPraos

      _        -> panic ("mkNodeClientProtocol TODO: " <> show protocol)

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
  | MockProtocolInstantiationError    MockProtocolInstantiationError
  deriving Show


renderProtocolInstantiationError :: ProtocolInstantiationError -> Text
renderProtocolInstantiationError pie =
  case pie of
    ByronProtocolInstantiationError bpie ->
      renderByronProtocolInstantiationError bpie

    ShelleyProtocolInstantiationError spie ->
      renderShelleyProtocolInstantiationError spie

    MockProtocolInstantiationError mpie ->
      renderMockProtocolInstantiationError mpie


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
