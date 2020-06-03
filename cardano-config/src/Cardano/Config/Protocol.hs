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
  , cardanoEraForProtocol
  , mkNodeClientProtocol
  , ncCardanoEra
  , withRealPBFT

    -- * Errors
  , RealPBFTError(..)
  , renderRealPBFTError
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, left)
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
import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import           Ouroboros.Consensus.Node.Run (RunNode)


-- | Perform an action that expects ProtocolInfo for Byron/PBFT,
--   with attendant configuration.
withRealPBFT
  :: NodeConfiguration
  -> (RunNode ByronBlock
        => Consensus.Protocol IO ByronBlock Consensus.ProtocolRealPBFT
        -> ExceptT RealPBFTError IO a)
  -> ExceptT RealPBFTError IO a
withRealPBFT nc action = do
  SomeConsensusProtocol p <- firstExceptT FromProtocolError $
                               mkConsensusProtocol nc Nothing
  case p of
    proto@Consensus.ProtocolRealPBFT{} -> action proto
    _ -> left $ IncorrectProtocolSpecified (ncProtocol nc)

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

-- | Many commands have variants or file formats that depend on the era.
--

data CardanoEra = ByronEraLegacy | ByronEra | ShelleyEra
  deriving Show

cardanoEraForProtocol :: Protocol -> CardanoEra
cardanoEraForProtocol BFT      = ShelleyEra
cardanoEraForProtocol Praos    = ShelleyEra
cardanoEraForProtocol MockPBFT = ShelleyEra
cardanoEraForProtocol RealPBFT = ByronEra
cardanoEraForProtocol TPraos   = ShelleyEra

ncCardanoEra :: NodeConfiguration -> CardanoEra
ncCardanoEra = cardanoEraForProtocol . ncProtocol

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
