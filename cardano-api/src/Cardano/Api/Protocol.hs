{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}


module Cardano.Api.Protocol
  (
    -- * The enumeration of supported protocols
    Protocol(..)
  , MockProtocol(..)

    -- * Node client support
    -- | Support for the context needed to run a client of a node that is using
    -- a protocol.
  , mkNodeClientProtocol
  , SomeNodeClientProtocol(..)

    -- TODO: Does this really belong here?
  , ProtocolData(..)
  ) where

import           Cardano.Prelude

import           Control.Monad.Fail (fail)
import           Data.Aeson

import           Cardano.Chain.Slotting (EpochSlots(..))

import           Cardano.Api.Protocol.Types
import           Cardano.Api.Protocol.Byron
import           Cardano.Api.Protocol.Cardano
import           Cardano.Api.Protocol.Shelley

import qualified Ouroboros.Consensus.Cardano as Consensus

data Protocol = MockProtocol !MockProtocol
              | ByronProtocol
              | ShelleyProtocol
              | CardanoProtocol
  deriving (Eq, Show, Generic)

instance FromJSON Protocol where
  parseJSON =
    withText "Protocol" $ \str -> case str of

      -- The new names
      "MockBFT"   -> pure (MockProtocol MockBFT)
      "MockPBFT"  -> pure (MockProtocol MockPBFT)
      "MockPraos" -> pure (MockProtocol MockPraos)
      "Byron"     -> pure ByronProtocol
      "Shelley"   -> pure ShelleyProtocol
      "Cardano"   -> pure CardanoProtocol

      -- The old names
      "BFT"       -> pure (MockProtocol MockBFT)
    --"MockPBFT"  -- same as new name
      "Praos"     -> pure (MockProtocol MockPraos)
      "RealPBFT"  -> pure ByronProtocol
      "TPraos"    -> pure ShelleyProtocol

      _           -> fail $ "Parsing of Protocol failed. "
                         <> show str <> " is not a valid protocol"


deriving instance NFData Protocol
deriving instance NoUnexpectedThunks Protocol

data MockProtocol = MockBFT
                  | MockPBFT
                  | MockPraos
  deriving (Eq, Show, Generic)

deriving instance NFData MockProtocol
deriving instance NoUnexpectedThunks MockProtocol

mkNodeClientProtocol :: Protocol -> SomeNodeClientProtocol
mkNodeClientProtocol protocol =
    case protocol of
{-
      --TODO
      -- Mock protocols
      NodeProtocolConfigurationMock config ->
        case npcMockProtocol config of
          BFT      -> mkNodeClientProtocolMockBFT
          MockPBFT -> mkNodeClientProtocolMockPBFT
          Praos    -> mkNodeClientProtocolMockPraos
-}
      MockProtocol _ ->
        panic "TODO: mkNodeClientProtocol NodeProtocolConfigurationMock"

      -- Real protocols
      ByronProtocol ->
        mkSomeNodeClientProtocolByron
          --TODO: this is only the correct value for mainnet
          -- not for Byron testnets. This value is needed because
          -- to decode legacy EBBs one needs to know how many
          -- slots there are per-epoch. This info comes from
          -- the genesis file, but we don't have that in the
          -- client case.
          (EpochSlots 21600)
          (Consensus.SecurityParam 2160)

      ShelleyProtocol ->
        mkSomeNodeClientProtocolShelley

      CardanoProtocol ->
        mkSomeNodeClientProtocolCardano
          --TODO: this is only the correct value for mainnet
          -- not for Byron testnets. This value is needed because
          -- to decode legacy EBBs one needs to know how many
          -- slots there are per-epoch. This info comes from
          -- the genesis file, but we don't have that in the
          -- client case.
          (EpochSlots 21600)
          (Consensus.SecurityParam 2160)

data ProtocolData
  = ProtocolDataByron !EpochSlots !Consensus.SecurityParam
  | ProtocolDataShelley
  | ProtocolDataCardano !EpochSlots !Consensus.SecurityParam
  deriving (Eq, Show)
