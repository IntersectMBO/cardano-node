{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.Era
  ( CardanoEra(..)
  , cardanoEraForProtocol
  ) where

import           Cardano.Prelude hiding (atomically, catch, option)

import           Cardano.Config.Protocol (Protocol(..))


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

