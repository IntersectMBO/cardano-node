{-# LANGUAGE RankNTypes           #-}

module Cardano.Node.Types
  ( Protocol(..)
  , parseProtocol
  , parseProtocolAsCommand
  )
where

import Prelude
import Data.Foldable
import Options.Applicative

data Protocol
  = ByronLegacy
  | BFT
  | Praos
  | MockPBFT
  | RealPBFT
  deriving (Show)


