{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Benchmarking.TxCentrifuge.ValueBuilder
  ( ValueBuilder(..)
  , parseValueBuilder
  ) where

import Cardano.Benchmarking.PullFiction.Config.Raw qualified as Raw
import Control.Monad (when)
import Data.Aeson ((.:), (.:?), (.!=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types qualified as Aeson
import Numeric.Natural (Natural)

data ValueBuilder = ValueBuilder
  { inputsPerTx :: !Natural
  , outputsPerTx :: !Natural
  , fee :: !Integer
  , metadataBytes :: !Natural
  , txSize :: !(Maybe Natural)
  } deriving (Eq, Show)

parseValueBuilder :: Raw.Builder -> Either String ValueBuilder
parseValueBuilder raw
  | Raw.builderType raw /= "value" = Left "Builder: expected type \"value\""
  | otherwise = Aeson.parseEither parseParams (Raw.builderParams raw)
 where
  parseParams = Aeson.withObject "ValueParams" $ \o -> do
    when (KeyMap.member "metadata_bytes" o && KeyMap.member "tx_size" o) $
      fail "metadata_bytes and tx_size are mutually exclusive (omit one)"
    inputs <- o .:? "inputs_per_tx" .!= 1
    outputs <- o .:? "outputs_per_tx" .!= 1
    fixedFee <- o .: "fee"
    metadata <- o .:? "metadata_bytes" .!= 0
    size <- o .:? "tx_size"
    when (inputs == 0) $ fail "inputs_per_tx must be >= 1"
    when (outputs == 0) $ fail "outputs_per_tx must be >= 1"
    when (fixedFee < 0) $ fail "fee must be >= 0"
    when (size == Just 0) $ fail "tx_size must be >= 1"
    pure (ValueBuilder inputs outputs fixedFee metadata size)
