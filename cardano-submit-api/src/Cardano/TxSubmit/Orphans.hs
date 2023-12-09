{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.TxSubmit.Orphans
  (
  ) where

import           Cardano.Api
import           Cardano.Binary (DecoderError)
import           Data.Aeson (ToJSON (..))
import qualified Data.Aeson as Aeson
import           Ouroboros.Consensus.Cardano.Block

instance ToJSON DecoderError where
  toJSON = Aeson.String . textShow

deriving anyclass instance ToJSON EraMismatch
