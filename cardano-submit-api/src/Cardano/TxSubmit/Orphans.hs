{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.TxSubmit.Orphans
  (
  ) where

import           Cardano.Api (EraMismatch, ToJSON (toJSON), textShow)

import           Cardano.Binary (DecoderError)

import qualified Data.Aeson as Aeson

instance ToJSON DecoderError where
  toJSON = Aeson.String . textShow

deriving anyclass instance ToJSON EraMismatch
