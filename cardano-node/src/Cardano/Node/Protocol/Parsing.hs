{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Node.Protocol.Parsing
  ( Protocol(..)
  ) where

import           Cardano.Node.Orphans ()

import           Control.DeepSeq (NFData)
import           Data.Aeson
import           GHC.Generics (Generic)

import           NoThunks.Class (NoThunks)

data Protocol = CardanoProtocol
  deriving (Eq, Generic)

instance Show Protocol where
  show CardanoProtocol = "Byron; Shelley"

deriving instance NFData Protocol
deriving instance NoThunks Protocol

instance FromJSON Protocol where
  parseJSON =
    withText "Protocol" $ \str -> case str of
      "Cardano" -> pure CardanoProtocol
      _ -> fail $ "Parsing of Protocol failed. " <> show str <> " is not a valid protocol"
