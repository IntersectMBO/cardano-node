{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Hedgehog.Extras.Internal.Plan
  ( Plan(..)
  , Component(..)
  ) where

import           Control.Applicative
import           Data.Aeson
import           Data.Eq
import           Data.Function
import           Data.Maybe
import           Data.Text (Text)
import           GHC.Generics
import           Text.Show

data Component = Component
  { componentName :: Maybe Text
  , binFile :: Maybe Text
  }
  deriving (Generic, Eq, Show)

newtype Plan = Plan
  { installPlan :: [Component]
  }
  deriving (Generic, Eq, Show)

instance FromJSON Plan where
    parseJSON = withObject "Plan" $ \v -> Plan
        <$> v .: "install-plan"

instance FromJSON Component where
    parseJSON = withObject "Plan" $ \v -> Component
        <$> v .:? "component-name"
        <*> v .:? "bin-file"
