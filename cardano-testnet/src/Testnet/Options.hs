{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unused-local-binds -Wno-unused-matches #-}

module Testnet.Options
  ( BabbageTestnetOptions(..)
  , defaultTestnetOptions
  ) where

import           Prelude

import           Testnet.Util.Runtime (NodeLoggingFormat (..))

{- HLINT ignore "Redundant flip" -}

data BabbageTestnetOptions = BabbageTestnetOptions
  { babbageNumSpoNodes :: Int
  , babbageSlotDuration :: Int
  , babbageSecurityParam :: Int
  , babbageTotalBalance :: Int
  , babbageNodeLoggingFormat :: NodeLoggingFormat
  } deriving (Eq, Show)

defaultTestnetOptions :: BabbageTestnetOptions
defaultTestnetOptions = BabbageTestnetOptions
  { babbageNumSpoNodes = 3
  , babbageSlotDuration = 200
  , babbageSecurityParam = 10
  , babbageTotalBalance = 10020000000
  , babbageNodeLoggingFormat = NodeLoggingFormatAsJson
  }
