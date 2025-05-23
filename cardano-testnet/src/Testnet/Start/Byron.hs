{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unused-local-binds -Wno-unused-matches #-}
{-# LANGUAGE NumericUnderscores #-}

module Testnet.Start.Byron
  ( createByronGenesis
  , byronDefaultGenesisOptions
  ) where

import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Time.Clock (UTCTime)
import           GHC.Stack

import           Testnet.Process.Run

import           Hedgehog.Extras.Stock.Time (showUTCTimeSeconds)
import           Hedgehog.Internal.Property (MonadTest)

data ByronGenesisOptions = ByronGenesisOptions
  { byronNumBftNodes :: Int
  , byronSecurityParam :: Int
  , byronTotalBalance :: Int
  } deriving (Eq, Show)

byronDefaultGenesisOptions :: ByronGenesisOptions
byronDefaultGenesisOptions = ByronGenesisOptions
  { byronNumBftNodes = 3
  , byronSecurityParam = 5
  -- TODO: createByronGenesis should have a check that errors
  -- if totalBalance can be evenly split between numBftNodes
  -- with no remainder. Having a remainder results in rounding errors.
  , byronTotalBalance = 8_000_000_001 -- 8 billions. Should be smaller than 'cardanoMaxSupply' in Testnet.Start.Types
  }

-- TODO: We should not abuse the byron testnet options for genesis creation.
-- This will lead to confusion. We should create a separate type for genesis creation.
-- | Creates a default Byron genesis. This is required for any testnet, predominantly because
-- we inject our ADA supply into our testnet via the Byron genesis.
createByronGenesis
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => Int
  -> UTCTime
  -> ByronGenesisOptions
  -> String
  -> String
  -> m ()
createByronGenesis testnetMagic' startTime testnetOptions pParamFp genOutputDir =
  withFrozenCallStack $ execCli_
    [ "byron", "genesis", "genesis"
    , "--protocol-magic", show testnetMagic'
    , "--start-time", showUTCTimeSeconds startTime
    , "--k", show (byronSecurityParam testnetOptions)
    , "--n-poor-addresses", "0"
    , "--n-delegate-addresses", show @Int (byronNumBftNodes testnetOptions)
    , "--total-balance", show @Int (byronTotalBalance testnetOptions)
    , "--delegate-share", "1"
    , "--avvm-entry-count", "0"
    , "--avvm-entry-balance", "0"
    , "--avvm-balance-factor", "1"
    , "--protocol-parameters-file", pParamFp
    , "--genesis-output-dir", genOutputDir
    ]

