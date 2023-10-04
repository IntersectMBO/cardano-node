{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unused-local-binds -Wno-unused-matches #-}

module Testnet.Start.Byron
  ( createByronGenesis
  , createByronUpdateProposal
  , createByronUpdateProposalVote
  , byronDefaultTestnetOptions
  ) where

import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Time.Clock (UTCTime)
import           GHC.Stack

import           Hedgehog.Extras.Stock.Time (showUTCTimeSeconds)
import           Hedgehog.Internal.Property (MonadTest)

import           Testnet.Process.Run


{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant <&>" -}
{- HLINT ignore "Redundant flip" -}
{- HLINT ignore "Use head" -}

-- TODO: Remove me and replace with a type that is more specific to genesis creation.
data CardanoTestnetOptions = CardanoTestnetOptions
  { numBftNodes :: Int
  , slotDuration :: Int
  , securityParam :: Int
  , nPoorAddresses :: Int
  , testnetMagic :: Int
  , totalBalance :: Int
  , enableP2P :: Bool
  } deriving (Eq, Show)

byronDefaultTestnetOptions :: CardanoTestnetOptions
byronDefaultTestnetOptions = CardanoTestnetOptions
  { numBftNodes = 3
  , slotDuration = 2000
  , securityParam = 10
  , testnetMagic = 42
  , nPoorAddresses = 128
  -- TODO: createByronGenesis should have a check that errors
  -- if totalBalance can be evenly split between numBftNodes
  -- with no remainder. Having a remainder results in rounding errors.
  , totalBalance = 8000000000000001
  , enableP2P = False
  }

-- TODO: We should not abuse the byron testnet options for genesis creation.
-- This will lead to confusion. We should create a separate type for genesis creation.
-- | Creates a default Byron genesis. This is required for any testnet, predominantly because
-- we inject our ADA supply into our testnet via the Byron genesis.
createByronGenesis
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => Int
  -> UTCTime
  -> CardanoTestnetOptions
  -> String
  -> String
  -> m ()
createByronGenesis testnetMagic' startTime testnetOptions pParamFp genOutputDir =
  withFrozenCallStack $ execCli_
    [ "byron", "genesis", "genesis"
    , "--protocol-magic", show testnetMagic'
    , "--start-time", showUTCTimeSeconds startTime
    , "--k", show (securityParam testnetOptions)
    , "--n-poor-addresses", "0"
    , "--n-delegate-addresses", show @Int (numBftNodes testnetOptions)
    , "--total-balance", show @Int (totalBalance testnetOptions)
    , "--delegate-share", "1"
    , "--avvm-entry-count", "0"
    , "--avvm-entry-balance", "0"
    , "--avvm-balance-factor", "1"
    , "--protocol-parameters-file", pParamFp
    , "--genesis-output-dir", genOutputDir
    ]

createByronUpdateProposal
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => Int -> String -> String -> Int -> m ()
createByronUpdateProposal testnetMagic' signingKeyFp updateProposalFp ptclMajorVersion =
  withFrozenCallStack $ execCli_
    [ "byron", "governance", "create-update-proposal"
    , "--filepath", updateProposalFp
    , "--testnet-magic", show testnetMagic'
    , "--signing-key", signingKeyFp
    , "--protocol-version-major", show ptclMajorVersion
    , "--protocol-version-minor", "0"
    , "--protocol-version-alt", "0"
    , "--application-name", "cardano-sl"
    , "--software-version-num", "1"
    , "--system-tag", "linux"
    , "--installer-hash", "0"
    ]

createByronUpdateProposalVote
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => Int -> String -> String -> String -> m ()
createByronUpdateProposalVote testnetMagic' updateProposalFp signingKey outputFp =
    withFrozenCallStack $ execCli_
      [ "byron", "governance", "create-proposal-vote"
      , "--proposal-filepath", updateProposalFp
      , "--testnet-magic", show testnetMagic'
      , "--signing-key", signingKey
      , "--vote-yes"
      , "--output-filepath", outputFp
      ]

