{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Testnet.Property.Checks
  ( prop_spos_in_ledger_state
  ) where

import           Cardano.Api.Shelley

import           Prelude hiding (lines)

import           Control.Monad
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.IO.Class (MonadIO)
import qualified Data.Aeson as Aeson
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Stack (HasCallStack)
import qualified GHC.Stack as GHC

import           Hedgehog (MonadTest)
import qualified Hedgehog as H
import           Hedgehog.Extras.Test.Base (failMessage)
import qualified Hedgehog.Extras.Test.File as H
import           Hedgehog.Extras.Test.Process (ExecConfig)

import           Testnet.Process.Run
import           Testnet.Start.Types

-- | A sanity check that confirms that there are the expected number of SPOs in the ledger state
prop_spos_in_ledger_state
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => FilePath -- ^ Stake pools query output filepath
  -> CardanoTestnetOptions
  -> ExecConfig
  -> m ()
prop_spos_in_ledger_state output tNetOptions execConfig =
  GHC.withFrozenCallStack $ do
    let testnetMag = cardanoTestnetMagic tNetOptions
        numExpectedPools = length $ cardanoNodes tNetOptions

    void $ execCli' execConfig
        [ "query", "stake-pools"
        , "--testnet-magic", show @Int testnetMag
        , "--out-file", output
        ]

    poolSet <- H.evalEither =<< H.evalIO (Aeson.eitherDecodeFileStrict' @(Set PoolId) output)

    H.cat output

    let numPoolsInLedgerState = Set.size poolSet
    unless (numPoolsInLedgerState == numExpectedPools) $
      failMessage GHC.callStack
        $ unlines [ "Expected number of stake pools not found in ledger state"
                  , "Expected: ", show numExpectedPools
                  , "Actual: ", show numPoolsInLedgerState
                  ]
