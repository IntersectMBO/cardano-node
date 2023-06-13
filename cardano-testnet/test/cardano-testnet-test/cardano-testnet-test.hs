{-# LANGUAGE FlexibleInstances #-}

module Main
  ( main
  ) where

import           Prelude
import           System.IO (BufferMode (LineBuffering), hSetBuffering, hSetEncoding, stdout, utf8)
import           Test.Tasty (TestTree)

import qualified Cardano.Testnet.Test.Cli.Babbage.LeadershipSchedule
import qualified Cardano.Testnet.Test.Cli.Babbage.StakeSnapshot
import qualified Cardano.Testnet.Test.Cli.KesPeriodInfo
import qualified Cardano.Testnet.Test.Cli.QuerySlotNumber
import qualified Cardano.Testnet.Test.FoldBlocks
import qualified Cardano.Testnet.Test.Node.Shutdown
import qualified Cardano.Testnet.Test.ShutdownOnSlotSynced
import qualified System.Environment as E
import qualified Test.Tasty as T
import qualified Test.Tasty.Ingredients as T
import qualified Testnet.Property.Run as H


import qualified Cardano.Crypto.Init as Crypto

tests :: IO TestTree
tests = pure $ T.testGroup "test/Spec.hs"
  [ T.testGroup "Spec"
    [ H.ignoreOnWindows "Shutdown" Cardano.Testnet.Test.Node.Shutdown.hprop_shutdown
    , H.ignoreOnWindows "ShutdownOnSlotSynced" Cardano.Testnet.Test.ShutdownOnSlotSynced.hprop_shutdownOnSlotSynced
    -- TODO: This is failing. Disabling until we can figure out why
    -- , T.testGroup "Alonzo"
    --   [ H.ignoreOnMacAndWindows "leadership-schedule" Cardano.Testnet.Test.Cli.Alonzo.LeadershipSchedule.hprop_leadershipSchedule
    --   ]
    , T.testGroup "Babbage"
      [ H.ignoreOnWindows "leadership-schedule" Cardano.Testnet.Test.Cli.Babbage.LeadershipSchedule.hprop_leadershipSchedule
      , H.ignoreOnWindows "stake-snapshot" Cardano.Testnet.Test.Cli.Babbage.StakeSnapshot.hprop_stakeSnapshot
      ]
      -- Ignored on Windows due to <stdout>: commitBuffer: invalid argument (invalid character)
      -- as a result of the kes-period-info output to stdout.
      -- TODO: Babbage temporarily ignored due to broken protocol-state query
    , H.ignoreOnWindows "kes-period-info" Cardano.Testnet.Test.Cli.KesPeriodInfo.hprop_kes_period_info
    , H.ignoreOnWindows "query-slot-number" Cardano.Testnet.Test.Cli.QuerySlotNumber.hprop_querySlotNumber
    ]
  , H.ignoreOnWindows "foldBlocks receives ledger state" Cardano.Testnet.Test.FoldBlocks.prop_foldBlocks
  ]

ingredients :: [T.Ingredient]
ingredients = T.defaultIngredients

main :: IO ()
main = do
  Crypto.cryptoInit

  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8
  args <- E.getArgs

  E.withArgs args $ tests >>= T.defaultMainWithIngredients ingredients
