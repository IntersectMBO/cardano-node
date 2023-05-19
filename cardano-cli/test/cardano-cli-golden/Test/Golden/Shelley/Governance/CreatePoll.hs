{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Governance.CreatePoll
  ( golden_shelleyGovernanceCreatePoll
  , golden_shelleyGovernanceCreateLongPoll
  ) where

import           Control.Monad (void)
import           Hedgehog (Property)
import           Test.Cardano.CLI.Util

import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

golden_shelleyGovernanceCreatePoll :: Property
golden_shelleyGovernanceCreatePoll =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    pollFile <- noteTempFile tempDir "poll.json"

    stdout <- execCardanoCLI
      [ "governance", "create-poll"
      , "--question", "Pineapples on pizza?"
      , "--answer", "yes"
      , "--answer", "no"
      , "--out-file", pollFile
      ]

    void $ H.readFile pollFile
    noteInputFile "test/cardano-cli-golden/files/golden/shelley/governance/create/basic.json"
      >>= H.readFile
      >>= (H.===) stdout
    H.assertFileOccurences 1 "GovernancePoll" pollFile
    H.assertEndsWithSingleNewline pollFile

golden_shelleyGovernanceCreateLongPoll :: Property
golden_shelleyGovernanceCreateLongPoll =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    pollFile <- noteTempFile tempDir "poll.json"

    stdout <- execCardanoCLI
      [ "governance", "create-poll"
      , "--question", "What is the most adequate topping to put on a pizza (please consider all possibilities and take time to answer)?"
      , "--answer", "pineapples"
      , "--answer", "only traditional topics should go on a pizza, this isn't room for jokes"
      , "--out-file", pollFile
      ]

    void $ H.readFile pollFile
    noteInputFile "test/cardano-cli-golden/files/golden/shelley/governance/create/long-text.json"
      >>= H.readFile
      >>= (H.===) stdout
    H.assertFileOccurences 1 "GovernancePoll" pollFile
    H.assertEndsWithSingleNewline pollFile
