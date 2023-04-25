{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Governance.VerifyPoll
  ( golden_shelleyGovernanceVerifyPollVrf
  , golden_shelleyGovernanceVerifyPollVrfTempered
  , golden_shelleyGovernanceVerifyPollCold
  , golden_shelleyGovernanceVerifyPollColdTempered
  ) where

import           Cardano.Prelude

import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog as H

{- HLINT ignore "Use camelCase" -}

golden_shelleyGovernanceVerifyPollVrf :: Property
golden_shelleyGovernanceVerifyPollVrf = propertyOnce $ do
  pollFile <- noteInputFile "test/data/golden/shelley/governance/poll.json"
  metadataFile <- noteInputFile "test/data/golden/shelley/governance/answer-vrf.json"

  void $ execCardanoCLI
    [ "governance", "verify-poll"
    , "--poll-file", pollFile
    , "--metadata-file", metadataFile
    ]

golden_shelleyGovernanceVerifyPollCold :: Property
golden_shelleyGovernanceVerifyPollCold = propertyOnce $ do
  pollFile <- noteInputFile "test/data/golden/shelley/governance/poll.json"
  metadataFile <- noteInputFile "test/data/golden/shelley/governance/answer-cold.json"

  void $ execCardanoCLI
    [ "governance", "verify-poll"
    , "--poll-file", pollFile
    , "--metadata-file", metadataFile
    ]

golden_shelleyGovernanceVerifyPollVrfTempered :: Property
golden_shelleyGovernanceVerifyPollVrfTempered = propertyOnce $ do
  pollFile <- noteInputFile "test/data/golden/shelley/governance/poll.json"
  metadataFile <- noteInputFile "test/data/golden/shelley/governance/answer-vrf-tempered.json"

  result <- tryExecCardanoCLI
    [ "governance", "verify-poll"
    , "--poll-file", pollFile
    , "--metadata-file", metadataFile
    ]

  either (const H.success) (const H.failure) result

golden_shelleyGovernanceVerifyPollColdTempered :: Property
golden_shelleyGovernanceVerifyPollColdTempered = propertyOnce $ do
  pollFile <- noteInputFile "test/data/golden/shelley/governance/poll.json"
  metadataFile <- noteInputFile "test/data/golden/shelley/governance/answer-cold-tempered.json"

  result <- tryExecCardanoCLI
    [ "governance", "verify-poll"
    , "--poll-file", pollFile
    , "--metadata-file", metadataFile
    ]

  either (const H.success) (const H.failure) result
