{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Governance.AnswerPoll
  ( golden_shelleyGovernanceAnswerPollVrf
  , golden_shelleyGovernanceAnswerPollCold
  , golden_shelleyGovernanceAnswerPollInvalidAnswer
  ) where

import           Hedgehog (Property)
import           Test.OptParse
import           Test.Utilities (diffVsGoldenFile)

import qualified Hedgehog as H

{- HLINT ignore "Use camelCase" -}

golden_shelleyGovernanceAnswerPollVrf :: Property
golden_shelleyGovernanceAnswerPollVrf = propertyOnce $ do
  pollFile <- noteInputFile "test/data/golden/shelley/governance/poll.json"
  vrfKeyFile <- noteInputFile "test/data/golden/shelley/governance/vrf.sk"

  result <- execCardanoCLI
    [ "governance", "answer-poll"
    , "--poll-file", pollFile
    , "--signing-key-file", vrfKeyFile
    , "--answer", "0"
    ]

  diffVsGoldenFile result "test/data/golden/shelley/governance/answer-vrf.json"

golden_shelleyGovernanceAnswerPollCold :: Property
golden_shelleyGovernanceAnswerPollCold = propertyOnce $ do
  pollFile <- noteInputFile "test/data/golden/shelley/governance/poll.json"
  coldKeyFile <- noteInputFile "test/data/golden/shelley/governance/cold.sk"

  result <- execCardanoCLI
    [ "governance", "answer-poll"
    , "--poll-file", pollFile
    , "--signing-key-file", coldKeyFile
    , "--answer", "1"
    ]

  diffVsGoldenFile result "test/data/golden/shelley/governance/answer-cold.json"

golden_shelleyGovernanceAnswerPollInvalidAnswer :: Property
golden_shelleyGovernanceAnswerPollInvalidAnswer = propertyOnce $ do
  pollFile <- noteInputFile "test/data/golden/shelley/governance/poll.json"
  vrfKeyFile <- noteInputFile "test/data/golden/shelley/governance/vrf.sk"

  result <- tryExecCardanoCLI
    [ "governance", "answer-poll"
    , "--poll-file", pollFile
    , "--signing-key-file", vrfKeyFile
    , "--answer", "3"
    ]

  either (const H.success) (const H.failure) result
