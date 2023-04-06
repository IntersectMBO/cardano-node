{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Governance.AnswerPoll
  ( golden_shelleyGovernanceAnswerPollVrf
  , golden_shelleyGovernanceAnswerPollCold
  , golden_shelleyGovernanceAnswerPollInvalidAnswer
  ) where

import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

golden_shelleyGovernanceAnswerPollVrf :: Property
golden_shelleyGovernanceAnswerPollVrf = propertyOnce $ do
  pollFile <- noteInputFile "test/data/golden/shelley/governance/poll.json"
  vrfKeyFile <- noteInputFile "test/data/golden/shelley/governance/vrf.sk"

  stdout <- execCardanoCLI
    [ "governance", "answer-poll"
    , "--poll-file", pollFile
    , "--signing-key-file", vrfKeyFile
    , "--answer", "0"
    ]

  noteInputFile "test/data/golden/shelley/governance/answer-vrf.json"
    >>= H.readFile
    >>= (H.===) stdout

golden_shelleyGovernanceAnswerPollCold :: Property
golden_shelleyGovernanceAnswerPollCold = propertyOnce $ do
  pollFile <- noteInputFile "test/data/golden/shelley/governance/poll.json"
  coldKeyFile <- noteInputFile "test/data/golden/shelley/governance/cold.sk"

  stdout <- execCardanoCLI
    [ "governance", "answer-poll"
    , "--poll-file", pollFile
    , "--signing-key-file", coldKeyFile
    , "--answer", "1"
    ]

  noteInputFile "test/data/golden/shelley/governance/answer-cold.json"
    >>= H.readFile
    >>= (H.===) stdout

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
