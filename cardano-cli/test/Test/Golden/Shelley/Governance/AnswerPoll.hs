{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Governance.AnswerPoll
  ( golden_shelleyGovernanceAnswerPoll
  , golden_shelleyGovernanceAnswerPollInvalidAnswer
  ) where

import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

golden_shelleyGovernanceAnswerPoll :: Property
golden_shelleyGovernanceAnswerPoll = propertyOnce $ do
  pollFile <- noteInputFile "test/data/golden/shelley/governance/polls/basic.json"

  stdout <- execCardanoCLI
    [ "governance", "answer-poll"
    , "--poll-file", pollFile
    , "--answer", "1"
    ]

  noteInputFile "test/data/golden/shelley/governance/answer/basic.json"
    >>= H.readFile
    >>= (H.===) stdout

golden_shelleyGovernanceAnswerPollInvalidAnswer :: Property
golden_shelleyGovernanceAnswerPollInvalidAnswer = propertyOnce $ do
  pollFile <- noteInputFile "test/data/golden/shelley/governance/polls/basic.json"

  result <- tryExecCardanoCLI
    [ "governance", "answer-poll"
    , "--poll-file", pollFile
    , "--answer", "3"
    ]

  either (const H.success) (const H.failure) result
