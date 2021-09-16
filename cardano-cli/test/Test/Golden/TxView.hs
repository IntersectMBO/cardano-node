{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.TxView (txViewTests) where

import           Cardano.Prelude

import           Hedgehog (Group (..), Property, checkSequential)
import           Hedgehog.Extras (moduleWorkspace, note_, propertyOnce)

import           Test.OptParse (execCardanoCLI, noteTempFile)
import           Test.Utilities (diffVsGoldenFile)

{- HLINT ignore "Use camelCase" -}

txViewTests :: IO Bool
txViewTests =
  checkSequential $
    Group "`transaction view` Goldens"
      [ ("golden_view_byron", golden_view_byron)
      , ("golden_view_shelley", golden_view_shelley)
      , ("golden_view_allegra", golden_view_allegra)
      , ("golden_view_mary", golden_view_mary)
      , ("golden_view_alonzo", golden_view_alonzo)
      ]

golden_view_byron :: Property
golden_view_byron =
  propertyOnce $
  moduleWorkspace "tmp" $ \tempDir -> do
    transactionBodyFile <- noteTempFile tempDir "transaction-body-file"

    -- Create transaction body
    void $
      execCardanoCLI
        [ "transaction", "build-raw"
        , "--byron-era"
        , "--tx-in"
        ,   "F8EC302D19E3C8251C30B1434349BF2E949A1DBF14A4EBC3D512918D2D4D5C56\
            \#88"
        , "--tx-out"
        ,   "5oP9ib6ym3XfwXuy3ksXZzgtBzXSArXAACQVXKqcPhiLnHVYjXJNu2T6Zomh8LAWLV\
            \+68"
        , "--out-file", transactionBodyFile
        ]

    -- View transaction body
    result <-
      execCardanoCLI
        ["transaction", "view", "--tx-body-file", transactionBodyFile]
    diffVsGoldenFile result "test/data/golden/byron/transaction-view.out"

golden_view_shelley :: Property
golden_view_shelley =
  propertyOnce $
  moduleWorkspace "tmp" $ \tempDir -> do
    updateProposalFile <- noteTempFile tempDir "update-proposal"
    transactionBodyFile <- noteTempFile tempDir "transaction-body"

    let extraEntropySeed = "c0ffee"
    note_ $ "extra entropy seed: " ++ extraEntropySeed
    note_
      "extra entropy hash:\
      \ 88f04f011dcded879039ae4b9b20219d9448e5c7b42c2d1f638fb8740e0ab8be"

    note_
      "genesis-verification-key-file hash:\
      \ 81cb0bc5b6fbba391e6f7ec3d9271cbea25bcbf907181b7c4d5f8c2f"

    -- Create update proposal
    void $
      execCardanoCLI
        [ "governance", "create-update-proposal"
        , "--decentralization-parameter", "63/64"
        , "--epoch", "64"
        , "--extra-entropy", extraEntropySeed
        , "--genesis-verification-key-file"
        ,   "test/data/golden/shelley/keys/genesis_keys/verification_key"
        , "--key-reg-deposit-amt", "71"
        , "--max-block-body-size", "72"
        , "--max-block-header-size", "73"
        , "--max-tx-size", "74"
        , "--min-fee-constant", "75"
        , "--min-fee-linear", "76"
        , "--min-pool-cost", "77"
        , "--min-utxo-value", "78"
        , "--monetary-expansion", "79/80"
        , "--number-of-pools", "80"
        , "--out-file", updateProposalFile
        , "--pool-influence", "82/83"
        , "--pool-reg-deposit", "83"
        , "--pool-retirement-epoch-boundary", "84"
        , "--protocol-major-version", "85"
        , "--protocol-minor-version", "86"
        , "--treasury-expansion", "87/88"
        ]

    -- Create transaction body
    void $
      execCardanoCLI
        [ "transaction", "build-raw"
        , "--shelley-era"
        , "--tx-in"
        ,   "fe5dd07fb576bff960d6e066eade5b26cdb5afebe29f76ea58d0a098bce5d891\
            \#29"
        , "--tx-out"
        ,   "addr_test1vz7w0r9epak6nmnh3mc8e2ypkjyu8zsc3xf7dpct6k577acxmcfyv+31"
        , "--fee", "32"
        , "--invalid-hereafter", "33"
        , "--withdrawal"
        ,   "stake_test1up00fz9lyqs5sjks82k22eqz7a9srym9vysjgp3h2ua2v2cm522kg\
            \+42"
        , "--update-proposal-file", updateProposalFile
        , "--out-file", transactionBodyFile
        ]

    -- View transaction body
    result <-
      execCardanoCLI
        ["transaction", "view", "--tx-body-file", transactionBodyFile]
    diffVsGoldenFile result "test/data/golden/shelley/transaction-view.out"

golden_view_allegra :: Property
golden_view_allegra =
  propertyOnce $
  moduleWorkspace "tmp" $ \tempDir -> do
    transactionBodyFile <- noteTempFile tempDir "transaction-body-file"

    -- Create transaction body
    void $
      execCardanoCLI
        [ "transaction", "build-raw"
        , "--allegra-era"
        , "--tx-in"
        ,   "fe5dd07fb576bff960d6e066eade5b26cdb5afebe29f76ea58d0a098bce5d891\
            \#94"
        , "--tx-out"
        ,   "addr_test1\
            \qrefnr4k09pvge6dq83v6s67ruter8sftmky8qrmkqqsxy7q5psgn8tgqmupq4r7\
            \9jmxlyk4eqt6z6hj5g8jd8393msqaw47f4\
            \+99"
        , "--fee", "100"
        , "--invalid-hereafter", "101"
        , "--out-file", transactionBodyFile
        ]

    -- View transaction body
    result <-
      execCardanoCLI
        ["transaction", "view", "--tx-body-file", transactionBodyFile]
    diffVsGoldenFile result "test/data/golden/allegra/transaction-view.out"

golden_view_mary :: Property
golden_view_mary =
  propertyOnce $
  moduleWorkspace "tmp" $ \tempDir -> do
    transactionBodyFile <- noteTempFile tempDir "transaction-body-file"

    -- Create transaction body
    void $
      execCardanoCLI
        [ "transaction", "build-raw"
        , "--mary-era"
        , "--tx-in"
        ,   "fe5dd07fb576bff960d6e066eade5b26cdb5afebe29f76ea58d0a098bce5d891\
            \#135"
        , "--tx-out"
        ,   "addr_test1\
            \qrefnr4k09pvge6dq83v6s67ruter8sftmky8qrmkqqsxy7q5psgn8tgqmupq4r7\
            \9jmxlyk4eqt6z6hj5g8jd8393msqaw47f4\
            \ + \
            \138\
            \ + \
            \130 d441227553a0f1a965fee7d60a0f724b368dd1bddbc208730fccebcf\
            \ + \
            \132 52dc3d43b6d2465e96109ce75ab61abe5e9c1d8a3c9ce6ff8a3af528.cafe\
            \ + \
            \134 d441227553a0f1a965fee7d60a0f724b368dd1bddbc208730fccebcf.f00d\
            \ + \
            \136 52dc3d43b6d2465e96109ce75ab61abe5e9c1d8a3c9ce6ff8a3af528.dead\
            \ + \
            \138\
              \ d441227553a0f1a965fee7d60a0f724b368dd1bddbc208730fccebcf\
              \.736e6f77\
            \ + \
            \142\
              \ 52dc3d43b6d2465e96109ce75ab61abe5e9c1d8a3c9ce6ff8a3af528\
              \.736b79"
        , "--fee", "139"
        , "--invalid-before", "140"
        , "--mint"
        ,   "130 d441227553a0f1a965fee7d60a0f724b368dd1bddbc208730fccebcf\
            \ + \
            \132 52dc3d43b6d2465e96109ce75ab61abe5e9c1d8a3c9ce6ff8a3af528.cafe\
            \ + \
            \134 d441227553a0f1a965fee7d60a0f724b368dd1bddbc208730fccebcf.f00d\
            \ + \
            \136 52dc3d43b6d2465e96109ce75ab61abe5e9c1d8a3c9ce6ff8a3af528.dead\
            \ + \
            \138\
              \ d441227553a0f1a965fee7d60a0f724b368dd1bddbc208730fccebcf\
              \.736e6f77\
            \ + \
            \142\
              \ 52dc3d43b6d2465e96109ce75ab61abe5e9c1d8a3c9ce6ff8a3af528\
              \.736b79"
        , "--minting-script-file", "test/data/golden/mary/scripts/mint.all"
        , "--minting-script-file", "test/data/golden/mary/scripts/mint.any"
        , "--out-file", transactionBodyFile
        ]

    -- View transaction body
    result <-
      execCardanoCLI
        ["transaction", "view", "--tx-body-file", transactionBodyFile]
    diffVsGoldenFile result "test/data/golden/mary/transaction-view.out"

golden_view_alonzo :: Property
golden_view_alonzo =
  propertyOnce $
    moduleWorkspace "tmp" $ \tempDir -> do
      updateProposalFile <- noteTempFile tempDir "update-proposal"
      transactionBodyFile <- noteTempFile tempDir "transaction-body"

      note_
        "genesis-verification-key-file hash:\
        \ 1bafa294233a5a7ffbf539ae798da0943aa83d2a19398c2d0e5af114"

      -- Create update proposal
      void $
        execCardanoCLI
          [ "governance", "create-update-proposal"
          , "--epoch", "190"
          , "--genesis-verification-key-file"
          ,   "test/data/golden/shelley/keys/genesis_keys/verification_key"
          , "--utxo-cost-per-word", "194"
          , "--price-execution-steps", "195/196"
          , "--price-execution-memory", "196/197"
          , "--max-tx-execution-units", "(197, 198)"
          , "--max-block-execution-units", "(198, 199)"
          , "--max-value-size", "199"
          , "--collateral-percent", "200"
          , "--max-collateral-inputs", "201"
          , "--out-file", updateProposalFile
          ]

      -- Create transaction body
      void $
        execCardanoCLI
          [ "transaction", "build-raw"
          , "--alonzo-era"
          , "--tx-in"
          ,   "ed7c8f68c194cc763ee65ad22ef0973e26481be058c65005fd39fb93f9c43a20\
              \#212"
          , "--fee", "213"
          , "--update-proposal-file", updateProposalFile
          , "--out-file", transactionBodyFile
          ]

      -- View transaction body
      result <-
        execCardanoCLI
          ["transaction", "view", "--tx-body-file", transactionBodyFile]
      diffVsGoldenFile result "test/data/golden/alonzo/transaction-view.out"
