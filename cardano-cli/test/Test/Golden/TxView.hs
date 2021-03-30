{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.TxView (txViewTests) where

import           Cardano.Prelude

import           Hedgehog (Group (..), Property, checkSequential, (===))
import           Hedgehog.Extras.Test.Base (moduleWorkspace, propertyOnce)

import           Test.OptParse (execCardanoCLI, noteTempFile)

{- HLINT ignore "Use camelCase" -}

txViewTests :: IO Bool
txViewTests =
  checkSequential $
    Group "`transaction view` Goldens"
      [ ("golden_view_shelley", golden_view_shelley)
      , ("golden_view_allegra", golden_view_allegra)
      , ("golden_view_mary",    golden_view_mary)
      ]

golden_view_shelley :: Property
golden_view_shelley =
  propertyOnce $
  moduleWorkspace "tmp" $ \tempDir -> do
    transactionBodyFile <- noteTempFile tempDir "transaction-body-file"

    -- Create transaction body
    void $
      execCardanoCLI
        [ "transaction", "build-raw"
        , "--shelley-era"
        , "--tx-in"
        ,   "fe5dd07fb576bff960d6e066eade5b26cdb5afebe29f76ea58d0a098bce5d891\
            \#29"
        , "--tx-out"
        ,   "addr1v9wmu83pzajplrtpsq6tsqdgwr98x888trpmah2u0ezznsge7del3+31"
        , "--fee", "32"
        , "--invalid-hereafter", "33"
        , "--out-file", transactionBodyFile
        ]

    -- View transaction body
    result <-
      execCardanoCLI
        ["transaction", "view", "--tx-body-file", transactionBodyFile]

    lines (toS result) ===
      [ "auxiliary data: null"
      , "certificates: []"
      , "era: Shelley"
      , "fee: 32"
      , "inputs:"
      , "- fe5dd07fb576bff960d6e066eade5b26cdb5afebe29f76ea58d0a098bce5d891#29"
      , "metadata hash: null"
      , "outputs:"
      , "- address:"
      , "    Bech32: addr1v9wmu83pzajplrtpsq6tsqdgwr98x888trpmah2u0ezznsge7del3"
      , "    credential:"
      , "      key hash: 5dbe1e2117641f8d618034b801a870ca731ce758c3bedd5c7e4429c1"
      , "    network: Mainnet"
      , "    stake reference: StakeRefNull"
      , "  amount: 31"
      , "time to live: 33"
      , "update: null"
      , "withdrawals: []"
      , ""
      ]

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
        ,   "addr_test1qrefnr4k09pvge6dq83v6s67ruter8sftmky8qrmkqqsxy7q5psgn8tgqmupq4r79jmxlyk4eqt6z6hj5g8jd8393msqaw47f4\
            \+99"
        , "--fee", "100"
        , "--invalid-hereafter", "101"
        , "--out-file", transactionBodyFile
        ]

    -- View transaction body
    result <-
      execCardanoCLI
        ["transaction", "view", "--tx-body-file", transactionBodyFile]

    lines (toS result) ===
      [ "auxiliary data: null"
      , "auxiliary data hash: null"
      , "certificates: []"
      , "era: Allegra"
      , "fee: 100"
      , "inputs:"
      , "- fe5dd07fb576bff960d6e066eade5b26cdb5afebe29f76ea58d0a098bce5d891#94"
      , "outputs:"
      , "- address:"
      , "    Bech32: addr_test1qrefnr4k09pvge6dq83v6s67ruter8sftmky8qrmkqqsxy7q5psgn8tgqmupq4r79jmxlyk4eqt6z6hj5g8jd8393msqaw47f4"
      , "    credential:"
      , "      key hash: f2998eb67942c4674d01e2cd435e1f17919e095eec43807bb0010313"
      , "    network: Testnet"
      , "    stake reference: \
              \StakeRefBase (KeyHashObj (KeyHash \"c0a060899d6806f810547e2cb66f92d5c817a16af2a20f269e258ee0\"))"
      , "  amount: 99"
      , "update: null"
      , "validity interval:"
      , "  invalid before: null"
      , "  invalid hereafter: 101"
      , "withdrawals: []"
      , ""
      ]

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
        ,   "addr_test1qrefnr4k09pvge6dq83v6s67ruter8sftmky8qrmkqqsxy7q5psgn8tgqmupq4r79jmxlyk4eqt6z6hj5g8jd8393msqaw47f4\
            \+138"
        , "--fee", "139"
        , "--invalid-before", "140"
        -- , "--mint", "141" -- TODO(cblp, 2021-03-19, #2533) Transaction
        -- cannot mint ada, only non-ada assets
        , "--out-file", transactionBodyFile
        ]

    -- View transaction body
    result <-
      execCardanoCLI
        ["transaction", "view", "--tx-body-file", transactionBodyFile]

    lines (toS result) ===
      [ "auxiliary data: null"
      , "auxiliary data hash: null"
      , "certificates: []"
      , "era: Mary"
      , "fee: 139"
      , "inputs:"
      , "- fe5dd07fb576bff960d6e066eade5b26cdb5afebe29f76ea58d0a098bce5d891#135"
      , "mint:"
      , "  lovelace: 0"
      , "  policies: {}"
      , "outputs:"
      , "- address:"
      , "    Bech32: addr_test1qrefnr4k09pvge6dq83v6s67ruter8sftmky8qrmkqqsxy7q5psgn8tgqmupq4r79jmxlyk4eqt6z6hj5g8jd8393msqaw47f4"
      , "    credential:"
      , "      key hash: f2998eb67942c4674d01e2cd435e1f17919e095eec43807bb0010313"
      , "    network: Testnet"
      , "    stake reference: \
              \StakeRefBase (KeyHashObj (KeyHash \"c0a060899d6806f810547e2cb66f92d5c817a16af2a20f269e258ee0\"))"
      , "  amount:"
      , "    lovelace: 138"
      , "    policies: {}"
      , "update: null"
      , "validity interval:"
      , "  invalid before: 140"
      , "  invalid hereafter: null"
      , "withdrawals: []"
      , ""
      ]
