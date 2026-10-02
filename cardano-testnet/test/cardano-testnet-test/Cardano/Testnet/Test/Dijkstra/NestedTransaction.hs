{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Dijkstra.NestedTransaction
  ( hprop_nested_transaction_swap
  ) where

import           Cardano.Api

import           Cardano.Testnet
import           Cardano.Testnet.Test.Node.DijkstraEra (hardForkToDijkstra, waitForBlocksCli)

import           Prelude

import           Control.Monad
import           Control.Monad.Catch (MonadCatch)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Default.Class (def)
import           Data.List (find, sortOn)
import qualified Data.List.NonEmpty as NEL
import           Data.Ord (Down (..))
import qualified Data.Text as Text
import           GHC.Stack
import           System.Exit (ExitCode (..))
import           System.FilePath ((</>))

import           Testnet.Components.Query
import           Testnet.Defaults (simpleScript)
import           Testnet.Process.Run (execCli', execCliAny, mkExecConfig)
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Start.Types
import           Testnet.Types

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H
import           Hedgehog.Internal.Property (MonadTest)

-- | A multi-asset swap settled with a Dijkstra nested transaction.
--
-- Setup (Conway): the batcher wallet mints 100 @TokenA@ under a @sig@ simple script
-- and pays them to Alice, and pays Bob a 60 ADA UTxO. The cluster is then hard forked
-- into Dijkstra through governance.
--
-- Swap (Dijkstra): Alice offers her 100 @TokenA@ for 50 ADA, i.e. 0.5 ADA per token.
--
-- * Alice's sub-transaction spends her token UTxO and pays herself 53 ADA (the 50 ADA
--   price plus the 3 ADA that accompanied the tokens). It is signed with Alice's key
--   only, which spending her input already requires; no guards are needed.
-- * Bob's sub-transaction spends his 60 ADA UTxO and pays himself the 100 @TokenA@
--   (with 3 ADA) plus 7 ADA change, so he pays exactly 50 ADA. It is signed with Bob's
--   key only.
-- * The batcher's top-level transaction embeds both signed sub-transactions, adds its
--   own input to pay the fee, and is signed by the batcher only.
--
-- Neither sub-transaction balances on its own; the ledger only accepts the top-level
-- transaction because value is conserved across the batch. So if the transaction is
-- accepted, the exchange rate was honoured. The test then checks the resulting UTxO
-- set: Bob holds the 100 @TokenA@, Alice holds 50 ADA more, Bob 50 ADA less, and the
-- batcher paid the fee.
--
-- Requires a @cardano-cli@ with the @dijkstra transaction sub-transaction@ commands
-- (IntersectMBO/cardano-cli#1453); point @CARDANO_CLI@ at it.
--
-- Known failure points, deepest first:
--
-- * ouroboros-consensus 4.2.1.0 never leaves Conway, so the node is still in Conway at
--   PV12 and rejects the Dijkstra-era top-level transaction at submission.
-- * With a consensus that does enter Dijkstra, UTxO-HD key fetching (`allInputsTxBodyF`)
--   does not load sub-transaction inputs and the ledger fails with @SubBadInputsUTxO@.
--
-- Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Nested transaction swap/"'@
hprop_nested_transaction_swap :: Property
hprop_nested_transaction_swap = integrationRetryWorkspace 2 "nested-transaction-swap" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  conf@Conf { tempAbsPath } <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath

  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  let creationOptions = def
        { creationEra = AnyShelleyBasedEra ShelleyBasedEraConway
        , creationGenesisOptions = def
            { genesisEpochLength = 300
            , genesisActiveSlotsCoeff = 0.3
            }
        }

      -- The client-side ledger fold cannot process Dijkstra blocks with cardano-api 11.7,
      -- so keep the epoch state logger off; the epoch state view is stopped by
      -- 'hardForkToDijkstra' before the fork.
      runtimeOptions = def { runtimeEnableNewEpochStateLogging = False }

  TestnetRuntime
    { testnetMagic
    , testnetNodes
    , wallets = batcher:alice:bob:_
    , configurationFile
    } <- createAndRunTestnet creationOptions runtimeOptions conf

  let node@TestnetNode{nodeSprocket} = NEL.head testnetNodes
      socketPath = nodeSocketPath node
  execConfig <- mkExecConfig tempBaseAbsPath nodeSprocket testnetMagic
  epochStateView <- getEpochStateView configurationFile socketPath

  let batcherAddr = Text.unpack $ paymentKeyInfoAddr batcher
      aliceAddr = Text.unpack $ paymentKeyInfoAddr alice
      bobAddr = Text.unpack $ paymentKeyInfoAddr bob

  ------------------------------------------------------------------------------
  H.note_ "Conway: mint 100 TokenA to Alice and fund Bob with a 60 ADA UTxO"
  ------------------------------------------------------------------------------
  setupDir <- H.createDirectoryIfMissing $ work </> "setup"

  batcherKeyHash <- keyHash execConfig (paymentKeyInfoPair batcher)
  mintingScriptFp <- H.note $ setupDir </> "tokenA-policy.json"
  H.writeFile mintingScriptFp $ Text.unpack $ simpleScript $ Text.pack batcherKeyHash
  policyId <- filter (/= '\n') <$> execCli' execConfig
    [ "conway", "transaction", "policyid", "--script-file", mintingScriptFp ]
  let assetName = "546f6b656e41" -- "TokenA", lowercase hex as the CLI renders it in query output
      tokenA n = show @Integer n <> " " <> policyId <> "." <> assetName

  setupTxIn <- findLargestUtxoForPaymentKey epochStateView ShelleyBasedEraConway batcher
  setupBodyFp <- H.note $ setupDir </> "setup.txbody"
  setupTxFp <- H.note $ setupDir </> "setup.tx"
  void $ execCli' execConfig
    [ "conway", "transaction", "build"
    , "--change-address", batcherAddr
    , "--tx-in", Text.unpack $ renderTxIn setupTxIn
    , "--mint", tokenA 100
    , "--mint-script-file", mintingScriptFp
    , "--tx-out", aliceAddr <> "+" <> show @Integer tokensAda <> "+" <> tokenA 100
    , "--tx-out", bobAddr <> "+" <> show @Integer bobFunds
    , "--out-file", setupBodyFp
    ]
  void $ execCli' execConfig
    [ "conway", "transaction", "sign"
    , "--tx-body-file", setupBodyFp
    , "--signing-key-file", signingKeyFp $ paymentKeyInfoPair batcher
    , "--out-file", setupTxFp
    ]
  void $ execCli' execConfig [ "conway", "transaction", "submit", "--tx-file", setupTxFp ]
  _ <- waitForBlocks epochStateView 2

  aliceUtxosBefore <- queryUtxos execConfig policyId assetName aliceAddr
  H.assertWith aliceUtxosBefore $ any ((== 100) . ueTokens)

  ------------------------------------------------------------------------------
  H.note_ "Hard fork into Dijkstra through governance"
  ------------------------------------------------------------------------------
  hardForkToDijkstra execConfig epochStateView tempAbsPath work batcher alice

  -- Deliberately not asserted: with a consensus that does not hard fork out of Conway
  -- (ouroboros-consensus 4.2.1.0) the node stays in Conway at PV12 and the nested
  -- transaction below is then rejected at submission, which is the failure worth seeing.
  H.noteM_ $ execCli' execConfig [ "query", "tip", "--output-json" ]

  ------------------------------------------------------------------------------
  H.note_ "Dijkstra: settle the swap with a nested transaction"
  ------------------------------------------------------------------------------
  -- The sub-transaction commands only exist in cardano-cli with IntersectMBO/cardano-cli#1453.
  -- Fail with a pointer rather than an option-parser error if the CLI in use lacks them.
  subTxHelp <- execCliAny execConfig [ "dijkstra", "transaction", "sub-transaction", "--help" ]
  case subTxHelp of
    (ExitSuccess, _, _) -> pure ()
    _ -> H.failMessage callStack $ unlines
      [ "The cardano-cli in use has no `dijkstra transaction sub-transaction` commands."
      , "Point CARDANO_CLI at a build of IntersectMBO/cardano-cli#1453 (branch jordan/basic-sub-tx-integration)."
      ]
  swapDir <- H.createDirectoryIfMissing $ work </> "swap"

  aliceBefore <- queryUtxos execConfig policyId assetName aliceAddr
  bobBefore <- queryUtxos execConfig policyId assetName bobAddr
  batcherBefore <- queryUtxos execConfig policyId assetName batcherAddr

  aliceTokenUtxo <- H.nothingFail $ find ((== 100) . ueTokens) aliceBefore
  bobAdaUtxo <- H.nothingFail $ find (\u -> ueLovelace u == bobFunds && ueTokens u == 0) bobBefore
  batcherUtxo <- H.nothingFail $ listToMaybeHead $ sortOn (Down . ueLovelace) batcherBefore

  H.note_ "Alice's sub-transaction: 100 TokenA out, 50 ADA in (0.5 ADA per token)"
  aliceSubUnsigned <- H.note $ swapDir </> "alice.sub.unsigned"
  aliceSubSigned <- H.note $ swapDir </> "alice.sub.signed"
  void $ execCli' execConfig
    [ "dijkstra", "transaction", "sub-transaction", "build-raw"
    , "--tx-in", ueTxIn aliceTokenUtxo
    , "--tx-out", aliceAddr <> "+" <> show @Integer (tokensAda + price)
    , "--out-file", aliceSubUnsigned
    ]
  void $ execCli' execConfig
    [ "dijkstra", "transaction", "sub-transaction", "sign"
    , "--sub-tx-file", aliceSubUnsigned
    , "--signing-key-file", signingKeyFp $ paymentKeyInfoPair alice
    , "--out-file", aliceSubSigned
    ]

  H.note_ "Bob's sub-transaction: 50 ADA out, 100 TokenA in"
  bobSubUnsigned <- H.note $ swapDir </> "bob.sub.unsigned"
  bobSubSigned <- H.note $ swapDir </> "bob.sub.signed"
  void $ execCli' execConfig
    [ "dijkstra", "transaction", "sub-transaction", "build-raw"
    , "--tx-in", ueTxIn bobAdaUtxo
    , "--tx-out", bobAddr <> "+" <> show @Integer tokensAda <> "+" <> tokenA 100
    , "--tx-out", bobAddr <> "+" <> show @Integer (bobFunds - price - tokensAda)
    , "--out-file", bobSubUnsigned
    ]
  void $ execCli' execConfig
    [ "dijkstra", "transaction", "sub-transaction", "sign"
    , "--sub-tx-file", bobSubUnsigned
    , "--signing-key-file", signingKeyFp $ paymentKeyInfoPair bob
    , "--out-file", bobSubSigned
    ]

  H.note_ "Batcher's top-level transaction embedding both sub-transactions and paying the fee"
  topBodyFp <- H.note $ swapDir </> "swap.txbody"
  topTxFp <- H.note $ swapDir </> "swap.tx"
  void $ execCli' execConfig
    [ "dijkstra", "transaction", "build-raw"
    , "--tx-in", ueTxIn batcherUtxo
    , "--tx-out", batcherAddr <> "+" <> show @Integer (ueLovelace batcherUtxo - fee)
    , "--fee", show @Integer fee
    , "--sub-transaction", aliceSubSigned
    , "--sub-transaction", bobSubSigned
    , "--out-file", topBodyFp
    ]
  void $ execCli' execConfig
    [ "dijkstra", "transaction", "sign"
    , "--tx-body-file", topBodyFp
    , "--signing-key-file", signingKeyFp $ paymentKeyInfoPair batcher
    , "--testnet-magic", show testnetMagic
    , "--out-file", topTxFp
    ]
  H.noteM_ $ execCli' execConfig [ "dijkstra", "transaction", "submit", "--tx-file", topTxFp ]
  waitForBlocksCli execConfig 2

  ------------------------------------------------------------------------------
  H.note_ "Proof: Bob holds the 100 TokenA and 50 ADA moved from Bob to Alice"
  ------------------------------------------------------------------------------
  aliceAfter <- queryUtxos execConfig policyId assetName aliceAddr
  bobAfter <- queryUtxos execConfig policyId assetName bobAddr
  batcherAfter <- queryUtxos execConfig policyId assetName batcherAddr

  H.note_ "Bob received all the TokenA"
  totalTokens bobAfter H.=== 100
  totalTokens aliceAfter H.=== 0

  H.note_ "Alice was paid the asking price and Bob paid it"
  totalLovelace aliceAfter H.=== totalLovelace aliceBefore + price
  totalLovelace bobAfter H.=== totalLovelace bobBefore - price

  H.note_ "the batcher paid only the fee"
  totalLovelace batcherAfter H.=== totalLovelace batcherBefore - fee
 where
  bobFunds, tokensAda, price, fee :: Integer
  bobFunds = 60_000_000  -- Bob's UTxO to pay from
  tokensAda = 3_000_000  -- ADA travelling with the tokens (covers the min-UTxO value)
  price = 50_000_000     -- 100 TokenA at 0.5 ADA each
  fee = 1_000_000        -- paid by the batcher; comfortably above the minimum fee

  listToMaybeHead :: [a] -> Maybe a
  listToMaybeHead (x:_) = Just x
  listToMaybeHead [] = Nothing

-- | One entry of @cardano-cli query utxo --output-json@, reduced to what the swap cares about.
data UtxoEntry = UtxoEntry
  { ueTxIn :: String
  , ueLovelace :: Integer
  , ueTokens :: Integer -- ^ quantity of the swap's token in this UTxO
  } deriving Show

totalLovelace, totalTokens :: [UtxoEntry] -> Integer
totalLovelace = sum . map ueLovelace
totalTokens = sum . map ueTokens

-- | Query the UTxOs at an address and extract lovelace and the given asset's quantity.
queryUtxos
  :: (HasCallStack, MonadTest m, MonadIO m, MonadCatch m)
  => H.ExecConfig
  -> String -- ^ Policy id of the asset to track
  -> String -- ^ Hex-encoded asset name
  -> String -- ^ Address
  -> m [UtxoEntry]
queryUtxos execConfig policyId assetName addr = withFrozenCallStack $ do
  out <- execCli' execConfig [ "query", "utxo", "--address", addr, "--output-json" ]
  utxoJson <- H.leftFail $ A.eitherDecode $ B.pack out
  H.leftFail $ A.parseEither (utxoEntries policyId assetName) utxoJson

utxoEntries :: String -> String -> A.Value -> A.Parser [UtxoEntry]
utxoEntries policyId assetName = A.withObject "utxo set" $ \o ->
  forM (KM.toList o) $ \(txIn, entry) ->
    flip (A.withObject "utxo entry") entry $ \e -> do
      value <- e A..: "value"
      lovelace <- value A..: "lovelace"
      mPolicy <- value A..:? K.fromString policyId
      tokens <- case mPolicy of
        Nothing -> pure 0
        Just policy -> policy A..:? K.fromString assetName A..!= 0
      pure UtxoEntry { ueTxIn = K.toString txIn, ueLovelace = lovelace, ueTokens = tokens }

-- | Hash of a payment verification key, used in the @sig@ minting policy.
keyHash
  :: (HasCallStack, MonadTest m, MonadIO m, MonadCatch m)
  => H.ExecConfig
  -> KeyPair PaymentKey
  -> m String
keyHash execConfig keys = withFrozenCallStack $
  filter (/= '\n') <$> execCli' execConfig
    [ "address", "key-hash", "--payment-verification-key-file", verificationKeyFp keys ]
