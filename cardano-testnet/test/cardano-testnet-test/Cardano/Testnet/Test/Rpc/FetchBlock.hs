{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Rpc.FetchBlock
  ( hprop_rpc_fetch_block
  )
where

import           Cardano.Api
import qualified Cardano.Api.Experimental as Exp
import qualified Cardano.Api.Experimental.AnyScriptWitness as Exp
import qualified Cardano.Api.Experimental.Tx as Exp
import qualified Cardano.Api.Ledger as L

import           Cardano.CLI.Type.Output (QueryTipLocalStateOutput (..))
import qualified Cardano.Ledger.Shelley.Scripts as Shelley
import qualified Cardano.Rpc.Client as Rpc
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Query as Query
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Submit as Submit
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Sync as U5c
import           Cardano.Rpc.Server.Internal.UtxoRpc.Predicate (exactAddressPredicate)
import           Cardano.Rpc.Server.Internal.UtxoRpc.Type (txoRefUtxoRpcToTxIn,
                   utxoRpcBigIntToInteger, utxoRpcPParamsToProtocolParams)
import           Cardano.Testnet

import           Prelude

import           Control.Exception (try)
import           Control.Monad ((<=<))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import           Data.Default.Class
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Data.Maybe (isJust)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           Data.Word (Word64)
import           GHC.Exts (fromList)
import           GHC.Stack (callStack)
import           Lens.Micro
import           Network.GRPC.Spec (GrpcError (..), GrpcException (..))

import           Testnet.Process.Run
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Start.Types
import           Testnet.Types

import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- | Run with:
-- @TASTY_PATTERN='/RPC FetchBlock/' cabal test cardano-testnet-test@
hprop_rpc_fetch_block :: H.Property
hprop_rpc_fetch_block = integrationRetryWorkspace 2 "rpc-fetch-block" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  conf@Conf{tempAbsPath} <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath

  let era = Exp.ConwayEra
      sbe = convert era
      eraName = eraToString sbe
      creationOptions = def{creationEra = AnyShelleyBasedEra sbe}
      runtimeOptions = def{runtimeEnableRpc = RpcEnabled}

  tr@TestnetRuntime
    { configurationFile
    , testnetMagic
    , testnetNodes = node0@TestnetNode{nodeSprocket} :| _
    , wallets = wallet0@(PaymentKeyInfo _ addressText0) : PaymentKeyInfo _ addressText1 : _
    } <-
    createAndRunTestnet creationOptions runtimeOptions conf

  execConfig <- mkExecConfig tempAbsPath' nodeSprocket testnetMagic
  rpcSocket <- H.note . unFile $ nodeRpcSocketPath node0
  let rpcServer = Rpc.ServerUnix rpcSocket
      fee = 500
      amount = 200_000_000
      validityUpperBound = 100_000_000

  do
    H.note_ "Fetch the tip block and verify its header and timestamp"

    -- Get chain tip via CLI
    QueryTipLocalStateOutput{localStateChainTip} <-
      H.noteShowM $ execCliStdoutToJson execConfig [eraName, "query", "tip"]
    (slot, tipHash, tipBlockNumber) <- case localStateChainTip of
      ChainTipAtGenesis -> H.failure
      ChainTip (SlotNo tipSlot) (HeaderHash hash) (BlockNo bn) -> pure (tipSlot, SBS.fromShort hash, bn)

    H.note_ $ "Tip slot: " <> show slot
    H.note_ $ "Tip block number: " <> show tipBlockNumber
    H.note_ $ "Tip hash: " <> show (BS.length tipHash) <> " bytes"

    -- Call FetchBlock via gRPC
    let blockRef = def & U5c.slot .~ slot & U5c.hash .~ tipHash
        request = def & U5c.ref .~ blockRef

    response <- H.evalIO . Rpc.withConnection def rpcServer $ \conn ->
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf U5c.SyncService "fetchBlock")) request

    let block = response ^. U5c.block

    -- Verify nativeBytes is non-empty
    let rawBytes = block ^. U5c.nativeBytes
    H.note_ $ "Block CBOR: " <> show (BS.length rawBytes) <> " bytes"
    H.assertWith rawBytes $ not . BS.null

    -- Verify cardano block header matches the requested tip
    block ^. U5c.cardano . U5c.header . U5c.slot H.=== slot
    block ^. U5c.cardano . U5c.header . U5c.hash H.=== tipHash

    -- height is the block number from ChainDB
    block ^. U5c.cardano . U5c.header . U5c.height H.=== tipBlockNumber

    -- Verify timestamp matches the slot time derived from EraHistory
    connectionInfo <- nodeConnectionInfo tr 0
    (systemStart, eraHistory) <-
      (H.leftFail <=< H.leftFailM) . H.evalIO $
        executeLocalStateQueryExpr connectionInfo VolatileTip $ do
          ss <- querySystemStart
          eh <- queryEraHistory
          pure $ (,) <$> ss <*> eh
    expectedTimestampMs :: Word64 <- H.leftFail $ do
      utcTime <- slotToUTCTime systemStart eraHistory (SlotNo slot)
      pure . round $ utcTimeToPOSIXSeconds utcTime * 1000
    H.assertWithinTolerance (block ^. U5c.cardano . U5c.timestamp) expectedTimestampMs 1000

    let fetchBlockExpectingError expectedError ref = do
          result <-
            H.evalIO . try . Rpc.withConnection def rpcServer $ \conn ->
              Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf U5c.SyncService "fetchBlock")) $
                def & U5c.ref .~ ref
          case result of
            Left GrpcException{grpcError}
              | grpcError == expectedError -> pure ()
              | otherwise -> do
                  H.note_ $ "expected " <> show expectedError <> ", got: " <> show grpcError
                  H.failure
            Right (_ :: Rpc.Proto U5c.FetchBlockResponse) -> do
              H.note_ $ "expected " <> show expectedError <> ", but the call succeeded"
              H.failure

    H.note_ "FetchBlock with an unknown block hash fails with NOT_FOUND"
    fetchBlockExpectingError GrpcNotFound $
      def & U5c.slot .~ slot & U5c.hash .~ BS.replicate 32 0xab

    H.note_ "FetchBlock with an invalid hash length fails with INVALID_ARGUMENT"
    fetchBlockExpectingError GrpcInvalidArgument $
      def & U5c.slot .~ slot & U5c.hash .~ "abc"

    H.note_ "ReadTip returns the current tip"
    readTipResponse <- H.evalIO . Rpc.withConnection def rpcServer $ \conn ->
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf U5c.SyncService "readTip")) def
    -- the chain may have advanced since the CLI tip query above, so slot and
    -- height are only bounded from below
    let tipRef = readTipResponse ^. U5c.tip
    H.assertWith (tipRef ^. U5c.slot) (>= slot)
    H.assertWith (tipRef ^. U5c.height) (>= tipBlockNumber)
    H.assertWith (tipRef ^. U5c.hash) ((== 32) . BS.length)

    -- the timestamp must agree with the slot time of the returned tip slot
    expectedTipTimestampMs :: Word64 <- H.leftFail $ do
      utcTime <- slotToUTCTime systemStart eraHistory (SlotNo (tipRef ^. U5c.slot))
      pure . round $ utcTimeToPOSIXSeconds utcTime * 1000
    H.assertWithinTolerance (tipRef ^. U5c.timestamp) expectedTipTimestampMs 1000

  (txId', txIn0, change, address0, address1, vkeyBytes0) <- do
    H.note_ "Build and submit a payment transaction via RPC"

    address0 <- H.nothingFail $ deserialiseAddress (asAddressInEra sbe) addressText0
    address1 <- H.nothingFail $ deserialiseAddress (asAddressInEra sbe) addressText1

    wit0 :: ShelleyWitnessSigningKey <-
      H.leftFailM . H.evalIO $
        readFileTextEnvelopeAnyOf
          [FromSomeType asType WitnessGenesisUTxOKey]
          (signingKey $ paymentKeyInfoPair wallet0)

    -- raw Ed25519 bytes of wallet0's verification key, as reported by the
    -- server in the witness set
    vkeyBytes0 <- case wit0 of
      WitnessGenesisUTxOKey signingKey0 ->
        pure . serialiseToRawBytes $ getVerificationKey signingKey0
      _ -> H.failure

    (pparamsResponse, searchResponse) <- H.evalIO . Rpc.withConnection def rpcServer $ \conn -> do
      pparams' <-
        Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf Query.QueryService "readParams")) def
      search' <-
        Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf Query.QueryService "searchUtxos")) $
          def & Query.predicate .~ exactAddressPredicate address0
      pure (pparams', search')

    pparams <- H.leftFail $ utxoRpcPParamsToProtocolParams era $ pparamsResponse ^. Query.values . Query.cardano
    txOut0 : _ <- H.noteShow $ searchResponse ^. Query.items
    txIn0 <- H.leftFail . txoRefUtxoRpcToTxIn $ txOut0 ^. Query.txoRef
    outputCoin <- H.leftFail $ txOut0 ^. Query.cardano . Query.coin . to utxoRpcBigIntToInteger

    let change = outputCoin - amount - fee
        mkOut ledgerAddress coin =
          Exp.obtainCommonConstraints era $
            Exp.TxOut $
              L.mkBasicTxOut ledgerAddress $
                L.inject $
                  L.Coin coin
        content =
          Exp.defaultTxBodyContent
            & Exp.setTxIns [(txIn0, Exp.AnyKeyWitnessPlaceholder)]
            & Exp.setTxFee (L.Coin fee)
            & Exp.setTxOuts [mkOut (toShelleyAddr address1) amount, mkOut (toShelleyAddr address0) change]
            & Exp.setTxValidityUpperBound (SlotNo validityUpperBound)
            & Exp.setTxProtocolParams pparams

    unsignedTx <- H.leftFail $ Exp.makeUnsignedTx era content
    let keyWit = Exp.makeKeyWitness era unsignedTx wit0
        Exp.SignedTx signedLedgerTx = Exp.signTx era [] [keyWit] unsignedTx
    txId' <- H.noteShow . Exp.obtainCommonConstraints era . TxId $ Exp.hashTxBody (signedLedgerTx ^. L.bodyTxL)

    submitResponse <- H.noteShowM . H.evalIO . Rpc.withConnection def rpcServer $ \conn ->
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf Submit.SubmitService "submitTx")) $
        def & Submit.tx .~ (def & Submit.raw .~ serialiseToRawBytes (Exp.SignedTx signedLedgerTx))
    submittedTxId <- H.leftFail . deserialiseFromRawBytes AsTxId $ submitResponse ^. Submit.ref
    txId' H.=== submittedTxId
    pure (txId', txIn0, change, address0, address1, vkeyBytes0)

  (txBlockSlot, txBlockHash, txBlockTxCount) <- do
    H.note_ "Follow the chain until the block containing the submitted transaction appears"

    mTxBlock <-
      H.timeout 60_000_000 . runExceptT $
        foldBlocks configurationFile (nodeSocketPath node0) QuickValidation Nothing $
          \_env _ledgerState _events blockInMode acc -> do
            let BlockHeader (SlotNo foundSlot) (HeaderHash foundHash) _ = getBlockInModeHeader blockInMode
                txIds = blockTxIds blockInMode
            pure $
              if txId' `elem` txIds
                then (Just (foundSlot, SBS.fromShort foundHash, length txIds), StopFold)
                else (acc, ContinueFold)

    H.nothingFail mTxBlock >>= \case
      Left e -> H.failMessage callStack $ "foldBlocks failed with: " <> displayError e
      Right Nothing -> H.failMessage callStack "block containing the submitted transaction not found"
      Right (Just found) -> pure found

  do
    H.note_ "Fetch the block containing the submitted transaction and verify its transactions"

    let txBlockRef = def & U5c.slot .~ txBlockSlot & U5c.hash .~ txBlockHash
        txBlockRequest = def & U5c.ref .~ txBlockRef
    txBlockResponse <- H.evalIO . Rpc.withConnection def rpcServer $ \conn ->
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf U5c.SyncService "fetchBlock")) txBlockRequest

    let fetchedTxs = txBlockResponse ^. U5c.block . U5c.cardano . U5c.body . U5c.tx
    H.note_ "Ensure the fetched block contains all transactions of the block"
    length fetchedTxs H.=== txBlockTxCount

    H.note_ "Ensure the fetched block contains the submitted transaction"
    protoTx : _ <- H.noteShow $ filter (\t -> t ^. U5c.hash == serialiseToRawBytes txId') fetchedTxs
    feeCoin <- H.leftFail $ protoTx ^. U5c.fee . to utxoRpcBigIntToInteger
    feeCoin H.=== fee
    H.assertWith protoTx (^. U5c.successful)

    H.note_ "Verify the transaction inputs"
    let TxIn inputTxId (TxIx inputIx) = txIn0
    map (\i -> (i ^. U5c.txHash, i ^. U5c.outputIndex)) (protoTx ^. U5c.inputs)
      H.=== [(serialiseToRawBytes inputTxId, fromIntegral inputIx)]

    H.note_ "Verify the transaction outputs"
    map (\o -> (o ^. U5c.address, o ^. U5c.coin)) (protoTx ^. U5c.outputs)
      H.=== [ (serialiseToRawBytes address1, inject amount)
            , (serialiseToRawBytes address0, inject change)
            ]

    H.note_ "The transaction has no reference inputs and no certificates"
    protoTx ^. U5c.referenceInputs H.=== []
    protoTx ^. U5c.certificates H.=== []

    H.note_ "The transaction has no auxiliary data and no proposals"
    protoTx ^. U5c.maybe'auxiliary H.=== Nothing
    protoTx ^. U5c.proposals H.=== []

    H.note_ "Verify the transaction validity interval"
    protoTx ^. U5c.validity . U5c.start H.=== 0
    protoTx ^. U5c.validity . U5c.ttl H.=== validityUpperBound

    H.note_ "The transaction mints nothing and has no withdrawals or collateral"
    protoTx ^. U5c.mint H.=== []
    protoTx ^. U5c.withdrawals H.=== []
    protoTx ^. U5c.maybe'collateral H.=== Nothing

    H.note_ "Verify the witness set contains only the wallet key witness"
    let witnessSet = protoTx ^. U5c.witnesses
    [vkeyWitness] <- H.noteShow $ witnessSet ^. U5c.vkeywitness
    vkeyWitness ^. U5c.vkey H.=== vkeyBytes0
    BS.length (vkeyWitness ^. U5c.signature) H.=== 64
    witnessSet ^. U5c.script H.=== []
    witnessSet ^. U5c.bootstrapWitnesses H.=== []
    witnessSet ^. U5c.plutusDatums H.=== []
    witnessSet ^. U5c.redeemers H.=== []

  -- An "anyone can mint" policy: a native script requiring an empty set of conditions
  let mintScript :: Exp.SimpleScript (Exp.LedgerEra Exp.ConwayEra)
      mintScript = Exp.SimpleScript $ Shelley.RequireAllOf mempty
      mintPolicyId = PolicyId . fromShelleyScriptHash $ Exp.hashSimpleScript @Exp.ConwayEra mintScript
      mintAssetName = UnsafeAssetName "RpcTestToken"
      mintQuantity = 1000

  mintTxId <- do
    H.note_ "Build and submit a transaction minting a native-script token via RPC"

    -- Spend the change output of the previous transaction, which sits at index 1
    let changeTxIn = TxIn txId' (TxIx 1)

    wit0 :: ShelleyWitnessSigningKey <-
      H.leftFailM . H.evalIO $
        readFileTextEnvelopeAnyOf
          [FromSomeType asType WitnessGenesisUTxOKey]
          (signingKey $ paymentKeyInfoPair wallet0)

    -- The minted assets have to appear in an output to conserve value.
    -- No protocol parameters are needed: a transaction witnessed only by
    -- native scripts has no script integrity hash.
    let mintWitness = Exp.AnyScriptWitnessSimple $ Exp.SScript mintScript
        mintValue =
          Exp.TxMintValue $
            fromList [(mintPolicyId, (fromList [(mintAssetName, Quantity mintQuantity)], mintWitness))]
        mintTxOut =
          Exp.obtainCommonConstraints era $
            Exp.TxOut $
              L.mkBasicTxOut (toShelleyAddr address0) $
                toMaryValue $
                  fromList
                    [ (AdaAssetId, Quantity (change - fee))
                    , (AssetId mintPolicyId mintAssetName, Quantity mintQuantity)
                    ]
        mintContent =
          Exp.defaultTxBodyContent
            & Exp.setTxIns [(changeTxIn, Exp.AnyKeyWitnessPlaceholder)]
            & Exp.setTxFee (L.Coin fee)
            & Exp.setTxOuts [mintTxOut]
            & Exp.setTxMintValue mintValue
            & Exp.setTxValidityUpperBound (SlotNo validityUpperBound)

    mintUnsignedTx <- H.leftFail $ Exp.makeUnsignedTx era mintContent
    let mintKeyWit = Exp.makeKeyWitness era mintUnsignedTx wit0
        Exp.SignedTx mintSignedLedgerTx = Exp.signTx era [] [mintKeyWit] mintUnsignedTx
    mintTxId <- H.noteShow . Exp.obtainCommonConstraints era . TxId $ Exp.hashTxBody (mintSignedLedgerTx ^. L.bodyTxL)

    mintSubmitResponse <- H.noteShowM . H.evalIO . Rpc.withConnection def rpcServer $ \conn ->
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf Submit.SubmitService "submitTx")) $
        def & Submit.tx .~ (def & Submit.raw .~ serialiseToRawBytes (Exp.SignedTx mintSignedLedgerTx))
    mintSubmittedTxId <- H.leftFail . deserialiseFromRawBytes AsTxId $ mintSubmitResponse ^. Submit.ref
    mintTxId H.=== mintSubmittedTxId
    pure mintTxId

  (mintBlockSlot, mintBlockHash, mintBlockTxCount) <- do
    H.note_ "Follow the chain until the block containing the minting transaction appears"

    mMintBlock <-
      H.timeout 60_000_000 . runExceptT $
        foldBlocks configurationFile (nodeSocketPath node0) QuickValidation Nothing $
          \_env _ledgerState _events blockInMode acc -> do
            let BlockHeader (SlotNo foundSlot) (HeaderHash foundHash) _ = getBlockInModeHeader blockInMode
                txIds = blockTxIds blockInMode
            pure $
              if mintTxId `elem` txIds
                then (Just (foundSlot, SBS.fromShort foundHash, length txIds), StopFold)
                else (acc, ContinueFold)

    H.nothingFail mMintBlock >>= \case
      Left e -> H.failMessage callStack $ "foldBlocks failed with: " <> displayError e
      Right Nothing -> H.failMessage callStack "block containing the minting transaction not found"
      Right (Just found) -> pure found

  do
    H.note_ "Fetch the block containing the minting transaction and verify the minted assets"

    let mintBlockRef = def & U5c.slot .~ mintBlockSlot & U5c.hash .~ mintBlockHash
        mintBlockRequest = def & U5c.ref .~ mintBlockRef
    mintBlockResponse <- H.evalIO . Rpc.withConnection def rpcServer $ \conn ->
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf U5c.SyncService "fetchBlock")) mintBlockRequest

    let mintFetchedTxs = mintBlockResponse ^. U5c.block . U5c.cardano . U5c.body . U5c.tx
    H.note_ "Ensure the fetched block contains all transactions of the block"
    length mintFetchedTxs H.=== mintBlockTxCount

    H.note_ "Ensure the fetched block contains the minting transaction"
    mintProtoTx : _ <- H.noteShow $ filter (\t -> t ^. U5c.hash == serialiseToRawBytes mintTxId) mintFetchedTxs
    mintFeeCoin <- H.leftFail $ mintProtoTx ^. U5c.fee . to utxoRpcBigIntToInteger
    mintFeeCoin H.=== fee
    H.assertWith mintProtoTx (^. U5c.successful)

    let assetsOf :: Rpc.Proto U5c.Multiasset -> [(BS.ByteString, Rpc.Proto U5c.BigInt)]
        assetsOf multiasset =
          map (\a -> (a ^. U5c.name, a ^. U5c.quantity)) (multiasset ^. U5c.assets)

    H.note_ "Verify the minted assets"
    [mintedPolicy] <- H.noteShow $ mintProtoTx ^. U5c.mint
    mintedPolicy ^. U5c.policyId H.=== serialiseToRawBytes mintPolicyId
    assetsOf mintedPolicy H.=== [(serialiseToRawBytes mintAssetName, inject mintQuantity)]

    H.note_ "Verify the output carries the minted asset"
    [mintOutput] <- H.noteShow $ mintProtoTx ^. U5c.outputs
    mintOutput ^. U5c.address H.=== serialiseToRawBytes address0
    mintOutput ^. U5c.coin H.=== inject (change - fee)
    map (\ma -> (ma ^. U5c.policyId, assetsOf ma)) (mintOutput ^. U5c.assets)
      H.=== [(serialiseToRawBytes mintPolicyId, [(serialiseToRawBytes mintAssetName, inject mintQuantity)])]

    H.note_ "Verify the witness set contains the native mint script and the wallet key witness"
    let mintWitnessSet = mintProtoTx ^. U5c.witnesses
    [mintVkeyWitness] <- H.noteShow $ mintWitnessSet ^. U5c.vkeywitness
    mintVkeyWitness ^. U5c.vkey H.=== vkeyBytes0
    [mintScriptWitness] <- H.noteShow $ mintWitnessSet ^. U5c.script
    H.assertWith mintScriptWitness $ isJust . (^. U5c.maybe'native)

asAddressInEra :: ShelleyBasedEra era -> AsType (AddressInEra era)
asAddressInEra s = shelleyBasedEraConstraints s $ AsAddressInEra asType

getBlockInModeHeader :: BlockInMode -> BlockHeader
getBlockInModeHeader (BlockInMode _ block) = getBlockHeader block

-- | Transaction ids of all transactions in a block.
blockTxIds :: BlockInMode -> [TxId]
blockTxIds (BlockInMode era block) =
  forEraInEon era [] $ \sbe ->
    shelleyBasedEraConstraints
      sbe
      [ TxId $ Exp.hashTxBody (ledgerTx ^. L.bodyTxL)
      | ShelleyTx _ ledgerTx <- getBlockTxs block
      ]
