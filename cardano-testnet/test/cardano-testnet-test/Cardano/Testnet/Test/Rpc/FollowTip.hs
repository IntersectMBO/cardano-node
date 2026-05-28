{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Rpc.FollowTip
  ( hprop_rpc_follow_tip
  )
where

import           Cardano.Api
import qualified Cardano.Api.Experimental as Exp
import qualified Cardano.Api.Experimental.Tx as Exp
import qualified Cardano.Api.Ledger as L

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
import           Control.Monad (replicateM, void)
import qualified Data.ByteString as BS
import           Data.Default.Class
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           GHC.Stack (HasCallStack, callStack)
import           Lens.Micro
import           Network.GRPC.Spec (GrpcError (..), GrpcException (..), NextElem (..))

import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Types

import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- | Run with:
-- @TASTY_PATTERN='/RPC FollowTip/' cabal test cardano-testnet-test@
hprop_rpc_follow_tip :: H.Property
hprop_rpc_follow_tip = integrationRetryWorkspace 2 "rpc-follow-tip" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  conf <- mkConf tempAbsBasePath'

  let era = Exp.ConwayEra
      sbe = convert era
      creationOptions = def{creationEra = AnyShelleyBasedEra sbe}
      runtimeOptions = def{runtimeEnableRpc = RpcEnabled}

  TestnetRuntime
    { testnetNodes = node0 :| _
    , wallets = wallet0@(PaymentKeyInfo _ addressText0) : PaymentKeyInfo _ addressText1 : _
    } <-
    createAndRunTestnet creationOptions runtimeOptions conf

  rpcSocket <- H.note . unFile $ nodeRpcSocketPath node0
  let rpcServer = Rpc.ServerUnix rpcSocket

      -- Open a fresh FollowTip stream intersecting at the given references,
      -- read exactly the requested number of messages, then let the
      -- connection close: the stream is never consumed beyond what the
      -- caller asked for.
      followTipN :: Int -> [Rpc.Proto U5c.BlockRef] -> H.Integration [NextElem (Rpc.Proto U5c.FollowTipResponse)]
      followTipN messageCount intersectRefs =
        H.evalIO . Rpc.withConnection def rpcServer $ \conn ->
          Rpc.serverStreaming conn (Rpc.rpc @(Rpc.Protobuf U5c.SyncService "followTip")) (def & U5c.intersect .~ intersectRefs) $
            \recv -> replicateM messageCount recv

      -- Open a stream expecting it to fail before any message is delivered,
      -- mirroring FetchBlock's @fetchBlockExpectingError@ pattern.
      --
      -- This deliberately bypasses 'Rpc.serverStreaming': its @recv@ action
      -- is grapesy's 'Rpc.recvNextOutputElem', which reports both a clean
      -- end of stream and an error end of stream as the same 'NoNextElem'
      -- value, swallowing the gRPC status. 'Rpc.recvOutput' checks the
      -- trailers and raises 'GrpcException' when the stream ended with a
      -- non-OK status, so the low-level 'Rpc.withRPC' call is used instead.
      followTipExpectingError :: GrpcError -> [Rpc.Proto U5c.BlockRef] -> H.Integration ()
      followTipExpectingError expectedError intersectRefs = do
        result <-
          H.evalIO . try . Rpc.withConnection def rpcServer $ \conn ->
            Rpc.withRPC conn def (Proxy @(Rpc.Protobuf U5c.SyncService "followTip")) $ \call -> do
              Rpc.sendFinalInput call (def & U5c.intersect .~ intersectRefs)
              void $ Rpc.recvOutput call
        case result of
          Left GrpcException{grpcError}
            | grpcError == expectedError -> pure ()
            | otherwise -> do
                H.note_ $ "expected " <> show expectedError <> ", got: " <> show grpcError
                H.failure
          Right () -> do
            H.note_ $ "expected " <> show expectedError <> ", but the call succeeded"
            H.failure

      -- Tail a FollowTip stream from the given intersection point, scanning
      -- apply messages for one whose parsed body contains a transaction
      -- with the given hash. Bounded by 'maxApplyMessages' apply messages
      -- (resets don't count against the bound) so a missing transaction
      -- fails promptly with a clear cause instead of hanging until the
      -- watchdog kills the whole property.
      followTipUntilTx
        :: [Rpc.Proto U5c.BlockRef]
        -> BS.ByteString
        -> Int
        -> H.Integration (Maybe (Rpc.Proto U5c.Tx))
      followTipUntilTx intersectRefs txHash maxApplyMessages =
        H.evalIO . Rpc.withConnection def rpcServer $ \conn ->
          Rpc.serverStreaming conn (Rpc.rpc @(Rpc.Protobuf U5c.SyncService "followTip")) (def & U5c.intersect .~ intersectRefs) $ \recv ->
            let scanApplies remaining
                  | remaining <= 0 = pure Nothing
                  | otherwise = do
                      next <- recv
                      case next of
                        NoNextElem -> pure Nothing
                        NextElem message ->
                          case message ^. U5c.maybe'apply of
                            Nothing -> scanApplies remaining
                            Just block ->
                              case filter (\t -> t ^. U5c.hash == txHash) (block ^. U5c.cardano . U5c.body . U5c.tx) of
                                (tx : _) -> pure (Just tx)
                                [] -> scanApplies (remaining - 1)
             in scanApplies maxApplyMessages

      -- Origin is a BlockRef with an empty hash: clients append it as an
      -- infallible catch-all requesting full-history sync.
      originRef = def & U5c.slot .~ 0 & U5c.hash .~ BS.empty

  (seenSlot, seenHash, observedTipSlot) <- do
    H.note_ "Open a FollowTip stream from origin: the first message must be a reset to slot 0 with an empty hash"
    [resetElem, applyElem1, applyElem2, applyElem3] <- followTipN 4 [originRef]
    resetMsg <- nextElemFail resetElem
    apply1 <- nextElemFail applyElem1
    apply2 <- nextElemFail applyElem2
    apply3 <- nextElemFail applyElem3

    resetRef <- H.nothingFail (resetMsg ^. U5c.maybe'reset)
    resetRef ^. U5c.slot H.=== 0
    H.assertWith (resetRef ^. U5c.hash) BS.null

    tipAfterReset <- H.nothingFail (resetMsg ^. U5c.maybe'tip)
    H.assertWith (tipAfterReset ^. U5c.hash) $ (== 32) . BS.length

    H.note_ "Subsequent messages are apply actions carrying non-empty native bytes and a populated header"
    _block1 <- assertAppliedBlock apply1
    _block2 <- assertAppliedBlock apply2
    block3 <- assertAppliedBlock apply3

    let seenSlot = block3 ^. U5c.cardano . U5c.header . U5c.slot
        seenHash = block3 ^. U5c.cardano . U5c.header . U5c.hash
    tipAfterApply3 <- H.nothingFail (apply3 ^. U5c.maybe'tip)
    let observedTipSlot = tipAfterApply3 ^. U5c.slot

    H.note_ $
      "Remembering the block at slot "
        <> show seenSlot
        <> " for the resume test, and tip slot "
        <> show observedTipSlot
        <> " for the empty-intersect test"
    pure (seenSlot, seenHash, observedTipSlot)

  do
    H.note_ "Re-opening a stream intersecting at the previously seen block resumes exactly there, without replaying from origin"
    [resumeElem] <- followTipN 1 [def & U5c.slot .~ seenSlot & U5c.hash .~ seenHash]
    resumeMsg <- nextElemFail resumeElem
    resumeResetRef <- H.nothingFail (resumeMsg ^. U5c.maybe'reset)
    resumeResetRef ^. U5c.slot H.=== seenSlot
    resumeResetRef ^. U5c.hash H.=== seenHash

  do
    H.note_ "An empty intersect list follows from the current tip: the reset lands at or after the previously observed tip"
    [emptyIntersectElem] <- followTipN 1 []
    emptyIntersectMsg <- nextElemFail emptyIntersectElem
    emptyIntersectResetRef <- H.nothingFail (emptyIntersectMsg ^. U5c.maybe'reset)
    H.assertWith (emptyIntersectResetRef ^. U5c.slot) (>= observedTipSlot)

  H.note_ "An intersect list containing only an unknown block reference fails the stream with NOT_FOUND before any message"
  followTipExpectingError GrpcNotFound [def & U5c.slot .~ seenSlot & U5c.hash .~ BS.replicate 32 0xab]

  do
    H.note_ "Submit a transaction over gRPC and confirm it arrives via FollowTip as an apply action"

    preSubmissionTipResponse <-
      H.evalIO . Rpc.withConnection def rpcServer $ \conn ->
        Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf U5c.SyncService "readTip")) def
    preSubmissionTip <- H.nothingFail (preSubmissionTipResponse ^. U5c.maybe'tip)
    H.note_ $ "Pre-submission tip slot: " <> show (preSubmissionTip ^. U5c.slot)

    address0 <- H.nothingFail $ deserialiseAddress (asAddressInEra sbe) addressText0
    address1 <- H.nothingFail $ deserialiseAddress (asAddressInEra sbe) addressText1

    wit0 :: ShelleyWitnessSigningKey <-
      H.leftFailM . H.evalIO $
        readFileTextEnvelopeAnyOf
          [FromSomeType asType WitnessGenesisUTxOKey]
          (signingKey $ paymentKeyInfoPair wallet0)

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

    let fee = 500
        amount = 100_000_000
        validityUpperBound = 100_000_000
        change = outputCoin - amount - fee
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
    let txHash = serialiseToRawBytes txId'

    H.note_ "Submitting the transaction"
    submitResponse <- H.noteShowM . H.evalIO . Rpc.withConnection def rpcServer $ \conn ->
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf Submit.SubmitService "submitTx")) $
        def & Submit.tx .~ (def & Submit.raw .~ serialiseToRawBytes (Exp.SignedTx signedLedgerTx))
    submittedTxId <- H.leftFail . deserialiseFromRawBytes AsTxId $ submitResponse ^. Submit.ref
    txId' H.=== submittedTxId

    H.note_ "Tailing FollowTip from the pre-submission tip until the submitted transaction appears in an apply message"
    foundTx <- followTipUntilTx [preSubmissionTip] txHash 30
    tx <- case foundTx of
      Just found -> pure found
      Nothing ->
        H.failMessage callStack "submitted transaction did not appear in a FollowTip apply message within 30 apply messages"

    H.note_ "The found transaction's hash, fee and parsed inputs/outputs match what was submitted"
    tx ^. U5c.hash H.=== txHash
    txFee <- H.leftFail $ tx ^. U5c.fee . to utxoRpcBigIntToInteger
    txFee H.=== fee
    H.assertWith (tx ^. U5c.inputs) $ not . null
    H.assertWith (tx ^. U5c.outputs) $ not . null

-- | Unwrap a streamed element, failing with a descriptive message if the
-- stream ended before the expected message arrived.
nextElemFail :: (HasCallStack, H.MonadTest m) => NextElem a -> m a
nextElemFail (NextElem value) = pure value
nextElemFail NoNextElem = H.failMessage callStack "stream ended before the expected message arrived"

-- | Assert that a FollowTip response is an @apply@ action carrying non-empty
-- native bytes and a populated Cardano header.
assertAppliedBlock :: H.MonadTest m => Rpc.Proto U5c.FollowTipResponse -> m (Rpc.Proto U5c.AnyChainBlock)
assertAppliedBlock message = do
  block <- H.nothingFail (message ^. U5c.maybe'apply)
  H.assertWith (block ^. U5c.nativeBytes) $ not . BS.null
  H.assertWith (block ^. U5c.cardano . U5c.header . U5c.hash) $ (== 32) . BS.length
  H.assertWith (block ^. U5c.cardano . U5c.header . U5c.slot) (> 0)
  pure block

asAddressInEra :: ShelleyBasedEra era -> AsType (AddressInEra era)
asAddressInEra s = shelleyBasedEraConstraints s $ AsAddressInEra asType
