{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM (TMVar, TQueue, TVar)
import Control.Concurrent.STM qualified as STM
import Control.Monad (forever, when)
import Control.Monad.Trans.Except (runExceptT)
import Data.IORef qualified as IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import System.Environment (getArgs)
import System.Exit (die)
import System.IO
  ( BufferMode (LineBuffering)
  , hPutStrLn
  , hSetBuffering
  , hSetEncoding
  , stderr
  , utf8
  )

import Cardano.Api
  ( AddressAny
  , AddressInEra
  , AnyCardanoEra (..)
  , CardanoEra (DijkstraEra)
  , ConsensusModeParams (CardanoModeParams)
  , File (File)
  , FromSomeType (FromSomeType)
  , LocalNodeClientProtocols
      ( LocalNodeClientProtocols
      , localChainSyncClient
      , localStateQueryClient
      , localTxMonitoringClient
      , localTxSubmissionClient
      )
  , LocalChainSyncClient (NoLocalChainSyncClient)
  , LocalNodeConnectInfo (LocalNodeConnectInfo)
  , NetworkId (Testnet)
  , NetworkMagic (NetworkMagic)
  , PaymentCredential (PaymentCredentialByKey)
  , QueryInEra (QueryInShelleyBasedEra)
  , QueryInMode (QueryCurrentEra, QueryInEra)
  , QueryInShelleyBasedEra (QueryUTxO)
  , QueryUTxOFilter (QueryUTxOByAddress)
  , SigningKey
  , StakeAddressReference (NoStakeAddress)
  , SubmitResult (SubmitFail, SubmitSuccess)
  , Target (VolatileTip)
  , TxInMode (TxInMode)
  , TxValidationErrorInCardanoMode
  , UTxO (UTxO)
  )
import Cardano.Api qualified as Api

import Cardano.Ledger.Coin qualified as L

import Ouroboros.Network.Protocol.LocalTxSubmission.Client
  ( LocalTxClientStIdle (SendMsgSubmitTx)
  , LocalTxSubmissionClient (LocalTxSubmissionClient)
  )

import Cardano.Benchmarking.TxFirehose.Config
  ( Config (..)
  , loadConfig
  )
import Cardano.Benchmarking.TxFirehose.Tx (Fund (Fund, fundTxIn, fundValue))
import Cardano.Benchmarking.TxFirehose.Tx qualified as Tx

--------------------------------------------------------------------------------

-- | Byron epoch length passed in ConsensusModeParams -- only used to decode
-- Byron EBBs, which we never do. Matches every historical Cardano network
-- and is what cardano-cli hardcodes.
byronEpochSlots :: Api.EpochSlots
byronEpochSlots = Api.EpochSlots 21600

-- | An outstanding submission: the tx to submit, and a reply slot the
-- persistent client fills once the node responds.
data Pending = Pending
    !TxInMode
    !(TMVar (SubmitResult TxValidationErrorInCardanoMode))

main :: IO ()
main = do
  -- Force stderr to UTF-8 + line buffering so log messages don't get
  -- garbled or merged when the process runs under `LANG=C` (the default
  -- inside process-compose / nix sandbox), and each hPutStrLn appears
  -- on its own line.
  hSetEncoding stderr utf8
  hSetBuffering stderr LineBuffering

  cfgPath <- getArgs >>= \case
    [p] -> pure p
    _ -> die "usage: tx-firehose <config.json>"
  cfg <- loadConfig cfgPath

  signingKey <- loadSigningKey (cfgSigningKeyFile cfg)
  mStakeVk <- traverse loadStakingKey (cfgStakingKeyFile cfg)

  let networkId = Testnet (NetworkMagic (fromIntegral (cfgNetworkMagic cfg)))
      addrInEra = deriveAddress networkId signingKey mStakeVk
      addrAny   = case addrInEra of
        Api.AddressInEra _ addr -> Api.toAddressAny addr
      connInfo  = LocalNodeConnectInfo
        { Api.localConsensusModeParams = CardanoModeParams byronEpochSlots
        , Api.localNodeNetworkId       = networkId
        , Api.localNodeSocketPath      = File (cfgSocketPath cfg)
        }

  ensureDijkstra connInfo

  hPutStrLn stderr $ "tx-firehose: querying UTxO for " ++ show addrAny
  initialFunds <- queryFunds connInfo addrAny
  when (Map.null initialFunds) $
    die "tx-firehose: no UTxO found at derived address - fund it first"
  hPutStrLn stderr $ "tx-firehose: seeded with "
                  ++ show (Map.size initialFunds) ++ " UTxO(s), total "
                  ++ show (sum (Map.elems initialFunds)) ++ " lovelace"

  fundsVar     <- STM.newTVarIO initialFunds
  pendingQueue <- STM.newTQueueIO

  -- The submission client owns the wire; if it dies (socket closed, node
  -- restarted, protocol error) we want to die with it. Link so the exception
  -- propagates while the submit loop runs in the foreground.
  Async.withAsync (runSubmissionClient connInfo pendingQueue) $ \clientA -> do
    Async.link clientA
    submitLoop cfg addrInEra signingKey fundsVar pendingQueue

--------------------------------------------------------------------------------
-- Signing key & address
--------------------------------------------------------------------------------

loadSigningKey :: FilePath -> IO (SigningKey Api.PaymentKey)
loadSigningKey path = do
  result <- Api.readFileTextEnvelopeAnyOf accepted (File path)
  case result of
    Left err -> die $ "tx-firehose: cannot read signing key "
                    ++ show path ++ ": " ++ show err
    Right sk -> pure sk
  where
    accepted :: [FromSomeType Api.HasTextEnvelope (SigningKey Api.PaymentKey)]
    accepted =
      [ FromSomeType (Api.AsSigningKey Api.AsGenesisUTxOKey) Api.castSigningKey
      , FromSomeType (Api.AsSigningKey Api.AsPaymentKey) id
      ]

-- | Load a stake signing key (from a .skey text envelope) and derive its
-- verification key. Only the vkey hash is needed to build a base address —
-- the stake key does not have to sign anything (payment credential does).
loadStakingKey :: FilePath -> IO (Api.VerificationKey Api.StakeKey)
loadStakingKey path = do
  result <- Api.readFileTextEnvelope (File path)
  case result of
    Left err -> die $ "tx-firehose: cannot read stake key "
                    ++ show path ++ ": " ++ show err
    Right (sk :: SigningKey Api.StakeKey) -> pure (Api.getVerificationKey sk)

deriveAddress
  :: NetworkId
  -> SigningKey Api.PaymentKey
  -> Maybe (Api.VerificationKey Api.StakeKey)
  -> AddressInEra Api.DijkstraEra
deriveAddress networkId sk mStakeVk =
  Api.shelleyAddressInEra (Api.shelleyBasedEra @Api.DijkstraEra) $
    Api.makeShelleyAddress networkId
      (PaymentCredentialByKey
        (Api.verificationKeyHash (Api.getVerificationKey sk)))
      stakeRef
  where
    stakeRef = case mStakeVk of
      Nothing -> NoStakeAddress
      Just vk -> Api.StakeAddressByValue
                   (Api.StakeCredentialByKey (Api.verificationKeyHash vk))

--------------------------------------------------------------------------------
-- Era sanity check
--------------------------------------------------------------------------------

ensureDijkstra :: LocalNodeConnectInfo -> IO ()
ensureDijkstra connInfo = do
  res <- runExceptT $ Api.queryNodeLocalState connInfo VolatileTip QueryCurrentEra
  case res of
    Left af -> die $ "tx-firehose: failed to acquire tip: " ++ show af
    Right (AnyCardanoEra DijkstraEra) -> pure ()
    Right (AnyCardanoEra other) ->
      die $ "tx-firehose: node is in era " ++ show other
         ++ ", but this tool only builds Dijkstra-era transactions"

--------------------------------------------------------------------------------
-- UTxO query (ephemeral connection)
--------------------------------------------------------------------------------

queryFunds
  :: LocalNodeConnectInfo -> AddressAny -> IO (Map Api.TxIn Integer)
queryFunds connInfo addrAny = do
  let q = QueryInEra
            (QueryInShelleyBasedEra
               (Api.shelleyBasedEra @Api.DijkstraEra)
               (QueryUTxO (QueryUTxOByAddress (Set.singleton addrAny))))
  res <- runExceptT $ Api.queryNodeLocalState connInfo VolatileTip q
  case res of
    Left af -> die $ "tx-firehose: failed to acquire tip: " ++ show af
    Right (Left mismatch) ->
      die $ "tx-firehose: era mismatch on UTxO query: " ++ show mismatch
    Right (Right (UTxO m)) -> pure (Map.map toLovelace m)
  where
    toLovelace :: Api.TxOut Api.CtxUTxO Api.DijkstraEra -> Integer
    toLovelace (Api.TxOut _ v _ _) =
      let L.Coin c = Api.txOutValueToLovelace v in c

--------------------------------------------------------------------------------
-- Persistent submission client
--------------------------------------------------------------------------------

-- | Reads pending submissions from the queue and drives them one at a time
-- over a single LocalTxSubmission session. The protocol is inherently
-- request/response so pipelining a queue in front of it is the right shape:
-- multiple producers can enqueue concurrently, this thread serialises them
-- onto the wire and posts each reply back to the caller.
mkPersistentClient
  :: TQueue Pending
  -> LocalTxSubmissionClient TxInMode TxValidationErrorInCardanoMode IO ()
mkPersistentClient q = LocalTxSubmissionClient loop
  where
    loop :: IO (LocalTxClientStIdle TxInMode TxValidationErrorInCardanoMode IO ())
    loop = do
      Pending tx reply <- STM.atomically (STM.readTQueue q)
      pure $ SendMsgSubmitTx tx $ \result -> do
        STM.atomically $ STM.putTMVar reply result
        loop

runSubmissionClient :: LocalNodeConnectInfo -> TQueue Pending -> IO ()
runSubmissionClient connInfo q =
  Api.connectToLocalNode connInfo LocalNodeClientProtocols
    { localChainSyncClient    = NoLocalChainSyncClient
    , localStateQueryClient   = Nothing
    , localTxSubmissionClient = Just (mkPersistentClient q)
    , localTxMonitoringClient = Nothing
    }

-- | Enqueue a tx and block until the node replies.
submitViaQueue
  :: TQueue Pending
  -> TxInMode
  -> IO (SubmitResult TxValidationErrorInCardanoMode)
submitViaQueue q tx = do
  reply <- STM.newEmptyTMVarIO
  STM.atomically $ STM.writeTQueue q (Pending tx reply)
  STM.atomically $ STM.takeTMVar reply

--------------------------------------------------------------------------------
-- Submit loop
--------------------------------------------------------------------------------

-- | Rate-limited loop: take @inputsPerTx@ funds, build a tx, submit via the
-- persistent queue, dispatch on the reply.
--
-- We deliberately do NOT try to reconcile the local fund set with the ledger
-- while running: the local set is populated optimistically from every
-- SubmitSuccess and will diverge under normal chain progress. Instead we
-- track consecutive rejects and die once they cross the configured
-- threshold. The supervisor restarts us and we requery from a clean slate.
submitLoop
  :: Config
  -> AddressInEra Api.DijkstraEra
  -> SigningKey Api.PaymentKey
  -> TVar (Map Api.TxIn Integer)
  -> TQueue Pending
  -> IO ()
submitLoop cfg addr sk fundsVar pendingQueue = do
  let period = round (1_000_000 / cfgTps cfg) :: Int
      n      = fromIntegral (cfgInputsPerTx cfg) :: Int
      maxErrs = cfgMaxConsecutiveErrors cfg
  consecutive <- IORef.newIORef (0 :: Int)
  let bumpError reason = do
        c <- IORef.atomicModifyIORef' consecutive (\c -> (c + 1, c + 1))
        when (c >= maxErrs) $
          die $ "tx-firehose: " ++ show maxErrs
             ++ " consecutive rejects, exiting for restart (last: "
             ++ reason ++ ")"
      resetError = IORef.writeIORef consecutive 0
  forever $ do
    inputs <- STM.atomically $ do
      m <- STM.readTVar fundsVar
      when (Map.size m < n) STM.retry
      let taken = take n (Map.toList m)
          m' = foldr (Map.delete . fst) m taken
      STM.writeTVar fundsVar m'
      pure taken
    let inFunds =
          [ Fund { fundTxIn = tin, fundValue = v } | (tin, v) <- inputs ]
    case Tx.buildTx addr sk inFunds (cfgOutputsPerTx cfg)
                      (L.Coin (cfgFee cfg)) of
      Left err -> do
        hPutStrLn stderr $ "tx-firehose: buildTx failed: " ++ err
        returnFunds fundsVar inputs
        bumpError ("buildTx: " ++ err)
        threadDelay period
      Right (signedTx, outFunds) -> do
        let txId = Api.getTxId (Api.getTxBody signedTx)
            txInMode = TxInMode (Api.shelleyBasedEra @Api.DijkstraEra) signedTx
        result <- submitViaQueue pendingQueue txInMode
        case result of
          SubmitSuccess -> do
            hPutStrLn stderr $ "tx-firehose: submitted " ++ show txId
            STM.atomically $ STM.modifyTVar' fundsVar $ \m ->
              foldr (\f -> Map.insert (fundTxIn f) (fundValue f)) m outFunds
            resetError
          SubmitFail reason -> do
            hPutStrLn stderr $ "tx-firehose: rejected " ++ show txId
                            ++ ": " ++ show reason
            returnFunds fundsVar inputs
            bumpError (show reason)
        threadDelay period

returnFunds :: TVar (Map Api.TxIn Integer) -> [(Api.TxIn, Integer)] -> IO ()
returnFunds fundsVar inputs = STM.atomically $
  STM.modifyTVar' fundsVar $ \m ->
    foldr (uncurry Map.insert) m inputs
