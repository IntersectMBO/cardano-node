{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Applicative (optional)
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (Value, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Numeric.Natural (Natural)
import Options.Applicative qualified as Opt
import System.Exit (die)
import System.IO
  ( BufferMode (LineBuffering)
  , hSetBuffering
  , hSetEncoding
  , stderr
  , utf8
  )

import Cardano.Api
  ( AddressAny
  , AddressInEra
  , AnyCardanoEra (AnyCardanoEra)
  , ConsensusModeParams (CardanoModeParams)
  , File (File)
  , FromSomeType (FromSomeType)
  , LocalChainSyncClient (NoLocalChainSyncClient)
  , LocalNodeClientProtocols
      ( LocalNodeClientProtocols
      , localChainSyncClient
      , localStateQueryClient
      , localTxMonitoringClient
      , localTxSubmissionClient
      )
  , LocalNodeConnectInfo (LocalNodeConnectInfo)
  , NetworkId (Testnet)
  , NetworkMagic (NetworkMagic)
  , PaymentCredential (PaymentCredentialByKey)
  , QueryInEra (QueryInShelleyBasedEra)
  , QueryInMode (QueryCurrentEra, QueryInEra)
  , QueryInShelleyBasedEra (QueryUTxO)
  , QueryUTxOFilter (QueryUTxOByAddress)
  , ShelleyBasedEra
  , SigningKey
  , StakeAddressReference (NoStakeAddress)
  , SubmitResult (SubmitFail, SubmitSuccess)
  , Target (VolatileTip)
  , TxInMode (TxInMode)
  , TxValidationErrorInCardanoMode
  , UTxO (UTxO)
  )
import Cardano.Api qualified as Api

import Cardano.Ledger.Api.Tx.In (TxIn)
import Cardano.Ledger.Coin (Coin (Coin))

import Ouroboros.Network.Protocol.LocalTxSubmission.Client
  ( LocalTxClientStIdle (SendMsgDone, SendMsgSubmitTx)
  , LocalTxSubmissionClient (LocalTxSubmissionClient)
  )

import Cardano.Benchmarking.TxFirehose.Tx
  ( BuiltTx (BuiltTx, btxId, btxOutputs, btxSigned, btxSize)
  , Fund (Fund, fundTxIn, fundValue)
  )
import Cardano.Benchmarking.TxFirehose.Tx qualified as Tx

--------------------------------------------------------------------------------
-- CLI
--------------------------------------------------------------------------------

data Options = Options
  { optSocketPath           :: !FilePath
  , optNetworkMagic         :: !Natural
  , optSigningKey           :: !FilePath
  , optStakingKey           :: !(Maybe FilePath)
  , optTps                  :: !Double
  , optInputsPerTx          :: !Natural
  , optOutputsPerTx         :: !Natural
  , optFee                  :: !Integer
  , optMaxConsecutiveErrors :: !Int
  }

parseOptions :: IO Options
parseOptions = Opt.execParser $ Opt.info (optionsParser Opt.<**> Opt.helper)
  ( Opt.fullDesc
 <> Opt.progDesc "Push-based single-node tx load generator over node-to-client."
 <> Opt.header "tx-firehose - hose transactions at one Cardano node" )

optionsParser :: Opt.Parser Options
optionsParser = Options
  <$> Opt.strOption
        ( Opt.long "socket-path" <> Opt.metavar "SOCKET_PATH"
       <> Opt.help "Path to the node socket (node-to-client)" )
  <*> Opt.option Opt.auto
        ( Opt.long "testnet-magic" <> Opt.metavar "NATURAL"
       <> Opt.help "Specify a testnet magic id (e.g. 164 for leios proto-devnet)" )
  <*> Opt.strOption
        ( Opt.long "signing-key-file" <> Opt.metavar "FILEPATH"
       <> Opt.help "Payment signing key (.skey; GenesisUTxOKey also accepted)" )
  <*> optional (Opt.strOption
        ( Opt.long "staking-key-file" <> Opt.metavar "FILEPATH"
       <> Opt.help "Stake signing key (.skey). If set, derive a base address; else enterprise." ))
  <*> Opt.option Opt.auto
        ( Opt.long "tps" <> Opt.metavar "NATURAL"
       <> Opt.help "Target submissions per second (rate ceiling)" )
  <*> Opt.option Opt.auto
        ( Opt.long "inputs-per-tx" <> Opt.metavar "NATURAL"
       <> Opt.value 1 <> Opt.showDefault
       <> Opt.help "Number of inputs per generated tx" )
  <*> Opt.option Opt.auto
        ( Opt.long "outputs-per-tx" <> Opt.metavar "NATURAL"
       <> Opt.value 1 <> Opt.showDefault
       <> Opt.help "Number of outputs per generated tx" )
  <*> Opt.option Opt.auto
        ( Opt.long "fee" <> Opt.metavar "LOVELACE"
       <> Opt.value 200_000 <> Opt.showDefault
       <> Opt.help "Fixed fee per tx (lovelace)" )
  <*> Opt.option Opt.auto
        ( Opt.long "max-consecutive-errors" <> Opt.metavar "NATURAL"
       <> Opt.value 50 <> Opt.showDefault
       <> Opt.help "Exit after this many consecutive rejects (for supervisor restart)" )

--------------------------------------------------------------------------------
-- Entry point
--------------------------------------------------------------------------------

main :: IO ()
main = do
  -- Force stderr to UTF-8 + line buffering so log messages don't get
  -- garbled or merged when the process runs under `LANG=C`.
  hSetEncoding stderr utf8
  hSetBuffering stderr LineBuffering

  opts <- parseOptions
  validateOptions opts

  signingKey <- loadSigningKey (optSigningKey opts)
  mStakeVk <- traverse loadStakingKey (optStakingKey opts)

  let networkId = Testnet (NetworkMagic (fromIntegral (optNetworkMagic opts)))
      connInfo  = LocalNodeConnectInfo
        { Api.localConsensusModeParams = CardanoModeParams byronEpochSlots
        , Api.localNodeNetworkId       = networkId
        , Api.localNodeSocketPath      = File (optSocketPath opts)
        }

  -- Dispatch on whatever era the node reports; the tx builder is
  -- generic over ShelleyBasedEra.
  currentEra <- queryCurrentEra connInfo
  runInEra currentEra $ \sbe ->
    runFirehoseInEra sbe opts connInfo networkId signingKey mStakeVk

-- | Fail if the node is in Byron; otherwise run the continuation with
-- the era's 'ShelleyBasedEra' witness.
runInEra
  :: AnyCardanoEra
  -> (forall era. Api.ShelleyBasedEraConstraints era => ShelleyBasedEra era -> IO ())
  -> IO ()
runInEra (AnyCardanoEra ce) k =
  Api.caseByronOrShelleyBasedEra
    (die "tx-firehose: Byron era is not supported")
    k
    ce

validateOptions :: Options -> IO ()
validateOptions opts = do
  when (optTps opts <= 0) $ die "--tps must be > 0"
  when (optInputsPerTx opts == 0) $ die "--inputs-per-tx must be >= 1"
  when (optOutputsPerTx opts == 0) $ die "--outputs-per-tx must be >= 1"
  when (optMaxConsecutiveErrors opts <= 0) $
    die "--max-consecutive-errors must be >= 1"

byronEpochSlots :: Api.EpochSlots
byronEpochSlots = Api.EpochSlots 21600

--------------------------------------------------------------------------------
-- Per-era firehose
--------------------------------------------------------------------------------

-- | Query the initial UTxO for the derived address, then open a single
-- N2C connection whose LocalTxSubmission client drives the whole loop.
-- No separate thread and no shared TVar: the loop's state (funds and
-- consecutive-error counter) lives in the client's recursive
-- continuation.
runFirehoseInEra
  :: Api.ShelleyBasedEraConstraints era
  => ShelleyBasedEra era
  -> Options
  -> LocalNodeConnectInfo
  -> NetworkId
  -> SigningKey Api.PaymentKey
  -> Maybe (Api.VerificationKey Api.StakeKey)
  -> IO ()
runFirehoseInEra sbe opts connInfo networkId signingKey mStakeVk = do
  let addrInEra = deriveAddress sbe networkId signingKey mStakeVk
      addrAny   = case addrInEra of
        Api.AddressInEra _ addr -> Api.toAddressAny addr

  trace "TxFirehose.Startup.Query" "Info" $
    Aeson.object ["address" .= T.pack (show addrAny), "era" .= show sbe]

  initialFunds <- queryFundsInEra sbe connInfo addrAny
  when (Map.null initialFunds) $
    die "tx-firehose: no UTxO found at derived address - fund it first"

  trace "TxFirehose.Startup.Seeded" "Info" $ Aeson.object
    [ "utxos"         .= Map.size initialFunds
    , "totalLovelace" .= sum (Map.elems initialFunds)
    ]

  Api.connectToLocalNode connInfo LocalNodeClientProtocols
    { localChainSyncClient    = NoLocalChainSyncClient
    , localStateQueryClient   = Nothing
    , localTxSubmissionClient =
        Just (mkFirehoseClient sbe opts addrInEra signingKey initialFunds)
    , localTxMonitoringClient = Nothing
    }

-- | The LocalTxSubmission state machine. Holds the fund set and the
-- consecutive-error counter as strict recursive parameters — no TVars,
-- no IORefs, no async.
mkFirehoseClient
  :: forall era
   . Api.ShelleyBasedEraConstraints era
  => ShelleyBasedEra era
  -> Options
  -> AddressInEra era
  -> SigningKey Api.PaymentKey
  -> Map TxIn Integer
  -> LocalTxSubmissionClient TxInMode TxValidationErrorInCardanoMode IO ()
mkFirehoseClient sbe opts addr sk initialFunds =
    LocalTxSubmissionClient (step initialFunds 0)
  where
    !period  = round (1_000_000 / optTps opts) :: Int
    !n       = fromIntegral (optInputsPerTx opts) :: Int
    !maxErrs = optMaxConsecutiveErrors opts

    -- One step of the loop, in IO. Returns the next client state.
    step
      :: Map TxIn Integer
      -> Int
      -> IO (LocalTxClientStIdle TxInMode TxValidationErrorInCardanoMode IO ())
    step !funds !consec
      | Map.size funds < n =
          -- We recycle outputs on every success and never lose funds on
          -- reject (inputs stay put), so running dry is catastrophic —
          -- exit and let the supervisor restart with a fresh query.
          pure (SendMsgDone ())
      | otherwise =
          let (chosen, funds') = takeInputs n funds
              inFunds =
                [ Fund { fundTxIn = tin, fundValue = v }
                | (tin, v) <- chosen
                ]
          in case Tx.buildTx sbe addr sk inFunds
                    (optOutputsPerTx opts) (Coin (optFee opts)) of
               Left err -> do
                 trace "TxFirehose.Build.Fail" "Error" $
                   Aeson.object ["error" .= T.pack err]
                 -- Inputs stay in the fund set: buildTx never touched
                 -- them, so this is just a signal for restart.
                 onError funds consec ("buildTx: " ++ err)
               Right built ->
                 pure (submitStep funds funds' consec built)

    -- SendMsgSubmitTx state: post-submit handling of the reply.
    submitStep
      :: Map TxIn Integer  -- ^ funds to keep on reject (inputs still there)
      -> Map TxIn Integer  -- ^ funds after removing this tx's inputs
      -> Int
      -> BuiltTx era
      -> LocalTxClientStIdle TxInMode TxValidationErrorInCardanoMode IO ()
    submitStep fundsOnFail fundsOnSuccess !consec
               BuiltTx { btxSigned, btxId, btxSize, btxOutputs } =
      SendMsgSubmitTx (TxInMode sbe btxSigned) $ \result -> do
        threadDelay period
        case result of
          SubmitSuccess -> do
            trace "TxFirehose.Submit.Success" "Info" $ Aeson.object
              [ "txId" .= btxId, "size" .= btxSize ]
            let !funds'' = foldr addOutput fundsOnSuccess btxOutputs
            step funds'' 0
          SubmitFail reason -> do
            trace "TxFirehose.Submit.Reject" "Warning" $ Aeson.object
              [ "txId" .= btxId, "size" .= btxSize
              , "reason" .= T.pack (show reason)
              ]
            onError fundsOnFail consec (show reason)

    -- Bump the consecutive-error counter and either exit or loop.
    onError
      :: Map TxIn Integer
      -> Int
      -> String
      -> IO (LocalTxClientStIdle TxInMode TxValidationErrorInCardanoMode IO ())
    onError funds !consec reason = do
      let !consec' = consec + 1
      if consec' >= maxErrs
        then do
          trace "TxFirehose.Exit.MaxErrors" "Error" $ Aeson.object
            [ "threshold"  .= maxErrs
            , "lastReason" .= T.pack reason
            ]
          die $ "tx-firehose: " ++ show maxErrs
             ++ " consecutive rejects, exiting for restart (last: "
             ++ reason ++ ")"
        else step funds consec'

-- | Deterministically pull @n@ entries out of a fund set.
takeInputs :: Int -> Map TxIn Integer -> ([(TxIn, Integer)], Map TxIn Integer)
takeInputs n m =
  let taken = take n (Map.toList m)
      m'    = foldr (Map.delete . fst) m taken
  in (taken, m')

addOutput :: Fund -> Map TxIn Integer -> Map TxIn Integer
addOutput f = Map.insert (fundTxIn f) (fundValue f)

--------------------------------------------------------------------------------
-- Structured logging
--------------------------------------------------------------------------------

-- | Emit one JSON line in the cardano-node trace schema
-- (@{at, sev, host, thread, ns, data}@). The proto-devnet Alloy
-- pipeline turns @ns@ into a Loki label, so Grafana can filter on
-- @TxFirehose.Submit.Success@ etc.
trace :: Text -> Text -> Value -> IO ()
trace ns sev dat = do
  now <- getCurrentTime
  let payload = Aeson.object
        [ "at"     .= T.pack (iso8601Show now)
        , "sev"    .= sev
        , "host"   .= ("tx-firehose" :: Text)
        , "thread" .= ("main" :: Text)
        , "ns"     .= ns
        , "data"   .= dat
        ]
  BSL.hPutStrLn stderr (Aeson.encode payload)

--------------------------------------------------------------------------------
-- Startup helpers
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

-- | Load a stake signing key and derive its verification key. Only the
-- vkey hash is used (to build a base address); the stake key does not
-- sign anything.
loadStakingKey :: FilePath -> IO (Api.VerificationKey Api.StakeKey)
loadStakingKey path = do
  result <- Api.readFileTextEnvelope (File path)
  case result of
    Left err -> die $ "tx-firehose: cannot read stake key "
                    ++ show path ++ ": " ++ show err
    Right (sk :: SigningKey Api.StakeKey) -> pure (Api.getVerificationKey sk)

deriveAddress
  :: ShelleyBasedEra era
  -> NetworkId
  -> SigningKey Api.PaymentKey
  -> Maybe (Api.VerificationKey Api.StakeKey)
  -> AddressInEra era
deriveAddress sbe networkId sk mStakeVk =
  Api.shelleyAddressInEra sbe $
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
-- Local-state queries
--------------------------------------------------------------------------------

queryCurrentEra :: LocalNodeConnectInfo -> IO AnyCardanoEra
queryCurrentEra connInfo = do
  res <- runExceptT $ Api.queryNodeLocalState connInfo VolatileTip QueryCurrentEra
  case res of
    Left af -> die $ "tx-firehose: failed to acquire tip: " ++ show af
    Right ce -> pure ce

-- | Query the current UTxO at our derived address in whatever era the
-- node reports. Converts each 'Api.TxIn' to the ledger 'TxIn' so the
-- fund set the client owns is in ledger types from the start.
queryFundsInEra
  :: ShelleyBasedEra era
  -> LocalNodeConnectInfo
  -> AddressAny
  -> IO (Map TxIn Integer)
queryFundsInEra sbe connInfo addrAny = do
  let q = QueryInEra
            (QueryInShelleyBasedEra sbe
               (QueryUTxO (QueryUTxOByAddress (Set.singleton addrAny))))
  res <- runExceptT $ Api.queryNodeLocalState connInfo VolatileTip q
  case res of
    Left af -> die $ "tx-firehose: failed to acquire tip: " ++ show af
    Right (Left mismatch) ->
      die $ "tx-firehose: era mismatch on UTxO query: " ++ show mismatch
    Right (Right (UTxO m)) ->
      pure . Map.fromList $
        [ (Api.toShelleyTxIn tin, toLovelace txOut)
        | (tin, txOut) <- Map.toList m
        ]
  where
    toLovelace :: Api.TxOut Api.CtxUTxO era -> Integer
    toLovelace (Api.TxOut _ v _ _) =
      let Coin c = Api.txOutValueToLovelace v in c
