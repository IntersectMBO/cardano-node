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
import Cardano.Benchmarking.TxFirehose.Color
  ( Color
  , ColorSpec
  , colorHex
  , colorSwatch
  , parseColorSpec
  , resolveColor
  )
import Cardano.Benchmarking.TxFirehose.Tx
  ( BuiltTx (BuiltTx, btxId, btxInputs, btxOutputs, btxSigned, btxSize)
  , Fund (Fund, fundTxIn, fundValue)
  )
import Cardano.Benchmarking.TxFirehose.Tx qualified as Tx
import Cardano.Ledger.Api.Tx.In (TxIn)
import Cardano.Ledger.Coin (Coin (Coin))
import Control.Applicative (optional)
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (Value, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.List (isInfixOf, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Numeric.Natural (Natural)
import Options.Applicative qualified as Opt
import Ouroboros.Network.Protocol.LocalTxSubmission.Client
  ( LocalTxClientStIdle (SendMsgDone, SendMsgSubmitTx)
  , LocalTxSubmissionClient (LocalTxSubmissionClient)
  )
import System.Exit (die)
import System.Environment (lookupEnv)
import System.IO
  ( BufferMode (LineBuffering)
  , hIsTerminalDevice
  , hPutStrLn
  , hSetBuffering
  , hSetEncoding
  , stderr
  , utf8
  )

--------------------------------------------------------------------------------
-- CLI
--------------------------------------------------------------------------------

data Options = Options
  { optSocketPath :: !FilePath
  , optNetworkMagic :: !Natural
  , optSigningKey :: !FilePath
  , optStakingKey :: !(Maybe FilePath)
  , optTps :: !Double
  , optInputsPerTx :: !(Maybe Natural)
  , optOutputsPerTx :: !Natural
  , optFee :: !Integer
  , optMaxConsecutiveErrors :: !Int
  , optColor :: !(Maybe ColorSpec)
  }

parseOptions :: IO Options
parseOptions =
  Opt.execParser $
    Opt.info
      (optionsParser Opt.<**> Opt.helper)
      ( Opt.fullDesc
          <> Opt.progDesc "Push-based single-node tx load generator over node-to-client."
          <> Opt.header "tx-firehose - hose transactions at one Cardano node"
      )

optionsParser :: Opt.Parser Options
optionsParser =
  Options
    <$> Opt.strOption
      ( Opt.long "socket-path"
          <> Opt.metavar "SOCKET_PATH"
          <> Opt.help "Path to the node socket (node-to-client)"
      )
    <*> Opt.option
      Opt.auto
      ( Opt.long "testnet-magic"
          <> Opt.metavar "NATURAL"
          <> Opt.help "Specify a testnet magic id (e.g. 164 for leios proto-devnet)"
      )
    <*> Opt.strOption
      ( Opt.long "signing-key-file"
          <> Opt.metavar "FILEPATH"
          <> Opt.help "Payment signing key (.skey; GenesisUTxOKey also accepted)"
      )
    <*> optional
      ( Opt.strOption
          ( Opt.long "staking-key-file"
              <> Opt.metavar "FILEPATH"
              <> Opt.help "Stake signing key (.skey). If set, derive a base address; else enterprise."
          )
      )
    <*> Opt.option
      Opt.auto
      ( Opt.long "tps"
          <> Opt.metavar "NATURAL"
          <> Opt.help "Target submissions per second (rate ceiling)"
      )
    <*> optional
      ( Opt.option
          Opt.auto
          ( Opt.long "inputs-per-tx"
              <> Opt.metavar "NATURAL"
              <> Opt.help
                "Number of inputs per generated tx. Omit to derive it from \
                \--outputs-per-tx and keep the UTxO set size constant."
          )
      )
    <*> Opt.option
      Opt.auto
      ( Opt.long "outputs-per-tx"
          <> Opt.metavar "NATURAL"
          <> Opt.value 1
          <> Opt.showDefault
          <> Opt.help "Number of outputs per generated tx"
      )
    <*> Opt.option
      Opt.auto
      ( Opt.long "fee"
          <> Opt.metavar "LOVELACE"
          <> Opt.value 200_000
          <> Opt.showDefault
          <> Opt.help "Fixed fee per tx (lovelace)"
      )
    <*> Opt.option
      Opt.auto
      ( Opt.long "max-consecutive-errors"
          <> Opt.metavar "NATURAL"
          <> Opt.value 50
          <> Opt.showDefault
          <> Opt.help "Exit after this many consecutive rejects (for supervisor restart)"
      )
    <*> optional
      ( Opt.option
          (Opt.eitherReader parseColorSpec)
          ( Opt.long "color"
              <> Opt.metavar "HEX|auto"
              <> Opt.help
                "Tag every tx with this colour as metadata, e.g. ff0000, or 'auto' to derive one from the signing key"
          )
      )

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
      connInfo =
        LocalNodeConnectInfo
          { Api.localConsensusModeParams = CardanoModeParams byronEpochSlots
          , Api.localNodeNetworkId = networkId
          , Api.localNodeSocketPath = File (optSocketPath opts)
          }

  -- Dispatch on whatever era the node reports; the tx builder is
  -- generic over ShelleyBasedEra.
  -- Resolve the colour once, here, so the swatch we print and the metadata we
  -- attach cannot disagree.
  let mColor = flip resolveColor (Api.getVerificationKey signingKey) <$> optColor opts
  traverse_ announceColor mColor

  currentEra <- queryCurrentEra connInfo
  runInEra currentEra $ \sbe ->
    runFirehoseInEra sbe opts connInfo networkId signingKey mStakeVk mColor

-- | Show the colour on stderr at startup, as a block when the terminal can
-- render it and as bare hex otherwise.
announceColor :: Color -> IO ()
announceColor color = do
  tty <- hIsTerminalDevice stderr
  noColor <- lookupEnv "NO_COLOR"
  let swatch
        | tty && noColor == Nothing = " " ++ colorSwatch color
        | otherwise = ""
  hPutStrLn stderr ("tx-firehose colour: " ++ colorHex color ++ swatch)

-- | Fail if the node is in Byron; otherwise run the continuation with
-- the era's 'ShelleyBasedEra' witness.
runInEra ::
  AnyCardanoEra ->
  (forall era. Api.ShelleyBasedEraConstraints era => ShelleyBasedEra era -> IO ()) ->
  IO ()
runInEra (AnyCardanoEra ce) k =
  Api.caseByronOrShelleyBasedEra
    (die "tx-firehose: Byron era is not supported")
    k
    ce

validateOptions :: Options -> IO ()
validateOptions opts = do
  when (optTps opts <= 0) $ die "--tps must be > 0"
  when (optInputsPerTx opts == Just 0) $ die "--inputs-per-tx must be >= 1"
  when (optOutputsPerTx opts == 0) $ die "--outputs-per-tx must be >= 1"
  when (optMaxConsecutiveErrors opts <= 0) $
    die "--max-consecutive-errors must be >= 1"

-- | Byron epoch length passed in ConsensusModeParams -- only used to
-- decode Byron EBBs, which we never do. Matches every historical
-- Cardano network and is what cardano-cli hardcodes.
byronEpochSlots :: Api.EpochSlots
byronEpochSlots = Api.EpochSlots 21600

-- | Query the initial UTxO for the derived address, then open a single
-- N2C connection whose LocalTxSubmission client drives the whole loop.
-- No separate thread and no shared TVar: the loop's state (funds and
-- consecutive-error counter) lives in the client's recursive
-- continuation.
runFirehoseInEra ::
  Api.ShelleyBasedEraConstraints era =>
  ShelleyBasedEra era ->
  Options ->
  LocalNodeConnectInfo ->
  NetworkId ->
  SigningKey Api.PaymentKey ->
  Maybe (Api.VerificationKey Api.StakeKey) ->
  Maybe Color ->
  IO ()
runFirehoseInEra sbe opts connInfo networkId signingKey mStakeVk mColor = do
  trace "TxFirehose.Startup.Query" "Info" $
    Aeson.object ["address" .= T.pack (show addrAny), "era" .= show sbe]

  initialFunds <- queryFundsInEra sbe connInfo addrAny
  when (Map.null initialFunds) $
    die "tx-firehose: no UTxO found at derived address - fund it first"

  trace "TxFirehose.Startup.Seeded" "Info" $
    Aeson.object
      [ "utxos" .= Map.size initialFunds
      , "totalLovelace" .= sum (Map.elems initialFunds)
      ]

  Api.connectToLocalNode
    connInfo
    LocalNodeClientProtocols
      { localChainSyncClient = NoLocalChainSyncClient
      , localStateQueryClient = Nothing
      , localTxSubmissionClient =
          Just (mkFirehoseClient sbe opts addrInEra signingKey initialFunds mColor)
      , localTxMonitoringClient = Nothing
      }
 where
  addrInEra = deriveAddress sbe networkId signingKey mStakeVk
  addrAny = case addrInEra of
    Api.AddressInEra _ addr -> Api.toAddressAny addr

-- | The LocalTxSubmission state machine. Holds the fund set and the
-- consecutive-error counter as strict recursive parameters — no TVars,
-- no IORefs, no async.
mkFirehoseClient ::
  forall era.
  Api.ShelleyBasedEraConstraints era =>
  ShelleyBasedEra era ->
  Options ->
  AddressInEra era ->
  SigningKey Api.PaymentKey ->
  Map TxIn Integer ->
  Maybe Color ->
  LocalTxSubmissionClient TxInMode TxValidationErrorInCardanoMode IO ()
mkFirehoseClient sbe opts addr sk initialFunds mColor =
  LocalTxSubmissionClient (step initialFunds 0)
 where
  !period = round (1_000_000 / optTps opts) :: Int
  !target = fromIntegral (optOutputsPerTx opts) :: Int
  !mFixedInputs = fromIntegral <$> optInputsPerTx opts :: Maybe Int
  !maxErrs = optMaxConsecutiveErrors opts

  -- With a fixed input count we can only ever build a tx while that many
  -- funds are on hand; ramping instead always has a move, down to one
  -- remaining fund.
  !minFunds = fromMaybe 1 mFixedInputs

  -- One step of the loop, in IO. Returns the next client state.
  step ::
    Map TxIn Integer ->
    Int ->
    IO (LocalTxClientStIdle TxInMode TxValidationErrorInCardanoMode IO ())
  step !funds !consec
    | Map.size funds < minFunds =
        -- We recycle outputs on every success and never lose funds on
        -- reject (inputs stay put), so running dry is catastrophic —
        -- exit and let the supervisor restart with a fresh query.
        pure (SendMsgDone ())
    | otherwise =
        case Tx.buildTx
          sbe
          addr
          sk
          inFunds
          (optOutputsPerTx opts)
          (Coin (optFee opts))
          mColor of
          Left err -> do
            trace "TxFirehose.Build.Fail" "Error" $
              Aeson.object ["error" .= T.pack err]
            -- Inputs stay in the fund set: buildTx never touched
            -- them, so this is just a signal for restart.
            onError funds consec ("buildTx: " ++ err)
          Right built ->
            pure (submitStep funds funds' consec built)
   where
    -- Fixed input count: build exactly that shape, whatever it does to
    -- the UTxO set. Otherwise take the @target@ largest funds, which both
    -- reaches the steady state from any starting shape and keeps it there:
    -- @target@ in, @target@ out leaves the set size unchanged.
    (chosen, funds') = case mFixedInputs of
      Just n -> takeInputs n funds
      Nothing -> takeLargest target (optFee opts) funds
    inFunds =
      [ Fund{fundTxIn = tin, fundValue = v}
      | (tin, v) <- chosen
      ]

  -- SendMsgSubmitTx state: post-submit handling of the reply.
  submitStep ::
    Map TxIn Integer ->
    -- \^ funds to keep on reject (inputs still there)
    Map TxIn Integer ->
    -- \^ funds after removing this tx's inputs
    Int ->
    BuiltTx era ->
    LocalTxClientStIdle TxInMode TxValidationErrorInCardanoMode IO ()
  submitStep
    fundsOnFail
    fundsOnSuccess
    !consec
    BuiltTx{btxSigned, btxId, btxSize, btxInputs, btxOutputs} =
      SendMsgSubmitTx (TxInMode sbe btxSigned) $ \result -> do
        threadDelay period
        case result of
          SubmitSuccess -> do
            trace "TxFirehose.Submit.Success" "Info" $
              Aeson.object
                [ "txId" .= btxId
                , "size" .= btxSize
                , "inputs" .= btxInputs
                , "outputs" .= length btxOutputs
                ]
            let !funds'' = foldr addOutput fundsOnSuccess btxOutputs
            step funds'' 0
          SubmitFail reason -> do
            trace "TxFirehose.Submit.Reject" "Warning" $
              Aeson.object
                [ "txId" .= btxId
                , "size" .= btxSize
                , "reason" .= T.pack (show reason)
                ]
            -- Keeping the inputs is right for a transient reject, but wrong when
            -- the ledger says they are gone: 'takeInputs' is deterministic, so
            -- the retry rebuilds this exact tx and earns this exact rejection,
            -- forever. Worse at startup with a single UTxO and a fan-out target,
            -- where that one tx is the only one buildable. Drop them so the next
            -- build differs; if that drains the set we exit and the supervisor
            -- restarts with a fresh query, which is the intended recovery.
            onError
              (if inputsAreGone (show reason) then fundsOnSuccess else fundsOnFail)
              consec
              (show reason)

  -- Bump the consecutive-error counter and either exit or loop.
  onError ::
    Map TxIn Integer ->
    Int ->
    String ->
    IO (LocalTxClientStIdle TxInMode TxValidationErrorInCardanoMode IO ())
  onError funds !consec reason
    | consec' >= maxErrs = do
        trace "TxFirehose.Exit.MaxErrors" "Error" $
          Aeson.object
            [ "threshold" .= maxErrs
            , "lastReason" .= T.pack reason
            ]
        die $
          "tx-firehose: "
            ++ show maxErrs
            ++ " consecutive rejects, exiting for restart (last: "
            ++ reason
            ++ ")"
    | otherwise = step funds consec'
   where
    !consec' = consec + 1

-- | Does this rejection mean the inputs we spent no longer exist?
--
-- Matched on the rendered error because the useful constructors sit behind
-- several era-parameterised wrappers; the alternative is threading an era
-- dictionary through purely to name two of them.
inputsAreGone :: String -> Bool
inputsAreGone reason =
  any (`isInfixOf` reason) ["AllInputsAreSpent", "BadInputsUTxO"]

-- | Deterministically pull @n@ entries out of a fund set.
takeInputs :: Int -> Map TxIn Integer -> ([(TxIn, Integer)], Map TxIn Integer)
takeInputs n m = (taken, m')
 where
  taken = take n (Map.toList m)
  m' = foldr (Map.delete . fst) m taken

-- | Every generated output stays comfortably above min-UTxO, so a tx never
-- creates a fund too small to be spent again.
outputFloor :: Integer
outputFloor = 1_500_000

-- | Cap on inputs when consolidating, so a recovery tx cannot outgrow maxTxSize.
maxConsolidationInputs :: Int
maxConsolidationInputs = 100

-- | Select inputs for a derived-shape tx: the @target@ largest funds, extended
-- with the next largest until they cover the fee and leave every one of
-- @target@ outputs above 'outputFloor'.
--
-- Selecting by value is the whole point. 'takeInputs' picks by 'TxIn' order,
-- which is effectively random, so it can pick @target@ dust entries and split
-- their sum @target@ ways — producing finer dust, and repeating until the
-- selection cannot cover a fee. Taking the largest instead never sharpens the
-- spiral, strands whatever dust exists rather than compounding it, and
-- consolidates when the top funds alone are not enough.
takeLargest ::
  Int -> Integer -> Map TxIn Integer -> ([(TxIn, Integer)], Map TxIn Integer)
takeLargest target fee m = go [] 0 byValue
 where
  byValue = sortOn (negate . snd) (Map.toList m)
  needed = fee + fromIntegral target * outputFloor
  done acc = (reverse acc, foldr (Map.delete . fst) m acc)
  go acc total rest
    -- Enough value, and at least the target shape (or everything we have).
    | total >= needed, length acc >= min target (Map.size m) = done acc
    | length acc >= maxConsolidationInputs = done acc
    | otherwise = case rest of
        [] -> done acc
        (entry : more) -> go (entry : acc) (total + snd entry) more

addOutput :: Fund -> Map TxIn Integer -> Map TxIn Integer
addOutput f = Map.insert (fundTxIn f) (fundValue f)

-- | Emit one JSON line in the cardano-node trace schema
-- (@{at, sev, host, thread, ns, data}@). The proto-devnet Alloy
-- pipeline turns @ns@ into a Loki label, so Grafana can filter on
-- @TxFirehose.Submit.Success@ etc.
trace :: Text -> Text -> Value -> IO ()
trace ns sev dat = do
  now <- getCurrentTime
  BSL.hPutStrLn stderr (Aeson.encode (payload now))
 where
  payload now =
    Aeson.object
      [ "at" .= T.pack (iso8601Show now)
      , "sev" .= sev
      , "host" .= ("tx-firehose" :: Text)
      , "thread" .= ("main" :: Text)
      , "ns" .= ns
      , "data" .= dat
      ]

loadSigningKey :: FilePath -> IO (SigningKey Api.PaymentKey)
loadSigningKey path = do
  result <- Api.readFileTextEnvelopeAnyOf accepted (File path)
  case result of
    Left err ->
      die $
        "tx-firehose: cannot read signing key "
          ++ show path
          ++ ": "
          ++ show err
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
    Left err ->
      die $
        "tx-firehose: cannot read stake key "
          ++ show path
          ++ ": "
          ++ show err
    Right (sk :: SigningKey Api.StakeKey) -> pure (Api.getVerificationKey sk)

deriveAddress ::
  ShelleyBasedEra era ->
  NetworkId ->
  SigningKey Api.PaymentKey ->
  Maybe (Api.VerificationKey Api.StakeKey) ->
  AddressInEra era
deriveAddress sbe networkId sk mStakeVk =
  Api.shelleyAddressInEra sbe $
    Api.makeShelleyAddress
      networkId
      ( PaymentCredentialByKey
          (Api.verificationKeyHash (Api.getVerificationKey sk))
      )
      stakeRef
 where
  stakeRef = case mStakeVk of
    Nothing -> NoStakeAddress
    Just vk ->
      Api.StakeAddressByValue
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
queryFundsInEra ::
  ShelleyBasedEra era ->
  LocalNodeConnectInfo ->
  AddressAny ->
  IO (Map TxIn Integer)
queryFundsInEra sbe connInfo addrAny = do
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
  q =
    QueryInEra
      ( QueryInShelleyBasedEra
          sbe
          (QueryUTxO (QueryUTxOByAddress (Set.singleton addrAny)))
      )
  toLovelace :: Api.TxOut Api.CtxUTxO era -> Integer
  toLovelace (Api.TxOut _ v _ _) = c
   where
    Coin c = Api.txOutValueToLovelace v
