{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Growth (Options (..), parser, validateOptions, withJournal, run) where

import Cardano.Api qualified as Api
import Cardano.Benchmarking.TxFirehose.Color (Color, colorHex)
import Cardano.Benchmarking.TxFirehose.Growth.State
import Cardano.Benchmarking.TxFirehose.Tx qualified as Tx
import Cardano.Ledger.Api qualified as L
import Control.Concurrent (threadDelay)
#ifndef mingw32_HOST_OS
import Control.Exception (bracket)
#endif
import Control.Monad (foldM, unless, when)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson ((.=))
import Data.Aeson qualified as JSON
import Data.Aeson.Types (Pair)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BSL
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (Down))
import Data.Set qualified as Set
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Lens.Micro ((^.))
import Options.Applicative qualified as O
#ifdef mingw32_HOST_OS
import System.Directory (createDirectoryIfMissing, doesFileExist)
#else
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
#endif
import System.Exit (die, exitSuccess)
import System.FileLock (SharedExclusive (Exclusive), withTryFileLock)
import System.FilePath (takeDirectory)
import System.IO (stderr)
#ifndef mingw32_HOST_OS
import System.Posix.Files (setFileMode)
import System.Posix.IO (OpenMode (ReadOnly), closeFd, defaultFileFlags, openFd)
import System.Posix.Unistd (fileSynchronise)
#endif

-- The target is net additional entries, including lane initialization.
data Options = Options
  { growBy :: Integer
  , stateFile :: FilePath
  , retainedKey :: FilePath
  , laneCount :: Int
  , fanout :: Int
  , outputLovelace :: Integer
  , pipelineDepth :: Int
  , pollSeconds :: Int
  , dryRun :: Bool
  , drain :: Bool
  }
  deriving Show

parser :: O.Parser Options
parser =
  Options
    <$> O.option
      O.auto
      ( O.long "grow-by"
          <> O.metavar "COUNT"
          <> O.help "Grow by this many net UTxOs, including lane setup; exclusive with ordinary load options"
      )
    <*> O.strOption
      ( O.long "growth-state"
          <> O.metavar "FILE"
          <> O.help "Durable resume journal; keep this file and use the same funding key"
      )
    <*> O.strOption
      ( O.long "retained-verification-key-file"
          <> O.metavar "FILE"
          <> O.help "Separate payment verification key for retained outputs; keep its signing key for cleanup"
      )
    <*> O.option
      O.auto
      ( O.long "growth-lanes"
          <> O.value 64
          <> O.showDefault
          <> O.help "Maximum prefunded lanes; one funding UTxO is split into up to 64 lanes"
      )
    <*> O.option
      O.auto
      ( O.long "fanout"
          <> O.value 200
          <> O.showDefault
          <> O.help "Maximum retained outputs per batch; reduced to fit signed maxTxSize and funds"
      )
    <*> O.option
      O.auto
      ( O.long "retained-lovelace"
          <> O.value 2000000
          <> O.showDefault
          <> O.help "Lovelace per retained output; checked against queried minimum UTxO"
      )
    <*> O.option
      O.auto
      ( O.long "pipeline-depth"
          <> O.value 64
          <> O.showDefault
          <> O.help "Maximum unfinalized batches per lane; tune for immutable-tip delay"
      )
    <*> O.option
      O.auto
      ( O.long "growth-poll-seconds"
          <> O.value 5
          <> O.showDefault
          <> O.help "Delay between bounded growth rounds and confirmation polls"
      )
    <*> O.switch
      ( O.long "growth-dry-run"
          <> O.help
            "Query funding and parameters, print signed size/fee calibration, then exit without writing a journal or submitting"
      )
    <*> O.switch
      ( O.long "growth-drain"
          <> O.help
            "Resume an existing journal with original arguments; retry only recorded work and exit when immutable, even below target; incompatible with --growth-dry-run"
      )

validateOptions :: Options -> IO ()
validateOptions opts = do
  when (drain opts && dryRun opts) $
    die "--growth-drain and --growth-dry-run are mutually exclusive"
  when
    ( growBy opts <= 0
        || laneCount opts < 1
        || laneCount opts > 4096
        || fanout opts < 1
        || fanout opts > 65534
        || outputLovelace opts <= 0
        || pipelineDepth opts < 1
        || pollSeconds opts < 1
    )
    $ die "Invalid growth options: positive values required; lanes <= 4096, fanout <= 65534"

-- Keep loading/identity validation common to ordinary resume and drain. Check
-- drain prerequisites before creating even the parent directory or lock file,
-- then recheck the checkpoint under the lock before considering initialization.
withJournal :: Options -> Identity -> IO Journal -> (Journal -> IO a) -> IO a
withJournal opts ident initialize action = do
  validateOptions opts
  when (drain opts) $ do
    exists <- doesFileExist (stateFile opts)
    walExists <- doesFileExist (stateFile opts ++ ".wal")
    unless (exists && walExists) $
      die "--growth-drain requires an existing checkpoint and .wal; no state created"
  createDirectoryIfMissing True (takeDirectory (stateFile opts))
  locked <- withTryFileLock (stateFile opts ++ ".lock") Exclusive $ \_ -> do
    exists <- doesFileExist (stateFile opts)
    journal <-
      if exists
        then loadJournal (stateFile opts)
        else
          if drain opts
            then die "--growth-drain requires an existing checkpoint; no state created"
            else initialize
    unless (identity journal == ident) $
      die "Growth journal configuration mismatch; resume with the original key/network/era/target/shape"
    when (confirmed journal < 0 || confirmed journal + pendingGrowth journal > target ident) $
      die "Invalid journal progress"
    action journal
  case locked of
    Nothing -> die "Another process holds the growth journal lock"
    Just result -> pure result

syncFile :: FilePath -> IO ()
#ifdef mingw32_HOST_OS
syncFile _ = die "Durable growth journals require a POSIX host (Linux/macOS)"
#else
syncFile file = bracket (openFd file ReadOnly defaultFileFlags) closeFd fileSynchronise
#endif

checkpoint :: FilePath -> Journal -> IO ()
#ifdef mingw32_HOST_OS
checkpoint _ _ = die "Durable growth journals require a POSIX host (Linux/macOS)"
#else
checkpoint path journal = do
  let tmp = path ++ ".tmp"
      wal = path ++ ".wal"
  -- Create the WAL before the first main-file checkpoint, without truncating
  -- an existing WAL until the replacement checkpoint is safely persisted.
  BSL.appendFile wal ""
  setFileMode wal 0o600
  syncFile wal
  BSL.writeFile tmp ""
  setFileMode tmp 0o600
  BSL.writeFile tmp (JSON.encode journal)
  syncFile tmp
  renameFile tmp path
  syncFile (takeDirectory path)
  BSL.writeFile wal ""
  syncFile wal
#endif

recordLane :: FilePath -> Int -> Lane -> Journal -> IO Journal
recordLane path index updated journal = do
  let next =
        journal
          { revision = revision journal + 1
          , lanes = take index (lanes journal) ++ [updated] ++ drop (index + 1) (lanes journal)
          }
      delta = LaneDelta (revision next) index updated
      wal = path ++ ".wal"
  BSL.appendFile wal (JSON.encode delta <> "\n")
  syncFile wal -- Must complete before submitting this transaction.
  pure next

loadJournal :: FilePath -> IO Journal
loadJournal path = do
  journal <- BSL.readFile path >>= require . JSON.eitherDecode
  let wal = path ++ ".wal"
  exists <- doesFileExist wal
  unless exists $ die "Missing growth .wal file; restore the complete journal before resuming"
  raw <- BS.readFile wal
  -- An incomplete final record cannot have passed the pre-submission fsync.
  -- Keep complete records (including a fully written uncertain submission).
  let complete = case BS.elemIndices 10 raw of
        [] -> BS.empty
        positions -> BS.take (last positions + 1) raw
  foldM apply journal (BSC.lines complete)
 where
  apply journal line = do
    delta <- require (JSON.eitherDecodeStrict' line)
    require (applyDelta journal delta)

logEvent :: String -> [Pair] -> IO ()
logEvent event fields = do
  now <- getCurrentTime
  BSL.hPutStr stderr $
    JSON.encode
      ( JSON.object
          [ "at" .= iso8601Show now
          , "sev" .= ("Info" :: String)
          , "host" .= ("localhost" :: String)
          , "thread" .= ("growth" :: String)
          , "ns" .= ("TxFirehose.Growth." ++ event)
          , "data" .= JSON.object fields
          ]
      )
      <> "\n"

require :: Either String a -> IO a
require = either die pure

run ::
  forall era.
  Api.ShelleyBasedEraConstraints era =>
  Api.ShelleyBasedEra era ->
  Options ->
  Double ->
  Api.LocalNodeConnectInfo ->
  Api.NetworkId ->
  Api.SigningKey Api.PaymentKey ->
  Api.AddressInEra era ->
  Color ->
  IO ()
run sbe opts tps conn networkId sk changeAddr color = do
  validateOptions opts
  vk <- Api.readFileTextEnvelope (Api.File (retainedKey opts)) >>= either (die . show) pure
  let retainedAddr =
        Api.shelleyAddressInEra sbe $
          Api.makeShelleyAddress
            networkId
            (Api.PaymentCredentialByKey (Api.verificationKeyHash (vk :: Api.VerificationKey Api.PaymentKey)))
            Api.NoStakeAddress
      keyHash = Api.verificationKeyHash (Api.getVerificationKey sk)
  when (keyHash == Api.verificationKeyHash vk) $
    die "Use a distinct payment key for retained outputs, outside normal firehose discovery"
  let ident =
        Identity
          3
          (show networkId)
          (show sbe)
          (show changeAddr)
          (show retainedAddr)
          (colorHex color)
          (growBy opts)
          (laneCount opts)
          (fanout opts)
          (outputLovelace opts)
  when (dryRun opts) $ do
    _ <- initialize ident retainedAddr False
    exitSuccess
  withJournal opts ident (initialize ident retainedAddr True) $ \journal -> do
    -- Rebuild and verify every pending transaction before contacting submission.
    mapM_ (replay retainedAddr) (lanes journal)
    save journal
    ready <- if seeded journal then pure journal else seedLoop journal
    loop retainedAddr ready Set.empty (0 :: Int)
 where
  wait = threadDelay (pollSeconds opts * 1000000)
  save = checkpoint (stateFile opts)
  asAny (Api.AddressInEra _ address) = Api.toAddressAny address
  query targetPoint filterBy = do
    result <-
      runExceptT $
        Api.queryNodeLocalState conn targetPoint $
          Api.QueryInEra (Api.QueryInShelleyBasedEra sbe (Api.QueryUTxO filterBy))
    case result of
      Left err -> die (show err)
      Right (Left err) -> die (show err)
      Right (Right (Api.UTxO utxo)) -> pure utxo
  queryRefs point funds =
    query point $
      Api.QueryUTxOByTxIn $
        Set.fromList (map reference funds)
  parameters = do
    result <-
      runExceptT $
        Api.queryNodeLocalState conn Api.VolatileTip $
          Api.QueryInEra (Api.QueryInShelleyBasedEra sbe Api.QueryProtocolParameters)
    case result of
      Left err -> die (show err)
      Right (Left err) -> die (show err)
      Right (Right pp) -> pure pp
  txFee tx = case Tx.btxSigned tx of
    Api.ShelleyTx _ ledgerTx -> Api.unCoin (ledgerTx ^. L.bodyTxL . L.feeTxBodyL)
  seedTx j = case seed j of
    Nothing -> Left "No seed transaction in prefunded-lane journal"
    Just plan ->
      Tx.buildTx
        sbe
        changeAddr
        sk
        [restoreFund (seedInput plan)]
        (fromIntegral (length (lanes j)))
        (Api.Coin (seedFee plan))
        color
  seedFunds j = maybe [] (pure . seedInput) (seed j)
  submit tx = do
    result <- Api.submitTxToNodeLocal conn (Api.TxInMode sbe (Tx.btxSigned tx))
    threadDelay (max 1 (round (1000000 / tps)))
    case result of
      Api.TxSubmitSuccess -> do
        logEvent "Accepted" ["txId" .= Tx.btxId tx, "serializedSize" .= Tx.btxSerializedSize tx]
        pure True
      Api.TxSubmitFail err -> do
        logEvent "Retry" ["txId" .= Tx.btxId tx, "reason" .= show err]
        pure False
      Api.TxSubmitError err -> do
        logEvent "ConnectionError" ["reason" .= show err]
        pure False
  initialize ident retainedAddr persist = do
    walExists <- doesFileExist (stateFile opts ++ ".wal")
    when (persist && walExists) $ do
      walBytes <- BS.readFile (stateFile opts ++ ".wal")
      unless (BS.null walBytes) $ die "Orphaned growth WAL: restore its checkpoint before resuming"
    -- One initial address query. Prefunded lanes can start from selected-chain
    -- outputs immediately; their first immutable observation establishes the anchor.
    utxo <- query Api.VolatileTip (Api.QueryUTxOByAddress (Set.singleton (asAny changeAddr)))
    let candidates =
          sortOn
            (Down . lovelace)
            [ SavedFund ref (Api.unCoin coin)
            | (ref, Api.TxOut _ value Api.TxOutDatumNone Api.ReferenceScriptNone) <- Map.toList utxo
            , let coin = Api.txOutValueToLovelace value
            , Api.txOutValueToValue value == Api.lovelaceToValue coin
            ]
    when (null candidates) $
      die "No ADA-only, datum/script-free funding UTxO at the funding address"
    pp <- parameters
    j <- case candidates of
      [input] -> do
        let n = min (toInteger (min 64 (laneCount opts))) (growBy opts + 1)
        tx <- require $ Tx.autoFee sbe pp $ \feeCoin ->
          Tx.buildTx sbe changeAddr sk [restoreFund input] (fromInteger n) feeCoin color
        pure $
          Journal
            0
            ident
            (Just (SeedPlan input (txFee tx)))
            False
            0
            [Lane (saveFund f) [] False | f <- Tx.btxOutputs tx]
      inputs ->
        pure $
          Journal
            0
            ident
            Nothing
            True
            0
            [Lane input [] False | input <- take (laneCount opts) inputs]
    let available = sum (map (lovelace . anchor) (lanes j))
    when (available <= growBy opts * outputLovelace opts) $
      die
        "Selected funding lanes cannot cover retained outputs plus fees; fund larger outputs or increase --growth-lanes"
    -- Calibrate against the actual signed serialization before persisting/submitting.
    let remaining = growBy opts - pendingGrowth j
    when (remaining > 0) $ do
      sample <-
        require $
          Tx.fitFanout
            sbe
            pp
            changeAddr
            retainedAddr
            sk
            (restoreFund (anchor (head (lanes j))))
            (fromInteger (min remaining (toInteger (fanout opts))))
            (outputLovelace opts)
            color
      logEvent
        "Calibration"
        [ "retainedOutputs" .= (length (Tx.btxOutputs sample) - 1)
        , "serializedSize" .= Tx.btxSerializedSize sample
        , "feeLovelace" .= txFee sample
        , "maxTxSize" .= (pp ^. L.ppMaxTxSizeL)
        , "retainedLovelace" .= outputLovelace opts
        ]
    when persist (save j) -- Must precede the first submission.
    logEvent
      "Initialized"
      [ "growBy" .= growBy opts
      , "lanes" .= length (lanes j)
      , "prefunded" .= seeded j
      , "fundingLovelace" .= available
      ]
    pure j
  seedLoop j = do
    tx <- require (seedTx j)
    unless (map saveFund (Tx.btxOutputs tx) == map anchor (lanes j)) $
      die "Seed transaction no longer reproduces the journal"
    -- Start pipelining as soon as the seed is on the selected chain. Its
    -- original input remains in the journal until immutable state proves it.
    volatile <- Map.keysSet <$> queryRefs Api.VolatileTip (seedFunds j ++ concatMap laneFunds (lanes j))
    if all (either (const False) (const True) . observedPrefix volatile) (lanes j)
      then pure j
      else
        if any ((`Set.member` volatile) . reference) (seedFunds j)
          then do
            pp <- parameters
            _ <- require (Tx.validateTx sbe pp tx)
            _ <- submit tx
            logEvent "WaitingForSeedInclusion" []
            wait
            seedLoop j
          else die "Seed input/known changes disappeared; check external spending before resuming"
  replay retainedAddr lane = reverse . snd <$> foldM build (anchor lane, []) (pending lane)
   where
    build (input, built) batch = do
      tx <-
        require $
          Tx.buildFanout
            sbe
            changeAddr
            retainedAddr
            sk
            (restoreFund input)
            (count batch)
            (outputLovelace opts)
            (Api.Coin (fee batch))
            color
      unless (saveFund (last (Tx.btxOutputs tx)) == change batch) $
        die "Pending transaction does not reproduce journal txid/change; refusing replacement"
      pure (change batch, tx : built)
  loop retainedAddr journal accepted roundNo = do
    let refs = seedFunds journal ++ concatMap laneFunds (lanes journal)
    stable <- Map.keysSet <$> queryRefs Api.ImmutableTip refs
    j <- require $ reconcile stable journal
    when (j /= journal) (save j)
    logEvent
      "Progress"
      [ "finalizedGrowth" .= confirmed j
      , "pendingGrowth" .= pendingGrowth j
      , "targetGrowth" .= growBy opts
      ]
    if finished (drain opts) j
      then
        if drain opts
          then
            logEvent
              "Drained"
              [ "finalizedGrowth" .= confirmed j
              , "pendingGrowth" .= pendingGrowth j
              , "targetGrowth" .= target (identity j)
              ]
          else logEvent "Complete" ["finalizedGrowth" .= confirmed j]
      else do
        volatile <- Map.keysSet <$> queryRefs Api.VolatileTip (seedFunds j ++ concatMap laneFunds (lanes j))
        if not (seeded j) && any ((`Set.member` volatile) . reference) (seedFunds j)
          then do
            -- A rollback can remove the lane-setup transaction as well as its
            -- descendants. Replay the same setup before resuming those lanes.
            ready <- seedLoop j
            loop retainedAddr ready Set.empty 0
          else do
            pp <- parameters -- Refresh at every round, including on resume.
            let pendingRefs = Set.fromList (map (reference . change) (concatMap pending (lanes j)))
                -- Retry identical transactions after eviction/rejection/disconnect.
                cached = if roundNo * pollSeconds opts >= 60 then Set.empty else Set.intersection accepted pendingRefs
            (next, sent) <- foldM (stepLane retainedAddr pp volatile) (j, cached) [0 .. length (lanes j) - 1]
            when (not (drain opts) && next == j && pendingGrowth j == 0) $
              die "No funding lane can make progress; check calibration/funding and protocol parameters"
            wait
            loop retainedAddr next sent (if roundNo * pollSeconds opts >= 60 then 0 else roundNo + 1)
  stepLane retainedAddr pp volatile (j, accepted) index = do
    let lane = lanes j !! index
    prefix <- require (observedPrefix volatile lane)
    built <- replay retainedAddr lane
    (ok, sent) <- foldM sendPending (True, accepted) (drop prefix (zip (pending lane) built))
    let remaining = remainingToAllocate (drain opts) j
    if not ok || remaining <= 0 || length (pending lane) >= pipelineDepth opts
      then pure (j, sent)
      else do
        let input = last (laneFunds lane)
        case Tx.fitFanout
          sbe
          pp
          changeAddr
          retainedAddr
          sk
          (restoreFund input)
          (fromInteger (min remaining (toInteger (fanout opts))))
          (outputLovelace opts)
          color of
          Left err -> do
            logEvent "LaneUnavailable" ["lane" .= index, "reason" .= err]
            pure (j, sent)
          Right tx -> do
            let batch = Batch (length (Tx.btxOutputs tx) - 1) (txFee tx) (saveFund (last (Tx.btxOutputs tx)))
                updated = lane{pending = pending lane ++ [batch]}
            next <- recordLane (stateFile opts) index updated j
            success <- submit tx
            pure (next, if success then Set.insert (reference (change batch)) sent else sent)
   where
    sendPending (False, sent) _ = pure (False, sent)
    sendPending (True, sent) (batch, tx)
      | reference (change batch) `Set.member` sent = pure (True, sent)
      | otherwise = do
          _ <- require (Tx.validateTx sbe pp tx)
          success <- submit tx
          pure (success, if success then Set.insert (reference (change batch)) sent else sent)
