{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Testnet.Status.Run (
    runCheckStatusOptions,
    checkStatus,
    blocksBehind,
    calcNodeState,
    classifyNetwork,
    newestTip,
    renderStatusReportHumanReadable,
) where

import           Cardano.Api (BlockNo (..), CardanoEra (..), ChainTip (..),
                   ConsensusModeParams (..), EpochNo (..), LocalNodeConnectInfo (..),
                   LocalTxMonitoringQuery (..), LocalTxMonitoringResult (..),
                   MempoolSizeAndCapacity (..), NetworkId (..), NetworkMagic (..), ShelleyGenesis,
                   SlotNo (..), SlotsInEpoch (..), SlotsToEpochEnd (..), SocketPath, Target (..),
                   executeLocalStateQueryExpr, getLocalChainTip, mapFile, queryCurrentEra,
                   queryEraHistory, queryTxMonitoringLocal, slotToEpoch)

import qualified Cardano.Ledger.Shelley.API as SL
import           Cardano.Node.Testnet.Paths (defaultGenesisFilepath, defaultManifestFile)
import           Cardano.Prelude (Nat, NonEmpty, Word32, toList, transpose)

import           Prelude

import           Data.Aeson (FromJSON, eitherDecodeFileStrict')
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable (maximumBy)
import           Data.List.NonEmpty (nonEmpty)
import           Data.Maybe (fromMaybe, isNothing, mapMaybe)
import           Data.Ord (comparing)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import           Data.Word (Word64)
import           GHC.Stack (HasCallStack)
import           Prettyprinter (Doc, concatWith, hardline, parens, pretty, softline)
import           Prettyprinter.Render.Text (putDoc)
import           Prettyprinter.Util (reflow)
import           System.Directory (doesDirectoryExist, doesFileExist)
import           System.Exit (exitWith)
import           System.FilePath (normalise, (</>))
import           System.Timeout (timeout)
import           Text.Printf (printf)

import           Testnet.ChainWatchdog (chainForecastHorizon, chainStallTimeoutFromHorizon)
import           Testnet.Manifest (Manifest (..), ManifestGenesisFiles (..), ManifestNetwork (..),
                   ManifestNode (..), ManifestNodeRole (..), ManifestPaths (..))
import           Testnet.Signal (isProcessAlive)
import           Testnet.Start.Types (anyEraToString, defaultTestnetDir)
import           Testnet.Status.Types (ChainInfo (..), CheckStatusOptions (..), MempoolInfo (..),
                   NetworkStatus (..), NoTestnetReason (..), NodeAnswer (..), NodeProbeResult (..),
                   NodeState (..), OutputFormat (..), StatusReport (..), TipInfo (..),
                   exitCodeForStatus)
import           Testnet.Types (sgSlotLength, sgSystemStart, testnetEpochSlots)

import           RIO (displayException, hFlush, stdout, throwString, tryAny)
import           UnliftIO.Async (async, forConcurrently, wait)

-- | Run the @status@ command: print the report (a table, or JSON) on stdout
-- and exit with 0 (running or degraded), 3 (stalled), 4 (stopped) or 5 (no
-- testnet). When the manifest or the genesis file cannot be read, print the
-- error on stderr and exit with 1.
runCheckStatusOptions :: HasCallStack => CheckStatusOptions -> IO ()
runCheckStatusOptions
  CheckStatusOptions
    { testnetDir = dir
    , statusCheckTimeout = timeoutSeconds
    , outputFormat = format
    } = do
      status <- checkStatus (fromMaybe defaultTestnetDir dir) timeoutSeconds
      case format of
        OutputFormatText -> putDoc $ renderStatusReportHumanReadable status
        OutputFormatJson -> do LBS.putStr $ encodePretty status
                               putStrLn ""
      hFlush stdout
      exitWith . exitCodeForStatus $ reportStatus status

-- | Check the testnet in the given output directory. All nodes are asked for
-- their tips at the same time. Then, also at the same time, each node that
-- answered is asked about its mempool, and the node with the best tip is asked
-- where the chain is. Each query gets the given number of seconds, so the
-- check takes at most twice that long. It throws when the check itself
-- fails: when the manifest, or the shelley genesis it points to, cannot be
-- read.
checkStatus :: HasCallStack => FilePath -> Nat -> IO StatusReport
checkStatus outputDir timeoutSeconds = do
  let manifestFile = outputDir </> defaultManifestFile
  hasManifest <- doesFileExist manifestFile
  if not hasManifest
    then noTestnetReport outputDir
    else do
      manifest@Manifest{manifestPaths = ManifestPaths{mpGenesisFiles = ManifestGenesisFiles{mgfShelley = shelleyGenesisFile}}}
        <- readJsonFile manifestFile
      genesis <- readJsonFile $ outputDir </> shelleyGenesisFile
      checkNodes outputDir timeoutSeconds manifest genesis
 where
   readJsonFile :: (HasCallStack, FromJSON a) => String -> IO a
   readJsonFile file =
     either (\e -> throwString $ "cannot decode " <> file <> ": " <> e) pure
       =<< eitherDecodeFileStrict' file

checkNodes :: FilePath -> Nat -> Manifest -> ShelleyGenesis -> IO StatusReport
checkNodes outputDir timeoutSeconds manifest@(Manifest{manifestNetwork = ManifestNetwork{mnMagic = magic}}) genesis = do
  checkedAt <- getCurrentTime
  nodes <- forConcurrently (manifestNodes manifest) $
              probeNode outputDir timeoutSeconds magic limit slotStart
  let probed = zip (toList $ manifestNodes manifest) (toList nodes)
      tips = map probeNodeTipInfo $ toList nodes
      bestTip = newestTip . mapMaybe probeNodeTipInfo $ toList nodes
  chainQuery <- async $
    case [ (node, tip) | (node, probe) <- probed, Just tip <- [probeNodeTipInfo probe], Just tip == bestTip ] of
      (node, tip) : _ -> askChainInfo timeoutSeconds (connectInfoOf node) (tipSlot tip)
      [] -> pure Nothing
  mempools <- forConcurrently probed $ \(node, probe) ->
    if isNothing (probeError probe)
      then askMempool timeoutSeconds (connectInfoOf node)
      else pure Nothing
  chain <- wait chainQuery
  pure StatusReport
         { reportOutputDir = outputDir
         , reportCheckedAt = checkedAt
         , reportStatus = classifyNetwork (probeNodeState <$> nodes)
         , reportBestTip = bestTip
         , reportNodes =
             [ probe{probeBlocksBehind = behind, probeMempool = mempool}
             | (probe, behind, mempool) <- zip3 (toList nodes) (blocksBehind tips) mempools
             ]
         , reportProblemExplanation = Nothing
         , reportChain = chain
         }
  where
    -- The chain watchdog's threshold for a dead chain: one number for both.
    limit = chainStallTimeoutFromHorizon (chainForecastHorizon genesis)
    slotLength = SL.fromNominalDiffTimeMicro (sgSlotLength genesis)
    slotStart (SlotNo slot) = addUTCTime (fromIntegral slot * slotLength) (sgSystemStart genesis)
    connectInfoOf ManifestNode{mnodeSocketPath = socketPath} = nodeConnectInfo outputDir magic socketPath

-- | Check the status of one node in the testnet.
probeNode
  :: FilePath -- ^ output directory
  -> Nat -- ^ timeout, in seconds
  -> Word32 -- ^ network magic
  -> NominalDiffTime -- ^ tip age limit
  -> (SlotNo -> UTCTime) -- ^ start time of a slot
  -> ManifestNode
  -> IO NodeProbeResult
probeNode outputDir timeoutSeconds magic limit slotStart
          ManifestNode
            { mnodeName = nodeName
            , mnodeRole = role
            , mnodePid = pid
            , mnodeSocketPath = socketPath
            , mnodeStdoutFile = stdoutFile
            , mnodeStderrFile = stderrFile
            } = do
  answer <- askTip
  pidAlive <- maybe (pure Nothing) isProcessAlive pid
  pure NodeProbeResult
         { probeNodeName = nodeName
         , probeNodeRole = role
         , probePid = pid
         , probePidIsAlive = pidAlive
         , probeNodeState = calcNodeState limit pidAlive answer
         , probeNodeTipInfo = case answer of
                                AnsweredTip tip -> Just tip
                                AnsweredNoBlocks -> Nothing
                                NoAnswer _ -> Nothing
         , probeError = case answer of
                          NoAnswer reason -> Just reason
                          _ -> Nothing
         , probeStdoutFile = stdoutFile
         , probeStderrFile = stderrFile
         , probeBlocksBehind = Nothing -- set by 'checkNodes'
         , probeMempool = Nothing -- set by 'checkNodes'
         }
  where
    askTip :: IO NodeAnswer
    askTip = do
      -- Any failure (no socket, refused, handshake error) counts as no answer.
     result <- tryAny $ timeout (fromIntegral (timeoutSeconds * 1_000_000)) (getLocalChainTip connectInfo)
     answeredAt <- getCurrentTime
     pure $ case result of
              Right (Just (ChainTip slot hash blockNo)) ->
                AnsweredTip TipInfo
                              { tipSlot = slot
                              , tipBlockNo = blockNo
                              , tipHash = hash
                              , tipAge = answeredAt `diffUTCTime` slotStart slot
                              }
              Right (Just ChainTipAtGenesis) -> AnsweredNoBlocks
              Right Nothing -> NoAnswer $ "no answer within " <> show timeoutSeconds <> "s"
              Left e -> NoAnswer $ displayException e

    connectInfo :: LocalNodeConnectInfo
    connectInfo = nodeConnectInfo outputDir magic socketPath

-- | How to connect to a node of the testnet in the given output directory.
nodeConnectInfo :: FilePath -> Word32 -> SocketPath -> LocalNodeConnectInfo
nodeConnectInfo outputDir magic socketPath =
  LocalNodeConnectInfo
    { localNodeSocketPath = mapFile (outputDir </>) socketPath
    , localNodeNetworkId = Testnet (NetworkMagic $ fromIntegral magic)
    , localConsensusModeParams = CardanoModeParams testnetEpochSlots
    }

-- | Ask a node what is in its mempool, on a connection of its own.
-- 'Nothing' when it does not answer in time, or fails.
askMempool :: Nat -> LocalNodeConnectInfo -> IO (Maybe MempoolInfo)
askMempool timeoutSeconds connectInfo = do
  result <- tryAny . timeout (fromIntegral (timeoutSeconds * 1_000_000)) $
    queryTxMonitoringLocal connectInfo LocalTxMonitoringMempoolInformation
  pure $ case result of
    Right (Just (LocalTxMonitoringMempoolSizeAndCapacity sizes _)) ->
      Just MempoolInfo
             { mempoolTxs = numberOfTxs sizes
             , mempoolBytes = sizeInBytes sizes
             , mempoolCapacityBytes = capacityInBytes sizes
             }
    _ -> Nothing

-- | Ask a node where the chain is: the era it is in, and the epoch of the
-- given slot. 'Nothing' when it does not answer in time, or cannot tell.
askChainInfo :: Nat -> LocalNodeConnectInfo -> SlotNo -> IO (Maybe ChainInfo)
askChainInfo timeoutSeconds connectInfo slot = do
  result <- tryAny . timeout (fromIntegral (timeoutSeconds * 1_000_000)) $
    executeLocalStateQueryExpr connectInfo VolatileTip $ do
      era <- queryCurrentEra
      eraHistory <- queryEraHistory
      pure $ (,) <$> era <*> eraHistory
  pure $ case result of
    Right (Just (Right (Right (era, eraHistory)))) ->
      case slotToEpoch slot eraHistory of
        Right (epoch, SlotsInEpoch inEpoch, SlotsToEpochEnd toEnd) ->
          Just ChainInfo
                 { chainEra = era
                 , chainEpoch = epoch
                 , chainSlotInEpoch = inEpoch
                 , chainSlotsToEpochEnd = toEnd
                 }
        Left _ -> Nothing
    _ -> Nothing

-- | The rule for the whole network.
classifyNetwork :: NonEmpty NodeState -> NetworkStatus
classifyNetwork states
  | all (== NodeOk) states = NetworkRunning
  | NodeOk `elem` states = NetworkDegraded
  | NodeStalled `elem` states = NetworkStalled
  | otherwise = NetworkStopped

-- | The rule for one node. The answer decides; the pid only tells "down"
-- (the process is gone) from "unreachable" (alive, or unknown).
calcNodeState
  :: NominalDiffTime -- ^ tip age limit
  -> Maybe Bool -- ^ whether the node's process is alive, if known
  -> NodeAnswer
  -> NodeState
calcNodeState limit _pidAlive (AnsweredTip tip)
    | tipAge tip <= limit = NodeOk
    | otherwise = NodeStalled
calcNodeState _limit _pidAlive AnsweredNoBlocks = NodeStalled
calcNodeState _limit pidAlive (NoAnswer _)
    | pidAlive == Just False = NodeDown
    | otherwise = NodeUnreachable

-- | The rule for the best tip: the newest one, which is the one with the
-- highest slot. The ages cannot tell: each age is measured when its node
-- answered, and the nodes answer at different times.
newestTip :: [TipInfo] -> Maybe TipInfo
newestTip = fmap (maximumBy (comparing tipSlot)) . nonEmpty

-- | How many blocks each tip is behind the tip with the most blocks: how many
-- blocks its node is missing. A node without a tip has no number.
blocksBehind :: [Maybe TipInfo] -> [Maybe Word64]
blocksBehind tips = map (fmap behind) tips
  where
    most = maximum (0 : [unBlockNo (tipBlockNo tip) | Just tip <- tips])
    behind tip = most - unBlockNo (tipBlockNo tip)

-- | The 'StatusReport' when there is no manifest, with a hint about why.
noTestnetReport :: FilePath -> IO StatusReport
noTestnetReport outputDir = do
  now <- getCurrentTime
  dirExists <- doesDirectoryExist outputDir
  hasGenesis <- doesFileExist $ outputDir </> defaultGenesisFilepath ShelleyEra
  let explanation
        | not dirExists = DirMissing
        | hasGenesis = NotReady
        | otherwise = NotATestnetDir
  pure StatusReport
         { reportOutputDir = outputDir
         , reportCheckedAt = now
         , reportStatus = NoTestnet
         , reportBestTip = Nothing
         , reportNodes = []
         , reportProblemExplanation = Just explanation
         , reportChain = Nothing
         }

-- | Render a 'StatusReport' in a human readable way.
renderStatusReportHumanReadable :: StatusReport -> Doc ann
renderStatusReportHumanReadable
  StatusReport
    { reportStatus = status
    , reportBestTip = bestTip
    , reportNodes = nodes
    , reportOutputDir = outputDir
    , reportProblemExplanation = problemExplanation
    , reportChain = chain
    } =
  pretty (T.pack ("Network: " <> summary status)) <> hardline <> chainLine <> details <> hardline <> errors
  where
    notOk = length $ filter ((/= NodeOk) . probeNodeState) nodes

    seconds :: NominalDiffTime -> String
    seconds s = printf "%.1fs" (realToFrac s :: Double)

    summary :: NetworkStatus -> String
    summary NetworkRunning =
      "RUNNING - chain is producing blocks"
        <> foldMap
             ( \TipInfo
                 { tipSlot = slotNo
                 , tipBlockNo = blockNo
                 , tipAge = age
                 } -> " (tip: slot "
                      <> show (unSlotNo slotNo)
                      <> ", block "
                      <> show (unBlockNo blockNo)
                      <> ", "
                      <> seconds age
                      <> " old)"
                )
                bestTip
    summary NetworkDegraded =
      "DEGRADED - chain is producing blocks, but "
        <> show notOk
        <> " of "
        <> show (length nodes)
        <> (if notOk == 1 then " nodes is" else " nodes are")
        <> " not ok"
    summary NetworkStalled =
      "STALLED - chain is not producing blocks"
        <> case bestTip of
             Just t -> " (tip is " <> seconds (tipAge t) <> " old)"
             _ -> " (no node has a block yet)"
    summary NetworkStopped = "STOPPED - no node answers"
    summary NoTestnet = "NO-TESTNET - no " <> defaultManifestFile <> " in " <> outputDir

    chainLine :: Doc ann
    chainLine =
      foldMap
        ( \ChainInfo
            { chainEra = era
            , chainEpoch = epoch
            , chainSlotInEpoch = inEpoch
            , chainSlotsToEpochEnd = toEnd
            } -> pretty (T.pack ( "Chain: era " <> anyEraToString era
                                    <> ", epoch " <> show (unEpochNo epoch)
                                    <> ", slot " <> show inEpoch
                                    <> " of " <> show (inEpoch + toEnd)
                                ))
                   <> hardline
        )
        chain

    details :: Doc ann
    details =
      case status of
        NoTestnet -> "No testnet" <> maybe "" (\x -> softline <> parens (explain x)) problemExplanation
        _ -> renderTable $
               ["NODE", "ROLE", "STATE", "PID", "TIP SLOT", "TIP BLOCK", "TIP AGE", "BEHIND", "MEMPOOL"]
                 : [ [ nodeName
                     , case nodeRole of
                         RoleSpo -> "spo"
                         RoleRelay -> "relay"
                     , case nodeState of
                         NodeOk -> "ok"
                         NodeStalled -> "stalled"
                         NodeUnreachable -> "unreachable"
                         NodeDown -> "down"
                     , maybe "-" show mNodePid
                     ] <> case mNodeTipInfo of
                            Nothing -> ["-", "-", "-"]
                            Just ( TipInfo
                                     { tipSlot = slot
                                     , tipBlockNo = blockNo
                                     , tipAge = age
                                     }
                                 ) -> [ show $ unSlotNo slot
                                      , show $ unBlockNo blockNo
                                      , seconds age
                                      ]
                       <> [maybe "-" show mBlocksBehind, maybe "-" mempoolCell mMempool]
                    | NodeProbeResult
                        { probeNodeName = nodeName
                        , probeNodeRole = nodeRole
                        , probePid = mNodePid
                        , probeNodeState = nodeState
                        , probeNodeTipInfo = mNodeTipInfo
                        , probeBlocksBehind = mBlocksBehind
                        , probeMempool = mMempool
                        }
                      <- nodes
                   ]

    mempoolCell :: MempoolInfo -> String
    mempoolCell
      MempoolInfo
        { mempoolTxs = txs
        , mempoolBytes = bytes
        , mempoolCapacityBytes = capacity
        } = show txs <> " tx, " <> show bytes <> "/" <> show capacity <> " B"

    -- The directory is one word: a line can break around it, never inside it.
    explain :: NoTestnetReason -> Doc ann
    explain DirMissing = reflow "The directory" <> softline <> pretty outputDir <> softline <> reflow "does not exist."
    explain NotReady = reflow "Genesis files exist, but no manifest: the testnet is still starting or it never became ready."
    explain NotATestnetDir = pretty outputDir <> softline <> reflow "is not a testnet output directory."

    -- The log paths are normalised, so that on Windows they use only '\',
    -- also with the default directory "./testnet".
    errors :: Doc ann
    errors = mconcat
      [ pretty (T.pack ( nodeName <> ": " <> err
                           <> " (logs: " <> normalise (outputDir </> stdoutFile) <> ", " <> normalise (outputDir </> stderrFile) <> ")"
                       ))
          <> hardline
      | NodeProbeResult
          { probeNodeName = nodeName
          , probeError = Just err
          , probeStdoutFile = stdoutFile
          , probeStderrFile = stderrFile
          }
        <- nodes
      ]

-- | Render rows of cells as a table, with a border around every cell.
renderTable :: [[String]] -> Doc ann
renderTable rows =
  hLine
    <> hardline
    <> concatWith (\x y -> x <> hardline <> y)
         (map (\row ->
                pretty (T.concat ("|" : [ " " <> pad width cell <> " |" | (cell, width) <- zip row widths ]))
                  <> hardline
                  <> hLine
              ) rows)
  where
    hLine :: Doc ann
    hLine = pretty $ T.concat ("+" : [ "-" <> T.replicate w "-" <> "-+" | w <- widths ])

    widths :: [Int]
    widths = map (maximum . map (length . sanitize)) (transpose rows)

    pad :: Int -> String -> Text
    pad w s = let ss = sanitize s in T.pack ss <> T.replicate (w - length ss) " "

    sanitize :: String -> String
    sanitize = unwords . lines . filter (/= '\r')
