{-# LANGUAGE NumericUnderscores #-}

module Testnet.Status.Run (
    runCheckStatusOptions,
    checkStatus,
    calcNodeState,
    classifyNetwork,
    newestTip,
    renderStatusReportHumanReadable,
) where

import           Cardano.Api (BlockNo (..), CardanoEra (..), ChainTip (..),
                   ConsensusModeParams (..), LocalNodeConnectInfo (..), NetworkId (..),
                   NetworkMagic (..), ShelleyGenesis, SlotNo (..), getLocalChainTip, mapFile)

import qualified Cardano.Ledger.Shelley.API as SL
import           Cardano.Node.Testnet.Paths (defaultGenesisFilepath, defaultManifestFile)
import           Cardano.Prelude (Nat, NonEmpty, Word32, toList, transpose)

import           Prelude

import           Data.Aeson (FromJSON, eitherDecodeFileStrict')
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable (maximumBy)
import           Data.List.NonEmpty (nonEmpty)
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Ord (comparing)
import           Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import           GHC.Stack (HasCallStack)
import           System.Directory (doesDirectoryExist, doesFileExist)
import           System.Exit (exitWith)
import           System.FilePath ((</>))
import           System.Timeout (timeout)
import           Text.Printf (printf)

import           Testnet.ChainWatchdog (chainForecastHorizon, chainStallTimeoutFromHorizon)
import           Testnet.Manifest (Manifest (..), ManifestGenesisFiles (..), ManifestNetwork (..),
                   ManifestNode (..), ManifestNodeRole (..), ManifestPaths (..))
import           Testnet.Signal (isProcessAlive)
import           Testnet.Start.Types (defaultTestnetDir)
import           Testnet.Status.Types (CheckStatusOptions (..), NetworkStatus (..),
                   NoTestnetReason (..), NodeAnswer (..), NodeProbeResult (..), NodeState (..),
                   OutputFormat (..), StatusReport (..), TipInfo (..), exitCodeForStatus)
import           Testnet.Types (sgSlotLength, sgSystemStart, testnetEpochSlots)

import           RIO (displayException, throwString, tryAny)
import           UnliftIO.Async (forConcurrently)

-- | Run the @status@ command: print the report (a table, or JSON) on stdout
-- and exit with 0 (running or degraded), 3 (stalled), 4 (stopped) or 5 (no
-- testnet). When the manifest or the genesis file cannot be read, print the
-- error on stderr and exit with 1.
runCheckStatusOptions :: CheckStatusOptions -> IO ()
runCheckStatusOptions
    CheckStatusOptions
        { testnetDir = dir
        , statusCheckTimeout = timeoutSeconds
        , outputFormat = format
        } = do
        status <- checkStatus (fromMaybe defaultTestnetDir dir) timeoutSeconds
        case format of
            OutputFormatText -> putStr $ renderStatusReportHumanReadable status
            OutputFormatJson -> do LBS.putStr $ encodePretty status
                                   putStrLn ""
        exitWith . exitCodeForStatus $ reportStatus status

{- | Check the testnet in the given output directory. Each node gets the
given number of seconds to answer, and all nodes are asked at the same
time. It throws when the check itself fails: when the manifest, or the
shelley genesis it points to, cannot be read.
-}
checkStatus :: HasCallStack => FilePath -> Nat -> IO StatusReport
checkStatus outputDir timeoutSeconds = do
    let manifestFile = outputDir </> defaultManifestFile
    hasManifest <- doesFileExist manifestFile
    if not hasManifest
        then noTestnetReport outputDir
        else do
            manifest@Manifest{manifestPaths = ManifestPaths{mpGenesisFiles = ManifestGenesisFiles{mgfShelley = shelleyGenesisFile}}} <-
                readJsonFile manifestFile
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
    nodes <-
        forConcurrently (manifestNodes manifest) $
            probeNode outputDir timeoutSeconds magic limit slotStart
    pure
        StatusReport
            { reportOutputDir = outputDir
            , reportCheckedAt = checkedAt
            , reportStatus = classifyNetwork (probeNodeState <$> nodes)
            , reportBestTip = newestTip . mapMaybe probeNodeTipInfo $ toList nodes
            , reportNodes = toList nodes
            , reportProblemExplanation = Nothing
            }
  where
    -- The chain watchdog's threshold for a dead chain: one number for both.
    limit = chainStallTimeoutFromHorizon (chainForecastHorizon genesis)
    slotLength = SL.fromNominalDiffTimeMicro (sgSlotLength genesis)
    slotStart (SlotNo slot) = addUTCTime (fromIntegral slot * slotLength) (sgSystemStart genesis)

{- | Check the status of one node in the testnet.
-}
probeNode ::
    -- | output directory
    FilePath ->
    -- | timeout, in seconds
    Nat ->
    -- | network magic
    Word32 ->
    -- | tip age limit
    NominalDiffTime ->
    -- | start time of a slot
    (SlotNo -> UTCTime) ->
    ManifestNode ->
    IO NodeProbeResult
probeNode
    outputDir
    timeoutSeconds
    magic
    limit
    slotStart
    ManifestNode
        { mnodeName = nodeName
        , mnodeRole = role
        , mnodePid = pid
        , mnodeSocketPath = socketPath
        } = do
        answer <- askTip
        pidAlive <- maybe (pure Nothing) isProcessAlive pid
        pure
            NodeProbeResult
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
                }
      where
        askTip :: IO NodeAnswer
        askTip = do
            -- Any failure (no socket, refused, handshake error) counts as no answer.
            result <- tryAny $ timeout (fromIntegral (timeoutSeconds * 1_000_000)) (getLocalChainTip connectInfo)
            answeredAt <- getCurrentTime
            pure $ case result of
                Right (Just (ChainTip slot hash blockNo)) ->
                    AnsweredTip
                        TipInfo
                            { tipSlot = slot
                            , tipBlockNo = blockNo
                            , tipHash = hash
                            , tipAge = answeredAt `diffUTCTime` slotStart slot
                            }
                Right (Just ChainTipAtGenesis) -> AnsweredNoBlocks
                Right Nothing -> NoAnswer $ "no answer within " <> show timeoutSeconds <> "s"
                Left e -> NoAnswer $ displayException e

        connectInfo :: LocalNodeConnectInfo
        connectInfo =
            LocalNodeConnectInfo
                { localNodeSocketPath = mapFile (outputDir </>) socketPath
                , localNodeNetworkId = Testnet (NetworkMagic $ fromIntegral magic)
                , localConsensusModeParams = CardanoModeParams testnetEpochSlots
                }

-- | The rule for the whole network.
classifyNetwork :: NonEmpty NodeState -> NetworkStatus
classifyNetwork states
    | all (== NodeOk) states = NetworkRunning
    | NodeOk `elem` states = NetworkDegraded
    | NodeStalled `elem` states = NetworkStalled
    | otherwise = NetworkStopped

{- | The rule for one node. The answer decides; the pid only tells "down"
(the process is gone) from "unreachable" (alive, or unknown).
-}
calcNodeState ::
    -- | tip age limit
    NominalDiffTime ->
    -- | whether the node's process is alive, if known
    Maybe Bool ->
    NodeAnswer ->
    NodeState
calcNodeState limit _pidAlive (AnsweredTip tip)
    | tipAge tip <= limit = NodeOk
    | otherwise = NodeStalled
calcNodeState _limit _pidAlive AnsweredNoBlocks = NodeStalled
calcNodeState _limit pidAlive (NoAnswer _)
    | pidAlive == Just False = NodeDown
    | otherwise = NodeUnreachable

{- | The rule for the best tip: the newest one, which is the one with the
highest slot. The ages cannot tell: each age is measured when its node
answered, and the nodes answer at different times.
-}
newestTip :: [TipInfo] -> Maybe TipInfo
newestTip = fmap (maximumBy (comparing tipSlot)) . nonEmpty

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
    pure
        StatusReport
            { reportOutputDir = outputDir
            , reportCheckedAt = now
            , reportStatus = NoTestnet
            , reportBestTip = Nothing
            , reportNodes = []
            , reportProblemExplanation = Just explanation
            }

-- | Render a 'StatusReport' in a human readable way.
renderStatusReportHumanReadable :: StatusReport -> String
renderStatusReportHumanReadable
    StatusReport
        { reportStatus = status
        , reportBestTip = bestTip
        , reportNodes = nodes
        , reportOutputDir = outputDir
        , reportProblemExplanation = problemExplanation
        } =
        unlines $ ("Network: " <> summary status) : details <> errors
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
                        } ->
                            " (tip: slot "
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
            "STALLED - chain is not producing blocks" <> case bestTip of
                Just t -> " (tip is " <> seconds (tipAge t) <> " old)"
                _ -> " (no node has a block yet)"
        summary NetworkStopped = "STOPPED - no node answers"
        summary NoTestnet = "NO-TESTNET - no " <> defaultManifestFile <> " in " <> outputDir

        details :: [String]
        details = case status of
            NoTestnet -> ["No testnet" <> maybe "" (\x -> " (" <> explain x <> ")") problemExplanation]
            _ ->
                renderTable $
                    ["NODE", "ROLE", "STATE", "PID", "TIP SLOT", "TIP BLOCK", "TIP AGE"]
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
                            ]
                                <> case mNodeTipInfo of
                                    Nothing -> ["-", "-", "-"]
                                    Just
                                        ( TipInfo
                                                { tipSlot = slot
                                                , tipBlockNo = blockNo
                                                , tipAge = age
                                                }
                                            ) ->
                                            [ show $ unSlotNo slot
                                            , show $ unBlockNo blockNo
                                            , seconds age
                                            ]
                          | NodeProbeResult
                                { probeNodeName = nodeName
                                , probeNodeRole = nodeRole
                                , probePid = mNodePid
                                , probeNodeState = nodeState
                                , probeNodeTipInfo = mNodeTipInfo
                                } <-
                                nodes
                          ]

        explain :: NoTestnetReason -> String
        explain DirMissing = "The directory " <> outputDir <> " does not exist."
        explain NotReady = "Genesis files exist, but no manifest: the testnet is still starting or it never became ready."
        explain NotATestnetDir = outputDir <> " is not a testnet output directory."

        errors :: [String]
        errors =
            [ nodeName <> ": " <> err
            | NodeProbeResult{probeNodeName = nodeName, probeError = Just err} <- nodes
            ]

-- | Render an array of strings as a table, where each string in the output list is a line.
renderTable :: [[String]] -> [String]
renderTable rows =
    ("+" ++ concat ["-" ++ replicate width '-' ++ "-+" | width <- widths])
        : concatMap
            ( \row ->
                [ concat ("|" : [" " ++ pad width cell ++ " |" | (cell, width) <- zip row widths])
                , concat ("+" : ["-" ++ replicate width '-' ++ "-+" | width <- widths])
                ]
            )
            rows
  where
    widths :: [Int]
    widths = map (maximum . map length) (transpose rows)

    pad :: Int -> String -> [Char]
    pad w s = s ++ replicate (w - length s) ' '
