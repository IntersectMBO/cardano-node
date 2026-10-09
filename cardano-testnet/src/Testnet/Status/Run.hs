{-# LANGUAGE NumericUnderscores #-}

module Testnet.Status.Run (
    runCheckStatusOptions,
) where

import           Cardano.Api (BlockNo (..), CardanoEra (..), ChainTip (..),
                   ConsensusModeParams (..), LocalNodeConnectInfo (..), NetworkId (..),
                   NetworkMagic (..), ShelleyGenesis, SlotNo (..), getLocalChainTip, mapFile)

import qualified Cardano.Ledger.Shelley.API as SL
import           Cardano.Node.Testnet.Paths (defaultGenesisFilepath, defaultManifestFile)
import           Cardano.Prelude (ExceptT (..), ExitCode (..), MonadIO (..), Nat, NonEmpty,
                   SomeException, Word32, runExceptT, toList, transpose, try)

import           Prelude

import           Data.Aeson (FromJSON, eitherDecodeFileStrict')
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as LBS
import           Data.Either (fromRight)
import           Data.Either.Extra (mapLeft)
import qualified Data.List as List
import           Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import           Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import           System.Directory (doesDirectoryExist, doesFileExist)
import           System.Exit (exitWith)
import           System.FilePath ((</>))
import qualified System.IO as IO
import           Text.Printf (printf)

import           Testnet.ChainWatchdog (chainForecastHorizon, chainStallTimeoutFromHorizon)
import           Testnet.Manifest (Manifest (..), ManifestGenesisFiles (..), ManifestNetwork (..),
                   ManifestNode (..), ManifestNodeRole (..), ManifestPaths (..))
import           Testnet.Signal (isProcessAlive)
import           Testnet.Status.Types (CheckStatusOptions (..), NetworkStatus (..), NodeAnswer (..),
                   NodeProbeResult (..), NodeState (..), OutputFormat (..), StatusReport (..),
                   TipInfo (..))
import           Testnet.Types (sgSlotLength, sgSystemStart, testnetEpochSlots)

import           Hedgehog.Extras (forConcurrently)
import System.IO.Error (ioeGetErrorString, tryIOError)
import System.Timeout (timeout)

runCheckStatusOptions :: CheckStatusOptions -> IO ()
runCheckStatusOptions
    CheckStatusOptions
        { testnetDir = dir
        , statusCheckTimeout = timeoutSeconds
        , outputFormat = format
        } = do
        eStatus <- checkStatus (fromMaybe "./testnet" dir) timeoutSeconds
        status <-
            either
                ( \err -> do
                    IO.hPutStrLn IO.stderr $ "cardano-testnet status: " <> err
                    exitWith $ ExitFailure 1
                )
                pure
                eStatus
        case format of
            OutputFormatText -> putStr $ renderStatusReportHumanReadable status
            OutputFormatJson -> do LBS.putStr $ encodePretty status
                                   putStrLn ""
        exitWith . exitCodeForStatus $ reportStatus status

-- | Determine the appropriate exit code for a given network status.
exitCodeForStatus :: NetworkStatus -> ExitCode
exitCodeForStatus NetworkRunning = ExitSuccess
exitCodeForStatus NetworkDegraded = ExitSuccess
exitCodeForStatus NetworkStalled = ExitFailure 3
exitCodeForStatus NetworkStopped = ExitFailure 4
exitCodeForStatus NoTestnet = ExitFailure 5

{- | Check the testnet in the given output directory. Each node gets the
given number of seconds to answer, and all nodes are asked at the same
time. 'Left' means the check itself failed: the manifest, or the shelley
genesis it points to, cannot be read.
-}
checkStatus :: FilePath -> Nat -> IO (Either String StatusReport)
checkStatus outputDir timeoutSeconds = do
    let manifestFile = outputDir </> defaultManifestFile
    hasManifest <- doesFileExist manifestFile
    if not hasManifest
        then Right <$> noTestnetReport outputDir
        else runExceptT $ do
            manifest@Manifest{manifestPaths = ManifestPaths{mpGenesisFiles = ManifestGenesisFiles{mgfShelley = shelleyGenesisFile}}} <-
                readJsonFile manifestFile
            genesis <- readJsonFile $ outputDir </> shelleyGenesisFile
            liftIO $ checkNodes outputDir timeoutSeconds manifest genesis
  where
    readJsonFile :: (FromJSON a) => String -> ExceptT String IO a
    readJsonFile file =
        ExceptT $
            either
                (\err -> Left $ "cannot read " <> file <> ": " <> ioeGetErrorString err)
                (mapLeft (\e -> "cannot decode " <> file <> ": " <> e))
                <$> tryIOError (eitherDecodeFileStrict' file)

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
            , reportBestTip = listToMaybe . List.sortOn tipAge . mapMaybe probeNodeTipInfo $ toList nodes
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
                    NoAnswer -> Nothing
                }
      where
        askTip :: IO NodeAnswer
        askTip = do
            -- Any failure (no socket, refused, handshake error) counts as no answer.
            result <-
                fromRight Nothing
                    <$> ( try $ timeout (fromIntegral (timeoutSeconds * 1_000_000)) (getLocalChainTip connectInfo) ::
                            IO (Either SomeException (Maybe ChainTip))
                        )
            answeredAt <- getCurrentTime
            pure $ case result of
                Just (ChainTip slot hash blockNo) ->
                    AnsweredTip
                        TipInfo
                            { tipSlot = slot
                            , tipBlockNo = blockNo
                            , tipHash = hash
                            , tipAge = answeredAt `diffUTCTime` slotStart slot
                            }
                Just ChainTipAtGenesis -> AnsweredNoBlocks
                Nothing -> NoAnswer

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
calcNodeState _limit pidAlive NoAnswer
    | pidAlive == Just False = NodeDown
    | otherwise = NodeUnreachable

-- | The 'StatusReport' when there is no manifest, with a hint about why.
noTestnetReport :: FilePath -> IO StatusReport
noTestnetReport outputDir = do
    now <- getCurrentTime
    dirExists <- doesDirectoryExist outputDir
    hasGenesis <- doesFileExist $ outputDir </> defaultGenesisFilepath ShelleyEra
    let explanation
            | not dirExists = "The directory " <> outputDir <> " does not exist."
            | hasGenesis = "Genesis files exist, but no manifest: the testnet is still starting or it never became ready."
            | otherwise = outputDir <> " is not a testnet output directory."
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
        unlines $ ("Network: " <> summary status) : details
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
            NoTestnet -> ["No testnet" <> maybe "" (\x -> " (" <> x <> ")") problemExplanation]
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
