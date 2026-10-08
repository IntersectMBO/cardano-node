module Testnet.Status.Run (
    runCheckStatusOptions,
) where

import           Cardano.Api (BlockNo (..), SlotNo (..))

import           Cardano.Node.Testnet.Paths (defaultManifestFile)
import           Cardano.Prelude (transpose)

import           Prelude

import           Data.Time (NominalDiffTime, getCurrentTime)
import           Text.Printf (printf)

import           Testnet.Manifest (ManifestNodeRole (..))
import           Testnet.Status.Types (CheckStatusOptions, NetworkStatus (..), NodeProbeResult (..),
                   NodeState (..), StatusReport (..), TipInfo (..))

runCheckStatusOptions :: CheckStatusOptions -> IO ()
runCheckStatusOptions _ = do
    currentTime <- getCurrentTime
    putStr $
        renderStatusReportAsTable
            ( StatusReport
                { reportOutputDir = "./testnet"
                , reportCheckedAt = currentTime
                , reportStatus = NoTestnet
                , reportBestTip = Nothing
                , reportNodes = []
                , reportProblemExplanation = Just "Not implemented yet"
                }
            )

renderStatusReportAsTable :: StatusReport -> String
renderStatusReportAsTable
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
