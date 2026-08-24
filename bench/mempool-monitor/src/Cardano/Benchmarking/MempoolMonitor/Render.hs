{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE LambdaCase #-}

-- | Rendering a snapshot: a repainting pane when there is a terminal, one line
-- per snapshot when the output is a log, and a TSV column set for analysis.
module Cardano.Benchmarking.MempoolMonitor.Render
  ( renderPane
  , renderLine
  , tsvHeader
  , tsvRow
  ) where

import Cardano.Benchmarking.MempoolMonitor.Snapshot
  ( ColorKey (Colored, Uncolored)
  , Snapshot (snapDrained, snapDuration, snapSizes, snapSlot)
  , colorKeyLabel
  , distinctColors
  , localShare
  , shares
  )
import Cardano.Benchmarking.TxFirehose.Color (Color, colorHex, colorRed, colorGreen, colorBlue)
import Cardano.Api qualified as Api
import Data.List (intercalate)
import Ouroboros.Network.Protocol.LocalTxMonitor.Type
  ( MempoolSizeAndCapacity (capacityInBytes, numberOfTxs, sizeInBytes)
  )
import Text.Printf (printf)

-- | Width of both bars, in cells.
barWidth :: Int
barWidth = 34

-- | The whole pane, ANSI-positioned so successive snapshots repaint in place.
renderPane :: String -> Maybe Color -> Snapshot -> String
renderPane label mOwn snap =
  unlines $
    ["\ESC[H\ESC[2J" ++ header]
      ++ [depthLine, capacityBar]
      ++ [colorLine, compositionBar]
      ++ map shareLine (shares snap)
      ++ [drainLine]
 where
  sizes = snapSizes snap

  header =
    printf
      "mempool-monitor  %-20s slot %s"
      label
      (show (Api.unSlotNo (snapSlot snap)))

  -- The protocol reports capacity in bytes only, so depth in transactions
  -- stands alone and the ratio below it is the byte one.
  depthLine =
    printf
      "depth   %d tx      %.1f / %.1f MB"
      (numberOfTxs sizes)
      (mb (sizeInBytes sizes))
      (mb (capacityInBytes sizes))

  capacityBar = meter (fillRatio (sizeInBytes sizes) (capacityInBytes sizes))

  colorLine =
    printf "colours %d%s" (distinctColors snap) ownSuffix

  ownSuffix = case (mOwn, localShare mOwn snap) of
    (Just own, Just share) -> printf "    local %s %.0f%%" (colorHex own) (100 * share)
    _ -> ""

  -- The composition bar is the point of the whole tool: one cell per share of
  -- the mempool, painted in the colour the transactions actually carry.
  compositionBar =
    concat [replicate (cells share) ' ' `paintedWith` key | (key, _, share) <- shares snap]
      ++ "\ESC[0m"
   where
    cells share = max 0 (round (share * fromIntegral barWidth))

  shareLine (key, n, share) =
    printf "  %-8s %7d  %3.0f%%  %s" (colorKeyLabel key) n (100 * share) (swatchFor key)

  drainLine =
    printf
      "drained %d tx in %.2fs"
      (snapDrained snap)
      (realToFrac (snapDuration snap) :: Double)

-- | One line per snapshot, for when stdout is a log rather than a terminal.
renderLine :: String -> Snapshot -> String
renderLine label snap =
  printf
    "%s slot=%d txs=%d drained=%d colours=%d in=%.2fs  %s"
    label
    (Api.unSlotNo (snapSlot snap))
    (numberOfTxs (snapSizes snap))
    (snapDrained snap)
    (distinctColors snap)
    (realToFrac (snapDuration snap) :: Double)
    (intercalate " " [printf "%s=%d" (colorKeyLabel k) n | (k, n, _) <- shares snap])

tsvHeader :: String
tsvHeader = intercalate "\t" ["slot", "txs", "bytes", "capacity", "drained", "colours", "drainSecs", "composition"]

tsvRow :: Snapshot -> String
tsvRow snap =
  intercalate
    "\t"
    [ show (Api.unSlotNo (snapSlot snap))
    , show (numberOfTxs sizes)
    , show (sizeInBytes sizes)
    , show (capacityInBytes sizes)
    , show (snapDrained snap)
    , show (distinctColors snap)
    , printf "%.3f" (realToFrac (snapDuration snap) :: Double)
    , intercalate "," [printf "%s:%d" (colorKeyLabel k) n | (k, n, _) <- shares snap]
    ]
 where
  sizes = snapSizes snap

-- Helpers ------------------------------------------------------------------

mb :: (Integral a) => a -> Double
mb n = fromIntegral n / 1_000_000

fillRatio :: (Integral a) => a -> a -> Double
fillRatio used capacity
  | capacity <= 0 = 0
  | otherwise = min 1 (fromIntegral used / fromIntegral capacity)

meter :: Double -> String
meter ratio = "[" ++ replicate filled '#' ++ replicate (barWidth - filled) '.' ++ "]"
 where
  filled = max 0 (min barWidth (round (ratio * fromIntegral barWidth)))

-- | Paint a run of cells with a colour's own background, so the bar shows the
-- real colours rather than a palette we invented.
paintedWith :: String -> ColorKey -> String
paintedWith cells = \case
  Colored c -> printf "\ESC[48;2;%d;%d;%dm" (colorRed c) (colorGreen c) (colorBlue c) ++ cells
  Uncolored -> "\ESC[48;5;238m" ++ cells

swatchFor :: ColorKey -> String
swatchFor = \case
  Colored c -> printf "\ESC[48;2;%d;%d;%dm  \ESC[0m" (colorRed c) (colorGreen c) (colorBlue c)
  Uncolored -> "\ESC[48;5;238m  \ESC[0m"
