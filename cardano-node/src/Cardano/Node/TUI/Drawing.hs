{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Node.TUI.Drawing
    ( drawUI

    -- * Themes
    , darkTheme
    , lightTheme

    , ColorTheme(..)
    , LiveViewState(..)
    , LiveViewThread(..)
    , Screen(..)
    ) where

import           Cardano.Prelude hiding (on)
import           Prelude (String)

import qualified Brick.AttrMap as A
import           Brick.Themes (Theme, newTheme)
import           Brick.Types (Padding(..), Widget)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import           Brick.Widgets.Core (hBox, padBottom, padLeft, hLimitPercent,
                   padTop, padRight, str, txt, updateAttrMap, vBox, vLimitPercent,
                   withAttr, withBorderStyle)
import qualified Brick.Widgets.Center as C
import           Brick.Widgets.ProgressBar (progressBar, progressCompleteAttr,
                   progressIncompleteAttr)
import           Brick.Util (fg, on)
import qualified Control.Concurrent.Async as Async
import           Control.DeepSeq (rwhnf)
import qualified Data.Text as Text
import           Data.Time.Clock (NominalDiffTime, UTCTime(..), addUTCTime)
import           Data.Time.Calendar (Day(..))
import           Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Graphics.Vty as Vty
import           Numeric (showFFloat)
import           Text.Printf (printf)

import           Cardano.Node.Types (Protocol(..))
import           Cardano.Tracing.Peer (Peer(..), ppPeer)

data ColorTheme
  = DarkTheme
  | LightTheme
  deriving (Eq, Generic, NoUnexpectedThunks, NFData)

data LiveViewState blk a = LiveViewState
  { lvsScreen               :: !Screen
  , lvsRelease              :: !String
  , lvsProtocol             :: !Protocol
  , lvsNodeId               :: !Text
  , lvsVersion              :: !String
  , lvsCommit               :: !String
  , lvsPlatform             :: !String
  , lvsUpTime               :: !NominalDiffTime
  , lvsEpoch                :: !Word64
  , lvsSlotNum              :: !Word64
  , lvsBlockNum             :: !Word64
  , lvsChainDensity         :: !Double
  , lvsBlocksMinted         :: !Word64
  , lvsNodeCannotLead       :: !Word64
  , lvsLeaderNum            :: !Word64
  , lvsSlotsMissedNum       :: !Word64
  , lvsTransactions         :: !Word64
  , lvsPeersConnected       :: !Word64
  , lvsMempool              :: !Word64
  , lvsMempoolPerc          :: !Float
  , lvsMempoolBytes         :: !Word64
  , lvsMempoolBytesPerc     :: !Float
  , lvsCPUUsagePerc         :: !Float
  , lvsMemoryUsageCurr      :: !Float
  , lvsMemoryUsageMax       :: !Float
  , lvsDiskUsageRPerc       :: !Float
  , lvsDiskUsageRCurr       :: !Float
  , lvsDiskUsageRMax        :: !Float
  , lvsDiskUsageWPerc       :: !Float
  , lvsDiskUsageWCurr       :: !Float
  , lvsDiskUsageWMax        :: !Float
  , lvsNetworkUsageInPerc   :: !Float
  , lvsNetworkUsageInCurr   :: !Float
  , lvsNetworkUsageInMax    :: !Float
  , lvsNetworkUsageOutPerc  :: !Float
  , lvsNetworkUsageOutCurr  :: !Float
  , lvsNetworkUsageOutMax   :: !Float
  -- internal state
  , lvsStartTime            :: !UTCTime
  , lvsCPUUsageLast         :: !Integer
  , lvsCPUUsageNs           :: !Word64
  , lvsDiskUsageRLast       :: !Word64
  , lvsDiskUsageRNs         :: !Word64
  , lvsDiskUsageWLast       :: !Word64
  , lvsDiskUsageWNs         :: !Word64
  , lvsNetworkUsageInLast   :: !Word64
  , lvsNetworkUsageInNs     :: !Word64
  , lvsNetworkUsageOutLast  :: !Word64
  , lvsNetworkUsageOutNs    :: !Word64
  , lvsMempoolMaxTxs        :: !Word64
  , lvsMempoolMaxBytes      :: !Word64
  , lvsMessage              :: !(Maybe a)
  -- Async threads.
  , lvsUIThread             :: !LiveViewThread
  , lvsMetricsThread        :: !LiveViewThread
  , lvsNodeThread           :: !LiveViewThread
  , lvsPeers                :: [Peer blk]
  , lvsColorTheme           :: !ColorTheme
  } deriving (Generic, NFData, NoUnexpectedThunks)

-- | Type wrapper to simplify derivations.
newtype LiveViewThread = LiveViewThread
    { getLVThread :: Maybe (Async.Async ())
    } deriving (Eq, Generic)

instance NoUnexpectedThunks LiveViewThread where
    whnfNoUnexpectedThunks _ _ = pure NoUnexpectedThunks

instance NFData LiveViewThread where
    rnf = rwhnf

data Screen
  = MainView
  | Peers
  deriving (Generic, NoUnexpectedThunks, NFData)

-------------------------------------------------------------------------------
-- UI drawing
-------------------------------------------------------------------------------

drawUI :: LiveViewState blk a -> [Widget ()]
drawUI p = case lvsScreen p of
  MainView -> [withBorder . withHeaderFooter p . withSideBar p $ systemStatsW p]
  Peers -> [withBorder . withHeaderFooter p . withSideBar p $ peerListContentW p]
 where
   withBorder :: Widget () -> Widget ()
   withBorder
     = C.hCenter . C.vCenter
     . hLimitPercent 96 . vLimitPercent 96
     . withBorderStyle BS.unicode . B.border

   withHeaderFooter :: LiveViewState blk a -> Widget () -> Widget ()
   withHeaderFooter lvs
     = vBox
     . (headerW lvs:)
     . (:[keysMessageW])

   withSideBar :: LiveViewState blk a -> Widget () -> Widget ()
   withSideBar lvs
     = hBox
     . (:[nodeInfoW lvs])

headerW :: LiveViewState blk a -> Widget ()
headerW p =
      C.hCenter
    . padTop   (Pad 1)
    . padLeft  (Pad 2)
    . padRight (Pad 2)
    $ hBox [   withAttr cardanoAttr
             . padRight (Pad 10)
             $ txt "CARDANO"
           , txt "release: "
           ,   withAttr releaseAttr
             $ str (lvsRelease p)
           , padLeft Max $ txt "Node: "
           ,   withAttr nodeIdAttr
             $ txt (lvsNodeId p)
           ]

keysMessageW :: Widget ()
keysMessageW =
      padBottom (Pad 1)
    . padLeft   (Pad 1)
    $ hBox [ txt "Press "
           , withAttr keyAttr $ txt "Q"
           , txt " to quit, "
           , withAttr keyAttr $ txt "L"
           , txt "/"
           , withAttr keyAttr $ txt "D"
           , txt " to change color theme, "
           , withAttr keyAttr $ txt "P"
           , txt " for peer list, "
           , withAttr keyAttr $ txt "Esc"
           , txt " return to main screen"
           ]

nodeInfoLabels :: Widget ()
nodeInfoLabels =
      padRight (Pad 3)
    $ vBox [                    txt "version:"
           ,                    txt "commit:"
           ,                    txt "platform:"
           , padTop (Pad 1) $ txt "uptime:"
           , padTop (Pad 1) $ txt "epoch / slot:"
           ,                    txt "block number:"
           ,                    txt "chain density:"
           , padTop (Pad 1) $ txt "blocks minted:"
           ,                    txt "slots lead:"
           ,                    txt "slots missed:"
           ,                    txt "cannot lead:"
           , padTop (Pad 1) $ txt "TXs processed:"
           , padTop (Pad 1) $ txt "peers:"
           ]

nodeInfoW :: LiveViewState blk a -> Widget ()
nodeInfoW p =
      padTop    (Pad 2)
    . padLeft   (Pad 1)
    . padRight  (Pad 2)
    . padBottom (Pad 2)
    $ hBox [nodeInfoLabels, nodeInfoValues p]


nodeInfoValues :: LiveViewState blk a -> Widget ()
nodeInfoValues lvs =
      withAttr valueAttr
    $ vBox [                  str (lvsVersion lvs)
           ,                  str (take 7 $ lvsCommit lvs)
           ,                  str (lvsPlatform lvs)
           , padTop (Pad 1) $ str (formatTime defaultTimeLocale "%X" $
                                        -- NominalDiffTime is not an instance of FormatTime before time-1.9.1
                                        addUTCTime (lvsUpTime lvs) (UTCTime (ModifiedJulianDay 0) 0))
           , padTop (Pad 1) $ str $ show (lvsEpoch lvs) ++ " / " ++ show (lvsSlotNum lvs)
           ,                  str (show . lvsBlockNum $ lvs)
           ,                  str $ withOneDecimal (lvsChainDensity lvs) ++ " %"
           , padTop (Pad 1) $ str (show . lvsBlocksMinted $ lvs)
           ,                  str (show . lvsLeaderNum $ lvs)
           ,                  str (show . lvsSlotsMissedNum $ lvs)
           ,                  str (show . lvsNodeCannotLead $ lvs)
           , padTop (Pad 1) $ str (show . lvsTransactions $ lvs)
           , padTop (Pad 1) $ str (show . lvsPeersConnected $ lvs)
           ]

peerListContentW :: LiveViewState blk a -> Widget ()
peerListContentW lvs
  = padLeft   (Pad 1)
  . padRight  (Pad 1)
  . padBottom (Pad 1)
  . padTop    (Pad 1)
  . vBox
  . ([ txt "Known peers"
       & padBottom (Pad 1)
     , txt . Text.pack $ printf "%-15s %-8s %-5s  %-10s"
       ("Address" :: String) ("Status" :: String) ("Slot" :: String) ("In flight:" :: String)
     , (txt . Text.pack $ printf "%31s Reqs Blocks   Bytes" ("" :: String))
       & padBottom (Pad 1)
     ] <>)
  $ txt . ppPeer <$> lvsPeers lvs

systemStatsW :: LiveViewState blk a -> Widget ()
systemStatsW p =
      padTop   (Pad 1)
    . padLeft  (Pad 1)
    . padRight (Pad 1)
    $ vBox [ hBox [ vBox [ hBox [ txt "Mempool (Bytes):"
                                , withAttr barValueAttr . padLeft Max . str .
                                  show . (floor :: Float -> Int) . fromIntegral $ lvsMempoolMaxBytes p
                                , txt " max"
                                ]
                         , padBottom (Pad 1) memPoolBytesBar
                         ]
                  , padLeft (Pad 2) $
                    vBox [ hBox [ txt "Mempool (Txs):"
                                , withAttr barValueAttr . padLeft Max . str . show $ lvsMempoolMaxTxs p
                                , txt " max"
                                ]
                         , padBottom (Pad 1) memPoolBar
                         ]
                  ]
           , vBox [ hBox [ txt "Memory usage:"
                         , withAttr barValueAttr . padLeft Max $ str $ withOneDecimal (max (lvsMemoryUsageMax p) 200.0) <> " MB"
                         ]
                  , padBottom (Pad 1) memUsageBar
                  ]
           , vBox [ hBox [ txt "CPU usage:"
                         , withAttr barValueAttr . padLeft Max $ str "100%"
                         ]
                  , padBottom (Pad 1) cpuUsageBar
                  ]
           , hBox [ vBox [ hBox [ txt "Disk R:"
                                , withAttr barValueAttr . padLeft Max $ str $ withOneDecimal (max (lvsDiskUsageRMax p) 1.0) <> " KB/s"
                                ]
                         , padBottom (Pad 1) diskUsageRBar
                         ]
                  , padLeft (Pad 3) $
                    vBox [ hBox [ txt "Disk W:"
                                , withAttr barValueAttr . padLeft Max $ str $ withOneDecimal (max (lvsDiskUsageWMax p) 1.0) <> " KB/s"
                                ]
                         , padBottom (Pad 1) diskUsageWBar
                         ]
                  ]
           , hBox [ vBox [ hBox [ txt "Network In:"
                                , withAttr barValueAttr . padLeft Max $ str $ withOneDecimal (max (lvsNetworkUsageInMax p) 1.0) <> " KB/s"
                                ]
                         , padBottom (Pad 1) networkUsageInBar
                         ]
                  , padLeft (Pad 3) $
                    vBox [ hBox [ txt "Network Out:"
                                , withAttr barValueAttr . padLeft Max $ str $ withOneDecimal (max (lvsNetworkUsageOutMax p) 1.0) <> " KB/s"
                                ]
                         , padBottom (Pad 1) networkUsageOutBar
                         ]
                  ]
           ]
  where
    -- use mapAttrNames
    memPoolBar :: forall n. Widget n
    memPoolBar = updateAttrMap
                 (A.mapAttrNames [ (mempoolDoneAttr, progressCompleteAttr)
                                 , (mempoolToDoAttr, progressIncompleteAttr)
                                 ]
                 ) $ bar mempoolLabel (lvsMempoolPerc p)
    mempoolLabel = Just $ (show . lvsMempool $ p)
    memPoolBytesBar :: forall n. Widget n
    memPoolBytesBar = updateAttrMap
                 (A.mapAttrNames [ (mempoolDoneAttr, progressCompleteAttr)
                                 , (mempoolToDoAttr, progressIncompleteAttr)
                                 ]
                 ) $ bar mempoolBytesLabel (lvsMempoolBytesPerc p)
    mempoolBytesLabel = Just $ (show . lvsMempoolBytes $ p)
    memUsageBar :: forall n. Widget n
    memUsageBar = updateAttrMap
                  (A.mapAttrNames [ (memDoneAttr, progressCompleteAttr)
                                  , (memToDoAttr, progressIncompleteAttr)
                                  ]
                  ) $ bar memLabel lvsMemUsagePerc
    memLabel = Just $ withOneDecimal (lvsMemoryUsageCurr p) ++ " MB / max " ++ withOneDecimal (lvsMemoryUsageMax p) ++ " MB"
    cpuUsageBar :: forall n. Widget n
    cpuUsageBar = updateAttrMap
                  (A.mapAttrNames [ (cpuDoneAttr, progressCompleteAttr)
                                  , (cpuToDoAttr, progressIncompleteAttr)
                                  ]
                  ) $ bar cpuLabel (lvsCPUUsagePerc p)
    cpuLabel = Just $ withOneDecimal (lvsCPUUsagePerc p * 100) ++ "%"

    diskUsageRBar :: forall n. Widget n
    diskUsageRBar = updateAttrMap
                    (A.mapAttrNames [ (diskIODoneAttr, progressCompleteAttr)
                                    , (diskIOToDoAttr, progressIncompleteAttr)
                                    ]
                    ) $ bar diskUsageRLabel (lvsDiskUsageRPerc p)
    diskUsageRLabel = Just $ withOneDecimal (lvsDiskUsageRCurr p) ++ " KB/s"

    diskUsageWBar :: forall n. Widget n
    diskUsageWBar = updateAttrMap
                    (A.mapAttrNames [ (diskIODoneAttr, progressCompleteAttr)
                                    , (diskIOToDoAttr, progressIncompleteAttr)
                                    ]
                    ) $ bar diskUsageWLabel (lvsDiskUsageWPerc p)
    diskUsageWLabel = Just $ withOneDecimal (lvsDiskUsageWCurr p) ++ " KB/s"

    networkUsageInBar :: forall n. Widget n
    networkUsageInBar = updateAttrMap
                        (A.mapAttrNames [ (networkIODoneAttr, progressCompleteAttr)
                                        , (networkIOToDoAttr, progressIncompleteAttr)
                                        ]
                        ) $ bar networkUsageInLabel (lvsNetworkUsageInPerc p)
    networkUsageInLabel = Just $ withOneDecimal (lvsNetworkUsageInCurr p) ++ " KB/s"

    networkUsageOutBar :: forall n. Widget n
    networkUsageOutBar = updateAttrMap
                         (A.mapAttrNames [ (networkIODoneAttr, progressCompleteAttr)
                                         , (networkIOToDoAttr, progressIncompleteAttr)
                                         ]
                         ) $ bar networkUsageOutLabel (lvsNetworkUsageOutPerc p)
    networkUsageOutLabel = Just $ withOneDecimal (lvsNetworkUsageOutCurr p) ++ " KB/s"

    bar :: forall n. Maybe String -> Float -> Widget n
    bar = progressBar
    lvsMemUsagePerc = lvsMemoryUsageCurr p / max 200 (lvsMemoryUsageMax p)

-------------------------------------------------------------------------------
-- Attributes
-------------------------------------------------------------------------------

keyAttr :: A.AttrName
keyAttr = "quit"

valueAttr :: A.AttrName
valueAttr = "value"

cardanoAttr :: A.AttrName
cardanoAttr = "cardano"

releaseAttr :: A.AttrName
releaseAttr = "release"

nodeIdAttr :: A.AttrName
nodeIdAttr = "nodeId"

barValueAttr :: A.AttrName
barValueAttr = "barValue"

theBaseAttr :: A.AttrName
theBaseAttr = A.attrName "theBase"

mempoolDoneAttr, mempoolToDoAttr :: A.AttrName
mempoolDoneAttr = theBaseAttr <> A.attrName "mempool:done"
mempoolToDoAttr = theBaseAttr <> A.attrName "mempool:remaining"

memDoneAttr, memToDoAttr :: A.AttrName
memDoneAttr = theBaseAttr <> A.attrName "mem:done"
memToDoAttr = theBaseAttr <> A.attrName "mem:remaining"

cpuDoneAttr, cpuToDoAttr :: A.AttrName
cpuDoneAttr = theBaseAttr <> A.attrName "cpu:done"
cpuToDoAttr = theBaseAttr <> A.attrName "cpu:remaining"

diskIODoneAttr, diskIOToDoAttr :: A.AttrName
diskIODoneAttr = theBaseAttr <> A.attrName "diskIO:done"
diskIOToDoAttr = theBaseAttr <> A.attrName "diskIO:remaining"

networkIODoneAttr, networkIOToDoAttr :: A.AttrName
networkIODoneAttr = theBaseAttr <> A.attrName "networkIO:done"
networkIOToDoAttr = theBaseAttr <> A.attrName "networkIO:remaining"

-- Please note that there's no full support of RGB, it's just a terminal. :-)
progressToDoColorLFG
  , progressToDoColorLBG
  , progressDoneColorLFG
  , progressDoneColorLBG
  , progressToDoColorDFG
  , progressToDoColorDBG
  , progressDoneColorDFG
  , progressDoneColorDBG
  , darkMainBG
  :: Vty.Color
progressToDoColorLFG = Vty.white
progressToDoColorLBG = Vty.Color240 19
progressDoneColorLFG = Vty.white
progressDoneColorLBG = Vty.Color240 6
progressToDoColorDFG = Vty.black
progressToDoColorDBG = Vty.white
progressDoneColorDFG = Vty.black
progressDoneColorDBG = Vty.Color240 19
darkMainBG           = Vty.Color240 0

bold :: Vty.Attr -> Vty.Attr
bold a = Vty.withStyle a Vty.bold

lightThemeAttributes :: [(A.AttrName, Vty.Attr)]
lightThemeAttributes =
    [ (cardanoAttr,       bold $ fg Vty.black)
    , (releaseAttr,       bold $ fg Vty.blue)
    , (nodeIdAttr,        bold $ fg Vty.blue)
    , (valueAttr,         bold $ fg Vty.black)
    , (keyAttr,           bold $ fg Vty.magenta)
    , (barValueAttr,      bold $ fg Vty.black)
    , (mempoolDoneAttr,   bold $ progressDoneColorLFG `on` progressDoneColorLBG)
    , (mempoolToDoAttr,   bold $ progressToDoColorLFG `on` progressToDoColorLBG)
    , (memDoneAttr,       bold $ progressDoneColorLFG `on` progressDoneColorLBG)
    , (memToDoAttr,       bold $ progressToDoColorLFG `on` progressToDoColorLBG)
    , (cpuDoneAttr,       bold $ progressDoneColorLFG `on` progressDoneColorLBG)
    , (cpuToDoAttr,       bold $ progressToDoColorLFG `on` progressToDoColorLBG)
    , (diskIODoneAttr,    bold $ progressDoneColorLFG `on` progressDoneColorLBG)
    , (diskIOToDoAttr,    bold $ progressToDoColorLFG `on` progressToDoColorLBG)
    , (networkIODoneAttr, bold $ progressDoneColorLFG `on` progressDoneColorLBG)
    , (networkIOToDoAttr, bold $ progressToDoColorLFG `on` progressToDoColorLBG)
    ]

darkThemeAttributes :: [(A.AttrName, Vty.Attr)]
darkThemeAttributes =
    [ (cardanoAttr,       bold $ fg Vty.white)
    , (releaseAttr,       bold $ fg Vty.cyan)
    , (nodeIdAttr,        bold $ fg Vty.cyan)
    , (valueAttr,         bold $ fg Vty.white)
    , (keyAttr,           bold $ fg Vty.white)
    , (barValueAttr,      bold $ fg Vty.white)
    , (mempoolDoneAttr,   bold $ progressDoneColorDFG `on` progressDoneColorDBG)
    , (mempoolToDoAttr,   bold $ progressToDoColorDFG `on` progressToDoColorDBG)
    , (memDoneAttr,       bold $ progressDoneColorDFG `on` progressDoneColorDBG)
    , (memToDoAttr,       bold $ progressToDoColorDFG `on` progressToDoColorDBG)
    , (cpuDoneAttr,       bold $ progressDoneColorDFG `on` progressDoneColorDBG)
    , (cpuToDoAttr,       bold $ progressToDoColorDFG `on` progressToDoColorDBG)
    , (diskIODoneAttr,    bold $ progressDoneColorDFG `on` progressDoneColorDBG)
    , (diskIOToDoAttr,    bold $ progressToDoColorDFG `on` progressToDoColorDBG)
    , (networkIODoneAttr, bold $ progressDoneColorDFG `on` progressDoneColorDBG)
    , (networkIOToDoAttr, bold $ progressToDoColorDFG `on` progressToDoColorDBG)
    ]

lightTheme :: Theme
lightTheme = newTheme (Vty.black `on` Vty.white)
                      lightThemeAttributes

darkTheme :: Theme
darkTheme = newTheme (Vty.white `on` darkMainBG)
                     darkThemeAttributes

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

withOneDecimal :: RealFloat f => f -> String
withOneDecimal = flip (showFFloat (Just 1)) ""
