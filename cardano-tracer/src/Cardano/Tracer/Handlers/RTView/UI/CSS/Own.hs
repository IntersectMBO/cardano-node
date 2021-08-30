{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.UI.CSS.Own
  ( ownCSS
  ) where

import           Prelude hiding ((**))

import           Clay
import qualified Clay.Media as M
import           Data.Text (unpack)
import qualified Data.Text.Lazy as TL

ownCSS :: String
ownCSS = unpack . TL.toStrict . render $ do
  body ? do
    fontFamily        [] [sansSerif] -- We use system font, no web fonts.
    fontSizePx        20
    color             "#1b2238"

  ".rt-view-top-bar" ? do
    backgroundColor   cardanoDark
    color             whitesmoke
    paddingTopPx      8
    paddingBottomPx   2

  ".rt-view-cardano-logo" ** "svg" ? do
    widthPx           48
    color             cardanoLight
    marginLeftPx      5

  ".rt-view-name" ? do
    color             cardanoLight
    marginLeftPx      12
    marginRightPx     6
    marginBottomPx    6

  ".rt-view-info-icon" ** "svg" ? do
    widthPx           25
    paddingTopPx      2
    color             white
    cursor            pointer

  ".rt-view-notify-icon" ** "svg" ? do
    widthPx           23
    paddingTopPx      2
    color             white
    cursor            pointer

  ".rt-view-what-icon" ** "svg" ? do
    widthPx           16
    marginLeftPx      8
    color             "#888"
    cursor            pointer

  ".rt-view-overview-icon" ** "svg" ? do
    widthPx           16
    marginRightPx     10
    color             "#999"

  ".rt-view-node-tab-icon" ** "svg" ? do
    widthPx           16
    marginRightPx     2
    color             "#485fc7"
    maxHeightPx       16
    "object-fit"      -: "contain"

  ".rt-view-node-panel" ? do
    backgroundColor   "#dedede"

  ".rt-view-node-panel-down" ** "svg" ? do
    widthPx           13
    marginTopPx       4
    marginRightPx     3
    cursor            pointer

  ".rt-view-node-panel-block" ? do
    paddingTopPx      20
    paddingBottomPx   20
    paddingLeftPx     20
    paddingRightPx    20

  ".rt-view-node-panel-cols" ? do
    widthPct          100

  ".rt-view-no-nodes-icon" ** "svg" ? do
    widthPx           70
    marginBottomPx    18
    color             "#677deb"

  ".rt-view-no-nodes-message" ? do
    fontSizePx        22
 where
  cardanoLight    = whitesmoke -- rgb  31 193 195
  cardanoDark     = rgb   0  51 173

  paddingPx v     = padding (px v) (px v) (px v) (px v)
  paddingTopPx    = paddingTop . px
  paddingBottomPx = paddingBottom . px
  paddingLeftPx   = paddingLeft . px
  paddingRightPx  = paddingRight . px

  marginTopPx     = marginTop . px
  marginBottomPx  = marginBottom . px
  marginLeftPx    = marginLeft . px
  marginRightPx   = marginRight . px

  fontSizePx      = fontSize . px
  fontSizePct     = fontSize . pct

  heightPx        = height . px
  -- heightPct       = height . pct
  minHeightPx     = minHeight . px
  maxHeightPx     = maxHeight . px

  widthPx         = width . px
  widthPct        = width . pct
  minWidthPx      = minWidth . px
  maxWidthPx      = maxWidth . px
  maxWidthPct     = maxWidth . pct

  topPx           = top . px
  leftPx          = left . px
  rightPx         = right . px
  bottomPx        = bottom . px

  borderRadiusPx  v = borderRadius (px v) (px v) (px v) (px v)
  borderRadiusPct v = borderRadius (pct v) (pct v) (pct v) (pct v)

  progressBarColors bg c = do
    backgroundColor   bg
    important $ color c

  errorTag aColor padLeft padRight cur = do
    fontSizePct       85
    color             white
    marginRightPx     15
    paddingTopPx      2
    paddingBottomPx   0
    paddingLeftPx     padLeft
    paddingRightPx    padRight
    backgroundColor   aColor
    border            solid (px 1) aColor
    borderRadiusPx    4
    cursor            cur
