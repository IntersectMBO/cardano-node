{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.TraceDispatcher.Common.Formatting
  (
  ) where

import           Data.Aeson ((.=))
import           Text.Show

import           Cardano.Logging
import           Cardano.Prelude hiding (Show, show)

import           Cardano.TraceDispatcher.Render (showT)

import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch


instance
  (  Show peer
  ,  LogFormatting a)
  => LogFormatting (BlockFetch.TraceLabelPeer peer a) where
  forMachine dtal (BlockFetch.TraceLabelPeer peerid a) =
    mkObject [ "peer" .= showT peerid ] <> forMachine dtal a

  forHuman (BlockFetch.TraceLabelPeer peerid m) = "Peer is " <> showT peerid <> ". " <> forHuman m

  asMetrics (BlockFetch.TraceLabelPeer _peerid m) = asMetrics m
