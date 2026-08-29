{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}

-- | Watch one node's mempool over node-to-client and show what colours it
-- holds, so mempool fragmentation is visible per pool rather than in aggregate.
module Main (main) where

import Cardano.Api qualified as Api
import Cardano.Benchmarking.MempoolMonitor.Render (renderLine, renderPane, tsvHeader, tsvRow)
import Cardano.Benchmarking.MempoolMonitor.Snapshot (monitorClient)
import Cardano.Benchmarking.TxFirehose.Color (Color, ColorSpec (ColorFromKey, ColorLiteral), parseColorSpec)
import Control.Applicative (optional)
import Control.Monad (when)
import Data.Foldable (traverse_)
import Numeric.Natural (Natural)
import Options.Applicative qualified as Opt
import System.Exit (die)
import System.IO
  ( BufferMode (LineBuffering)
  , IOMode (AppendMode)
  , hFileSize
  , hIsTerminalDevice
  , hPutStrLn
  , hSetBuffering
  , openFile
  , stdout
  )

data Options = Options
  { optSocketPath :: !FilePath
  , optNetworkMagic :: !Natural
  , optLabel :: !(Maybe String)
  , optInterval :: !Double
  , optOwnColor :: !(Maybe Color)
  , optTsv :: !(Maybe FilePath)
  }

main :: IO ()
main = do
  opts <- parseOptions
  when (optInterval opts <= 0) $ die "--interval must be > 0"

  hSetBuffering stdout LineBuffering
  -- A repainting pane is right in a terminal and garbage in a log file, so the
  -- choice follows the handle rather than a flag.
  pane <- hIsTerminalDevice stdout

  mTsvHandle <- traverse openTsv (optTsv opts)

  let label = maybe (optSocketPath opts) id (optLabel opts)
      emit snapshot = do
        putStr $
          if pane
            then renderPane label (optOwnColor opts) snapshot
            else renderLine label snapshot ++ "\n"
        traverse_ (\h -> hPutStrLn h (tsvRow snapshot)) mTsvHandle

  Api.connectToLocalNode
    (connectInfo opts)
    Api.LocalNodeClientProtocols
      { Api.localChainSyncClient = Api.NoLocalChainSyncClient
      , Api.localStateQueryClient = Nothing
      , Api.localTxSubmissionClient = Nothing
      , Api.localTxMonitoringClient =
          Just (monitorClient (round (optInterval opts * 1_000_000)) emit)
      }
 where
  -- Header only for a fresh file: a restart appends to the existing one, and a
  -- header in the middle of it breaks every reader downstream.
  openTsv path = do
    handle <- openFile path AppendMode
    hSetBuffering handle LineBuffering
    size <- hFileSize handle
    when (size == 0) $ hPutStrLn handle tsvHeader
    pure handle

connectInfo :: Options -> Api.LocalNodeConnectInfo
connectInfo opts =
  Api.LocalNodeConnectInfo
    { Api.localConsensusModeParams = Api.CardanoModeParams (Api.EpochSlots 21600)
    , Api.localNodeNetworkId =
        Api.Testnet (Api.NetworkMagic (fromIntegral (optNetworkMagic opts)))
    , Api.localNodeSocketPath = Api.File (optSocketPath opts)
    }

parseOptions :: IO Options
parseOptions =
  Opt.execParser $
    Opt.info
      (optionsParser Opt.<**> Opt.helper)
      ( Opt.fullDesc
          <> Opt.progDesc "Show which colours one node's mempool is holding."
          <> Opt.header "mempool-monitor - watch a single mempool's composition"
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
    <*> optional
      ( Opt.strOption
          ( Opt.long "label"
              <> Opt.metavar "NAME"
              <> Opt.help "Name for this node in the display (defaults to the socket path)"
          )
      )
    <*> Opt.option
      Opt.auto
      ( Opt.long "interval"
          <> Opt.metavar "SECONDS"
          <> Opt.value 10
          <> Opt.showDefault
          <> Opt.help "Seconds between snapshots; a drain is one round trip per tx, so keep it generous"
      )
    <*> optional
      ( Opt.option
          (Opt.eitherReader readOwnColor)
          ( Opt.long "own-color"
              <> Opt.metavar "HEX"
              <> Opt.help "This node's own colour, to report the local share"
          )
      )
    <*> optional
      ( Opt.strOption
          ( Opt.long "tsv"
              <> Opt.metavar "FILEPATH"
              <> Opt.help "Also append one row per snapshot to this file"
          )
      )

-- | @auto@ needs a signing key to resolve, which an observer does not have.
readOwnColor :: String -> Either String Color
readOwnColor s = case parseColorSpec s of
  Right (ColorLiteral c) -> Right c
  Right ColorFromKey -> Left "--own-color needs an explicit colour, not 'auto'"
  Left err -> Left err
