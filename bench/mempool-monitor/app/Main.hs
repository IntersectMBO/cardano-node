{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Watch one node's mempool over node-to-client and show what colours it
-- holds, so mempool fragmentation is visible per pool rather than in aggregate.
module Main (main) where

import Cardano.Api qualified as Api
import Cardano.Benchmarking.MempoolMonitor.Render (renderLine, renderPane, tsvHeader, tsvRow)
import Cardano.Benchmarking.MempoolMonitor.Snapshot (monitorClient)
import Cardano.Benchmarking.TxFirehose.Color (Color, colorFromPublicKey, parseColor)
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
  , optOwnKeyFile :: !(Maybe FilePath)
  , optOwnColor :: !(Maybe Color)
  , optTsv :: !(Maybe FilePath)
  }

main :: IO ()
main = do
  opts <- parseOptions
  when (optInterval opts <= 0) $ die "--interval must be > 0"

  -- --own-color overrides, --own-key-file derives, neither leaves the local
  -- share off. Making the two mutually exclusive here means the resolved
  -- colour is unambiguous rather than depending on a precedence rule.
  mOwnColor <- case (optOwnColor opts, optOwnKeyFile opts) of
    (Just _, Just _) ->
      die "pass either --own-color or --own-key-file, not both"
    (Just c, Nothing) -> pure (Just c)
    (Nothing, Just path) -> Just . colorFromPublicKey <$> loadVerificationKey path
    (Nothing, Nothing) -> pure Nothing

  hSetBuffering stdout LineBuffering
  -- A repainting pane is right in a terminal and garbage in a log file, so the
  -- choice follows the handle rather than a flag.
  pane <- hIsTerminalDevice stdout

  mTsvHandle <- traverse openTsv (optTsv opts)

  let label = maybe (optSocketPath opts) id (optLabel opts)
      emit snapshot = do
        putStr $
          if pane
            then renderPane label mOwnColor snapshot
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
      ( Opt.strOption
          ( Opt.long "own-key-file"
              <> Opt.metavar "FILEPATH"
              <> Opt.help
                "Payment verification or signing key the attached generator \
                \uses; the local colour is derived from the public portion. \
                \This is the default way to pick the local colour."
          )
      )
    <*> optional
      ( Opt.option
          (Opt.eitherReader parseColor)
          ( Opt.long "own-color"
              <> Opt.metavar "HEX"
              <> Opt.help
                "Override the local colour with an explicit hex value, e.g. \
                \ff0000. Mutually exclusive with --own-key-file."
          )
      )
    <*> optional
      ( Opt.strOption
          ( Opt.long "tsv"
              <> Opt.metavar "FILEPATH"
              <> Opt.help "Also append one row per snapshot to this file"
          )
      )

-- | Load a payment key file and return its verification key. Accepts both a
-- verification key file and a signing key file (payment or genesis-utxo),
-- since the colour derivation only needs the public portion.
loadVerificationKey :: FilePath -> IO (Api.VerificationKey Api.PaymentKey)
loadVerificationKey path = do
  result <- Api.readFileTextEnvelopeAnyOf accepted (Api.File path)
  case result of
    Left err ->
      die $
        "mempool-monitor: cannot read key "
          ++ show path
          ++ ": "
          ++ show err
    Right vk -> pure vk
 where
  accepted :: [Api.FromSomeType Api.HasTextEnvelope (Api.VerificationKey Api.PaymentKey)]
  accepted =
    [ Api.FromSomeType (Api.AsVerificationKey Api.AsPaymentKey) id
    , Api.FromSomeType (Api.AsVerificationKey Api.AsGenesisUTxOKey) Api.castVerificationKey
    , Api.FromSomeType (Api.AsSigningKey Api.AsPaymentKey) Api.getVerificationKey
    , Api.FromSomeType
        (Api.AsSigningKey Api.AsGenesisUTxOKey)
        (Api.castVerificationKey . Api.getVerificationKey)
    ]
