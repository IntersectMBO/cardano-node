{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-local-binds -Wno-unused-matches #-}

module Test.Cardano.Node.Chairman.ByronShelley
  ( tests
  ) where

import           Chairman.IO.Network.Sprocket (Sprocket (..))
import           Chairman.Time
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson ((.=))
import           Data.Bool
import           Data.Eq
import           Data.Function
import           Data.Functor
import           Data.Int
import           Data.List ((\\))
import           Data.Maybe
import           Data.Ord
import           Data.Semigroup
import           Data.String
import           GHC.Num
import           GHC.Real
import           Hedgehog (Property, discover)
import           System.Exit (ExitCode (..))
import           System.IO (IO)
import           Text.Read
import           Text.Show

import qualified Chairman.Hedgehog.Base as H
import qualified Chairman.Hedgehog.File as H
import qualified Chairman.Hedgehog.Process as H
import qualified Chairman.IO.File as IO
import qualified Chairman.IO.Network.Socket as IO
import qualified Chairman.IO.Network.Sprocket as IO
import qualified Chairman.OS as OS
import qualified Chairman.String as S
import qualified Data.Aeson as J
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Time.Clock as DTC
import qualified Hedgehog as H
import qualified System.Directory as IO
import qualified System.Environment as IO
import qualified System.FilePath.Posix as FP
import qualified System.IO as IO
import qualified System.Process as IO
import qualified System.Random as IO

{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant <&>" -}
{- HLINT ignore "Redundant flip" -}

prop_chairman :: Property
prop_chairman = H.propertyOnce . H.workspace "chairman" $ \tempAbsPath -> unless OS.isWin32 $ do
  tempBaseAbsPath <- H.noteShow $ FP.takeDirectory tempAbsPath
  tempRelPath <- H.noteShow $ FP.makeRelative tempBaseAbsPath tempAbsPath
  base <- H.noteShowM H.getProjectBase
  logDir <- H.noteShow $ tempAbsPath <> "/logs"
  env <- H.evalIO IO.getEnvironment
  currentTime <- H.noteShowIO DTC.getCurrentTime
  startTime <- H.noteShow $ DTC.addUTCTime 10 currentTime -- 10 seconds into the future
  socketDir <- H.noteShow $ tempRelPath <> "/socket"
  let bftNodes = ["node-bft1", "node-bft2"]
  let poolNodes = ["node-pool1"]
  let allNodes = bftNodes <> poolNodes
  let bftNodesN = [1, 2]
  let numBftNodes = 2
  let numUtxoKeys = 1
  let initSupply = 1000000000
  let maxSupply = 1000000000
  let fundsPerGenesisAddress = initSupply `div` numBftNodes
  let fundsPerByronAddress = fundsPerGenesisAddress * 9 `div` 10

  portBase <- H.noteShowIO $ IO.randomRIO (3000, 50000)
  allPorts <- H.noteShow ((+ portBase) <$> [1 .. L.length allNodes])
  nodeToPort <- H.noteShow (M.fromList (L.zip allNodes allPorts))

  let networkMagic = 42
  let securityParam = 10

  H.createDirectoryIfMissing logDir

  forM_ allNodes $ \node -> do
    H.createDirectoryIfMissing $ tempAbsPath <> "/" <> node
    H.createDirectoryIfMissing $ tempAbsPath <> "/" <> node <> "/byron"
    H.createDirectoryIfMissing $ tempAbsPath <> "/" <> node <> "/shelley"

  -- Make topology files
  forM_ allNodes $ \node -> do
    let port = fromJust $ M.lookup node nodeToPort
    H.lbsWriteFile (tempAbsPath <> "/" <> node <> "/topology.json") $ J.encode $
      J.object
      [ "Producers" .= J.toJSON
        [ J.object
          [ "addr" .= J.toJSON @String "127.0.0.1"
          , "port" .= J.toJSON @Int peerPort
          , "valency" .= J.toJSON @Int 1
          ]
        | peerPort <- allPorts \\ [port]
        ]
      ]
    H.writeFile (tempAbsPath <> "/" <> node <> "/port") (show port)

  H.lbsWriteFile (tempAbsPath <> "/byron.genesis.spec.json") . J.encode $ J.object
    [ "heavyDelThd" .= J.toJSON @String "300000000000"
    , "maxBlockSize" .= J.toJSON @String "2000000"
    , "maxTxSize" .= J.toJSON @String "4096"
    , "maxHeaderSize" .= J.toJSON @String "2000000"
    , "maxProposalSize" .= J.toJSON @String "700"
    , "mpcThd" .= J.toJSON @String "20000000000000"
    , "scriptVersion" .= J.toJSON @Int 0
    , "slotDuration" .= J.toJSON @String "2000"
    , "softforkRule" .= J.object
      [ "initThd" .= J.toJSON @String "900000000000000"
      , "minThd" .= J.toJSON @String "600000000000000"
      , "thdDecrement" .= J.toJSON @String "50000000000000"
      ]
    , "txFeePolicy" .= J.object
      [ "multiplier" .= J.toJSON @String "43946000000"
      , "summand" .= J.toJSON @String "155381000000000"
      ]
    , "unlockStakeEpoch" .= J.toJSON @String "18446744073709551615"
    , "updateImplicit" .= J.toJSON @String "10000"
    , "updateProposalThd" .= J.toJSON @String "100000000000000"
    , "updateVoteThd" .= J.toJSON @String "1000000000000"
    ]

  -- stuff
  void . H.execCli $
    [ "genesis"
    , "--protocol-magic", show @Int networkMagic
    , "--start-time", showUTCTimeSeconds startTime
    , "--k", show @Int securityParam
    , "--n-poor-addresses", "0"
    , "--n-delegate-addresses", show @Int numBftNodes
    , "--total-balance", show @Int initSupply
    , "--byron-formats"
    , "--delegate-share", "1"
    , "--avvm-entry-count", "0"
    , "--avvm-entry-balance", "0"
    , "--protocol-parameters-file", tempAbsPath <> "/byron.genesis.spec.json"
    , "--genesis-output-dir", tempAbsPath <> "/byron"
    ]

  H.renameFile
    (tempAbsPath <> "/byron.genesis.spec.json")
    (tempAbsPath <> "/byron/genesis.spec.json")

  -- Symlink the BFT operator keys from the genesis delegates, for uniformity
  forM_ bftNodesN $ \n -> do
    H.createFileLink (tempAbsPath <> "/byron/delegate-keys.00" <> show (n - 1) <> ".key") (tempAbsPath <> "/node-bft" <> show n <> "/byron/delegate.key")
    H.createFileLink (tempAbsPath <> "/byron/delegation-cert.00" <> show (n - 1) <> ".json") (tempAbsPath <> "/node-bft" <> show n <> "/byron/delegate.cert")


  ------------------------------------------------------------------------------------------------------------------------------------

  do
    H.readFile (base <> "/configuration/chairman/byron-shelley/configuration.yaml")
      <&> L.unlines . (<> ["TestShelleyHardForkAtVersion: 1"]) . L.lines
      >>= H.writeFile (tempAbsPath <> "/configuration.yaml")

    nodeStdoutFile <- H.noteTempFile logDir "mkfiles.stdout.log"
    nodeStderrFile <- H.noteTempFile logDir "mkfiles.stderr.log"

    hNodeStdout <- H.openFile nodeStdoutFile IO.WriteMode
    hNodeStderr <- H.openFile nodeStderrFile IO.WriteMode

    (_, _, _, hProcess, _) <- H.createProcess $ (IO.proc "bash" ["-x", base <> "/cardano-node-chairman/mkfiles.sh"])
      { IO.std_in = IO.CreatePipe
      , IO.std_out = IO.UseHandle hNodeStdout
      , IO.std_err = IO.UseHandle hNodeStderr
      , IO.env = Just
          $ ("BFT_NODES_N", L.unwords (fmap (show @Int) bftNodesN))
          : ("BFT_NODES", L.unwords bftNodes)
          : ("INIT_SUPPLY", show @Int initSupply)
          : ("MAX_SUPPLY", show @Int maxSupply)
          : ("NETWORK_MAGIC", show @Int networkMagic)
          : ("NUM_BFT_NODES", show @Int numBftNodes)
          : ("NUM_UTXO_KEYS", show @Int numUtxoKeys)
          : ("POOL_NODES", L.unwords poolNodes)
          : ("ROOT", tempAbsPath)
          : ("SECURITY_PARAM", show @Int securityParam)
          : ("START_TIME", showUTCTimeSeconds startTime)
          : ("ALL_NODES", L.unwords (bftNodes <> poolNodes))
          : ("FUNDS_PER_GENESIS_ADDRESS", show @Int fundsPerGenesisAddress)
          : ("FUNDS_PER_BYRON_ADDRESS", show @Int fundsPerGenesisAddress)
          : env
      }

    exitCode <- H.waitForProcess hProcess

    void $ H.noteShow exitCode

    unless (exitCode == Just ExitSuccess) $ do
      H.cat $ logDir <> "/mkfiles.stdout.log"
      H.cat $ logDir <> "/mkfiles.stderr.log"
      H.failure

    H.noteM_ . liftIO $ IO.readProcess "find" [tempAbsPath] ""

    H.assertIsJsonFile $ tempAbsPath <> "/byron/genesis.spec.json"

  --------------------------------
  -- Launch cluster of three nodes

  forM_ bftNodes $ \node -> do
    dbDir <- H.noteShow $ tempAbsPath <> "/db/" <> node
    nodeStdoutFile <- H.noteTempFile logDir $ node <> ".stdout.log"
    nodeStderrFile <- H.noteTempFile logDir $ node <> ".stderr.log"
    sprocket <- H.noteShow $ Sprocket tempBaseAbsPath (socketDir <> "/" <> node)

    H.createDirectoryIfMissing dbDir
    H.createDirectoryIfMissing $ tempBaseAbsPath <> "/" <> socketDir

    hNodeStdout <- H.openFile nodeStdoutFile IO.WriteMode
    hNodeStderr <- H.openFile nodeStderrFile IO.WriteMode

    H.diff (L.length (IO.sprocketArgumentName sprocket)) (<=) IO.maxSprocketArgumentNameLength

    portString <- fmap S.strip . H.readFile $ tempAbsPath <> "/" <> node <> "/port"

    (_, _, _, hProcess, _) <- H.createProcess =<<
      ( H.procNode
        [ "run"
        , "--config",  tempAbsPath <> "/configuration.yaml"
        , "--topology",  tempAbsPath <> "/" <> node <> "/topology.json"
        , "--database-path", tempAbsPath <> "/" <> node <> "/db"
        , "--socket-path", IO.sprocketArgumentName sprocket
        , "--shelley-kes-key", tempAbsPath <> "/" <> node <> "/shelley/kes.skey"
        , "--shelley-vrf-key", tempAbsPath <> "/" <> node <> "/shelley/vrf.skey"
        , "--shelley-operational-certificate", tempAbsPath <> "/" <> node <> "/shelley/node.cert"
        , "--port",  portString
        , "--delegation-certificate",  tempAbsPath <> "/" <> node <> "/byron/delegate.cert"
        , "--signing-key", tempAbsPath <> "/" <> node <> "/byron/delegate.key"
        ] <&>
        ( \cp -> cp
          { IO.std_in = IO.CreatePipe
          , IO.std_out = IO.UseHandle hNodeStdout
          , IO.std_err = IO.UseHandle hNodeStderr
          , IO.cwd = Just tempBaseAbsPath
          }
        )
      )

    H.noteShowM_ $ H.getPid hProcess

  H.threadDelay 100000


  forM_ poolNodes $ \node -> do
    dbDir <- H.noteShow $ tempAbsPath <> "/db/" <> node
    nodeStdoutFile <- H.noteTempFile logDir $ node <> ".stdout.log"
    nodeStderrFile <- H.noteTempFile logDir $ node <> ".stderr.log"
    sprocket <- H.noteShow $ Sprocket tempBaseAbsPath (socketDir <> "/" <> node)

    H.createDirectoryIfMissing dbDir
    H.createDirectoryIfMissing $ tempBaseAbsPath <> "/" <> socketDir

    hNodeStdout <- H.openFile nodeStdoutFile IO.WriteMode
    hNodeStderr <- H.openFile nodeStderrFile IO.WriteMode

    H.diff (L.length (IO.sprocketArgumentName sprocket)) (<=) IO.maxSprocketArgumentNameLength

    portString <- fmap S.strip . H.readFile $ tempAbsPath <> "/" <> node <> "/port"

    void $ H.createProcess =<<
      ( H.procNode
        [ "run"
        , "--config", tempAbsPath <> "/configuration.yaml"
        , "--topology", tempAbsPath <> "/" <> node <> "/topology.json"
        , "--database-path", tempAbsPath <> "/" <> node <> "/db"
        , "--socket-path", IO.sprocketArgumentName sprocket
        , "--shelley-kes-key", tempAbsPath <> "/" <> node <> "/shelley/kes.skey"
        , "--shelley-vrf-key", tempAbsPath <> "/" <> node <> "/shelley/vrf.skey"
        , "--shelley-operational-certificate", tempAbsPath <> "/" <> node <> "/shelley/node.cert"
        , "--port",  portString
        ] <&>
        ( \cp -> cp
          { IO.std_in = IO.CreatePipe
          , IO.std_out = IO.UseHandle hNodeStdout
          , IO.std_err = IO.UseHandle hNodeStderr
          , IO.cwd = Just tempBaseAbsPath
          }
        )
      )

  H.noteShowIO_ DTC.getCurrentTime

  deadline <- H.noteShowIO $ DTC.addUTCTime 90 <$> DTC.getCurrentTime

  forM_ allNodes $ \node -> do
    portString <- H.noteShowM . fmap S.strip . H.readFile $ tempAbsPath <> "/" <> node <> "/port"
    H.assertByDeadlineIOFinally deadline (IO.isPortOpen (read portString)) $ do
      nodeStdoutFile <- H.noteTempFile logDir $ node <> ".stdout.log"
      nodeStderrFile <- H.noteTempFile logDir $ node <> ".stderr.log"
      H.cat nodeStdoutFile
      H.cat nodeStderrFile

  forM_ allNodes $ \node -> do
    sprocket <- H.noteShow $ Sprocket tempBaseAbsPath (socketDir <> "/" <> node)
    _spocketSystemNameFile <- H.noteShow $ IO.sprocketSystemName sprocket
    H.assertIO $ IO.doesSprocketExist sprocket

  forM_ allNodes $ \node -> do
    nodeStdoutFile <- H.noteTempFile logDir $ node <> ".stdout.log"
    H.assertByDeadlineIO deadline $ IO.fileContains "until genesis start time at" nodeStdoutFile
    H.assertByDeadlineIO deadline $ IO.fileContains "Chain extended, new tip" nodeStdoutFile

  H.noteShowIO_ DTC.getCurrentTime

  -- Run chairman
  forM_ (L.take 1 bftNodes) $ \node -> do
    nodeStdoutFile <- H.noteTempFile logDir $ "chairman-" <> node <> ".stdout.log"
    nodeStderrFile <- H.noteTempFile logDir $ "chairman-" <> node <> ".stderr.log"
    sprocket <- H.noteShow $ Sprocket tempBaseAbsPath (socketDir <> "/" <> node)

    H.createDirectoryIfMissing $ tempBaseAbsPath <> "/" <> socketDir

    hNodeStdout <- H.evalIO $ IO.openFile nodeStdoutFile IO.WriteMode
    hNodeStderr <- H.evalIO $ IO.openFile nodeStderrFile IO.WriteMode

    (_, _, _, hProcess, _) <- H.createProcess =<<
      ( H.procChairman
        [ "--timeout", "100"
        , "--socket-path", IO.sprocketArgumentName sprocket
        , "--config", tempAbsPath <> "/configuration.yaml"
        , "--security-parameter", "2160"
        , "--testnet-magic", show @Int networkMagic
        , "--slot-length", "20"
        ] <&>
        ( \cp -> cp
          { IO.std_in = IO.CreatePipe
          , IO.std_out = IO.UseHandle hNodeStdout
          , IO.std_err = IO.UseHandle hNodeStderr
          , IO.cwd = Just tempBaseAbsPath
          }
        )
      )

    void $ H.waitSecondsForProcess 110 hProcess

    H.cat nodeStdoutFile
    H.cat nodeStderrFile

tests :: IO Bool
tests = H.checkParallel $$discover
