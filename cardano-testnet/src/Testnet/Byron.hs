{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-local-binds -Wno-unused-matches #-}

module Testnet.Byron
  ( testnet

  , TestnetOptions(..)
  , defaultTestnetOptions
  ) where

import           Control.Monad
import           Data.Aeson (Value, (.=))
import           Data.Bool (Bool(..))
import           Data.ByteString.Lazy (ByteString)
import           Data.Eq
import           Data.Function
import           Data.Functor
import           Data.Int
import           Data.Maybe
import           Data.Ord
import           Data.Semigroup
import           Data.String
import           GHC.Num
import           GHC.Real
import           Hedgehog.Extras.Stock.Aeson (rewriteObject)
import           Hedgehog.Extras.Stock.IO.Network.Sprocket (Sprocket (..))
import           Hedgehog.Extras.Stock.Time
import           System.FilePath.Posix ((</>))
import           Text.Show

import qualified Cardano.Node.Configuration.Topology    as NonP2P
import qualified Cardano.Node.Configuration.TopologyP2P as P2P
import           Ouroboros.Network.PeerSelection.RelayAccessPoint (RelayAccessPoint (..))
import           Ouroboros.Network.PeerSelection.LedgerPeers (UseLedgerAfter (..))

import qualified Data.Aeson as J
import qualified Data.HashMap.Lazy as HM
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Time.Clock as DTC
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.File as IO
import qualified Hedgehog.Extras.Stock.IO.Network.Socket as IO
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Stock.String as S
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Network as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified System.Info as OS
import qualified System.IO as IO
import qualified System.Process as IO
import qualified Test.Process as H
import qualified Testnet.Conf as H
import qualified Testnet.List as L

{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant <&>" -}
{- HLINT ignore "Redundant flip" -}
{- HLINT ignore "Use head" -}

data TestnetOptions = TestnetOptions
  { numBftNodes :: Int
  , slotDuration :: Int
  , securityParam :: Int
  , nPoorAddresses :: Int
  , totalBalance :: Int
  , enableP2P :: Bool
  } deriving (Eq, Show)

defaultTestnetOptions :: TestnetOptions
defaultTestnetOptions = TestnetOptions
  { numBftNodes = 3
  , slotDuration = 2000
  , securityParam = 10
  , nPoorAddresses = 128
  , totalBalance = 8000000000000000
  , enableP2P = False
  }

replaceNodeLog :: Int -> String -> String
replaceNodeLog n s = T.unpack (T.replace "logs/node-0.log" replacement (T.pack s))
  where replacement = T.pack ("logs/node-" <> show @Int n <> ".log")

-- | Rewrite a line in the configuration file
rewriteConfiguration :: Bool -> Int -> String -> String
rewriteConfiguration _ _ "TraceBlockchainTime: False"          = "TraceBlockchainTime: True"
rewriteConfiguration _ n s | "logs/node-0.log" `L.isInfixOf` s = replaceNodeLog n s
rewriteConfiguration True _ "EnableP2P: False"                 = "EnableP2P: True"
rewriteConfiguration False _ "EnableP2P: True"                 = "EnableP2P: False"
rewriteConfiguration _ _ s                                     = s

rewriteParams :: TestnetOptions -> Value -> Value
rewriteParams testnetOptions = rewriteObject
  $ HM.insert "slotDuration" (J.toJSON @String (show @Int (slotDuration testnetOptions)))

mkTopologyConfig :: Int -> Int -> [Int]
                 -> Bool -- ^ if true use p2p topology configuration
                 -> ByteString
mkTopologyConfig i numBftNodes allPorts False = J.encode topologyNonP2P
  where
    topologyNonP2P :: NonP2P.NetworkTopology
    topologyNonP2P =
      NonP2P.RealNodeTopology
        $ flip fmap ([0 .. numBftNodes - 1] L.\\ [i])
        $ \j -> NonP2P.RemoteAddress "127.0.0.1"
                                    (fromIntegral $ allPorts L.!! j)
                                    1
mkTopologyConfig i numBftNodes allPorts True = J.encode topologyP2P
  where
    rootConfig :: P2P.RootConfig
    rootConfig =
      P2P.RootConfig
        (flip fmap ([0 .. numBftNodes - 1] L.\\ [i])
        $ \j -> RelayAccessAddress "127.0.0.1"
                            (fromIntegral $ allPorts L.!! j)
        )
        P2P.DoNotAdvertisePeer

    localRootPeerGroups :: P2P.LocalRootPeersGroups
    localRootPeerGroups =
      P2P.LocalRootPeersGroups
        [ P2P.LocalRootPeersGroup rootConfig
                                  (numBftNodes - 1)
        ]

    topologyP2P :: P2P.NetworkTopology
    topologyP2P =
      P2P.RealNodeTopology
        localRootPeerGroups
        []
        (P2P.UseLedger DontUseLedger)


testnet :: TestnetOptions -> H.Conf -> H.Integration [String]
testnet testnetOptions H.Conf {..} = do
  void $ H.note OS.os
  baseConfig <- H.noteShow $ base </> "configuration/chairman/defaults/simpleview"
  currentTime <- H.noteShowIO DTC.getCurrentTime
  startTime <- H.noteShow $ DTC.addUTCTime 15 currentTime -- 15 seconds into the future
  allPorts <- H.noteShowIO $ IO.allocateRandomPorts (numBftNodes testnetOptions)

  H.copyRewriteJsonFile
    (base </> "scripts/protocol-params.json")
    (tempAbsPath </> "protocol-params.json")
    (rewriteParams testnetOptions)

  -- Generate keys
  void $ H.execCli
    [ "byron"
    , "genesis"
    , "genesis"
    , "--genesis-output-dir", tempAbsPath </> "genesis"
    , "--start-time", showUTCTimeSeconds startTime
    , "--protocol-parameters-file", tempAbsPath </> "protocol-params.json"
    , "--k", show @Int (securityParam testnetOptions)
    , "--protocol-magic", show @Int testnetMagic
    , "--n-poor-addresses", show @Int (nPoorAddresses testnetOptions)
    , "--n-delegate-addresses", show @Int (numBftNodes testnetOptions)
    , "--total-balance", show @Int (totalBalance testnetOptions)
    , "--avvm-entry-count", "128"
    , "--avvm-entry-balance", "10000000000000"
    , "--delegate-share", "0.9"
    , "--secret-seed", "2718281828"
    ]

  H.writeFile (tempAbsPath </> "genesis/GENHASH") . S.lastLine =<< H.execCli
    [ "print-genesis-hash"
    , "--genesis-json"
    , tempAbsPath </> "genesis/genesis.json"
    ]

  let nodeIndexes = [0..numBftNodes testnetOptions - 1]
  let allNodes = fmap (\i -> "node-" <> show @Int i) nodeIndexes

  H.createDirectoryIfMissing logDir

  -- Launch cluster of three nodes in P2P Mode
  forM_ nodeIndexes $ \i -> do
    si <- H.noteShow $ show @Int i
    dbDir <- H.noteShow $ tempAbsPath </> "db/node-" <> si
    nodeStdoutFile <- H.noteTempFile tempAbsPath $ "cardano-node-" <> si <> ".stdout.log"
    nodeStderrFile <- H.noteTempFile tempAbsPath $ "cardano-node-" <> si <> ".stderr.log"
    sprocket <- H.noteShow $ Sprocket tempBaseAbsPath (socketDir </> "node-" <> si)
    portString <- H.note $ show @Int (allPorts L.!! i)
    topologyFile <- H.noteShow $ tempAbsPath </> "topology-node-" <> si <> ".json"
    configFile <- H.noteShow $ tempAbsPath </> "config-" <> si <> ".yaml"
    signingKeyFile <- H.noteShow $ tempAbsPath </> "genesis/delegate-keys.00" <> si <> ".key"
    delegationCertificateFile <- H.noteShow $ tempAbsPath </> "genesis/delegation-cert.00" <> si <> ".json"

    H.createDirectoryIfMissing dbDir
    H.createDirectoryIfMissing $ tempBaseAbsPath </> "" <> socketDir

    H.lbsWriteFile (tempAbsPath </> "topology-node-" <> si <> ".json") $
      mkTopologyConfig i (numBftNodes testnetOptions) allPorts (enableP2P testnetOptions)

    H.writeFile (tempAbsPath </> "config-" <> si <> ".yaml") . L.unlines . fmap (rewriteConfiguration (enableP2P testnetOptions) i) . L.lines =<<
      H.readFile (baseConfig </> "config-0.yaml")

    hNodeStdout <- H.openFile nodeStdoutFile IO.WriteMode
    hNodeStderr <- H.openFile nodeStderrFile IO.WriteMode

    H.diff (L.length (IO.sprocketArgumentName sprocket)) (<=) IO.maxSprocketArgumentNameLength

    void $ H.createProcess =<<
      ( H.procNode
        [ "run"
        , "--database-path", dbDir
        , "--socket-path", IO.sprocketArgumentName sprocket
        , "--port", portString
        , "--topology", topologyFile
        , "--config", configFile
        , "--signing-key", signingKeyFile
        , "--delegation-certificate", delegationCertificateFile
        , "--shutdown-ipc", "0"
        ] <&>
        ( \cp -> cp
          { IO.std_in = IO.CreatePipe
          , IO.std_out = IO.UseHandle hNodeStdout
          , IO.std_err = IO.UseHandle hNodeStderr
          , IO.cwd = Just tempBaseAbsPath
          }
        )
      )

    when (OS.os `L.elem` ["darwin", "linux"]) $ do
      H.onFailure . H.noteIO_ $ IO.readProcess "lsof" ["-iTCP:" <> portString, "-sTCP:LISTEN", "-n", "-P"] ""

  now <- H.noteShowIO DTC.getCurrentTime
  deadline <- H.noteShow $ DTC.addUTCTime 90 now

  forM_ nodeIndexes $ \i -> do
    si <- H.noteShow $ show @Int i
    sprocket <- H.noteShow $ Sprocket tempBaseAbsPath (socketDir </> "node-" <> si)
    _spocketSystemNameFile <- H.noteShow $ IO.sprocketSystemName sprocket
    H.waitByDeadlineM deadline $ H.doesSprocketExist sprocket

  forM_ nodeIndexes $ \i -> do
    si <- H.noteShow $ show @Int i
    nodeStdoutFile <- H.noteTempFile tempAbsPath $ "cardano-node-" <> si <> ".stdout.log"
    H.assertByDeadlineIO deadline $ IO.fileContains "until genesis start time at" nodeStdoutFile

  H.copyFile (tempAbsPath </> "config-1.yaml") (tempAbsPath </> "configuration.yaml")

  return allNodes
